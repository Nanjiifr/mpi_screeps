#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
gen_carte.py
Générateur procédural de carte symétrique avec murs compacts (îlots) et ressources.
Usage :
    python3 gen_carte.py [nb_joueurs=2] [largeur=30] [hauteur=30] [seed=None]
Exemples :
    python3 gen_carte.py 2 30 30 42
Sortie :
    - carte.bin : int8, ligne par ligne (valeurs -1 murs, -2 dépôts, 0..10 ressources)
    - carte.txt : version lisible pour vérification
Notes :
    - nb_joueurs doit être 2, 4 ou 8 (8 travaille mieux si la carte est carrée)
    - symétrie par réflexion (miroir) : 2 -> miroir vertical, 4 -> miroir horiz+vert, 8 -> idem 4 (avec dépôts placés 8 positions)
"""

import sys, random, math, struct, collections

def parse_args():
    args = sys.argv[1:]
    nb = int(args[0]) if len(args) >= 1 else 4
    w = int(args[1]) if len(args) >= 2 else 30
    h = int(args[2]) if len(args) >= 3 else 30
    seed = int(args[3]) if len(args) >= 4 else None
    return nb, w, h, seed

def make_empty_map(w, h, fill=0):
    return [[fill for _ in range(w)] for __ in range(h)]

def in_bounds(w,h,x,y):
    return 0 <= x < w and 0 <= y < h

def mirror_map(m, nb_players):
    # returns a new full-size map mirrored according to nb_players
    h = len(m); w = len(m[0])
    # if m already full-size (we assume it's full-size), we'll apply mirroring on quadrants
    # We assume input m is full-size but only a seed quadrant may have been written; nevertheless
    # We'll produce full map by mirroring quadrants based on central axes.
    full = make_empty_map(w,h,0)
    for y in range(h):
        for x in range(w):
            full[y][x] = m[y][x]
    # if nb_players == 2 -> vertical mirror (left-right)
    if nb_players == 2:
        for y in range(h):
            for x in range(w//2):
                left = full[y][x]
                right_x = w-1-x
                full[y][right_x] = left
    elif nb_players == 4:
        # mirror left-right then top-bottom to create 4-quadrant symmetry
        for y in range(h):
            for x in range(w//2):
                full[y][w-1-x] = full[y][x]
        for y in range(h//2):
            for x in range(w):
                full[h-1-y][x] = full[y][x]
    elif nb_players == 8:
        # create 4-fold as above then additionally mirror across main diagonal-ish by copying transpose blocks when square
        for y in range(h):
            for x in range(w//2):
                full[y][w-1-x] = full[y][x]
        for y in range(h//2):
            for x in range(w):
                full[h-1-y][x] = full[y][x]
        # if square, also copy transpose of quadrants to increase symmetry (helps 8 positions)
        if w == h:
            for y in range(h):
                for x in range(w):
                    full[x][y] = full[y][x]
    else:
        raise ValueError("nb_players must be 2,4 or 8")
    return full

def place_wall_seeds(w,h,seed_count, rng):
    seeds = []
    for _ in range(seed_count):
        x = rng.randrange(0, w)
        y = rng.randrange(0, h)
        seeds.append((x,y))
    return seeds

def grow_walls(grid, seeds, rng, grow_prob=0.7, max_iter=5):
    # compact islands: for each seed do a local random growth (prob decreases with distance)
    h = len(grid); w = len(grid[0])
    for (sx,sy) in seeds:
        frontier = collections.deque()
        frontier.append((sx,sy,0))
        while frontier:
            x,y,d = frontier.popleft()
            if not in_bounds(w,h,x,y): continue
            # probability to place wall decreases with distance d (but stays reasonably high)
            p = grow_prob * (0.85 ** d)
            if rng.random() < p:
                grid[y][x] = -1
                # push neighbors with incremented distance up to max_iter
                if d < max_iter:
                    for dx,dy in [(-1,0),(1,0),(0,-1),(0,1),(-1,-1),(1,-1),(-1,1),(1,1)]:
                        nx,ny = x+dx, y+dy
                        if in_bounds(w,h,nx,ny) and grid[ny][nx] != -1:
                            # random chance to expand to neighbor
                            if rng.random() < 0.9:
                                frontier.append((nx,ny,d+1))
    return grid

def add_resource_noise(grid, rng, density=0.7, max_resource=10):
    h = len(grid); w = len(grid[0])
    for y in range(h):
        for x in range(w):
            if grid[y][x] == -1:
                continue
            # with some chance be zero, otherwise small value biased to low numbers
            if rng.random() > density:
                grid[y][x] = 0
            else:
                # bias by squaring random -> skew toward small values
                v = int((rng.random()**1.8) * max_resource + 0.5)
                grid[y][x] = min(max_resource, max(0, v))
    return grid

def blur_resources(grid, rng, radius=1):
    # simple local average blur to smooth the resource field
    h = len(grid); w = len(grid[0])
    out = [[grid[y][x] for x in range(w)] for y in range(h)]
    for y in range(h):
        for x in range(w):
            if grid[y][x] == -1:
                continue
            s = 0; n = 0
            for dy in range(-radius, radius+1):
                for dx in range(-radius, radius+1):
                    nx, ny = x+dx, y+dy
                    if in_bounds(w,h,nx,ny) and grid[ny][nx] != -1:
                        s += grid[ny][nx]; n += 1
            if n > 0:
                out[y][x] = int(round(s / n))
    return out

def carve_passages(grid, rng, target_open_fraction=0.75):
    # ensure there are navigable passages: if walls too dense, erode some
    h = len(grid); w = len(grid[0])
    total = w*h
    wall_count = sum(1 for y in range(h) for x in range(w) if grid[y][x] == -1)
    current_open_frac = 1.0 - wall_count / total
    if current_open_frac < target_open_fraction:
        # remove some walls randomly but prefer isolated walls (to keep compact islands)
        walls = [(x,y) for y in range(h) for x in range(w) if grid[y][x] == -1]
        rng.shuffle(walls)
        remove_needed = int((target_open_fraction - current_open_frac) * total)
        removed = 0
        for (x,y) in walls:
            if removed >= remove_needed: break
            # only remove if it doesn't break compactness: check neighbors count
            neighbors = sum(1 for dx,dy in [(-1,0),(1,0),(0,-1),(0,1),(-1,-1),(1,-1),(-1,1),(1,1)]
                            if in_bounds(w,h,x+dx,y+dy) and grid[y+dy][x+dx] == -1)
            if neighbors <= 3:
                grid[y][x] = 0
                removed += 1
    return grid

def find_depot_positions(w,h, nb_players, grid, rng):
    # choose symmetric depot positions: for 2 -> left and right middle; for 4 -> four quadrants centers;
    # for 8 -> positions around midpoints of edges and corners where available.
    positions = []
    if nb_players == 2:
        candidates = [(1, h//2), (w-2, h//2)]
    elif nb_players == 4:
        candidates = [(1,1), (w-2,1), (1,h-2), (w-2,h-2)]
    elif nb_players == 8:
        # prefer eight positions: near corners and mid-edges
        candidates = [
            (1,1), (w//2,1), (w-2,1),
            (1,h//2),           (w-2,h//2),
            (1,h-2), (w//2,h-2), (w-2,h-2)
        ]
        # if map small, fallback to 4
        if w < 10 or h < 10:
            candidates = [(1,1), (w-2,1), (1,h-2), (w-2,h-2)]
    else:
        raise ValueError("nb players must be 2,4 or 8")
    # for each candidate, if it's a wall, search nearby free cell (BFS)
    def nearest_free(cx,cy):
        if grid[cy][cx] != -1:
            return (cx,cy)
        # BFS search
        q = collections.deque()
        q.append((cx,cy,0))
        seen = {(cx,cy)}
        while q:
            x,y,d = q.popleft()
            for dx,dy in [(-1,0),(1,0),(0,-1),(0,1),(-1,-1),(1,-1),(-1,1),(1,1)]:
                nx,ny = x+dx, y+dy
                if not in_bounds(w,h,nx,ny): continue
                if (nx,ny) in seen: continue
                seen.add((nx,ny))
                if grid[ny][nx] != -1:
                    return (nx,ny)
                q.append((nx,ny,d+1))
        return None
    for c in candidates[:nb_players]:
        pos = nearest_free(c[0], c[1])
        if pos is None:
            # fallback to random free
            free = [(x,y) for y in range(h) for x in range(w) if grid[y][x] != -1]
            if not free:
                raise RuntimeError("no free cell for depot")
            pos = rng.choice(free)
        positions.append(pos)
    # now enforce symmetry on depot list: if nb_players == 2 reflect, etc.
    # We'll generate full symmetric list from the first positions appropriately when writing to map.
    return positions

def write_carte_bin(grid, filename="carte.bin"):
    h = len(grid); w = len(grid[0])
    with open(filename, "wb") as f:
        for y in range(h):
            for x in range(w):
                v = int(grid[y][x])
                f.write(struct.pack("b", v))
    print("Wrote", filename)

def write_carte_txt(grid, depots=None, filename="carte.txt"):
    h = len(grid); w = len(grid[0])
    with open(filename, "w", encoding="utf-8") as f:
        for y in range(h):
            row = ""
            for x in range(w):
                v = grid[y][x]
                if depots and (x,y) in depots:
                    row += " D"
                elif v == -1:
                    row += "██"
                elif v == 0:
                    row += "  "
                else:
                    # small number display
                    row += f"{v:2d}"
            f.write(row + "\n")
    print("Wrote", filename)

def generate(nb_players=2, w=30, h=30, seed=None, wall_density_target=0.20):
    rng = random.Random(seed)
    # start with empty grid
    grid = make_empty_map(w,h,0)
    # determine seed count proportional to size to get compact islands
    seed_count = max(3, (w*h) // 200)
    seeds = place_wall_seeds(w, h, seed_count, rng)
    # grow walls on the base grid (we'll mirror after creating)
    grid = grow_walls(grid, seeds, rng, grow_prob=0.9, max_iter=3)
    # apply a mild erosion to approach target density
    # compute current wall fraction and randomly add/remove small clusters until near target
    h_total = w*h
    current_wall = sum(1 for y in range(h) for x in range(w) if grid[y][x] == -1)
    current_frac = current_wall / h_total
    # If too few walls, add some by seeding additional micro-islands
    tries = 0
    while current_frac < wall_density_target and tries < 50:
        # new small seed
        sx = rng.randrange(0, w); sy = rng.randrange(0, h)
        grid = grow_walls(grid, [(sx,sy)], rng, grow_prob=0.85, max_iter=2)
        current_wall = sum(1 for y in range(h) for x in range(w) if grid[y][x] == -1)
        current_frac = current_wall / h_total
        tries += 1
    # If too many walls, erode some
    if current_frac > wall_density_target * 1.5:
        grid = carve_passages(grid, rng, target_open_fraction=1.0 - wall_density_target)
    # Now add resources on non-wall cells
    grid = add_resource_noise(grid, rng, density=0.75, max_resource=10)
    grid = blur_resources(grid, rng, radius=1)
    # Apply symmetry by reflection
    grid = mirror_map(grid, nb_players)
    # find depot positions based on unsymmetrized candidates then reflect to place full symmetrical depots
    base_depots = find_depot_positions(w,h, nb_players, grid, rng)
    # Build full depot list symmetric to nb_players by reflecting base_depots appropriately
    depots = []
    if nb_players == 2:
        # two positions: we'll reflect horizontally left->right if necessary
        for (x,y) in base_depots[:2]:
            depots.append((x,y))
    elif nb_players == 4:
        depots = base_depots[:4]
    elif nb_players == 8:
        depots = base_depots[:8]
    # mark depots in grid as -2 (but keep deposit cell resources to 0)
    for (x,y) in depots:
        grid[y][x] = -2
    # final pass: ensure depots are not on walls (already ensured) and print stats
    wall_count = sum(1 for y in range(h) for x in range(w) if grid[y][x] == -1)
    print(f"Generated carte {w}x{h}, joueurs={nb_players}, walls={wall_count} ({wall_count/(w*h):.2%})")
    return grid, depots

def main():
    nb, w, h, seed = parse_args()
    if nb not in (2,4,8):
        print("nb_joueurs must be 2,4 or 8")
        return
    if seed is None:
        seed = random.randrange(0,2**30)
    print("Seed:", seed)
    grid, depots = generate(nb, w, h, seed, wall_density_target=0.20)
    write_carte_bin(grid, filename="carte.bin")
    write_carte_txt(grid, depots=set(depots), filename="carte.txt")
    # also write a short depot file for the engine convenience
    with open("depots.txt", "w", encoding="utf-8") as f:
        for i,(x,y) in enumerate(depots):
            f.write(f"{i} {x} {y}\n")
    print("Depots:", depots)

if __name__ == "__main__":
    main()
