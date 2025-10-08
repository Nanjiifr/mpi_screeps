#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
moteur_sans_oop.py
Moteur procédural corrigé pour jeu style Screeps simplifié.
Usage :
    python3 moteur_sans_oop.py [nb_joueurs=2]
"""

import sys, struct, tkinter as tk, time, random, subprocess

# ----------------------------
# CONFIGURATION
# ----------------------------
TAILLE_CASE = 20          # pixels
FPS = 2                   # tours par seconde pour l'affichage

# Valeurs par défaut des unités
VIE_BASE = 2
CAPACITE_BASE = 2
FORCE_BASE = 1
STOCK_DEBUT = 0

# ----------------------------
# UTILS
# ----------------------------
def in_bounds(w,h,x,y):
    return 0 <= x < w and 0 <= y < h

# ----------------------------
# LECTURE CARTE & DEPOTS
# ----------------------------
def read_carte_bin(filename="carte.bin"):
    with open(filename,"rb") as f:
        data = f.read()
    n = len(data)//2
    import math
    h = int(math.sqrt(n))
    w = n//h
    grid = [[0 for _ in range(w)] for _ in range(h)]
    idx = 0
    for y in range(h):
        for x in range(w):
            grid[y][x] = struct.unpack("b", data[idx:idx+2])[0]
            idx += 2
    return grid

def read_depots(filename="depots.txt"):
    depots = []
    with open(filename,"r", encoding="utf-8") as f:
        for line in f:
            line=line.strip()
            if line:
                parts = line.split()
                depots.append((int(parts[1]),int(parts[2])))
    return depots

# ----------------------------
# STRUCTURE DU JEU
# ----------------------------
def init_jeu(nb_joueurs):
    carte = read_carte_bin()
    h = len(carte)
    w = len(carte[0])
    depots = {"J"+str(i):pos for (i,pos) in enumerate(read_depots())}
    joueurs = ["J"+str(i) for i in range(nb_joueurs)]
    etats = {j:{} for j in joueurs}
    global_id = 1
    return carte, depots, joueurs, etats, global_id, w, h

# ----------------------------
# AFFICHAGE TKINTER
# ----------------------------
def draw_grid(root, carte, depots, etats, w, h):
    canvas = tk.Canvas(root, width=w*TAILLE_CASE, height=h*TAILLE_CASE, bg="white")
    canvas.pack()
    for y in range(h):
        for x in range(w):
            val = carte[y][x]
            x0 = x*TAILLE_CASE
            y0 = y*TAILLE_CASE
            x1 = x0+TAILLE_CASE
            y1 = y0+TAILLE_CASE
            if val==-1:
                canvas.create_rectangle(x0,y0,x1,y1,fill="gray20")
            elif val==-2:
                canvas.create_rectangle(x0,y0,x1,y1,fill="gold")
            elif val>0:
                green = int(255*val/10)
                color = f"#00{green:02x}00"
                canvas.create_rectangle(x0,y0,x1,y1,fill=color)
            else:
                canvas.create_rectangle(x0,y0,x1,y1,fill="white")
    for joueur,unites in etats.items():
        for uid,u in unites.items():
            ux,uy = u['x'],u['y']
            x0 = ux*TAILLE_CASE+4
            y0 = uy*TAILLE_CASE+4
            x1 = x0+TAILLE_CASE-8
            y1 = y0+TAILLE_CASE-8
            color = u.get('color','red')
            canvas.create_oval(x0,y0,x1,y1,fill=color)
            canvas.create_text(ux*TAILLE_CASE+TAILLE_CASE//2, uy*TAILLE_CASE+TAILLE_CASE//2,
                               text=str(u['id']), fill="white")
    return canvas

# ----------------------------
# LANCEMENT DES BOTS
# ----------------------------
def lancer_bot(joueur):
    bot_file = f"bot_{joueur}.py"
    try:
        subprocess.run(["python3", bot_file, joueur], check=True, timeout=1)
    except subprocess.TimeoutExpired:
        print(f"{joueur} n'a pas répondu dans le temps imparti")
    except Exception as e:
        print(f"Erreur lancement {joueur}: {e}")

# ----------------------------
# GESTION DES ETATS
# ----------------------------
# def lecture_etat(joueur):
#     etat = {}
#     try:
#         with open(f"etat_{joueur}.txt","r",encoding="utf-8") as f:
#             for line in f:
#                 parts=line.strip().split()
#                 if len(parts)>=7:
#                     uid = int(parts[0])
#                     etat[uid] = {'id':uid,'x':int(parts[1]),'y':int(parts[2]),
#                                  'vie':int(parts[3]),'capacite':int(parts[4]),
#                                  'force':int(parts[5]),'stock':int(parts[6])}
#     except FileNotFoundError:
#         pass
#     return etat

def ecrire_etat(joueur,unites, depot):
    filename = f"etat_{joueur}.txt"
    with open(filename,"w",encoding="utf-8") as f:
        f.write(f"{depot[0]} {depot[1]}\n")
        for uid in unites:
            u = unites[uid]
            f.write(f"{u['id']} {u['x']} {u['y']} {u['vie']} {u['capacite']} {u['force']} {u['stock']}\n")

# ----------------------------
# TRAITEMENT DES ORDRES
# ----------------------------
def traiter_ordres(joueur, carte, etat_joueur, etats_tous, w,h):
    filename = f"ordres_{joueur}.txt"
    try:
        with open(filename,"r",encoding="utf-8") as f:
            lines = f.readlines()
    except FileNotFoundError:
        lines = []
    for line in lines:
        line = line.strip().upper()
        if not line: continue
        parts = line.split()
        cmd = parts[0]
        
        # CREER UID VIE CAPACITE FORCE  (plus besoin de X Y)
        if cmd=="CREER" and len(parts)==5:
            uid = int(parts[1])
            vie = int(parts[2])
            cap = int(parts[3])
            force = int(parts[4])
            # placer automatiquement sur le dépôt
            # chercher le dépôt de ce joueur (id=0)
            depot_pos = None
            for y in range(h):
                for x in range(w):
                    if carte[y][x]==-2:  # dépôt
                        depot_pos = (x,y)
                        break
                if depot_pos: break
            if depot_pos:
                dx,dy = depot_pos
                # vérifier qu'aucune unité n'est déjà sur la case
                occupe = False
                for other_unites in etats_tous.values():
                    for u in other_unites.values():
                        if u['x']==dx and u['y']==dy:
                            occupe=True
                            break
                if not occupe:
                    etat_joueur[uid] = {'id':uid,'x':dx,'y':dy,'vie':vie,'capacite':cap,'force':force,'stock':0,
                                         'color':random.choice(['red','blue','green','purple','orange','pink','cyan','brown'])}

        # DEPLACER UID DX DY
        elif cmd=="DEPLACER" and len(parts)==4:
            uid=int(parts[1]); dx=int(parts[2]); dy=int(parts[3])
            if uid in etat_joueur:
                x0=etat_joueur[uid]['x']; y0=etat_joueur[uid]['y']
                x1=x0+dx; y1=y0+dy
                if in_bounds(w,h,x1,y1) and carte[y1][x1]!=-1:
                    collision=False
                    for other_j,other_unites in etats_tous.items():
                        if other_j==joueur: continue
                        for u in other_unites.values():
                            if u['x']==x1 and u['y']==y1:
                                # attaque simple
                                u['vie']-=etat_joueur[uid]['force']
                                etat_joueur[uid]['vie']-=u['force']
                                collision=True
                    if not collision or etat_joueur[uid]['vie']>0:
                        etat_joueur[uid]['x']=x1; etat_joueur[uid]['y']=y1

        # POMPER UID
        elif cmd=="POMPER" and len(parts)==2:
            uid=int(parts[1])
            if uid in etat_joueur:
                u=etat_joueur[uid]
                val = carte[u['y']][u['x']]
                if val>0 and u['stock']<u['capacite']:
                    quant = min(val, u['capacite']-u['stock'])
                    u['stock']+=quant
                    carte[u['y']][u['x']]-=quant

        # TRANSMETTRE UID1 UID2
        elif cmd=="TRANSMETTRE" and len(parts)==3:
            uid1=int(parts[1]); uid2=int(parts[2])
            if uid1 in etat_joueur:
                u1=etat_joueur[uid1]
                cible=None
                if uid2==0:
                    #TODO : Modifier pour que le dépôt soit en argumet
                    for dx in [-1,0,1]:
                        for dy in [-1,0,1]:
                            nx=u1['x']+dx; ny=u1['y']+dy
                            if in_bounds(w,h,nx,ny) and carte[ny][nx]==-2:
                                cible={'x':nx,'y':ny,'stock':0,'capacite':999}
                elif uid2 in etat_joueur: # transfert unité adjacente
                    cible=etat_joueur[uid2]
                
                # vérifier proximité
                if cible and abs(cible['x']-u1['x'])<=1 and abs(cible['y']-u1['y'])<=1:
                    q=min(u1['stock'], cible['capacite']-cible['stock'])
                    u1['stock']-=q
                    cible['stock']+=q

        # ATTENDRE : rien à faire
    return etat_joueur

# ----------------------------
# BOUCLE PRINCIPALE
# ----------------------------
def boucle_principale(nb_joueurs):
    carte, depots, joueurs, etats, global_id, w,h = init_jeu(nb_joueurs)
    root = tk.Tk()
    root.title("Moteur Screeps simplifié")
    canvas = draw_grid(root, carte, depots, etats, w, h)
    root.update()
    tour=0
    while True:
        # lancer bots et lire état
        for j in joueurs:
            ecrire_etat(j, etats[j], depots[j])
            lancer_bot(j)
            etats[j] = traiter_ordres(j, carte, etats[j], etats, w,h)
            ecrire_etat(j, etats[j], depots[j])

            
        # redraw
        canvas.destroy()
        canvas = draw_grid(root, carte, depots, etats, w, h)
        root.update()
        tour+=1
        print(f"Tour {tour} exécuté")
        time.sleep(1/FPS)

# ----------------------------
# MAIN
# ----------------------------
if __name__=="__main__":
    args = sys.argv[1:]
    nb_j = int(args[0]) if len(args)>=1 else 2
    if nb_j not in (2,4,8):
        print("nb_joueurs doit être 2,4 ou 8")
        sys.exit(1)
    boucle_principale(nb_j)
