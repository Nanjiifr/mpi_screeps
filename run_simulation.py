import sys
import tkinter as tk
import tkinter.font
import struct, time, random, subprocess

'''
argument 1    : map filename
argument 2    : max number of turns
arguments 3~N : players (au nombre de N-2)
'''

# global constants
try:
    MAPNAME   = sys.argv[1]
    MAX_TURNS = sys.argv[2]
    N_PLAYERS = len(sys.argv)-3
    MAPLEN    = 0
    assert(N_PLAYERS>=0)
except:
    print("Usage : python3 run___.py <mapName> <maxTurns> <players>",file=sys.stderr)
    assert 0

# graphics constant
WIDTH=1000
HEIGHT=800
TILE_SIZE=50
SP_OFFSET=TILE_SIZE/6

def isRelative(str):
    return (str.isdigit() or (len(str)>0 and str[0]=="-" and str[1:].isdigit()))

# useful parsing functions
def toTileName(str):
    if(isRelative(str)):
        if(int(str)>=0):
            return "RESO"
        else:
            return "WALL"
    elif(str == "D"):
        return "DASH"
    elif(str == "S"):
        return "PROT"
    elif(str == "F"):
        return "BONK"
    elif(str == "M"):
        return "GOLD"
    elif(str == "P"):
        return "SPED"
    else:
        print(str + ": unrecognized tile.",file=sys.stderr)
        assert 0

# parsing the data
# map = (string * int * int | int array) array array --> (tileName, resource, tileMeta)
map = []
iii=0
with open(MAPNAME) as file:
    fst=True
    for line in file:
        line=line[:-1]      # remove annoying \n at the end
        if(fst):
            # initialize everything
            fst = False
            MAPLEN = int(line)
            map = [["X" for _ in range(MAPLEN)] for _ in range(MAPLEN)]
        else:
            comp=line.split(" ")
            print(comp)
            for jjj in range(len(comp)):
                data=comp[jjj].split(",")
                tType=(data[0] if len(data)==1 else data[1])
                if(isRelative(tType)):
                    map[iii][jjj] = (toTileName(data[0]),max(0,int(data[0])),0)
                else:
                    map[iii][jjj] = (toTileName(data[1]),int(data[0]),int(data[2]))
            iii+=1

for line in map:
    print(line)

# function to avoid falling off the map
def areValid(i,j):
    return 0 <= i < MAPLEN and 0 <= j < MAPLEN

DIG_FONT = ""
def drawMap(root):
    canvas = tk.Canvas(root, width=MAPLEN*TILE_SIZE, height=MAPLEN*TILE_SIZE, bg="white")
    canvas.pack()
    for y in range(MAPLEN):
        for x in range(MAPLEN):
            data=map[y][x]
            val = data[1]
            x0 = x*TILE_SIZE
            y0 = y*TILE_SIZE
            x1 = x0+TILE_SIZE
            y1 = y0+TILE_SIZE
            if data[0]=="WALL":
                # wall
                canvas.create_rectangle(x0,y0,x1,y1,fill="gray20")
            elif val>0:
                green = int(255*val/10)
                color = f"#00{green:02x}00"
                canvas.create_rectangle(x0,y0,x1,y1,fill=color)
            else:
                canvas.create_rectangle(x0,y0,x1,y1,fill="white")

            # special tiles
            if(data[0]=="DASH"):
                canvas.create_rectangle(x0+SP_OFFSET,y0+SP_OFFSET,x1-SP_OFFSET,y1-SP_OFFSET,fill="#bb11bb")
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),fill="#ffffff",font=DIG_FONT)
            elif(data[0]=="PROT"):
                canvas.create_rectangle(x0+SP_OFFSET,y0+SP_OFFSET,x1-SP_OFFSET,y1-SP_OFFSET,fill="#0000ff")
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),fill="#ffffff",font=DIG_FONT)
            elif(data[0]=="BONK"):
                canvas.create_rectangle(x0+SP_OFFSET,y0+SP_OFFSET,x1-SP_OFFSET,y1-SP_OFFSET,fill="#ff0000")
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),font=DIG_FONT)
            elif(data[0]=="GOLD"):
                canvas.create_rectangle(x0+SP_OFFSET,y0+SP_OFFSET,x1-SP_OFFSET,y1-SP_OFFSET,fill="#c0c001")
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),width=1000,font=DIG_FONT)
            elif(data[0]=="SPED"):
                canvas.create_rectangle(x0+SP_OFFSET,y0+SP_OFFSET,x1-SP_OFFSET,y1-SP_OFFSET,fill="#25ffff")
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),font=DIG_FONT)

    # TODO #
    '''for joueur,unites in etats.items():
        for uid,u in unites.items():
            ux,uy = u['x'],u['y']
            x0 = ux*TILE_SIZE+4
            y0 = uy*TILE_SIZE+4
            x1 = x0+TILE_SIZE-8
            y1 = y0+TILE_SIZE-8
            color = u.get('color','red')
            canvas.create_oval(x0,y0,x1,y1,fill=color)
            canvas.create_text(ux*TAILLE_CASE+TAILLE_CASE//2, uy*TAILLE_CASE+TAILLE_CASE//2,
                               text=str(u['id']), fill="white")'''
    return canvas

def mainLoop():
    global DIG_FONT
    root = tk.Tk()
    DIG_FONT = tk.font.Font(family = "Symbol", size = 24)
    root.title("Moteur Screeps simplifié")
    canvas = drawMap(root)
    root.update()
    while(1):
        pass

mainLoop()