import sys
import tkinter as tk
import tkinter.font
import struct, time, random, subprocess, os

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
    assert(N_PLAYERS>0)
except:
    print("Usage : python3 run___.py <mapName> <maxTurns> <players>",file=sys.stderr)
    assert 0
map = []    
''' map = (string * int * int | int array) array array --> (tileName, resource, tileMeta) '''

# graphics constant
WIDTH=1000
HEIGHT=800
TILE_SIZE=50
SP_OFFSET=TILE_SIZE/6
MN_OFFSET=TILE_SIZE/5
PLAYER_COLOR = ["#dd0000", "#dddd00", "#00dd00", "#0000dd"]
DT=0.5

# player-related constants
PLAYER_RSCS = [20 for i in range(N_PLAYERS)]
PLAYER_SCORE = [0 for i in range(N_PLAYERS)]
PLAYER_SPAWN = []
PLAYER_MINIONS=[{} for _ in range(N_PLAYERS)]
''' ((x,y > cap,hp,maxCap,atk) dict) array '''
PLAYER_NAMES = []
for p in range(N_PLAYERS):
    pName = sys.argv[3+p]
    if(pName[0] != '.'):
        pName = "python3 "+pName
    PLAYER_NAMES.append(pName)

random.shuffle(PLAYER_NAMES)     # random starting points

# log file (useful for debugging)
logFile=open("logFile.txt","w")

# check if the arguments (ie the player functions) are valid
for pname in PLAYER_NAMES:
    assert(
        len(pname) >= 3 and (
            pname[0] == '.' and pname[1] == "/"     # executable file
        ) or (
            pname[-1] == 'y' and pname[-2] == 'p' and pname[-3] == '.'      # python file
        )
    )

# writes data
def write_player_data(pl_i):
    f=open("mapData.txt", "w")

    # map
    print(MAPLEN,file=f)
    for line in range(MAPLEN):
        for col in range(MAPLEN):
            (spec,rsc,meta)=map[line][col]
            if(spec=="RESO"):
                print("R,",file=f,end="")
            elif(spec=="WALL"):
                print("W,",file=f,end="")
            elif(spec=="DASH"):
                print("D,",file=f,end="")
            elif(spec=="PROT"):
                print("S,",file=f,end="")
            elif(spec=="BONK"):
                print("F,",file=f,end="")
            elif(spec=="GOLD"):
                print("M,",file=f,end="")
            elif(spec=="SPED"):
                print("P,",file=f,end="")
            else:
                print(f"ERROR : unrecognized tile type ({spec})",file=sys.stderr)
                assert 0

            print(f"{rsc},{meta}",file=f,end=(" " if col != MAPLEN-1 else ""))
        print("",file=f)

    # players
    nMinions=0
    for pl in range(N_PLAYERS):
        playerMinions=PLAYER_MINIONS[pl]
        nMinions += len(playerMinions)

    print(nMinions,file=f)

    for pl in range(N_PLAYERS):
        for (mX,mY),(mCar,mHp,mSize,mAtk) in PLAYER_MINIONS[pl].items():
            print(f"{pl},{mX},{mY},{mCar},{mHp},{mSize},{mAtk}",file=f)

    # current player + your resources
    print(pl_i, PLAYER_RSCS[pl_i],file=f)

    f.close()

# checks if the minion is attacking (can be optimized, but future me will have to deal with this)
def targetMinionData(pl_i,xdest,ydest):
    for p in range(N_PLAYERS):
        if(p != pl_i):
            if((xdest,ydest) in PLAYER_MINIONS[p].keys()):
                return p
    return -1

def read_player_data(pl_i):
    minionMoved={}      # to avoid moving the same minion mutiple times
    with open("answer.txt", "r") as file:
        for line in file:
            print(f"{[pl_i]}",file=logFile,end=" ")
            # create new minion
            if(line[0] == "C"):
                dat=line.split(" ")
                _,hp,size,atk=dat
                hp,size,atk=int(hp),int(size),int(atk)
                if(not (PLAYER_SPAWN[pl_i] in PLAYER_MINIONS[pl_i].keys())):   # check if spawn is free
                    if(PLAYER_RSCS[pl_i] >= 2*hp + 2*size + atk):       # check is the player has enough resources
                        PLAYER_MINIONS[pl_i][PLAYER_SPAWN[pl_i]] = [0,hp,size,atk]
                        PLAYER_RSCS[pl_i] -= 2*hp + 2*size + atk
                        minionMoved[PLAYER_SPAWN[pl_i]]=True
                        print(f"NEW_MINION {hp},{size},{atk}",file=logFile,end="\n")
                    else:
                        print(f"NEW_MINION_BROKE {hp},{size},{atk}",file=logFile,end="\n")
                else:
                    print(f"NEW_MINION_FAIL {hp},{size},{atk}",file=logFile,end="\n")

            # move existing minion
            else:
                dat=line.split(" ")
                x,y,xdest,ydest=dat
                x,y,xdest,ydest=int(x),int(y),int(xdest),int(ydest)
                # have to do this because for some reason list(map(int,line.split(" "))) throws an error 'list is not callable
                
                # check if the x,y coords is a minion
                if((not ((x,y) in minionMoved.keys())) and (x,y) in PLAYER_MINIONS[pl_i].keys()):
                    minData=PLAYER_MINIONS[pl_i][(x,y)]
                    if(xdest==x and ydest==y):
                        # pump
                        tile=map[x][y]
                        if(tile[1] > 0 and minData[0] < minData[2]):
                            tile[1] -= 1
                            minData[0] += 1
                            minionMoved[(x,y)]=True
                            print(f"PUMP {x},{y}",file=logFile,end="\n")
                        else:
                            print(f"PUMP_EMPTY {x},{y}",file=logFile,end="\n")

                    elif(abs(xdest-x)+abs(ydest-y)==1):
                        # movement-based action
                        # move into the depot
                        if((xdest,ydest) == PLAYER_SPAWN[pl_i]):
                            print(f"DEPOSIT {x},{y},{minData[0]}",file=logFile,end="\n")
                            PLAYER_SCORE[pl_i] += minData[0]
                            PLAYER_RSCS[pl_i] += minData[0]
                            minionMoved[(x,y)]=True
                            minData[0] = 0

                        # moving into a friendly minion
                        elif((xdest,ydest) in PLAYER_MINIONS[pl_i].keys()):
                            minTarget=PLAYER_MINIONS[pl_i][(xdest,ydest)]
                            toTransfer=min(minData[0],minTarget[2]-minTarget[0])
                            minData[0] -= toTransfer
                            minTarget[0] += toTransfer
                            minionMoved[(x,y)]=True
                            print(f"TRANSFER {x},{y},{xdest},{ydest},{toTransfer}",file=logFile,end="\n")

                        else:
                            # empty space
                            if(targetMinionData(pl_i,xdest,ydest) == -1):
                                if(map[xdest][ydest][0] != "WALL"): # not moving into a wall
                                    minCpy = [minData[0],minData[1],minData[2],minData[3]]
                                    del PLAYER_MINIONS[pl_i][(x,y)]
                                    PLAYER_MINIONS[pl_i][(xdest,ydest)] = minCpy
                                    minionMoved[(xdest,ydest)]=True
                                    print(f"MOVE {x},{y},{xdest},{ydest}",file=logFile,end="\n")
                                else:
                                    print(f"BONKED {x},{y},{xdest},{ydest}",file=logFile,end="\n")

                            # attack
                            else:
                                minionMoved[(x,y)]=True # depends on kill
                                print(f"ATTACK {x},{y},{xdest},{ydest}",file=logFile,end="\n")

                    else:
                        # invalid
                        print(f"INVALID_MOVE {x},{y},{xdest},{ydest}",file=logFile,end="\n")
                
                else:
                    # invalid
                    print(f"INVALID_MINION {x},{y},{xdest},{ydest}",file=logFile,end="\n")

# plays the turn of a player
def execute_player(pl_i):
    if(pl_i < 0):
        print(f"ERROR : trying to play negative player {pl_i}",file=sys.stderr)

    if(pl_i >= N_PLAYERS):
        print(f"ERROR : trying to play overflowing player {pl_i}",file=sys.stderr)
    
    pname = PLAYER_NAMES[pl_i]
    try:
        write_player_data(pl_i)
    except:
        print("Error while writing.",file=sys.stderr)

    execGood=False
    try:
        os.system(pname)
        execGood=True
    except:
        print(f"Error while executing player {pl_i}'s code ({PLAYER_NAMES[pl_i]}).",file=sys.stderr)

    if(execGood):
        try:
            read_player_data(pl_i)
        except:
            print(f"Error while reading data for player {pl_i}.",file=sys.stderr)

# remove all players with HP<=0
def killDeadMinions():
    for minionList in PLAYER_MINIONS:
        for (x,y),(cap,hp,_,_) in minionList.items():
            if(hp == 0):
                map[x][y][1] += cap
                del minionList[x,y]

# .isdigit() but for relative numbers as well
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
iii=0
with open(MAPNAME) as file:
    fst=True
    for line in file:
        line=line[:-1]      # remove annoying \n at the end
        if(fst):
            # initialize everything
            fst = False
            MAPLEN = int(line)
            PLAYER_SPAWN = [(2, 2), (MAPLEN-1-2, MAPLEN-1-2), (2, MAPLEN-1-2), (MAPLEN-1-2, 2)]
            PLAYER_SPAWN = PLAYER_SPAWN[:N_PLAYERS]     # only keeping players
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

# text fonts for tkinter
DIG_FONT = ""
MIN_FONT = ""

# self explainatory
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
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),font=DIG_FONT)
            elif(data[0]=="SPED"):
                canvas.create_rectangle(x0+SP_OFFSET,y0+SP_OFFSET,x1-SP_OFFSET,y1-SP_OFFSET,fill="#25ffff")
                canvas.create_text((x0+x1)//2,(y0+y1)//2,text=str(data[2]),font=DIG_FONT)

    for i in range(N_PLAYERS):
        minionList=PLAYER_MINIONS[i]
        for (x,y),(cap,hp,size,atk) in minionList.items():
            x0 = x*TILE_SIZE
            y0 = y*TILE_SIZE
            x1 = x0+TILE_SIZE
            y1 = y0+TILE_SIZE

            canvas.create_rectangle(x0+MN_OFFSET,y0+MN_OFFSET,x1-MN_OFFSET,y1-MN_OFFSET, fill=PLAYER_COLOR[i])
            canvas.create_text((x0+x1)//2, (y0+y1)//2, text=str(cap),font=MIN_FONT)
    
    return canvas

turnOrder = [i for i in range(N_PLAYERS)]
def mainLoop():
    global DIG_FONT
    global MIN_FONT
    root = tk.Tk()
    DIG_FONT = tk.font.Font(family = "Symbol", size = 24)
    MIN_FONT = tk.font.Font(family = "monospace", size = 20)
    root.title("Moteur Screeps simplifié")
    canvas = drawMap(root)
    root.update()
    while(1):
        # turn order is randomized
        random.shuffle(turnOrder)
        for p in turnOrder:
            execute_player(p)

        # kill
        killDeadMinions()

        # log the end of the turn + update the canvas
        print("",file=logFile)
        canvas.destroy()
        canvas = drawMap(root)
        root.update()

        # sleep
        time.sleep(DT)

    logFile.close()

mainLoop()