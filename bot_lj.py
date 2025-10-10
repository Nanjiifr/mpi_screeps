#! /usr/bin/python3
from random import choice

CASE_RESS = 0
CASE_TYPE = 1
CASE_SPEC = 2

TYPE_MUR = 'W'
TYPE_NORMAL = 'R'
TYPE_DASH = 'D'
TYPE_SHIELD = 'S'
TYPE_FORCE = 'F'
TYPE_MIDAS = 'M'
TYPE_VITESSE = 'P'

MINION_PROP = 0
MINION_X = 1
MINION_Y = 2
MINION_CARG = 3
MINION_HP = 4
MINION_CARG_MAX = 5
MINION_ATTAQUE = 6

with open("mapData.txt") as mapFile:
    
    n = int(mapFile.readline())

    carte = [[None]*n for i in range(n)]
    for i in range(n):
        ligne = mapFile.readline()
        for j,case in enumerate(ligne.strip().split()):
            info = case.split(",")
            carte[i][j] = int(info[1])

    m = int(mapFile.readline())
    minions = []
    
    minionsJoueurs = {}
    for i in range(m):
        *minion, = map(int, mapFile.readline().strip().split(","))
        if minion[MINION_PROP] in minionsJoueurs:
            minionsJoueurs[minion[MINION_PROP]].append(minion[1:])
        else:
            minionsJoueurs[minion[MINION_PROP]]=[minion[1:]]
        
    id, ressActuelles = map(int, mapFile.readline().strip().split())
    depotX, depotY = map(int, mapFile.readline().strip().split())

    if id in minionsJoueurs:
        mesMinions = minionsJoueurs[id]
    else:
        mesMinions = []
    with open("answer.txt", "w") as reponse:
        for mX, mY, mCarg, mHp, mCargMax, mForce in mesMinions:
            if mCargMax > 0: # pas un soldat
                if mCarg == mCargMax:
                # Retour en direction du dépôt
                    cands = []
                    if mX < depotX:
                        cands.append([mX+1, mY])
                    elif mx > depotX:
                        cands.append([mX-1, mY])
                    if mY < depotY:
                        cands.append([mX, mY+1])
                    elif mY > depotY:
                        cands.append([mX, mY-1])
                    newX, newY = choice(cands)
                    print(mX, mY, newX, newY, file=reponse)
                else:
                    if carte[mX][mY]>0:
                        print(mX, mY, mX, mY, file=reponse) # Pomper
                    else:
                        cands = [(mX-1, mY), (mX+1, mY), (mX, mY-1), (mX, mY+1)]
                        newX, newY = max(zip(map(cands,lambda c:carte[c[0]][c[1]]), cands))[1]
                        print(mX, mY, newX, newY, file=reponse) # Se déplacer
            
        print("CREATE", 1, min(10, ressActuelles-1), 0, file=reponse)