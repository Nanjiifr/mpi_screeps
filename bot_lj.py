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
    
    mimionsJoueurs = {}
    for i in range(m):
        *minion, = map(int, mapFile.readline().strip().split(","))
        if minion[MINION_PROP] in minionsJoueurs:
            minionsJoueurs[minion[MINION_PROP]].append(minion)
        else:
            minionsJoueurs[minion[MINION_PROP]]=[minion]
        
    id, rsc = map(int, mapFile.readline().strip().split())

    