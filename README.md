# mpi_screeps (draft)

Les caractéristiques des unités : VIE, CAPACITE, FORCE

Impacts :
- lors d'une attaque, la FORCE de l'un est soustraite à la VIE de l'autre et réciproquement
- la CAPACITE permet de charger plus de ressource

valeurs par défaut (bonus) : VIE : 2 (+2/pt), CAPACITE : 2 (+2/pt), FORCE : 1 (+1/pt)

Chaque unité peut :
- se déplacer
- pomper une unité de ressource (si son stock est inférieur à sa capacité)
- transmettre son stock à une unité voisine (ou au dépôt) (min(stock unité, capacite voisine - stock voisine))
- 
Une tentative de déplacement vers une unité ennemie voisine est interprétée comme une attaque. Le déplacement a lieu si l'unité ennemie meurt au cours de l'attaque.

Les unités pop sur le dépôt. Une seule unité possible dans chaque case.

Chaque unité a un identifiant unique qui ne change pas au cours de la partie.
On commence avec 20 ressources

Il faut éviter que la stratégie la plus efficace soit de rush l'adversaire en début de partie pour ne pas tuer le jeu.

Dimensions de la map? map torique? nombre de joueurs?
Quelle structure pour faciliter la lecture de l'entrée/traitement des données?

Les cases de la map contiennent entre 0 et 10 unité de ressources.

L'objectif est de rendre viables différentes stratégies. Notamment des couloirs d'unités immobiles qui transmettent leur stock jusqu'au dépôt. Des stratégies de défense avec des unités WALL qui encercle ces couloirs, des sentinelles 

Un tour de jeu d'un joueur consiste en :
1) ordre de créer une unité avec 3 nombres indiquant les points d'améliorations sur les caractéristiques
2) une liste d'ordre pour chaque unité, données dans un ordre au choix du joueur et exécutées dans cet ordre

J'ai essayé avec ChatGPT de faire un début. La génération de la map est plutôt pas mal avec gen_carte.py. L'affichage dans le moteur fonctionne correctement mais par contre soit les bots, soit le moteur ne fonctionne pas correctement. Je n'ai pas débuggé davantage. C'était plus pour avoir une idée de ce à quoi ça pourrait ressembler. Peut-être que certaines choses sont réutilisables.