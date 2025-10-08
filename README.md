# mpi_screeps (draft)

Les caractéristiques des unités : VIE, CAPACITE, FORCE

Impacts :
- lors d'une attaque, la FORCE de l'un est soustraite à la VIE de l'autre et réciproquement
- la CAPACITE permet de charger plus de ressource

valeurs par défaut (bonus) : VIE : 2 (+2/pt), CAPACITE : 2 (+2/pt), FORCE : 1 (+1/pt), PORTEE : 1 (+1/pt)

Chaque unité peut :
- se déplacer
- pomper une unité de ressource
- transmettre son stock à une unité voisine (ou au dépôt)

Une tentative de déplacement vers une unité ennemie voisine est interprétée comme une attaque. Le déplacement a lieu si l'unité ennemie meurt au cours de l'attaque.

Chaque unité a un identifiant unique qui ne change pas au cours de la partie.
On commence avec 20 ressources

Il faut éviter que la stratégie la plus efficace soit de rush l'adversaire en début de partie pour ne pas tuer le jeu.

Dimensions de la map? map torique? nombre de joueurs?
Quelle structure pour faciliter la lecture de l'entrée/traitement des données?
