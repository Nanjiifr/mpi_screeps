#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
bot_recup.py
Bot qui crée sa première unité directement sur le dépôt, collecte les ressources et les renvoie au dépôt (id 0).
"""

import sys, os, random

J = sys.argv[1] if len(sys.argv) > 1 else "J0"

def lire_etat():
    etat = {}
    try:
        with open(f"etat_{J}.txt","r",encoding="utf-8") as f:
            for line in f:
                parts = line.strip().split()
                if len(parts)>=7:
                    uid = int(parts[0])
                    etat[uid] = {'id':uid,'x':int(parts[1]),'y':int(parts[2]),
                                 'vie':int(parts[3]),'capacite':int(parts[4]),
                                 'force':int(parts[5]),'stock':int(parts[6])}
    except FileNotFoundError:
        pass
    return etat

def ecrire_ordres(ordres):
    with open(f"ordres_{J}.txt","w",encoding="utf-8") as f:
        for o in ordres:
            f.write(o+"\n")

def bot():
    etat = lire_etat()
    ordres = []

    # créer la première unité si aucune unité existante
    if not etat:
        uid = random.randint(1000,9999)
        ordres.append(f"CREER {uid} 2 2 1")  # VIE=2, CAPACITE=2, FORCE=1
    else:
        for u in etat.values():
            # Si stock >= capacité, transmettre au dépôt (id=0)
            if u['stock'] >= u['capacite']:
                ordres.append(f"TRANSMETTRE {u['id']} 0")
            else:
                ordres.append(f"POMPER {u['id']}")
    ecrire_ordres(ordres)

if __name__=="__main__":
    bot()
