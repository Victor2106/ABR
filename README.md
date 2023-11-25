# Arbres binaires de recherche

Ce projet universitaire vise à représenter les arbres binaires de recherche en utilisant le langage OCaml.

## Structure du projet

Le projet est divisé en plusieurs modules :

### `btreeS.ml`

Ce module est responsable de la définition du type de l'arbre binaire de recherche.

### `Bst.ml`

Le module `Bst` contient diverses fonctions auxiliaires, telles que le calcul de la hauteur d'un arbre, la création d'un arbre, et d'autres opérations liées.

### `abr.ml`

Le fichier `abr.ml` permet de générer un arbre à partir d'une liste et d'effectuer des statistiques sur cet arbre.
Voici un extrait illustrant la création d'une liste avec des sous-suites composées de chiffres croissants de taille variable :

```ocaml
(* Sous-suites aléatoire *)
aleatoire : [5 ; 7 ; 9] [71 ; 80] [2 ; 50 ; 60 ; 97]
Ceci formera : [5 ; 7 ; 9 ; 71 ; 80 ; 2 ; 50 ; 60 ; 97]

(* Sous-suites croissantes *)
croissant : [5 ; 7 ; 9] [71 ; 80 ; 87 ; 99] [2 ; 50 ; 60 ; 97 ; 125]
Ceci formera : [5 ; 7 ; 9 ; 71 ; 80 ; 87 ; 99 ; 2 ; 50 ; 60 ; 97 ; 125]

(* Sous-suites décroissantes *)
decroissant : [5 ; 7 ; 9 ; 15] [25 ; 39 ; 87] [2 ; 12]
Ceci formera : [5 ; 7 ; 9 ; 15 ; 25 ; 39 ; 87 ; 2 ; 12]

(* Sous-suites de longueur fixe *)
fixe (longueur) : [5 ; 7 ; 9] [25 ; 39 ; 87] [2 ; 12 ; 36]
Ceci formera : [5 ; 7 ; 9 ; 25 ; 39 ; 87 ; 2 ; 12 ; 36]
```

Ces listes globales servent à créer un arbre. La taille de ces exemples est courte, et pour obtenir des statistiques significatives, la fonction `bst_avg_imbalance_V2` du fichier `abr.ml` est utilisée.

En espérant que cette implémentation des arbres binaires vous sera utile !
