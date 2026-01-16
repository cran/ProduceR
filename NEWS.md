# ProduceR 1.1 (2025-11-17)

### Nouvelles fonctionnalités
- La fonction `dup()` fonctionne même si on ne précise par `keyby` (par défaut : ensemble des colonnes du data.frame en entrée).
- La fonction `dup()` sauvegarde ses résultats dans une liste de df, si on précise une table en output

### Corrections de bugs
- La fonction `dup()` peut désormais être utilisée avec un vecteur de colonnes comme clef (avant ne fonctionnait qu'avec une seule colonne comme clef) 

### Modifications
- Le score calculé par la fonction `toc_score()` est passé par la fonction logistique, pour être ramené sur l'intervalle [-1, +1].
- Les tables en sortie de la fonction `tac()` ont un ordre de colonnes plus naturel : colonne, format puis modalité.
- La table en sortie de `toc()` est triée par nom de la colonne, puis par score décroissant en valeur absolue  
- La fonction `dup()` renvoie une table d'exemples de lignes avec la clef (keyby) manquante

---

# ProduceR 1.0 (2025-10-01)

Initialisation du package : première version
