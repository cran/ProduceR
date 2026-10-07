# En cours

### Modifications
- Dans la fonction `tac()`, remplacement du paramètre `num_but_discrete` par `force_discrete` (forcer tac() à considérer une variable comme discrète, c'est-à-dire à modalités)
- Amélioration `toc()` pour que les deux tables comparées aient le même traitement de leurs colonnes par `tac()` (identifiant, numérique, ...)

# ProduceR 1.3 (2026-06-18)

### Modifications
- Changement de noms des colonnes `chi2_find()` pour plus de lisibilité

### Corrections de bugs
- Correctif `tac()` sur le critère qui détermine que le type de colonne est "identifiant"
- Correctif `tac()` pour une prise en compte du paramètre `num_but_discrete` dans le pavé qui détermine que le type de colonne est numérique
- Correctif `chi2_find()` pour que la table tac_TRUE ait la même typologie de colonnes que tac_FALSE (sinon crash)

### Nouvelles fonctionnalités
- Création fonction `sums()` 
- La fonction `tac()` a un nouveau paramètre `force_identifier` (forcer tac() à considérer une variable comme identifiant)

# ProduceR 1.2 (2026-05-19)

### Modifications
- Les tables en sortie de la fonction `tac()` ont une colonne supplémentaire col_typology (identifiant, quantitative, ...).
- Les tables en sortie de la fonction `tac()` ont des modalités plus claires pour les variables quantitatives (positive, negative, etc, à la place de 1, -1, etc).
- Des tables d'exemples ont été ajoutées (il faudra mettre les bons exemples dans le champ @examples)
- Meilleure valeur par défaut de a pour la fonction `toc()` (plus bas si faible nombre d'observations)
- Meilleure présentation des colonnes de la table en sortie de la fonction `chi2_find()`

### Corrections de bugs
- correctif nom de variables dans chi2_find() et toc() (avant Freq, correctif freq)
- correctif critère tac() pour déterminer les colonnes "identifiant" (avant : mauvaise identification)

---

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
