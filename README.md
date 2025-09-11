# Keras3_personal_training

Ce projet est un petit projet de formation à l'utilisation de Keras3 et TensorFlow via R par l'entraînement et l'évaluation de modèles supervisés et non-supervisés avec Keras, et des analyses des modèles et des jeux de données.

## Structure du projet

```
Keras3_personal_training/
├─ DESCRIPTION                 # Métadonnées du package (Imports, Depends, etc.)
├─ NAMESPACE                   # Généré par roxygen2
├─ R/
│  ├─ models.R                 # Fonctions pour MLP supervisé et AutoEncodeur non-supervisé
│  ├─ analysis.R               # PCA, MDS et visualisations avec ggplot2
│  ├─ metrics.R                # Évaluation des modèles et métriques
│  ├─ wrappers.R               # (optionnel) Fonctions build-and-fit génériques
│  └─ utils.R                  # (optionnel) Helpers divers
├─ man/                        # Documentation Rd générée
├─ inst/
│  └─ examples/                # Scripts d’exemples d'utilisation
│     ├─ run_iris.R            # Pipeline sur le jeu de données iris
│     └─ run_msleep.R          # Pipeline sur le jeu de données msleep
├─ tests/                      # (optionnel) Tests unitaires avec testthat
├─ README.md
└─ .gitignore
```

## Utilisation

Consultez les scripts dans `inst/examples/` pour des exemples d'utilisation sur des jeux de données classiques (`iris`, `msleep`).  
Les fonctions principales sont dans le dossier `R/` et couvrent la modélisation, l'analyse, l'évaluation et des utilitaires.

## Installation

```r
# Depuis le dossier racine du projet
devtools::install()
```

## Licence

Ce projet est sous licence WTFPL.