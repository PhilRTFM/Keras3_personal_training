# Keras3_personal_training

Ce projet est un petit projet de formation à l'utilisation de Keras3 et TensorFlow via R par l'entraînement et l'évaluation de modèles supervisés et non-supervisés avec Keras, et des analyses des modèles et des jeux de données.

## Schéma de principe et fonctions

<img width="1029" height="402" alt="Pipeline_Flowchart" src="https://github.com/user-attachments/assets/25c0c377-964b-49f5-9b63-efb9be4a2e70" />

## Structure du projet

```
Keras3_personal_training/
├─ R/
│ ├─ utils.R # Prétraitements, helpers, export PDF
│ ├─ suppervised_mod.R # Modèles MLP (supervisé, multi-classes)
│ ├─ unsuppervised_mod.R # Autoencodeur (non supervisé)
│ ├─ supervised_mod_multilabel.R # Modèles MLP (supervisé, multi-label)
│ ├─ visualizations.R # PCA, MDS, Shepard plot
│ ├─ eval_sup.R # Évaluations supervisées (matrices de confusion, métriques)
│ ├─ eval_unsup.R # Évaluations non supervisées (erreurs reconstruction, confusion latente)
│ └─ eval_multi_label.R # Évaluations multi-label (confusion, métriques)
│
├─ inst/examples/
│ ├─ iris_suppervised.R # Pipeline complet MLP supervisé
│ ├─ iris_unsuppervised.R # Pipeline complet Autoencodeur
│ └─ iris_supervised_multilabel.R # Pipeline complet MLP multi-label
│
└─ outputs_iris/ # Dossier de sortie PDF (généré automatiquement)
├─ README.md
└─ .gitignore
```

## Utilisation

Consultez les scripts dans `inst/examples/` pour des exemples d'utilisation sur des jeux de données classiques (`iris`, `msleep`).  
Les fonctions principales sont dans le dossier `R/` et couvrent les modèles et les visualisations.

## Licence

Ce projet est sous licence WTFPL.
