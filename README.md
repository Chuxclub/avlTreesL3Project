# Projet d'expériences et rapports d'expériences sur les ABR et les AVL

Réalisé par Florian Legendre et Esteban Mauricio.


## L'architecture de dossier de ce projet est construite de la manière suivante:

   - codes: contient les codes sources liés aux différents exercices
   - docs: contient le sujet du projet ainsi que des éléménts de recherches réalisés pour approfondir 
           nos connaissances sur le sujet.
   - libairies: contient les modules fournis par M. Fuchs de l'université de Poitiers (dans le dossier
                modules_UP) ainsi que des modules de notre cru réalisés pour le projet (dans le dossier
                modules_etu). Ces-derniers ne soient pas le coeur du projet (le cas échéant ils seraient
                dans le dossier codes/).


## Pour tester les différents éléments du projet:

   1. Ouvrir un interpréteur Ocaml, par exemple dans Emacs
   2. Scinder l'écran en trois parties pour plus de confort (Ctrl+X 3 pour scinder verticalement, Ctrl+X 2 
      pour scinder horizontalement)
   3. Ouvrir dans un premier écran ex1.ml (ou ex2.ml) et dans un 2nd écran ex1\_experiments.ml (ou ex2_experiments.ml)
   4. Interpréter tout l'entête qui contient les #directory, #use et le Random.self_init();;
   5. **Ne jamais se servir que de la version _experiments.ml**. En effet, nous avons opté pour une logique où un fichier servirait
     de librairie de fonctions tandis que l'autre de terrain expérimental pour ces fonctions. Cela aidait à rendre ce que nous faisions
     plus lisible.


## Architecture de dossier détaillée de ce projet:

```
.
├── codes
│   ├── Ex1
│   │   ├── ex1_experiments.ml
│   │   └── ex1.ml
│   └── Ex2
│       ├── ex2_experiments.ml
│       ├── ex2.ml
│       └── img
│           ├── ajt2.png
│           ├── ajt.png
│           ├── seek.png
│           └── suppr.png
├── docs
│   ├── recherches
│   │   ├── avl
│   │   │   ├── AVL_Tree_Example.gif
│   │   │   └── AVL_tree.pdf
│   │   ├── TP4_ABR
│   │   │   ├── Ex1
│   │   │   │   ├── bst.cmi
│   │   │   │   ├── bst.cmo
│   │   │   │   ├── bst_exercise.ml
│   │   │   │   ├── bst.ml
│   │   │   │   ├── btree.cmi
│   │   │   │   └── btree.cmo
│   │   │   ├── Ex2
│   │   │   │   ├── bst.cmi
│   │   │   │   ├── bst.cmo
│   │   │   │   ├── bst.ml
│   │   │   │   ├── btree.cmi
│   │   │   │   ├── btree.cmo
│   │   │   │   └── ex2.ml
│   │   │   ├── Ex3
│   │   │   │   ├── bst.cmi
│   │   │   │   ├── bst.cmo
│   │   │   │   ├── bst.ml
│   │   │   │   ├── btree.cmi
│   │   │   │   ├── btree.cmo
│   │   │   │   └── ex3.ml
│   │   │   └── tp4ap3l3info.pdf
│   │   └── treeRotations
│   │       ├── AVLTreeTutorial.rtf
│   │       ├── Tree_Rebalancing.gif
│   │       ├── Tree_rotation_animation_250x250.gif
│   │       ├── Tree_rotation.pdf
│   │       └── Tree_Rotations.gif
│   └── sujet_projet.pdf
├── librairies
│   ├── modules_etu
│   │   ├── avl
│   │   │   ├── avlExperimentsUtils.ml
│   │   │   └── avlGraphicsUtils.ml
│   │   ├── bst
│   │   │   └── bst.ml
│   │   └── utils
│   │       └── treeUtils.ml
│   └── modules_UP
│       ├── 4.02.1+ocp1
│       │   ├── ap3queue.cmi
│       │   ├── ap3queue.cmo
│       │   ├── ap3stack.cmi
│       │   ├── ap3stack.cmo
│       │   ├── btree.cmi
│       │   ├── btree.cmo
│       │   ├── gtree.cmi
│       │   ├── gtree.cmo
│       │   ├── pointer.cmi
│       │   └── pointer.cmo
│       ├── 4.02.3
│       │   ├── ap3queue.cmi
│       │   ├── ap3queue.cmo
│       │   ├── ap3stack.cmi
│       │   ├── ap3stack.cmo
│       │   ├── btree.cmi
│       │   ├── btree.cmo
│       │   ├── gtree.cmi
│       │   ├── gtree.cmo
│       │   ├── pointer.cmi
│       │   └── pointer.cmo
│       ├── 4.05.0
│       │   ├── ap3queue.cmi
│       │   ├── ap3queue.cmo
│       │   ├── ap3stack.cmi
│       │   ├── ap3stack.cmo
│       │   ├── btree.cmi
│       │   ├── btree.cmo
│       │   ├── gtree.cmi
│       │   ├── gtree.cmo
│       │   ├── pointer.cmi
│       │   └── pointer.cmo
│       ├── 4.08.1
│       │   ├── ap3queue.cmi
│       │   ├── ap3queue.cmo
│       │   ├── ap3stack.cmi
│       │   ├── ap3stack.cmo
│       │   ├── btree.cmi
│       │   ├── btree.cmo
│       │   ├── gtree.cmi
│       │   ├── gtree.cmo
│       │   ├── pointer.cmi
│       │   └── pointer.cmo
│       ├── 4.10.0
│       │   ├── ap3queue.cmi
│       │   ├── ap3queue.cmo
│       │   ├── ap3stack.cmi
│       │   ├── ap3stack.cmo
│       │   ├── btree.cmi
│       │   ├── btree.cmo
│       │   ├── gtree.cmi
│       │   ├── gtree.cmo
│       │   ├── pointer.cmi
│       │   └── pointer.cmo
│       ├── 4.11.1
│       │   ├── ap3queue.cmi
│       │   ├── ap3queue.cmo
│       │   ├── ap3stack.cmi
│       │   ├── ap3stack.cmo
│       │   ├── btree.cmi
│       │   ├── btree.cmo
│       │   ├── gtree.cmi
│       │   ├── gtree.cmo
│       │   ├── pointer.cmi
│       │   └── pointer.cmo
│       └── graphiques
│           ├── AP2TP1draw.ml
│           ├── AP2util.ml
│           └── graphique.ml
├── rapport.pdf
└── README.md
```
