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
