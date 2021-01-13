#load "btree.cmo";;
#load "bst.cmo";;
#load "abrExperiments.cmo";;
open Btree;;
open Bst;;
open AbrExperiments;;

(* =========================================================================================== *)
(* ======================================== Exercice 1 ======================================= *)
(* =========================================================================================== *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

show_int_btree(bst_rnd_create(100, 20));;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)





(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

avgDesequilibre(1000, 10);;
avgAvgDesequilibre(100, 100, 100);;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)





(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (3) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* show_int_btree(bst_rndSeries_create(20, 3));;*) 
(* desequilibre(bst_rndSeries_create(10, 1));; *)

(*avgSeriesDesequilibre(1000, 100, 'r');;
avgSeriesDesequilibre(100, 100, 'f');;
avgSeriesDesequilibre(100, 100, 'a');;
avgSeriesDesequilibre(100, 100, 'd');;
avgSeriesDesequilibre(100, 100, 'e');;*)

avgAvgSeriesDesequilibre(100, 100, 100, 'r');;
avgAvgSeriesDesequilibre(100, 100, 100, 'f');;
avgAvgSeriesDesequilibre(100, 100, 100, 'a');;
avgAvgSeriesDesequilibre(100, 100, 100, 'd');;
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)




(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (4) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* 
   Compte-rendu d'expérimentation : 
   https://fr.overleaf.com/read/jsvdspcwzjjd
 *)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)


(* 
   QUESTIONS POUR L'ENSEIGNANT:
   ----------------------------

   1) Moyenne de moyennes?
   2) ABR avec doublons ou sans?
   3) Valeurs attendues (notamment moyennes souvent positives dans le 
      cas de random) ?
   4) Récursivité ou itérativité pour les fonctions?
   5) Fonctions auxiliaires dans des modules ou ici?
   6) Noms de fonctions (cf. avgAvg...) ?
   7) Complexité des fonctions d'expérimentations prises en compte?
 *)
