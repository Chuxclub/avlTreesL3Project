(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)

#directory "../../librairies/modules_UP/4.08.1/";;
#load "../../librairies/modules_UP/4.08.1/btree.cmo";;
open Btree;;

#directory "../../librairies/modules_etu/bst/";;
#use "bst.ml";;

#directory "../../librairies/modules_etu/utils/";;
#use "treeUtils.ml";;

#directory "../../codes/Ex1/";;
#use "ex1.ml";;

Random.self_init();;

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

avgAvgSeriesDesequilibre(100, 100, 100, 'r');;
avgAvgSeriesDesequilibre(100, 100, 100, 'f');;
avgAvgSeriesDesequilibre(100, 100, 100, 'a');;
avgAvgSeriesDesequilibre(100, 100, 100, 'd');;
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)




(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (4) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* 
   Compte-rendu d'expérimentation : 
   - Dans ce projet: ../rapport.pdf
   - En ligne: https://fr.overleaf.com/read/jsvdspcwzjjd
 *)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
