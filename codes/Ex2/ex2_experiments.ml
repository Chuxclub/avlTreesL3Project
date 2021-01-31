(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)

#directory "../../librairies/modules_UP/graphiques/";;
#use "AP2util.ml";;
#load "graphics.cma";;
#use "graphique.ml";;
#use "AP2TP1draw.ml";;

#directory "../../librairies/modules_UP/4.08.1/";;
#load "btree.cmo";;
open Btree;;

#directory "../../librairies/modules_etu/utils/";;
#use "treeUtils.ml";;

#directory "../../librairies/modules_etu/bst/";;
#use "bst.ml";;

#directory "../../codes/Ex2/";;
#use "ex2.ml";;

#directory "../../librairies/modules_etu/avl/";;
#use "avlExperimentsUtils.ml";;
#use "avlGraphicsUtils.ml";;




(* ============================================================================================= *)
(* ======================================== Exercice 2.1 ======================================= *)
(* ============================================================================================= *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let t1 : string avl  = rooting(("q", 3),
                               rooting(("p", 2),
                                       rooting(("u", 1), empty(), empty()),
                                       rooting(("v", 1), empty(), empty())
                                 ),
                               rooting(("w", 1), empty(), empty())
                         );;

let t2 : string avl = rooting(("r", 4),
                              rooting(("p", 3),
                                      rooting(("t", 1), empty(), empty()),
                                      rooting(("q", 2),
                                              rooting(("u", 1), empty(), empty()),
                                              rooting(("v", 1), empty(), empty())
                                        )
                                ),
                              rooting(("w", 1), empty(), empty())
                        );;

show_string_avl(t1);;
show_string_avl(rd(t1));;
deseqList(rd(t1));;


show_string_avl(t2);;
show_string_avl(rgd(t2));;
deseqList(rgd(t2));;


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2 : desequilibre & reequilibrer) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let unbalancedt1 : int avl = rooting((12, 3),
                                     rooting((3, 2),
                                             rooting((2, 1), empty(), empty()),
                                             empty()
                                       ),
                                     empty()
                               );;

show_int_avl(unbalancedt1);;
desequilibre(unbalancedt1);;
desequilibre(lson(lson(unbalancedt1)));;
show_int_avl(reequilibrer(unbalancedt1));;



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (3 : ajouts & suppressions) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)


(* Ajout: *)
let otherdummy : 'a avl = avl_rnd_create(1000, 30);;
show_int_avl(otherdummy);;
deseqList(otherdummy);;


(* Suppression: *)
let deleteTest : 'a avl = suppr_avl(611, otherdummy);;
show_int_avl(deleteTest);;
deseqList(deleteTest);;


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~ (4 : operation recherche du module Bst) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let otherdummy : 'a avl = avl_rnd_create(1000, 30);;
show_int_avl(otherdummy);;
show_int_avl(avl_seek(695, otherdummy));;







(* ============================================================================================= *)
(* ======================================== Exercice 2.2 ======================================= *)
(* ============================================================================================= *)



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : Complexité des opérations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* Ajout: *)
ajtGraph(64);;


(* Suppression: *)
supprGraph(64);;


(* Recherche: *)
seekGraph(64);;


(* ~~~~~~~~~~~~~~~~~~~~~~~ (2 : Sous-suites & nombre moyen de rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~ *)
