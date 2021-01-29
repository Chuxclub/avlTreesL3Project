#directory "../../librairies/modules_UP/4.08.1/";;
#load "btree.cmo";;
open Btree;;


#directory "../../librairies/modules_etu/graphiques/";;
#use "AP2util.ml";;
#load "graphics.cma";;
#use "graphique.ml";;
#use "AP2TP1draw.ml";;

#directory "../../codes/Ex2_Florian/ex2_V1/";;
#use "ex2_V1.ml";;

(* ============================================================================================= *)
(* ======================================== Exercice 2.1 ======================================= *)
(* ============================================================================================= *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let t1 : string t_btree = rooting("q",
                                rooting("p",
                                        rooting("u", empty(), empty()),
                                        rooting("v", empty(), empty())
                                  ),
                                rooting("w", empty(), empty())
                          );;

let t2 : string t_btree = rooting("r",
                                rooting("p",
                                        rooting("t", empty(), empty()),
                                        rooting("q",
                                                rooting("u", empty(), empty()),
                                                rooting("v", empty(), empty())
                                          )
                                  ),
                                rooting("w", empty(), empty())
                            );;

show_string_btree(t1);;
show_string_btree(t2);;

show_string_btree(rd(t1));;
show_string_btree(rgd(t2));;



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2 : desequilibre & reequilibrer) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let unbalancedt1 : int t_btree = rooting(12,
                                         rooting(3,
                                                 rooting(2, empty(), empty()),
                                                 empty()
                                           ),
                                         empty()
                                   )
;;

show_int_btree(unbalancedt1);;
desequilibre(unbalancedt1);; (* Pb avec axiomes hauteur / desequilibre *)
height(unbalancedt1);;
show_int_btree(reequilibrer(unbalancedt1));;



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (3 : ajouts & suppressions) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let ajt_test1 : int t_btree = ajt_avl(5, reequilibrer(unbalancedt1));;
show_int_btree(ajt_test1);;

let ajt_test2 : int t_btree = ajt_avl(4, ajt_test1);;
show_int_btree(ajt_test2);;

let dmax_test : int t_btree = dmax(ajt_test2);;
show_int_btree(dmax_test);;

treeMax(ajt_test2);;

show_int_btree(ajt_test2);;
let suppr_test1 : int t_btree = suppr_avl(3, ajt_test2);;
show_int_btree(suppr_test1);;



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~ (4 : operation recherche du module Bst) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

show_int_btree(bst_seek(ajt_test2, 4));;

(* CONCLUSION: Il est toujours possible d'utiliser la fonction de recherche des ABR *)





(* ============================================================================================= *)
(* ======================================== Exercice 2.2 ======================================= *)
(* ============================================================================================= *)

let nODES_VALUES_MAX : int = 1000;;

show_int_btree(avl_rnd_create(1000, 30));;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : Complexité des opérations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

show_int_btree(avl_rnd_create(nODES_VALUES_MAX, 30));;

let st : float = Sys.time() in
ignore(avl_rnd_create(nODES_VALUES_MAX, 10000)) ;
Sys.time() -. st
;;



(* /////////////// Graphes de complexité \\\\\\\\\\\\\ *)

(* Graphe de complexité de l'ajout:  *)
let chk_ajt(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n+1, 0.0) in
  let test : 'a t_btree ref = ref(empty()) in
    (
    for j = 1 to n
    do
      let randInt : int = Random.int nODES_VALUES_MAX in
      tm.(j) <- Sys.time() ;
      test := ajt_avl(randInt, !test); 
      tm.(j) <- Sys.time() -. tm.(j);
      ind.(j) <- float_of_int(j) ;
    done ;
    (tm, ind) ;
    )
;;

let testing_chk_ajt(n : int) : unit =

  let (value, ind) : float array * float array = chk_ajt(n) in
  let rep : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  let close : char ref = ref 'o' in
  (
    open_graph(1000, 600);
    draw_rep(rep);
    draw_curve(value, ind,  arr_len(ind) - 1, rep);

    print_string("Enter y;; when you're done to cleanly close the graph:");
    close := read_char();

    if(!close == 'y')
    then
    (
      clear_graph();
      close_graph();
    )
    
  )  
;;

testing_chk_ajt(10000);;


(* Graphe de complexité de la suppression:  *)
let chk_suppr(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n + 1, 0.0) in
  
  let emptyTree : int t_btree = empty() in
  let t : int t_btree ref = ref emptyTree in
    (
    for j = 1 to n
    do
      t := avl_rnd_create(nODES_VALUES_MAX, j);
      tm.(j) <- Sys.time() ;
      ignore(suppr_avl(42, !t)) ; 
      tm.(j) <- Sys.time() -. tm.(j);
      ind.(j) <- float_of_int(j) ;
    done ;
    (tm, ind) ;
    )
;;

let testing_chk_suppr(n : int) : unit =

  let (value, ind) : float array * float array = chk_suppr(n) in
  let rep : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  let close : char ref = ref 'o' in
  (
    open_graph(1000, 600);
    draw_rep(rep);
    draw_curve(value, ind,  arr_len(ind) - 1, rep);

    print_string("Enter y;; when you're done to cleanly close the graph:");
    close := read_char();

    if(!close == 'y')
    then
    (
      clear_graph();
      close_graph();
    )
    
  )  
;;

testing_chk_suppr(1000);;


(* Graphe de complexité de la recherche:  *)
let chk_seek(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n + 1, 0.0) in
  
  let emptyTree : int t_btree = empty() in
  let t : int t_btree ref = ref emptyTree in
    (
    for j = 1 to n
    do
      t := avl_rnd_create(nODES_VALUES_MAX, j);
      tm.(j) <- Sys.time() ;
      ignore(bst_seek(!t, 42)) ; 
      tm.(j) <- Sys.time() -. tm.(j);
      ind.(j) <- float_of_int(j) ;
    done ;
    (tm, ind) ;
    )
;;

let testing_chk_seek(n : int) : unit =

  let (value, ind) : float array * float array = chk_seek(n) in
  let rep : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  let close : char ref = ref 'o' in
  (
    open_graph(1000, 600);
    draw_rep(rep);
    draw_curve(value, ind,  arr_len(ind) - 1, rep);

    print_string("Enter y;; when you're done to cleanly close the graph:");
    close := read_char();

    if(!close == 'y')
    then
    (
      clear_graph();
      close_graph();
    )
    
  )  
;;

testing_chk_seek(1000);;



(* ~~~~~~~~~~~~~~~~~~~~~~~ (2 : Sous-suites & nombre moyen de rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~ *)


