#directory "../../librairies/modules_UP/4.08.1/";;
#load "btree.cmo";;
open Btree;;

#directory "../../librairies/modules_etu/utils/";;
#use "treeUtils.ml";;

#directory "../../librairies/modules_etu/bst/";;
#use "bst.ml";;

#directory "../../librairies/modules_etu/graphiques/";;
#use "AP2util.ml";;
#load "graphics.cma";;
#use "graphique.ml";;
#use "AP2TP1draw.ml";;

#directory "../../codes/Ex2_Florian/ex2_V2/";;
#use "ex2_V2.ml";;




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

let dummy : ('a * int) t_btree = avl_rnd_create(1000, 5);;
show_int_btree(avlToBtree(dummy));;
deseqList(dummy);;

let dummy2 : ('a * int) t_btree = ajt_avl(974, dummy);;
show_int_btree(avlToBtree(dummy2));;
deseqList(dummy2);;


let dummy3a : ('a * int) t_btree = ajt_avl(734, dummy2);;
show_int_btree(avlToBtree(dummy3a));;
deseqList(dummy3a);;

let dummy3b : ('a * int) t_btree = ajt_avl(32, dummy2);;
show_int_btree(avlToBtree(dummy3b));;
deseqList(dummy3b);;

let otherdummy : ('a * int) t_btree = avl_rnd_create(1000, 10);;
show_int_btree(avlToBtree(otherdummy));;
deseqList(otherdummy);;

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
desequilibre(unbalancedt1);;
desequilibre(lson(lson(unbalancedt1)));;
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

(* CONCLUSION: Il n'est plus possible la fonction bst_seek du module Bst *)





(* ============================================================================================= *)
(* ======================================== Exercice 2.2 ======================================= *)
(* ============================================================================================= *)

let nODES_VALUES_MAX : int = 1000;;



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
  let test : ('a * int) t_btree ref = ref(empty()) in      
    (
    for j = 1 to n
    do
      let randInt : int = Random.int nODES_VALUES_MAX in
      tm.(j) <- Sys.time() ;
      test := ajt_avl(randInt, !test); 
      tm.(j) <- Sys.time() -. tm.(j);
      ind.(j) <- float_of_int(j) ;
    done ;
    print_int(getDeseq(!test));
    (tm, ind) ;
    )
;;

let chk_ajt2(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n+1, 0.0) in
  (
    let j : int ref = ref 1 in
    let i : int ref = ref 1 in
    while !i < n
    do
      let test : ('a * int) t_btree = avl_rnd_create(nODES_VALUES_MAX, !j) in
      let randInt : int = Random.int nODES_VALUES_MAX in
      tm.(!i) <- Sys.time() ;
      ignore(ajt_avl(randInt, test)); 
      tm.(!i) <- Sys.time() -. tm.(!i);
      ind.(!i) <- float_of_int(!j) ;  
      i := !i + 1;
      j := int_of_float(2.**(float_of_int(!i)));
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
  let tm : float array = arr_make(n+1, 0.0) in
  
  let emptyTree : 'a t_btree = empty() in
  let t : 'a t_btree ref = ref emptyTree in
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
  
  let emptyTree : 'a t_btree = empty() in
  let t : 'a t_btree ref = ref emptyTree in
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


(* Problèmes à résoudre: *)

(*
  Problèmes de la version 1:
  1) Pas de stockage nécessaire?
  2) Si stockage, comment éviter les problèmes de la version 2?
  3) Complexité quadratique de l'ajout / bizarres pour suppression et recherche

  Problèmes de la version 2:
  1) Hauteur & Déséquilibre par rapport à l'exemple du cours vs les axiomes
  2) Complexité linéaire de l'ajout (devrait être logarithmique)
  3) Recherche dans l'AVL impossible en utilisant la fonction bst_seek du module Bst (c'est
   pourtant demandé de pouvoir le faire)
  4) Pouvoir afficher les AVL générés
 *)


(* ~~~~~~~~~~~~~~~~~~~~~~~ (2 : Sous-suites & nombre moyen de rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~ *)


