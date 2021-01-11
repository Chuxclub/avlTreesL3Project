#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;
#show Bst;;
#show Btree;;

(* =========================================================================================== *)
(* ======================================== Exercice 1 ======================================= *)
(* =========================================================================================== *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let bst_rnd_create(bound, treeSize : int * int) : int bst =

  Random.self_init();

  let empty_tree : int bst = empty() in
  let randABR : int bst ref = ref empty_tree in
  
  for i=1 to treeSize do
    let randInt : int = Random.int bound in
    randABR := bst_linsert(!randABR, randInt);
  done;

  !randABR
;;

show_int_btree(bst_rnd_create(100, 10));;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)


let isLeaf(tree: 'a t_btree) : bool =
  isEmpty(lson(tree)) && isEmpty(rson(tree))
;;

let max(a, b : int * int) : int =

  if(a > b)
  then a

  else b
;;

let rec height(tree : 'a t_btree) : int =

  if(isEmpty(tree) || isLeaf(tree))
  then 0

  else
    (
      let l : 'a t_btree = lson(tree) in
      let r : 'a t_btree = rson(tree) in
      1 + max(height(l), height(r))
    )
;;

let desequilibre(tree : 'a t_btree) : int =
  height(lson(tree)) - height(rson(tree))
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let avgDesequilibre(sampleSize, treeSize : int * int) : float =

  let sum : float ref = ref 0. in

  for i=1 to sampleSize do
    sum := !sum +. float_of_int(desequilibre(bst_rnd_create(100, treeSize)))
  done;

  !sum /. float_of_int(sampleSize)
;;

avgDesequilibre(1000, 10);;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
