#load "btree.cmo";;
#load "bst.cmo";;

open Btree;;
open Bst;;


(* ============================================================================================= *)
(* ======================================== Exercice 2.1 ======================================= *)
(* ============================================================================================= *)

let getValue(tree : ('a * int) t_btree) : 'a =
  let (v, deseq) : ('a * int) = root(tree) in
  v
;;

let getDeseq(tree : ('a * int) t_btree) : int =
  let (v, deseq) : ('a * int) = root(tree) in
  deseq
;;

let t1 : 'int t_btree = rooting(('a', 2), empty(), empty());;
(getValue(t1), getDeseq(t1));;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)


let rg(t : 'a t_btree) : 'a t_btree =
  if(isEmpty(t) || isEmpty(rson(t)))
  then failwith("Tree or right subtree is empty. Function rg can't continue")

  else
    (
      let (p, u, (q, v, w)) : 'a * 'a t_btree * ('a * 'a t_btree * 'a t_btree) =
        (
          (getValue(t), 0),
          lson(t),
          ((getValue(rson(t)), 0), lson(rson(t)), rson(rson(t)))
        )
      in
      rooting(q, rooting(p, u, v), w)
    )
;;



let rd(t : 'a t_btree) : 'a t_btree =
  if(isEmpty(t) || isEmpty(lson(t)))
  then failwith("Tree or right subtree is empty. Function rd can't continue")

  else
    (
      let (q, (p, u, v), w) : 'a * ('a * 'a t_btree * 'a t_btree) * 'a t_btree =
        (
          (getValue(t), 0),
          ((getValue(lson(t)), 0), lson(lson(t)), rson(lson(t))),
          rson(t) 
        )
      in
      rooting(p, u, rooting(q, v, w))
    )
;;



let rgd(t : 'a t_btree) : 'a t_btree =
  
  let (v, g, d) : 'a * 'a t_btree * 'a t_btree = (root(t), lson(t), rson(t)) in
  let t1 : 'a t_btree = rooting(v, rg(g), d) in
  let t_res : 'a t_btree = rd(t1) in

  t_res  
;;


let rdg(t : 'a t_btree) : 'a t_btree =
  
  let (v, g, d) : 'a * 'a t_btree * 'a t_btree = (root(t), lson(t), rson(t)) in
  let t1 : 'a t_btree = rooting(v, g, rd(d)) in
  let t_res : 'a t_btree = rg(t1) in

  t_res  
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2 : desequilibre & reequilibrer) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let max(a, b : int * int) : int =
  if(a > b)
  then a

  else b
;;

(*let isLeaf(tree: 'a t_btree) : bool =
  if(isEmpty(tree))
  then failwith("Function isLeaf, tree is empty")

  else
    isEmpty(lson(tree)) && isEmpty(rson(tree))
;;*)

let rec height(tree : 'a t_btree) : int =
  if(isEmpty(tree))
  then 0
  else
    (
      let l : 'a t_btree = lson(tree) in
      let r : 'a t_btree = rson(tree) in
      1 + max(height(l), height(r))
    )
;;

let desequilibre(tree : 'a t_btree) : int =
  if(isEmpty(tree))
  then 0

  else
    height(lson(tree)) - height(rson(tree))
;;




let reequilibrer(tree : 'a t_btree) : 'a t_btree =
  let deseq : int = getDeseq(tree) in

  if(deseq >= -1 && deseq <= 1)
  then tree

  else if(deseq = 2)
  then
    (
      let ldeseq : int = getDeseq(lson(tree)) in

      if(ldeseq = 1)
      then rd(tree)

      else if(ldeseq = -1)
      then rgd(tree)

      else rooting((getValue(tree), getDeseq(tree) - 1), lson(tree), rson(tree))
    )

  else if(deseq = -2)
  then
    (
      let rdeseq : int = getDeseq(rson(tree)) in

      if(rdeseq = 1)
      then rdg(tree)

      else if(rdeseq = -1)
      then rg(tree)

      else rooting((getValue(tree), getDeseq(tree) + 1), lson(tree), rson(tree))
    )

  else
    (
      failwith("Something went really wrong, 
                all nodes' unbalance values should be in {-2, -1, 0, 1, 2}")
    )
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (3 : ajouts & suppressions) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let rec ajt_avl(e, tree : 'b * 'a t_btree) : 'a  t_btree =
  if(isEmpty(tree))
  then rooting((e, 0), empty(), empty())

  else
    (
      let ((v, deseq), g, d) : (('b * int) * 'a t_btree * 'a t_btree) =
        ((getValue(tree), getDeseq(tree)), lson(tree), rson(tree)) in

      if(e < v)
      then reequilibrer(rooting((v, deseq), ajt_avl(e, g), d))

      else if(e > v)
      then reequilibrer(rooting((v, deseq), g, ajt_avl(e, d)))

      else tree
    )
;;


let rec treeMax(tree : 'a t_btree) : 'a =
  if(isEmpty(tree))
  then failwith("max isn't defined for empty trees")

  else
    (
      let (v, g, d) : ('a * 'a t_btree * 'a t_btree) = (root(tree), lson(tree), rson(tree)) in
      
      if(isEmpty(d))
      then v

      else treeMax(d)
    )
;;


let rec dmax(tree : 'a t_btree) : 'a t_btree =
  if(isEmpty(tree))
  then failwith("Tree is empty, can't apply dmax function on an empty tree")

  else
    (
      let (v, g, d) : ('a * 'a t_btree * 'a t_btree) = (root(tree), lson(tree), rson(tree)) in

      if(isEmpty(d))
      then g

      else reequilibrer(rooting(v, g, dmax(d)))
    )
;;


let rec suppr_avl(e, tree : 'b * 'a t_btree) : 'a t_btree =
  if(isEmpty(tree))
  then empty()

  else
    (
      let ((v, deseq), g, d) : (('b * int) * 'a t_btree * 'a t_btree) =
        ((getValue(tree), getDeseq(tree)), lson(tree), rson(tree)) in

      if(e < v)
      then reequilibrer(rooting((v, deseq), suppr_avl(e, g), d))

      else if(e > v)
      then reequilibrer(rooting((v, deseq), g, suppr_avl(e, d)))

      else
        (
          if(isEmpty(d))
          then g

          else if(isEmpty(g))
          then d

          else reequilibrer(rooting(treeMax(g), dmax(g), d))
        )
    )
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~ (4 : operation recherche du module Bst) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* cf. ex2_experiments.ml *)





(* ============================================================================================= *)
(* ======================================== Exercice 2.2 ======================================= *)
(* ============================================================================================= *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : Complexité des opérations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let avl_rnd_create(bound, treeSize : int * int) : ('a * int) t_btree =

  Random.self_init();

  let empty_tree : ('a * int) t_btree = empty() in
  let randABR : ('a * int) t_btree ref = ref empty_tree in
  
  for i=1 to treeSize do
    let randInt : int = Random.int bound in
    randABR := ajt_avl(randInt, !randABR);
  done;

  !randABR
;;

(* ~~~~~~~~~~~~~~~~~~~~~~~ (2 : Sous-suites & nombre moyen de rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~ *)
