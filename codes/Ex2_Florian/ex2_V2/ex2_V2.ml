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

      else rooting((getValue(tree), getDeseq(tree)-1), lson(tree), rson(tree))
    )

  else if(deseq = -2)
  then
    (
      let rdeseq : int = getDeseq(rson(tree)) in

      if(rdeseq = 1)
      then rdg(tree)

      else if(rdeseq = -1)
      then rg(tree)

      else rooting((getValue(tree), getDeseq(tree)+1), lson(tree), rson(tree))
    )

  else
    (
      failwith("reequilibrer: Something went really wrong, 
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
      then reequilibrer(rooting((v, deseq+1), ajt_avl(e, g), d))

      else if(e > v)
      then reequilibrer(rooting((v, deseq-1), g, ajt_avl(e, d)))

      else tree
    )
;;


let rec avlDmax(tree : 'a t_btree) : 'a t_btree =
  if(isEmpty(tree))
  then failwith("Tree is empty, can't apply dmax function on an empty tree")

  else
    (
      let (v, g, d) : ('a * 'a t_btree * 'a t_btree) = (root(tree), lson(tree), rson(tree)) in

      if(isEmpty(d))
      then g

      else reequilibrer(rooting(v, g, avlDmax(d)))
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

          else reequilibrer(rooting(bstMax(g), avlDmax(g), d))
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


let rec avlToBtree(tree : ('a * int) t_btree) : 'a t_btree =
  if(isEmpty(tree))
  then empty()

  else
    (
      rooting(getValue(tree), avlToBtree(lson(tree)), avlToBtree(rson(tree)))
    )
;;

let rec deseqList(tree : ('a * int) t_btree) : int list =
  if(isEmpty(tree))
  then []

  else
    (
      let res1 : int list = getDeseq(tree)::deseqList(lson(tree)) in
      let res : int list = res1@deseqList(rson(tree)) in
      res
    )
;;
