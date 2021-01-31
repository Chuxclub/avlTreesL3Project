(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)



(* ============================================================================================= *)
(* ======================================== Exercice 2.1 ======================================= *)
(* ============================================================================================= *)



(* ~~~~~~~~~~~~~~~~~~~~~ (0 : Quelques utilitaires à nous pour la suite) ~~~~~~~~~~~~~~~~~~~~~~~ *)

let getValue(tree : ('a * int) t_btree) : 'a =
  let (v, deseq) : ('a * int) = root(tree) in
  v
;;

let getHeight(tree : ('a * int) t_btree) : int =
  if(isEmpty(tree))
  then 0

  else
    (
      let (v, h) : ('a * int) = root(tree) in
      h
    )
;;

let getNewHeight(tree : ('a * int) t_btree) : ('a * int) t_btree =
  if(isEmpty(tree))
  then tree

  else
    (
      let ((v, h), g, d) : ('a * int) * ('a * int) t_btree * ('a * int) t_btree =
        (root(tree), lson(tree), rson(tree)) in
      rooting( ( v, 1 + max(getHeight(lson(tree)), getHeight(rson(tree))) ), g, d)
    )
;;

let desequilibre(tree : ('a * int) t_btree) : int =
  if(isEmpty(tree))
  then 0

  else
    getHeight(lson(tree)) - getHeight(rson(tree))
;;



(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1 : rotations) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)


let rg(t : 'a t_btree) : 'a t_btree =
  if(isEmpty(t) || isEmpty(rson(t)))
  then failwith("Tree or right subtree is empty. Function rg can't continue")

  else
    (
      let (p, u, (q, v, w)) : 'a * 'a t_btree * ('a * 'a t_btree * 'a t_btree) =
        (
          root(t),
          lson(t),
          (root(rson(t)), lson(rson(t)), rson(rson(t)))
        )
      in
      getNewHeight(rooting(q, getNewHeight(rooting(p, u, v)), w))
    )
;;



let rd(t : 'a t_btree) : 'a t_btree =
  if(isEmpty(t) || isEmpty(lson(t)))
  then failwith("Tree or right subtree is empty. Function rd can't continue")

  else
    (
      let (q, (p, u, v), w) : 'a * ('a * 'a t_btree * 'a t_btree) * 'a t_btree =
        (
          root(t),
          (root(lson(t)), lson(lson(t)), rson(lson(t))),
          rson(t) 
        )
      in
      getNewHeight(rooting(p, u, getNewHeight(rooting(q, v, w))))
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
  let deseq : int = desequilibre(tree) in

  if(deseq >= -1 && deseq <= 1)
  then tree

  else if(deseq = 2)
  then
    (
      let ldeseq : int = desequilibre(lson(tree)) in

      if(ldeseq = 1)
      then rd(tree)

      else rgd(tree)
    )

  else if(deseq = -2)
  then
    (
      let rdeseq : int = desequilibre(rson(tree)) in

      if(rdeseq = 1)
      then rdg(tree)

      else rg(tree)
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
  then rooting((e, 1), empty(), empty())

  else
    (
      let ((v, h), g, d) : (('b * int) * 'a t_btree * 'a t_btree) =
        ((getValue(tree), getHeight(tree)), lson(tree), rson(tree)) in

      if(e < v)
      then reequilibrer(getNewHeight(rooting((v, h), ajt_avl(e, g), d)))

      else if(e > v)
      then reequilibrer(getNewHeight(rooting((v, h), g, ajt_avl(e, d))))

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

      else reequilibrer(getNewHeight(rooting(v, g, avlDmax(d))))
    )
;;


let rec suppr_avl(e, tree : 'b * 'a t_btree) : 'a t_btree =
  if(isEmpty(tree))
  then empty()

  else
    (
      let ((v, h), g, d) : (('b * int) * 'a t_btree * 'a t_btree) =
        ((getValue(tree), getHeight(tree)), lson(tree), rson(tree)) in

      if(e < v)
      then reequilibrer(getNewHeight(rooting((v, h), suppr_avl(e, g), d)))

      else if(e > v)
      then reequilibrer(getNewHeight(rooting((v, h), g, suppr_avl(e, d))))

      else
        (
          if(isEmpty(d))
          then g

          else if(isEmpty(g))
          then d

          else reequilibrer(getNewHeight(rooting(bstMax(g), avlDmax(g), d)))
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
