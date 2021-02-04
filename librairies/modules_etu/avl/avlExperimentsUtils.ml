(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)


let show_int_avl (tree : (int * int) t_btree) =
  show(
      (fun (v,h:int*int) -> String.concat "," [string_of_int(v); string_of_int(h)]),
      tree
    )
;;

let show_string_avl (tree : (string * int) t_btree) =
  show(
      (fun (v,h:string*int) -> String.concat "," [v; string_of_int(h)]),
      tree
    )
;;

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
      let res1 : int list = desequilibre(tree)::deseqList(lson(tree)) in
      let res : int list = res1@deseqList(rson(tree)) in
      res
    )
;;

let chrono(func, args : ('a -> 'b) * ('c * 'c avl)) : float =
  let start_chrono : float = Sys.time() in
  ignore(func(args));
  Sys.time() -. start_chrono
;;

let avgAvlOp(avlOp, sampleSize, treeSize : (('a * 'a avl) -> 'c) * int * int) : float =

  let dummyAVL : 'a avl = avl_rnd_create(10000000, treeSize) in
  let sum : float ref = ref 0. in

  for i=1 to sampleSize do
    let randInt : int = Random.int 10000000 in
    sum := !sum +. chrono(avlOp, (randInt, dummyAVL));
  done;

  !sum /. float_of_int(sampleSize)
;;
