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

