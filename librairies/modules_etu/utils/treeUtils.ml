let isLeaf(tree: 'a t_btree) : bool =
  if(isEmpty(tree))
  then failwith("isLeaf : tree is empty")

  else
    isEmpty(lson(tree)) && isEmpty(rson(tree))
;;

let max(a, b : int * int) : int =

  if(a > b)
  then a

  else b
;;

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
