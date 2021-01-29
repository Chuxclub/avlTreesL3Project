type 'a bst = 'a t_btree ;;

let rec bst_seek(b,v : 'a bst * 'a) : 'a bst =

  if(isEmpty(b))
  then b

  else
    (
      let (r,fg,fd) : ('a * 'a bst * 'a bst) = (root(b),lson(b),rson(b))
      in

      if(v = r)
      then b

      else if(v < r)
      then bst_seek(fg, v)

      else bst_seek(fd, v)
    )
;;


let rec bst_linsert(b, v : 'a bst * 'a) : 'a bst =

  if(isEmpty(b))
  then rooting(v,empty(),empty())

  else
    (
      let (r,fg,fd) : ('a * 'a bst * 'a bst) = (root(b),lson(b),rson(b))
      in

      if(v <= r)
      then rooting(r, bst_linsert(fg,v), fd)

      else rooting(r, fg, bst_linsert(fd,v))
    )
;;


let bst_lbuild (l : 'a list) =
  
  let rec build_rec(l,t : 'a list * 'a bst) =
    match l with
    | [] -> t
    | hd::tl -> build_rec(tl, bst_linsert(t,hd))
  in

  build_rec(l,empty())
;;


let rec bst_delete_max(t : 'a bst) : 'a * 'a bst =
  
  if isEmpty(rson(t))
  then (root(t), lson(t))
  
  else
    let (v, newrst) : 'a * 'a bst = bst_delete_max(rson(t))
    in
    (v, rooting(root(t), lson(t), newrst))
;;


let bst_delete_root(r, lst, rst : 'a * 'a bst * 'a bst) : 'a bst =
  
  if isEmpty(lst)
  then
    if isEmpty(rst)
    then empty()
    else rst

  else if isEmpty(rst)
  then lst

  else
    let (newr, newlst) : 'a * 'a bst = bst_delete_max(lst)
    in
    rooting(newr, newlst, rst)
;;

let rec bst_delete(t, v : 'a bst * 'a) : 'a bst * bool =
  
  if isEmpty(t)
  then (t, false)

  else
    
    let (r, lst, rst) : 'a * 'a bst * 'a bst = (root(t), lson(t), rson(t))
    in
    
    if v < r
    then
      let (newlst, found) : 'a bst * bool = bst_delete(lst, v)
      in
      (rooting(r, newlst, rst), found)

    else if v > r
    then
      let (newrst, found) : 'a bst * bool = bst_delete(rst, v)
      in
      (rooting(r, lst, newrst), found)
      
    else (bst_delete_root(r, lst, rst), true)
;;

let rec bst_cut(t, x : 'a bst * 'a) : 'a bst * 'a bst =

  if(isEmpty(t))
  then (empty(), empty())

  else
    (
      let (v, l, r) : 'a * 'a bst * 'a bst = (root(t), lson(t), rson(t)) in

      if(x < v)
      then
        (
          let (l', r') : 'a bst * 'a bst = bst_cut(l, x) in 
          ( l', rooting(v, r', r) )
        )

      else
        (
          let (l', r') : 'a bst * 'a bst = bst_cut(r, x) in
          ( rooting(v, l, l'), r' )
        )
    )
;;

let ajt_r(t, x : 'a bst * 'a) : 'a bst =

  let (l', r') : 'a bst * 'a bst = bst_cut(t, x) in

  rooting(x, l', r')
;;
