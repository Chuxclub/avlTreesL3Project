#directory "/home/crex/MEGA/Bureau/Etudes/UE1_AP/TP/TP4/Ex2";;
#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
#show Bst;;

let t0 : 'a bst = empty();;
let t1 : 'a bst = bst_linsert(t0, "q");;
let t2 : 'a bst = bst_linsert(t1, "d");;
let t3 : 'a bst = bst_linsert(t2, "a");;
let t4 : 'a bst = bst_linsert(t3, "i");;
let t5 : 'a bst = bst_linsert(t4, "e");;
let t6 : 'a bst = bst_linsert(t5, "g");;
let t7 : 'a bst = bst_linsert(t6, "l");;
let t : 'a bst = bst_linsert(t7, "t");;


show_string_btree(t);;

(*
let rec bst_cut(t, x : 'a bst * 'a) : 'a bst * 'a bst = 
if isEmpty(t) 
then (empty(), empty()) 

else if x < root(t) 
then let (g, d) = bst_cut(lson(t), x) in (g, rooting(root(t), d, rson(t))) 

else let (g, d) = bst_cut(rson(t), x) in (rooting(root(t), lson(t), g), d) ;;
 *)

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

(*let (s1, s2) : 'a bst * 'a bst = bst_cut(t, "f");;
show_string_btree(s1);;
show_string_btree(s2);;*)

show_string_btree(ajt_r(t, "f"));;
