#load "btree.cmo";;
#load "bst.cmo";;
open Btree;;
open Bst;;
#show Bst;;
#show Btree;;

let r_rotate(a: 'a bst) : 'a bst =
  let (q,fg,fd) : ('a * 'a bst * 'a bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a) || isEmpty(fg))
  then a
  else
    (
      
      rooting(root(fg),lson(fg),rooting(q,rson(fg),fd))
    )
;;
      
let l_rotate(a: 'a bst) : 'a bst =
  let (p,fg,fd) : ('a * 'a bst * 'a bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a) || isEmpty(fd))
  then a
  else
    (
      rooting(root(fd),rooting(p,fg,lson(fd)),rson(fd))
    )
;;


let rl_rotate(a: 'a bst) : 'a bst =
  let (r,fg,fd) : ('a * 'a bst * 'a bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a)|| isEmpty(fd) || isEmpty(lson(fd)))
  then a
  else
    (
      rooting(root(lson(fd)),rooting(r, fg, lson(lson(fd))), rooting(root(fd), rson(lson(fd)), rson(fd)))
    )
;;


let lr_rotate(a: 'a bst) : 'a bst =
  let (r,fg,fd) : ('a * 'a bst * 'a bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a)|| isEmpty(fg) || isEmpty(rson(fg)))
  then a
  else
    (
      rooting(root(rson(fg)),rooting(root(fg), lson(fg), lson(rson(fg))), rooting(r, rson(rson(fg)), fd))
    )
;;



(*************************************** TEST ****************************************)

let abr_1 : 'a bst = rooting("q",rooting("p",rooting("u",empty(),empty()),rooting("v",empty(),empty())),rooting("w",empty(),empty()));;
let abr_2 : 'a bst = rooting("p",rooting("u",empty(),empty()),rooting("q",rooting("v",empty(),empty()),rooting("w",empty(),empty())));;
let abr_3 : 'a bst = rooting("r",rooting("p",rooting("t",empty(),empty()),rooting("q",rooting("u",empty(),empty()),rooting("v",empty(),empty()))),rooting("w",empty(),empty()));;
let abr_4 : 'a bst = rooting("r",rooting("t",empty(),empty()),rooting("p",rooting("q",rooting("u",empty(),empty()),rooting("v",empty(),empty())),rooting("w",empty(),empty())));;

(*rotation droite*)
show_string_btree(abr_1);;
r_rotate(abr_1);;
show_string_btree(r_rotate(abr_1));;

(*rotation gauche*)
show_string_btree(abr_2);;
l_rotate(abr_2);;
show_string_btree(l_rotate(abr_2));;

(*rotation gauche-droite*)
show_string_btree(abr_3);;
lr_rotate(abr_3);;
show_string_btree(lr_rotate(abr_3));;

(*rotation droite-gauche*)
show_string_btree(abr_4);;
rl_rotate(abr_4);;
show_string_btree(rl_rotate(abr_4));;

