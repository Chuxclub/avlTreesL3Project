#load "btree.cmo";;
#load "bst.cmo";;
open Btree;;
open Bst;;
#show Bst;;
#show Btree;;

let r_rotate(a: 'a t_btree) : 'a t_btree =
  let (q,fg,fd) : ('a * 'a t_btree * 'a t_btree ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a) || isEmpty(fg))
  then
    (
      print_endline "left rotate is not defined for this tree !";
      a
    )
  else
    (
      
      rooting(root(fg),lson(fg),rooting(q,rson(fg),fd))
    )
;;
      
let l_rotate(a: 'a t_btree) : 'a t_btree =
  let (p,fg,fd) : ('a * 'a t_btree * 'a t_btree ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a) || isEmpty(fd))
  then
    (
      print_endline "right rotate is not defined for this tree !";
      a
    )
  else
    (
      rooting(root(fd),rooting(p,fg,lson(fd)),rson(fd))
    )
;;


let rl_rotate(a: 'a bst) : 'a bst =
  let (r,fg,fd) : ('a * 'a bst * 'a bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a)|| isEmpty(fd) || isEmpty(lson(fd)))
  then
    (
      print_endline "right-left rotate is not defined for this tree !";
      a
    )
  else
    (
      rooting(root(lson(fd)),rooting(r, fg, lson(lson(fd))), rooting(root(fd), rson(lson(fd)), rson(fd)))
    )
;;


let lr_rotate(a: 'a bst) : 'a bst =
  let (r,fg,fd) : ('a * 'a bst * 'a bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a)|| isEmpty(fg) || isEmpty(rson(fg)))
  then
    (
      print_endline "left-right rotate is not defined for this tree !";
      a
    )
  else
    (
      rooting(root(rson(fg)),rooting(root(fg), lson(fg), lson(rson(fg))), rooting(r, rson(rson(fg)), fd))
    )
;;




(*en travaux*)
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



(*en supposant que le desequilibre ne soit pas stocké*)
let reequilibrer(a : 'a bst) : 'a bst = 
  if(desequilibre(a)==0||desequilibre(a)==1||desequilibre(a)== -1)
  then a
  else
    (
      if(desequilibre(a)== 2 && desequilibre(lson(a))==1)
      then r_rotate(a)
      else
        (
          if(desequilibre(a)== 2 && desequilibre(lson(a))== -1)
          then lr_rotate(a)
          else
            (
              if(desequilibre(a)== -2 && desequilibre(rson(a))== -1)
              then l_rotate(a)
              else
                (
                  if(desequilibre(a)== -2 && desequilibre(rson(a))==1)
                  then rl_rotate(a)
                  else failwith("error")
                )
            )
        )
    )
;;
 


(*en supposant que le desequilibre soit stocké*)
let reequilibrer2(a :  ('a * int) t_btree) : ('a * int) t_btree =
  let (rt,unbal): ('a * int) = root(a)
  and (rl,unball): ('a *int) = root(lson(a))
  and (rr,unbalr): ('a *int) = root(rson(a)) in
  if(unbal== 0 || unbal== 1 || unbal== -1)
  then a
  else
    (
      if(unbal== 2 && unball==1)
      then r_rotate(a)
      else
        (
          if(unbal == 2 && unball== -1)
          then lr_rotate(a)
          else
            (
              if(unbal == -2 && unbalr== -1)
              then l_rotate(a)
              else
                (
                  if(unbal == -2 && unbalr==1)
                  then rl_rotate(a)
                  else failwith("error")
                )
            )
        )
    )
;;


let rec ajt_avl(e, a : 'a * 'a bst) : 'a bst =
  if(isEmpty(a))
  then rooting(e,empty(),empty())
  else
    (
      let (v,fg,fd) : ('a * 'a bst * 'a bst) = (root(a),lson(a),rson(a))
      in

      if(e < v)
      then reequilibrer(rooting(v,ajt_avl(e, fg), fd))
      else
        (
          if(e>v)
          then reequilibrer(rooting(v, fg, ajt_avl(e, fd)))
          else rooting(v, fg, fd)

        )
    )
;;


let rec avl_dmax(t : 'a bst) : 'a * 'a bst =
  
  if isEmpty(rson(t))
  then (root(t), lson(t))
  
  else
    let (v, newrst) : 'a * 'a bst = avl_dmax(rson(t))
    in
    (v, reequilibrer(rooting(root(t), lson(t), newrst)))
;;


let rec suppr_avl(e, a : 'a * 'a bst) : 'a bst =
  if(isEmpty(a))
  then rooting(e,empty(),empty())
  else
    (
      let (v,fg,fd) : ('a * 'a bst * 'a bst) = (root(a),lson(a),rson(a))
      in

      if(e < v) then reequilibrer(rooting(v, suppr_avl(e, fg), fd))
      else
        (
          if(e > v) then reequilibrer(rooting(v, fg, suppr_avl(e, fd)))
          else
            (
              if(e = v && isEmpty(fd)) then fg
              else
                (
                  if(e = v && not(isEmpty(fd)) && isEmpty(fg)) then fd
                  else
                    (
                      let (max,dmax) : ('a * 'a bst) = avl_dmax(fg) in
                      reequilibrer(rooting(max, dmax, fd))
                    )
                )
            )
        
        )
    )
;;


(*
let rec suppr_avl1(e, a : 'a * 'a bst) : 'a bst =
  (* let (v,fg,fd) : ('a * 'a bst * 'a bst) = (root(a),lson(a),rson(a)) in*)
  match (e,a) with
  | suppr_avl1(e, rooting(v, g, d)) when e < v -> reequilibrer(rooting(v, suppr_avl1(e, g), d))
  | suppr_avl1(e, rooting(v, g ,d)) when e > v -> reequilibrer(rooting(v, g, suppr_avl1(e, d)))
  | suppr_avl1(e, rooting(v, g, empty)) when e = v  -> g
  | suppr_avl1(e, rooting(v, empty, d)) when e = v && not(isEmpty(d)) -> d
  | suppr_avl1(e, rooting(v, g, d)) when e = v && not(isEmpty(g)) && not(isEmpty(d)) ->
     let (max,dmax) : ('a * 'a bst) = avl_dmax(g) in
     reequilibrer(rooting(max, dmax, d))
;;
 *)

5;;



(*autre idée pour stocké le desequilibre*)
(*
type 'a t_btree =
      | Bnil
      | Bnode of 'a * 'a t_btree * 'a t_btree

let root(t : 'a t_btree) : 'a =
      match t with
      | Bnil -> failwith("tree is empty")
      | Bnode(v, l, r) -> v
 
type 'a bst = 'a t_btree ;;

let r_rotate(a: 'a t_bst) : 'a t_bst =
  let (q,fg,fd) : ('a * 'a t_bst * 'a t_bst ) = (root(a),lson(a),rson(a))
  in
  if(isEmpty(a) || isEmpty(fg))
  then a
  else
    (
      
      rooting(root(fg),lson(fg),rooting(q,rson(fg),fd))
    )
;;

let reequilibrer2(a : 'a bst) : 'a bst =
  
  match a with
  | Bnode((v,u),l,r) when u == 1 || u==0 || u== -1 -> a
  | Bnode((v,u),l,r) when u == 2 && root(l)== (v,1)   -> r_rotate(a)
  | Bnode((v,u),l,r) when u == 2 && root(l)== (v,-1)  -> lr_rotate(a)
  | Bnode((v,u),l,r) when u == -2 && root(r)== (v,-1) -> l_rotate(a)
  | Bnode((v,u),l,r) when u == -2 && root(r)== (v,1)  -> rl_rotate(a)
  | _ -> failwith("error")
;;
 *)
5;;

(*il faudrai refaire les fonction de rotation

type 'a t_avltree =
  | AVLnil
  | AVLnode of 'a * 'a t_avltree * 'a t_avltree * int
             
let unbal(t : 'a t_avltree) : int =
  match t with
  | AVLnil -> failwith("tree is empty")
  | AVLnode(v, l, r, u) -> u
                         
let reequilibrer3(a : 'a t_avltree) : 'a t_avltree =
  match a with
  | AVLnode(_,l,r,e) when e == 1 || e==0 || e== -1 -> a
  | AVLnode(_,l,r,e) when e == 2 && unbal(l)== 1   -> r_rotate(a)
  | AVLnode(_,l,r,e) when e == 2 && unbal(l)== -1  -> lr_rotate(a)
  | AVLnode(_,l,r,e) when e == -2 && unbal(r)== -1 -> l_rotate(a)
  | AVLnode(_,l,r,e) when e == -2 && unbal(r)== 1  -> rl_rotate(a)
  | _ -> failwith("error")
;;  
 *)

(*************************************** TEST ****************************************)

let abr_1 : 'a bst = rooting("q",
                             rooting("p",
                                     rooting("u",empty(),empty()),
                                     rooting("v",empty(),empty())),
                             rooting("w",empty(),empty()));;


let abr_2 : 'a bst = rooting("p",
                             rooting("u",empty(),empty()),
                             rooting("q",
                                     rooting("v",empty(),empty()),
                                     rooting("w",empty(),empty())));;


let abr_3 : 'a bst = rooting("r",
                             rooting("p",
                                     rooting("t",empty(),empty()),
                                     rooting("q",
                                             rooting("u",empty(),empty()),
                                             rooting("v",empty(),empty()))),
                             rooting("w",empty(),empty()));;


let abr_4 : 'a bst = rooting("r",
                             rooting("t",empty(),empty()),
                             rooting("p",
                                     rooting("q",
                                             rooting("u",empty(),empty()),
                                             rooting("v",empty(),empty())),
                                     rooting("w",empty(),empty())));;

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


(*test de la fonction d'ajout dans un avl*)
let abr_5 : 'a bst = rl_rotate(abr_4);;
show_string_btree(abr_5);;
let ajtabr5 : 'a bst = ajt_avl("d",abr_5);;
show_string_btree(ajtabr5);;
show_string_btree(ajt_avl("g",ajtabr5));;
let ajtabr6 : 'a bst = ajt_avl("g",ajtabr5);;
show_string_btree(ajtabr6);;
show_string_btree(ajt_avl("m",ajtabr6));;

show_string_btree(avl_dmax(ajtabr6));;
avl_dmax(ajtabr6);;

(*
let a_test : ('a * int) bst = rooting(("q",1),
                                      rooting(("p",1),
                                              rooting(("u",1),empty(),empty()),
                                              rooting(("v",1),empty(),empty())),
                                      rooting(("w",1),empty(),empty()));;
 *)
show(a_test);;
show_string_btree(a_test);;


(*si on utilise la fonction d'ajout dans un bst sur un avl pour voir la difference*)
show_string_btree(bst_linsert(ajtabr5, "g"));;
5;;


(*
module type AVLtree = 
  sig
    type 'a avltree 
    val empty : unit -> 'a avltree
    val rooting : 'a * 'a avltree * 'a avltree * int-> 'a avltree
    val root : 'a avltree -> 'a
    val lson : 'a avltree -> 'a avltree
    val rson : 'a avltree -> 'a avltree
    val unbal : 'a avltree -> int
    val isEmpty : 'a avltree -> bool
  end




module MyAVLtree : AVLtree =
  struct
    
    type 'a avltree =
      | AVLnil
      | AVLnode of 'a * 'a avltree * 'a avltree * int

               
    let empty() : 'a avltree = AVLnil
                           
    let rooting(v, l, r, u : 'a * 'a avltree * 'a avltree * int) : 'a avltree = AVLnode(v, l, r, u)
                                                               
    let root(t : 'a avltree) : 'a =
      match t with
      | AVLnil -> failwith("tree is empty")
      | AVLnode(v, l, r, u) -> v
                                                       
    let lson(t : 'a avltree) : 'a avltree =
      match t with
      | AVLnil -> failwith("tree is empty")
      | AVLnode(v, l, r, u) -> l
                                                    
    let rson(t : 'a avltree) : 'a avltree =
      match t with
      | AVLnil -> failwith("tree is empty")
      | AVLnode(v, l, r, u) -> r

    let unbal(t : 'a avltree) : int =
      match t with
      | AVLnil -> failwith("tree is empty")
      | AVLnode(v, l, r, u) -> u
                                                    
    let isEmpty(t : 'a avltree) : bool =
      t = AVLnil
      
  end
 *)

