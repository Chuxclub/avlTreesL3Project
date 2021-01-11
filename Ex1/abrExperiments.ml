#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
#show Bst;;
#show Btree;;


let bst_rnd_create(bound, treeSize : int * int) : int bst =

  Random.self_init();

  let empty_tree : int bst = Btree.empty() in
  let randABR : int bst ref = ref empty_tree in
  
  for i=1 to treeSize do
    let randInt : int = Random.int bound in
    randABR := bst_linsert(!randABR, randInt);
  done;

  !randABR
;;

Btree.show_int_btree(bst_rnd_create(100, 10));;
