#load "btree.cmo";;
#load "bst.cmo";;
open Bst;;
open Btree;;
(* #show Bst;;
#show Btree;; *)

(* =========================================================================================== *)
(* ======================================== Exercice 1 ======================================= *)
(* =========================================================================================== *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* -> Main <- *)

let bst_rnd_create(bound, treeSize : int * int) : int bst =

  Random.self_init();

  let empty_tree : int bst = empty() in
  let randABR : int bst ref = ref empty_tree in
  
  for i=1 to treeSize do
    let randInt : int = Random.int bound in
    randABR := bst_linsert(!randABR, randInt);
  done;

  !randABR
;;


(* -> Test <- *)

show_int_btree(bst_rnd_create(100, 20));;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)





(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* -> Auxiliaries <- *)

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


(* -> Main <- *)

let desequilibre(tree : 'a t_btree) : int =
  height(lson(tree)) - height(rson(tree))
;;


let avgDesequilibre(sampleSize, treeSize : int * int) : float =

  let sum : float ref = ref 0. in

  for i=1 to sampleSize do
    sum := !sum +. float_of_int(desequilibre(bst_rnd_create(100, treeSize)))
  done;

  !sum /. float_of_int(sampleSize)
;;

let avgAvgDesequilibre(nbAvgs, sampleSize, treeSize : int * int * int) : float =
  
  let sum : float ref = ref 0. in

  for i=1 to nbAvgs do
    sum := !sum +. avgDesequilibre(sampleSize, treeSize);
  done;

  !sum /. float_of_int(nbAvgs)
;;


(* -> Test <- *)

avgDesequilibre(1000, 10);;
avgAvgDesequilibre(100, 100, 100);;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)





(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (3) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* -> Auxiliaries <- *)

let bst_rndSeries_create(treeSize, seriesLen : int * int) : int bst =

  Random.self_init();

  let empty_tree : int bst = empty() in
  let randABR : int bst ref = ref empty_tree in
  let fillerCount : int ref = ref treeSize in

  while(!fillerCount > 0) do
    let randLowerBound : int = Random.int 1001 in
    let len : int = if(seriesLen<=0) then Random.int 101 else seriesLen in

    for i=1 to len do
      if(!fillerCount > 0)
      then (
        randABR := bst_linsert(!randABR, randLowerBound+(i-1));
        fillerCount := !fillerCount - 1;
      )
    done;
    
  done;
  
  !randABR
;;



(* -> Main <- *)

let avgSeriesDesequilibre(sampleSize, treeSize, seriesLenMode : int * int * char) : float =

  let sum : float ref = ref 0. in

  for i=1 to sampleSize do
    match seriesLenMode with
    | 'r' -> sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, -1)))
    | 'f' -> sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, 10)))
    | 'a' -> sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, i)))
    | 'd' -> sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, sampleSize-i)))
    | _ -> failwith("Wrong mode for series length... 'r' for random lengths, 'f' for fixed lengths, 'a' for ascending lengths, and 'd' for descending lengths."); 
  done;

  !sum /. float_of_int(sampleSize)
;;

let avgAvgSeriesDesequilibre(nbAvgs, sampleSize, treeSize, seriesLenMode : int * int * int * char) : float =

  let sum : float ref = ref 0. in

  for i=1 to nbAvgs do
    sum := !sum +. avgSeriesDesequilibre(sampleSize, treeSize, seriesLenMode)
  done;

  !sum /. float_of_int(nbAvgs)
;;


(* -> Test <- *)

(* show_int_btree(bst_rndSeries_create(20, 3));;*) 
(* desequilibre(bst_rndSeries_create(10, 1));; *)

(*avgSeriesDesequilibre(1000, 100, 'r');;
avgSeriesDesequilibre(100, 100, 'f');;
avgSeriesDesequilibre(100, 100, 'a');;
avgSeriesDesequilibre(100, 100, 'd');;
avgSeriesDesequilibre(100, 100, 'e');;*)

avgAvgSeriesDesequilibre(100, 100, 100, 'r');;
avgAvgSeriesDesequilibre(100, 100, 100, 'f');;
avgAvgSeriesDesequilibre(100, 100, 100, 'a');;
avgAvgSeriesDesequilibre(100, 100, 100, 'd');;
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)




(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (4) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* 
   Compte-rendu d'expérimentation : 
   https://fr.overleaf.com/read/jsvdspcwzjjd
 *)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)


(* 
   QUESTIONS POUR L'ENSEIGNANT:
   ----------------------------

   1) Moyenne de moyennes?
   2) ABR avec doublons ou sans?
   3) Valeurs attendues (notamment moyennes souvent positives dans le 
      cas de random) ?
   4) Récursivité ou itérativité pour les fonctions?
   5) Fonctions auxiliaires dans des modules ou ici?
   6) Noms de fonctions (cf. avgAvg...) ?
   7) Complexité des fonctions d'expérimentations prises en compte?
 *)
