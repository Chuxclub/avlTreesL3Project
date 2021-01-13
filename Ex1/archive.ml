(* let createSeries(len : int) : int list =

  Random.self_init();

  if(len<=0)
  then
    (
      let randLowerBound : int = Random.int 1000 in
      let res : int list ref = ref [] in

      for i=1 to Random.int 101 do
        res := (randLowerBound+(i-1))::!res
      done;

      !res
    )

  else
    (
      let randLowerBound : int = Random.int 1000 in
      let res : int list ref = ref [] in

      for i=1 to len do
        res := (randLowerBound+(i-1))::!res
      done;

      !res
    )
;; *)

(*

let bst_rndSeries_create(treeSize, seriesLen : int * int) : int bst =

  Random.self_init();

  let empty_tree : int bst = empty() in
  let randABR : int bst ref = ref empty_tree in
  let deltaSizes : int ref = ref treeSize in

  while(!deltaSizes != 0) do
    let rndSeries : int list = createSeries(seriesLen) in
    let rndSeriesRef : int list ref = ref rndSeries in
    let rndSeriesLen : int = List.length(rndSeries) in

    for i=0 to rndSeriesLen-1 do
      if(!deltaSizes > 0)
      then (
        randABR := bst_linsert(!randABR, List.hd(!rndSeriesRef));
        rndSeriesRef := List.tl(!rndSeriesRef);
        deltaSizes := !deltaSizes - 1;
      )
    done;

  done;

  !randABR
;;
 *)


(*
let avgSeriesDesequilibre(sampleSize, treeSize, seriesLenMode : int * int * char) : float =

  let sum : float ref = ref 0. in

  match seriesLenMode with
    
  | 'r' ->
     for i=1 to sampleSize do
       sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, -1)))
     done;

     !sum /. float_of_int(sampleSize)
     
  | 'f' ->
     for i=1 to sampleSize do
       sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, 10)))
     done;

     !sum /. float_of_int(sampleSize)
     
  | 'a' ->
     for i=1 to sampleSize do
       sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, i)))
     done;

     !sum /. float_of_int(sampleSize)
     
  | 'd' ->
     for i=1 to sampleSize do
       sum := !sum +. float_of_int(desequilibre(bst_rndSeries_create(treeSize, sampleSize-i)))
     done;

     !sum /. float_of_int(sampleSize)
     
  | _ -> failwith("Wrong mode for series length... 'r' for random lengths, 'f' for fixed lengths, 'a' for ascending lengths, and 'd' for descending lengths."); 
;;
 *)


(*let rec size(tree : 'a t_btree) : int =

  if(isEmpty(tree))
  then 0

  else
    (
      let l : 'a t_btree = lson(tree) in
      let r : 'a t_btree = rson(tree) in
      1 + size(l) + size(r);
    )
;;*)
