(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)


(* =========================================================================================== *)
(* ======================================== Exercice 1 ======================================= *)
(* =========================================================================================== *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (1) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let bst_rnd_create(bound, treeSize : int * int) : int bst =

  let randABR : int bst ref = ref (empty()) in
  
  for i=1 to treeSize do
    let randInt : int = Random.int bound in
    randABR := bst_linsert(!randABR, randInt);
  done;

  !randABR
;;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)





(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let desequilibre(tree : 'a t_btree) : int =
  if(isEmpty(tree))
  then 0

  else
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
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)




(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (3) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let bst_rndSeries_create(treeSize, seriesLen : int * int) : int bst =

  let randABR : int bst ref = ref (empty()) in
  let fillerCount : int ref = ref treeSize in

  (* Tant que l'arbre n'est pas rempli on génère des sous-suites d'entiers ordonnés
     qu'on ajoute dans l'arbre: *)
  while(!fillerCount > 0) do
    let len : int = if(seriesLen<=0) then Random.int 101 else seriesLen in
    let randLowerBound : int ref = ref (Random.int 101) in

    (* On boucle jusqu'à la longueur de la sous-suite: *)
    for i=1 to len do

      (* Si l'arbre est rempli avant d'arriver au bout de la sous-suite, ce test 
         garantit qu'aucun ajout supplémentaire ne sera fait: *) 
      if(!fillerCount > 0)
      then (
        randABR := bst_linsert(!randABR, !randLowerBound);

        (* On remplit l'arbre... À chaque ajout de noeud on décrémente: *)
        fillerCount := !fillerCount - 1;

        (* Nos entiers sont croissants mais choisis aléatoirement: *)
        randLowerBound := Random.int(101) + !randLowerBound;
      )
    done;
    
  done;
  
  !randABR
;;

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

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
