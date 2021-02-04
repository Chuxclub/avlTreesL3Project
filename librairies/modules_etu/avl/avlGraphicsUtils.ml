(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)



let _NODES_VALUES_MAX : int = 10000;;

(* ============================================================================================= *)
(* =========================================== AJOUT =========================================== *)
(* ============================================================================================= *)

(* Graphe de complexité de l'ajout:  *)
let ajtChrono(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n+1, 0.0) in
  let test : ('a * int) t_btree ref = ref(empty()) in      
    (
    for j = 1 to n
    do
      let randInt : int = Random.int _NODES_VALUES_MAX in
      tm.(j) <- Sys.time() ;
      test := ajt_avl(randInt, !test); 
      tm.(j) <- Sys.time() -. tm.(j);
      ind.(j) <- float_of_int(j) ;
    done ;
    (tm, ind) ;
    )
;;


let ajtGraph(n : int) : unit =

  let (value, ind) : float array * float array = ajtChrono(n) in
  let rep : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  let close : char ref = ref 'o' in
  (
    open_graph(1000, 600);
    draw_rep(rep);
    draw_curve(value, ind,  arr_len(ind) - 1, rep);

    print_string("Enter y;; when you're done to cleanly close the graph:");
    close := read_char();

    if(!close == 'y')
    then
    (
      clear_graph();
      close_graph();
    )
    
  )  
;;




(* ============================================================================================= *)
(* ======================================== SUPPRESSION ======================================== *)
(* ============================================================================================= *)

(* Graphe de complexité de la suppression:  *)
let supprChrono(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n+1, 0.0) in
  let avlTest : 'a t_btree ref = ref (empty()) in
    (
      for j = 1 to n
      do
        let randIntAjt : int = Random.int _NODES_VALUES_MAX in
        let randIntSuppr : int = Random.int _NODES_VALUES_MAX in
        avlTest := ajt_avl(randIntAjt, !avlTest);
        
        tm.(j) <- Sys.time() ;
        ignore(suppr_avl(randIntSuppr, !avlTest)) ; 
        tm.(j) <- Sys.time() -. tm.(j);
        ind.(j) <- float_of_int(j) ;
      done ;
      (tm, ind) ;
    )
;;

let supprGraph(n : int) : unit =
  let (value, ind) : float array * float array = supprChrono(n) in
  let rep : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  let close : char ref = ref 'o' in
  (
    open_graph(1000, 600);
    draw_rep(rep);
    draw_curve(value, ind,  arr_len(ind) - 1, rep);

    print_string("Enter y;; when you're done to cleanly close the graph:");
    close := read_char();

    if(!close == 'y')
    then
    (
      clear_graph();
      close_graph();
    )
    
  )  
;;



(* ============================================================================================= *)
(* ========================================= RECHERCHE ========================================= *)
(* ============================================================================================= *)

(* Graphe de complexité de la recherche:  *)
let seekChrono(n : int) : float array * float array = 
  let ind : float array = arr_make(n+1, 0.0) in
  let tm : float array = arr_make(n + 1, 0.0) in
  let avlTest : 'a t_btree ref = ref (empty()) in
    (
    for j = 1 to n
    do
      let randIntAjt : int = Random.int _NODES_VALUES_MAX in
      let randIntSeek : int = Random.int _NODES_VALUES_MAX in
      avlTest := ajt_avl(randIntAjt, !avlTest);
        
      tm.(j) <- Sys.time();
      ignore(seek_avl(randIntSeek, !avlTest)); 
      tm.(j) <- Sys.time() -. tm.(j);
      ind.(j) <- float_of_int(j) ;
    done ;
    (tm, ind) ;
    )
;;

let seekGraph(n : int) : unit =

  let (value, ind) : float array * float array = seekChrono(n) in
  let rep : t_rep = {orx = 50; ory = 50; extx = 900; exty = 500} in
  let close : char ref = ref 'o' in
  (
    open_graph(1000, 600);
    draw_rep(rep);
    draw_curve(value, ind,  arr_len(ind) - 1, rep);

    print_string("Enter y;; when you're done to cleanly close the graph:");
    close := read_char();

    if(!close == 'y')
    then
    (
      clear_graph();
      close_graph();
    )
    
  )  
;;
