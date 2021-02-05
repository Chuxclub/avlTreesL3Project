(* AUTEURS DE CE MODULE: Esteban Mauricio, Florian Legendre *)

let _NODES_VALUES_MAX : int = 10000;;


let graphDataMaker(avl_function, maxSize : ('a -> 'b) * int) : float array * float array = 
  let treeSizes : float array = arr_make(maxSize+1, 0.0) in
  let tm : float array = arr_make(maxSize+1, 0.0) in
  (
    
    for j = 0 to maxSize
    do
      
      let tree = avl_rnd_create(_NODES_VALUES_MAX,j) in

      (* On fait la moyenne du temps d'exécution de l'ajout
         sur l'arbre généré aléatoirement de taille j en effectuant 100
         ajouts sur cet arbre (les ajouts sont ignorés à chaque tour de boucle 
         pour conserver la taille de l'arbre et ainsi avoir une moyenne représentative) *)
      for i = 1 to 100
      do 
        let randInt : int = Random.int _NODES_VALUES_MAX in
        tm.(j) <- tm.(j) +. chrono(avl_function, (randInt, tree));
      done;

      (* On rentre la moyenne dans un tableau qui servira de valeurs en ordonnées,
         treeSizes étant les valeurs en abscisses *)
      tm.(j) <- tm.(j) /. 100. ;
      treeSizes.(j) <- float_of_int(j) ;
      
    done ;

    (* On renvoit le tuple de tableaux ainsi produit et qui servira à la fonction 
       de dessin du graphe.*)
    (tm, treeSizes) ;
  )
;;


let plotGraph(avl_function, maxSize : ('a -> 'b) * int) : unit =

  let (value, ind) : float array * float array = ajtChrono(maxSize) in
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

