
type t_rep = {orx : int ; ory : int ; extx : int ; exty : int} ;;


let draw_rep(rep : t_rep) : unit =
  moveto(rep.orx, rep.ory) ; lineto(rep.orx + rep.extx + 50, rep. ory) ;
  moveto(rep.orx, rep.ory) ; lineto(rep.orx, rep.ory + rep.exty + 50) ;
;;

let comp_rep(rep, xmin, xmax, sx, ymin, ymax, sy) = 
  moveto(rep.orx  + 10, rep.ory - 20) ; draw_str(string_of_float(xmin)) ;
  moveto(rep.orx + rep.extx-10, rep.ory - 20) ; draw_str(string_of_float(xmax)) ;
  moveto(rep.orx + rep.extx/2, rep.ory - 20) ; draw_str(sx) ;
  moveto(rep.orx - 20, rep.ory + 10) ; draw_str(string_of_float(ymin)) ;
  moveto(rep.orx - 20, rep.ory + rep.exty-10) ; draw_str(string_of_float(ymax)) ;
  moveto(rep.orx - 20, rep.ory + rep.exty/2) ; draw_str(sy)
;;

let min_max_float(t, ind_max : float array * int) : float * float =
  if ind_max < 0 
  then failwith "erreur min_max_float : tableau vide"
  else
    let min : float ref = ref (t.(0)) and max : float ref = ref (t.(0)) in
      for i = 1 to ind_max 
      do
        if !min > t.(i) then min := t.(i) ;
        if !max < t.(i) then max := t.(i) ;
      done ;
(!min, !max)
;;


let draw_curve(arr_val, arr_ind, ind_max, rep : float array * float array * int * t_rep) : unit =
  let (minx, maxx) : float * float = min_max_float(arr_ind, ind_max) in
  let (miny, maxy) : float * float = min_max_float(arr_val, ind_max) in
  let dx : float = float_of_int(rep.extx) /.  (maxx -. minx) in
  let dy : float = float_of_int(rep.exty) /. (maxy -. miny) in
  let x : int ref = ref 0 and y : int ref = ref 0 in
    (
    comp_rep(rep, minx, maxx, "taille de l'arbre", miny, maxy, "sec") ;
    moveto(rep.orx, rep.ory) ; 
    for i = 1 to ind_max 
    do
      x := rep.orx + int_of_float((arr_ind.(i) -. minx) *. dx) ;
      y := rep.ory + int_of_float((arr_val.(i) -. miny)*. dy) ;
      lineto(!x, !y)
    done
  )
;;
