
let open_graph(dx, dy : int * int) : unit = 
  let s = ":1 "^string_of_int(dx)^"x"^string_of_int(dy) in
     Graphics.open_graph s ;;

let close_graph() : unit = Graphics.close_graph() ;;

let clear_graph() : unit = Graphics.clear_graph() ;;

let resize_window(x, y) = Graphics.resize_window x y ;;




let moveto(x,y : int * int) : unit = Graphics.moveto x y ;;

let lineto(x, y : int * int) = Graphics.lineto x y ;;

let plot(x, y : int * int) = Graphics.plot x y ;;

let current_point() = Graphics.current_point() ;;

let draw_poly_line(t) = Graphics.draw_poly_line t ;;

let draw_circle(x, y, r : int * int * int) = Graphics.draw_circle x y r ;;

let draw_rect(x,y,dx,dy : int * int * int * int) = Graphics.draw_rect x y dx dy ;;

let fill_poly(t) = Graphics.draw_poly_line t ;;

let fill_circle(x, y, r : int * int * int) = Graphics.draw_circle x y r ;;

let fill_rect(x,y,dx,dy : int * int * int * int) = Graphics.draw_rect x y dx dy ;;

let set_line_width(e) = Graphics.set_line_width e ;;

let draw_str(s : string) : unit = Graphics.draw_string s ;;



type t_color = Graphics.color ;;


let set_color(color : t_color) = Graphics.set_color color ;;

let black : t_color = Graphics.black ;;
let blue : t_color = Graphics.blue ;;
let red : t_color = Graphics.red ;;
let green : t_color = Graphics.green ;;
let white : t_color = Graphics.white ;;
let yellow : t_color = Graphics.yellow ;;
let cyan : t_color = Graphics.cyan ;;
let magenta : t_color = Graphics.magenta ;;


