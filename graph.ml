(**********************************************************************)
(* Copyright (C) 2017 Clément Franchini                               *)
(*                                                                    *)
(* This file is part of mlcubes.                                      *)
(*                                                                    *)
(* mlcubes is free software: you can redistribute it and/or modify it *)
(* under the terms of the GNU General Public License as published by  *)
(* the Free Software Foundation, either version 3 of the License, or  *)
(* (at your option) any later version.                                *)
(*                                                                    *)
(* mlcubes is distributed in the hope that it will be useful, but     *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU   *)
(* General Public License for more details.                           *)
(*                                                                    *)
(* You should have received a copy of the GNU General Public License  *)
(* along with mlcubes. If not, see <http://www.gnu.org/licenses/>.    *)
(**********************************************************************)

type color = Graphics.color;;

type event_kind =
  | Mouse_move
  | Mouse_down
  | Mouse_up
  | Key of char
  | Window_resize of int * int
;;

type event =
  {
    mouse_x : int;
    mouse_y : int;
    event_kind : event_kind;
  }
;;

type rect =
  {
    left : int;
    bottom : int;
    width : int;
    height : int;
  }
;;

let dim m = Array.length m, Array.length m.(0);;

let foldi_matrix f e m =
  let h, w = dim m in
  let rec loop e i j =
    if i = h then
      e
    else if j = w then
      loop e (succ i) 0
    else
      let e = f e i j m.(i).(j) in
      loop e i (succ j) in
  loop e 0 0
;;


let get_image, fill_rect, draw_rect =
  (* Use that code to test rectangle fix
  let draw =
    Random.self_init ();
    let draw () =
      Random.int 3 - 1 in
    draw in
  let hack mthd =
    let dx = draw () in
    let dy = draw () in
    let dw = draw () in
    let dh = draw () in
    let hacked_method x y w h =
      mthd (x + dx) (y + dy) (w + dw) (h + dh) in
    hacked_method in
  let get_image = hack Graphics.get_image
  and fill_rect = hack Graphics.fill_rect
  and draw_rect = hack Graphics.draw_rect in
  get_image, fill_rect, draw_rect
   *)
  Graphics.get_image, Graphics.fill_rect, Graphics.draw_rect
;;

let extract_bound image =
  foldi_matrix
    (fun (min_i, min_j, max_i, max_j) i j color ->
     if color = Graphics.foreground then
       min min_i i, min min_j j, max max_i i, max max_j j
     else if color = Graphics.background then
       min_i, min_j, max_i, max_j
     else
       failwith "unknown color when fixing")
    (max_int, max_int, min_int, min_int)
    image
;;

let debug_clear w h =
  Graphics.set_color Graphics.background;
  let rec loop i j =
    if i = h then
      ()
    else if j = w then
      loop (succ i) 0
    else
      begin
        Graphics.plot j i;
        loop i (succ j)
      end in
  loop 0 0;
  Graphics.set_color Graphics.foreground
;;

let fix_get_image () =
  let get_diff get_image =
    debug_clear 50 50;
    Graphics.plot 10 10;
    let image = get_image 5 5 20 20 in
    let image = Graphics.dump_image image in
    let _, min_j, max_i, _ = extract_bound image in
    let h, w = dim image in
    Debug.fdebug
      1
      (fun epf ->
       Format.fprintf epf "fix_get_image %d %d %d %d" min_j max_i w h);
    let dx = min_j - 5 in
    let dy = h - max_i - 6 in
    let dw = 20 - w in
    let dh = 20 - h in
    Debug.fdebug
      1
      (fun epf ->
       Format.fprintf epf "fix_get_image %d %d %d %d" dx dy dw dh);
    dx, dy, dw, dh in
  let dx, dy, dw, dh = get_diff get_image in
  let get_image x y w h =
    get_image (x + dx) (y + dy) (w + dw) (h + dh) in
  assert (get_diff get_image = (0, 0, 0, 0));
  get_image
;;

let fix_rect_method debug_name rect_method get_image =
  let get_diff rect_method =
    debug_clear 50 50;
    rect_method 5 5 10 10;
    let image = get_image 0 0 20 20 in
    let image = Graphics.dump_image image in
    assert (dim image = (20, 20));
    let min_i, min_j, max_i, max_j = extract_bound image in
    Debug.fdebug
      1
      (fun epf ->
       Format.fprintf
         epf "fix_rect_method %s %d %d %d %d"
         debug_name min_i min_j max_i max_j);
    let dx = 5 - min_j in
    let dy = max_i - 14 in
    let dw = 9 - (max_j - min_j) in
    let dh = 9 - (max_i - min_i) in
    Debug.fdebug
      1
      (fun epf ->
       Format.fprintf
         epf "fix_rect_method %s %d %d %d %d" debug_name dx dy dw dh);
    dx, dy, dw, dh in
  let dx, dy, dw, dh = get_diff rect_method in
  let rect_method x y w h =
    rect_method (x + dx) (y + dy) (w + dw) (h + dh) in
  assert (get_diff rect_method = (0, 0, 0, 0));
  rect_method
;;

let fix_fill_rect = fix_rect_method "fill_rect" fill_rect
and fix_draw_rect = fix_rect_method "draw_rect" draw_rect
;;

let fix, get_image, fill_rect, draw_rect =
  let get_image_ref = ref (fun _ _ _ _ -> assert false) in
  let fill_rect_ref = ref (fun _ _ _ _ -> assert false) in
  let draw_rect_ref = ref (fun _ _ _ _ -> assert false) in
  let fix () =
    get_image_ref := fix_get_image ();
    fill_rect_ref := fix_fill_rect !get_image_ref;
    draw_rect_ref := fix_draw_rect !get_image_ref in
  let get_image x y w h = !get_image_ref x y w h in
  let fill_rect x y w h = !fill_rect_ref x y w h in
  let draw_rect x y w h = !draw_rect_ref x y w h in
  fix, get_image, fill_rect, draw_rect
;;

let debug_raw_event = function
  | { Graphics.
      mouse_x = mouse_x;
      mouse_y = mouse_y;
      button = button;
      keypressed = keypressed;
      key = key;
    } ->
    Debug.fdebug
      3
      (fun epf ->
       Debug.debug_sexp
         epf
         (fun epf ->
          Format.fprintf
            epf "RAWEVENT@ %d@ %d@ %B @ %B@ %C"
            mouse_x mouse_y button keypressed key))
;;

let debug_event event =
  Debug.fdebug
    2
    (fun epf ->
     Debug.debug_sexp
       epf
       (fun epf ->
        Format.fprintf
          epf "EVENT@ %d@ %d@ %t"
          event.mouse_x event.mouse_y
          (fun epf ->
           match event.event_kind with
           | Mouse_move -> Format.fprintf epf "MOVE"
           | Mouse_down -> Format.fprintf epf "DOWN"
           | Mouse_up -> Format.fprintf epf "UP"
           | Key k ->
             Debug.debug_sexp
               epf
               (fun epf -> Format.fprintf epf "KEY@ %C" k)
           | Window_resize (w, h) ->
             Debug.debug_sexp
               epf
               (fun epf -> Format.fprintf epf "RESIZE@ %d@ %d" w h))))
;;

let wait_next_event =
  let last_dim = ref (-1, -1) in
  let last_event =
    ref
      { Graphics.
        mouse_x = -1;
        mouse_y = -1;
        button = false;
        keypressed = false;
        key = '\000';
      } in
  let get, register =
    let events = Queue.create () in
    let get () =
      if Queue.is_empty events then
        None
      else
        Some (Queue.take events) in
    let register raw_event event =
      let mouse_x = raw_event.Graphics.mouse_x in
      let mouse_y = raw_event.Graphics.mouse_y in
      let event = {
          mouse_x = mouse_x;
          mouse_y = mouse_y;
          event_kind = event;
        } in
      Queue.add event events in
    get, register in
  let rec wait () =
    match get () with
    | None ->
      let event =
        Graphics.wait_next_event
          [
            Graphics.Button_down;
            Graphics.Button_up;
            Graphics.Key_pressed;
            Graphics.Mouse_motion;
          ] in
      debug_raw_event event;
      let (w, h) as dim = Graphics.size_x (), Graphics.size_y () in
      if dim <> !last_dim then
        begin
          last_dim := dim;
          register event (Window_resize (w, h))
        end;
      if event.Graphics.mouse_x <> !last_event.Graphics.mouse_x
         || event.Graphics.mouse_y <> !last_event.Graphics.mouse_y then
        register event Mouse_move;
      if event.Graphics.keypressed then
        register event (Key event.Graphics.key);
      if event.Graphics.button <> !last_event.Graphics.button then
        register
          event
          (if event.Graphics.button then Mouse_down else Mouse_up);
      last_event := event;
      wait ()
    | Some event ->
      debug_event event;
      event in
  wait
;;

let rgb = Graphics.rgb;;

let projection = ref (Geometry.identity 3);;

let model =
  let model = Stack.create () in
  Stack.push (Geometry.identity 3) model;
  model
;;

let with_graph title f =
  Graphics.open_graph "";
  fix ();
  Graphics.set_window_title title;
  let r =
    try
      Graphics.auto_synchronize false;
      let size_x = Graphics.size_x () in
      let size_y = Graphics.size_y () in
      let mouse_x, mouse_y = Graphics.mouse_pos () in
      f size_x size_y mouse_x mouse_y
    with
    | x ->
      Graphics.close_graph ();
      raise x in
  Graphics.close_graph ();
  r
;;

let mk_proj w h =
  let m = Geometry.identity 3 in
  let cx = float (w - 1) /. 2.0 in
  let cy = float (h - 1) /. 2.0 in
  let k = min cx cy in
  m.(0).(0) <- k;
  m.(0).(2) <- cx;
  m.(1).(1) <- k;
  m.(1).(2) <- cy;
  projection := m;
;;

let mk_hud () =
  let m = Geometry.identity 3 in
  projection := m;
;;

let clear color =
  Graphics.set_color color;
  fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ())
;;

let swap () = Graphics.synchronize ();;

let mult m =
  let om = Stack.pop model in
  let om = Geometry.mult_m_m om m in
  Stack.push om model
;;

let push () =
  let m = Stack.top model in
  let m2 = Geometry.init_m 3 3 (fun r c -> m.(r).(c)) in
  Stack.push m2 model
;;

let pop () = ignore (Stack.pop model);;

let with_proj f =
  push ();
  f ();
  pop ()
;;

let load_id () =
  ignore (Stack.pop model);
  let m = Geometry.identity 3 in
  Stack.push m model
;;

let rotate a =
  let c = cos a in
  let s = sin a in
  let m = Geometry.identity 3 in
  m.(0).(0) <- c;
  m.(0).(1) <- -. s;
  m.(1).(0) <- s;
  m.(1).(1) <- c;
  mult m
;;

let translate x y =
  let m = Geometry.identity 3 in
  m.(0).(2) <- x;
  m.(1).(2) <- y;
  mult m
;;

let scale k =
  let m = Geometry.identity 3 in
  m.(0).(0) <- k;
  m.(1).(1) <- k;
  mult m
;;

let project v =
  assert (Geometry.dim_v v = 3);
  let v = Geometry.mult_m_v (Stack.top model) v in
  let v = Geometry.mult_m_v !projection v in
  let x = v.(0) /. v.(2) in
  let y = v.(1) /. v.(2) in
  Common.int x, Common.int y
;;

let draw_poly points color =
  Graphics.set_color color;
  let points = Array.map project points in
  Graphics.draw_poly points
;;

let fill_poly points color =
  Graphics.set_color color;
  let points = Array.map project points in
  Graphics.fill_poly points
;;

let with_rect mthd color = function
  | {
      left = left;
      bottom = bottom;
      width = width;
      height = height;
    } ->
    Graphics.set_color color;
    mthd left bottom width height
;;

let fill_rect = with_rect fill_rect;;

let draw_rect = with_rect draw_rect;;

let text_size = Graphics.text_size;;

let draw_string =
  let mem = Hashtbl.create 10 in
  let draw_string x y bg fg scale text =
    begin
      if not (Hashtbl.mem mem (bg, fg, scale, text)) then
        let w, h = Graphics.text_size text in
        let old = get_image 0 0 w h in
        let rect =
          {
            left = 0;
            bottom = 0;
            width = w;
            height = h;
          } in
        fill_rect bg rect;
        Graphics.moveto 0 0;
        Graphics.set_color fg;
        Graphics.draw_string text;
        let text_image = get_image 0 0 w h in
        Graphics.draw_image old 0 0;
        let dump = Graphics.dump_image text_image in
        let scaled =
          Array.init
            (scale * Array.length dump)
            (fun i ->
             Array.init
               (scale * Array.length dump.(0))
               (fun j ->
                dump.(i / scale).(j / scale))) in
        Hashtbl.add
          mem (bg, fg, scale, text) (Graphics.make_image scaled);
    end;
    Graphics.moveto x y;
    Graphics.draw_image (Hashtbl.find mem (bg, fg, scale, text)) x y in
  draw_string
;;
