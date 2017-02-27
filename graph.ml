(**********************************************************************)
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
  Graphics.fill_rect
    0 0 (Graphics.size_x () - 1) (Graphics.size_y () - 1)
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

let fill_rect = with_rect Graphics.fill_rect;;

let text_size = Graphics.text_size;;

let draw_string =
  let mem = Hashtbl.create 10 in
  let draw_string x y bg fg scale text =
    begin
      if not (Hashtbl.mem mem (bg, fg, scale, text)) then
        let w, h = Graphics.text_size text in
        let old = Graphics.get_image 0 0 w h in
        Graphics.set_color bg;
        Graphics.fill_rect 0 0 w h;
        Graphics.moveto 0 0;
        Graphics.set_color fg;
        Graphics.draw_string text;
        let text_image = Graphics.get_image 0 0 w h in
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
