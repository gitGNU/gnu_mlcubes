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

let projection = ref (Geometry.identity 3);;

let model = Stack.create ();;

let with_graph f =
  assert (Stack.is_empty model);
  Graphics.clear_graph ();
  let w = Graphics.size_x () in
  let h = Graphics.size_y () in
  let m = Geometry.identity 3 in
  let cx = float (w - 1) /. 2.0 in
  let cy = float (h - 1) /. 2.0 in
  let k = min cx cy in
  m.(0).(0) <- k;
  m.(0).(2) <- cx;
  m.(1).(1) <- k;
  m.(1).(2) <- cy;
  projection := m;
  Stack.push (Geometry.identity 3) model;
  f ();
  ignore (Stack.pop model);
  Graphics.synchronize ()
;;

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

let int x = int_of_float (x +. if x < 0.0 then -0.5 else 0.5);;

let project v =
  assert (Geometry.dim_v v = 3);
  let v = Geometry.mult_m_v (Stack.top model) v in
  let v = Geometry.mult_m_v !projection v in
  let x = v.(0) /. v.(2) in
  let y = v.(1) /. v.(2) in
  int x, int y
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
