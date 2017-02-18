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

let pi = acos (-. 1.0);;

let draw_tile color =
  Graph.with_proj
    (fun () ->
     Graph.translate 0.5 0.5;
     Graph.scale (5.0 /. 1.0);
     let points =
       [|
         [| 0.0; 0.0; 1.0; |];
         [| 1.0; 0.0; 1.0; |];
         [| 1.0; 1.0; 1.0; |];
         [| 0.0; 1.0; 1.0; |];
        |] in
     Graph.fill_poly points color)
;;

let draw_dummy_cube () =
  Graph.with_proj
    (fun () ->
     Graph.scale 0.1;
     draw_tile Graphics.green;
     Graph.rotate (pi /. 2.0);
     draw_tile Graphics.red;
     Graph.rotate (pi /. 2.0);
     draw_tile Graphics.blue;
     Graph.rotate (pi /. 2.0);
     draw_tile Graphics.yellow)
;;

let main_loop () =
  let rec loop () =
    Graph.with_graph draw_dummy_cube;
    let event =
      Graphics.wait_next_event [ Graphics.Key_pressed; ] in
    if event.Graphics.key <> 'q' then
      loop () in
  Graphics.set_window_title "mlcubes v0";
  loop ()
;;

let with_graphics f =
  Graphics.open_graph "";
  let r =
    try
      Graphics.auto_synchronize false;
      f ()
    with
    | x ->
      Graphics.close_graph ();
      raise x in
  Graphics.close_graph ();
  r
;;

let main () =
  try
    with_graphics main_loop;
    0
  with
  | x ->
    Printf.eprintf "# error: %s" (Printexc.to_string x);
    1
;;

let () = exit (main ());;
