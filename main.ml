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

let mk_square_minx_4_3 () =
  let positions =
    let rec add_positions positions = function
      | 16 -> positions
      | i ->
        let r = i / 4 in
        let c = i mod 4 in
        let x = Expr.int (2 * c - 3) in
        let y = Expr.int (3 - 2 * r) in
        let positions = Maps.Int.add i (x, y) positions in
        add_positions positions (succ i) in
    add_positions Maps.Int.empty 0 in
  let rotations =
    let mk_rotation dr dc =
      let mk_index i = i + dc + 4 * dr in
      let mk_cycle cycle = Common.map mk_index cycle in
      { Cube.
        center = Expr.int (2 * dc - 1), Expr.int (-1 - 2 * dr);
        cycles =
          Common.map
            mk_cycle
            [
              [ 0; 2; 10; 8; ];
              [ 1; 6; 9; 4; ];
              [ 5; ];
            ];
        order = 4;
      } in
    [
      mk_rotation 0 0;
      mk_rotation 0 1;
      mk_rotation 1 0;
      mk_rotation 1 1;
    ] in
  let skeleton =
    { Cube.
      positions = positions;
      rotations = rotations;
    } in
  let tiles =
    let color_of_tile = function
      | 0 | 1 | 4 | 5 -> 255, 0, 0
      | 2 | 3 | 6 | 7 -> 0, 255, 0
      | 8 | 9 | 12 | 13 -> 0, 0, 255
      | 10 | 11 | 14 | 15 -> 255, 255, 0
      | _ -> assert false in
    let rec add_tiles tiles = function
      | 16 -> tiles
      | i ->
        let tile =
          { Cube.
            orientation = 0, 4;
            polygons =
              [
                { Cube.
                  points =
                    [
                      Expr.int (-1), Expr.int (-1);
                      Expr.int (1), Expr.int (-1);
                      Expr.int (1), Expr.int (1);
                      Expr.int (-1), Expr.int (1);
                    ];
                  color = color_of_tile i;
                };
              ];
          } in
        let tiles = Maps.Int.add i tile tiles in
        add_tiles tiles (succ i) in
    add_tiles Maps.Int.empty 0 in
  { Cube.
    skeleton = skeleton;
    tiles = tiles;
  }
;;

let draw_polygon = function
  | { Cube.
      points = points;
      color = r, g, b;
    } ->
    let vector_of_point (x, y) =
      let x = Expr.eval x in
      let y = Expr.eval y in
      [| x; y; 1.0; |] in
    let points =
      Array.of_list (Common.map vector_of_point points) in
    Graph.fill_poly points (Graphics.rgb r g b);
    Graph.draw_poly points (Graphics.rgb 0 0 0)
;;

let draw_tile positions position = function
  | { Cube.
      orientation = i, n;
      polygons = polygons;
    } ->
    Graph.with_proj
      (fun () ->
       let x, y =
         let x, y = Maps.Int.find position positions in
         Expr.eval x, Expr.eval y in
       let angle = 2.0 *. pi *. float i /. float n in
       Graph.translate x y;
       Graph.rotate angle;
       List.iter draw_polygon polygons)
;;

let draw_cube = function
  | { Cube.
      skeleton = skeleton;
      tiles = tiles;
    } ->
    Graph.scale 0.15;
    let positions =
      match skeleton with
      | { Cube.
          positions = positions;
          rotations = _;
        } -> positions in
    Maps.Int.iter (draw_tile positions) tiles
;;

let main_loop () =
  let rec loop cube =
    Graph.with_graph
      (fun () -> Graph.with_proj (fun () -> draw_cube cube));
    let event =
      Graphics.wait_next_event [ Graphics.Key_pressed; ] in
    if event.Graphics.key <> 'q' then
      loop cube in
  Graphics.set_window_title "mlcubes v0";
  loop (mk_square_minx_4_3 ())
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
