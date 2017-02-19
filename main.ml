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

type mouse_state =
  | Free
  | Locked of int * int * float
;;

type state =
  {
    cube : Cube.t;
    hl : int;
    mouse_state : mouse_state;
  }
;;

let pi = acos (-. 1.0);;

let ( / ) a b =
  let q = a / b in
  if a mod b < 0 then
    q - 1
  else
    q
and ( mod ) a b =
  let r = a mod b in
  if r < 0 then
    r + b
  else
    r
;;

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
      let mk_cycle cycle = Array.map mk_index cycle in
      { Cube.
        center = Expr.int (2 * dc - 1), Expr.int (1 - 2 * dr);
        cycles =
          Common.map
            mk_cycle
            [
              [| 0; 8; 10; 2; |];
              [| 1; 4; 9; 6; |];
              [| 5; |];
            ];
        order = 4;
      } in
    let add_rotation rotations (i, dr, dc) =
      let rotation = mk_rotation dr dc in
      Maps.Int.add i rotation rotations in
    List.fold_left
      add_rotation
      Maps.Int.empty
      [
        0, 0, 0;
        1, 0, 1;
        2, 1, 0;
        3, 1, 1;
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

let get_color is_hl r g b =
  let t b =
    if is_hl then
      b
    else
      Common.clamp 0 (b / 2) 255 in
  let r = t r in
  let g = t g in
  let b = t b in
  Graphics.rgb r g b
;;

let draw_polygon is_hl = function
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
    Graph.fill_poly points (get_color is_hl r g b);
    Graph.draw_poly points (Graphics.rgb 0 0 0)
;;

let draw_tile position is_hl = function
  | { Cube.
      orientation = i, n;
      polygons = polygons;
    } ->
    Graph.with_proj
      (fun () ->
       let x, y = position in
       let x, y = Expr.eval x, Expr.eval y in
       assert (i = i mod n);
       let angle = 2.0 *. pi *. float i /. float n in
       Graph.translate x y;
       Graph.rotate angle;
       List.iter (draw_polygon is_hl) polygons)
;;

let draw_cube hl hl_angle = function
  | { Cube.
      skeleton = skeleton;
      tiles = tiles;
    } ->
    let positions, center, hl_tiles =
      match skeleton with
      | { Cube.
          positions = positions;
          rotations = rotations;
        } ->
        let rotation = Maps.Int.find hl rotations in
        let tiles =
          List.fold_left
            (fun tiles cycle ->
             List.rev_append (Array.to_list cycle) tiles)
            []
            rotation.Cube.cycles in
        positions, rotation.Cube.center, tiles in
    let fg_tiles, bg_tiles =
      let predictate i _ = List.mem i hl_tiles in
      Maps.Int.partition predictate tiles in
    Maps.Int.iter
      (fun i tile ->
       let position = Maps.Int.find i positions in
       draw_tile position false tile)
      bg_tiles;
    Graph.with_proj
      (fun () ->
       if hl_angle <> 0.0 then
         begin
           let cx, cy = center in
           let cx, cy = Expr.eval cx, Expr.eval cy in
           Graph.translate cx cy;
           Graph.rotate hl_angle;
           Graph.translate (-. cx) (-. cy);
         end;
       Maps.Int.iter
         (fun i tile ->
          let position = Maps.Int.find i positions in
          draw_tile position true tile)
         fg_tiles);
;;

let kangle = -. 0.01;;

let get_angle cx cy lx ly mx my =
  match Config.get_rotation_mode () with
  | Config.Fast -> kangle *. (float mx -. float lx)
  | Config.Intuitive ->
    let cx, cy = Expr.eval cx, Expr.eval cy in
    let cx, cy = Graph.project [| cx; cy; 1.0; |] in
    let l = [| float lx -. float cx; float ly -. float cy; |] in
    let m = [| float mx -. float cx; float my -. float cy; |] in
    Geometry.norm_v
      l
      (fun () -> 0.0)
      (fun l ->
       Geometry.norm_v
         m
         (fun () -> 0.0)
         (fun m ->
          let c = Geometry.mult_v_v m l in
          let s = Geometry.mult_v_v m [| -. l.(1); l.(0); |] in
          atan2 s c))
;;

let draw_state = function
  | {
      cube = cube;
      hl = hl;
      mouse_state = mouse_state;
    } ->
    let angle =
      match mouse_state with
      | Free -> 0.0
      | Locked (_, _, angle) -> angle in
    draw_cube hl angle cube
;;

let mouse_of_event = function
    { Graphics.
      mouse_x = mouse_x;
      mouse_y = mouse_y;
      button = _;
      keypressed = _;
      key = _;
    } -> mouse_x, mouse_y
;;

let find_hl mx my = function
  | { Cube.
      positions = _;
      rotations = rotations;
    } ->
    let dist2_of_rotation = function
      | { Cube.
          center = center;
          cycles = _;
          order = _;
        } ->
        let cx, cy = center in
        let cx, cy = Expr.eval cx, Expr.eval cy in
        let cx, cy = Graph.project [| cx; cy; 1.0; |] in
        let dx = cx - mx in
        let dy = cy - my in
        dx * dx + dy * dy in
    let hl, _ =
      Maps.Int.fold
        (fun i rotation (hl, dist2) ->
         let d2 = dist2_of_rotation rotation in
         if d2 < dist2 then
           i, d2
         else
           hl, dist2)
        rotations
        (-1, max_int) in
    hl
;;

let add_rotation (a, b) (c, d) =
  let n = a * d + b * c in
  let d = b * d in
  let g = Common.gcd n d in
  let n = n / g in
  let d = d / g in
  n mod d, d
;;

let apply_rotation tiles rotation count =
  Debug.fdebug 1 (fun epf -> Format.fprintf epf "rotate %d" count);
  let cycles = rotation.Cube.cycles in
  let mappings = Hashtbl.create 10 in
  let map_cycle cycle =
    let l = Array.length cycle in
    let rec loop i =
      if i < l then
        let ni = (i + count) mod l in
        Hashtbl.add mappings cycle.(i) cycle.(ni);
        loop (succ i) in
    loop 0 in
  List.iter map_cycle cycles;
  Hashtbl.fold
    (fun src dst new_tiles ->
     let tile = Maps.Int.find src tiles in
     let new_orientation =
       add_rotation
         tile.Cube.orientation (count, rotation.Cube.order) in
     let tile =
       {
         tile with Cube.orientation = new_orientation;
       } in
     Maps.Int.add dst tile new_tiles)
    mappings
    tiles
;;

let handle_event state event =
  match event with
  | { Graphics.
      mouse_x = mx;
      mouse_y = my;
      button = button;
      keypressed =_;
      key = _;
    } ->
    match state.mouse_state with
    | Free ->
      let hl = find_hl mx my state.cube.Cube.skeleton in
      let state =
        {
          state with
          hl = hl;
        } in
      if button then
        let mouse_state = Locked (mx, my, 0.0) in
        { state with mouse_state = mouse_state; }
      else
        state
    | Locked (lx, ly, _) ->
      let rotation =
        Maps.Int.find
          state.hl
          state.cube.Cube.skeleton.Cube.rotations in
      let angle =
        let cx, cy = rotation.Cube.center in
        get_angle cx cy lx ly mx my in
      if button then
        let mouse_state = Locked (lx, ly, angle) in
        { state with mouse_state = mouse_state; }
      else
        let order = rotation.Cube.order in
        let angle = float order *. angle /. (2.0 *. pi) in
        let angle = Common.int angle in
        let angle =
          if angle < 0 then
            angle + order
          else
            angle in
        let tiles =
          apply_rotation state.cube.Cube.tiles rotation angle in
        let cube = { state.cube with Cube.tiles = tiles; } in
        {
          cube = cube;
          hl = find_hl mx my state.cube.Cube.skeleton;
          mouse_state = Free;
        }
;;

let debug_event = function
  | { Graphics.
      mouse_x = mouse_x;
      mouse_y = mouse_y;
      button = button;
      keypressed = keypressed;
      key = key;
    } ->
    Debug.fdebug
      2
      (fun epf ->
       Debug.debug_sexp
         epf
         (fun epf ->
          Format.fprintf
            epf "EVENT@ %d@ %d@ %B @ %B@ %C"
            mouse_x mouse_y button keypressed key))
;;

let rec loop state =
  Graph.mk_proj ();
  Graph.clear 0 0 0;
  draw_state state;
  Graph.swap ();
  let event =
    Graphics.wait_next_event
      [
        Graphics.Button_down;
        Graphics.Button_up;
        Graphics.Key_pressed;
        Graphics.Mouse_motion;
      ] in
  debug_event event;
  if event.Graphics.key <> 'q' then
    let state = handle_event state event in
    loop state
;;

let main_loop () =
  Graphics.set_window_title "mlcubes v0";
  Graph.mk_proj ();
  Graph.scale 0.15;
  let event = Graphics.wait_next_event [ Graphics.Poll; ] in
  let mx, my = mouse_of_event event in
  let cube = mk_square_minx_4_3 () in
  let hl = find_hl mx my cube.Cube.skeleton in
  let state =
    {
      cube = cube;
      hl = hl;
      mouse_state = Free;
    } in
  loop state
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

let speclist =
  let set_debug = Arg.Int Config.set_debug_level in
  let set_rotation_mode =
    Arg.Symbol
      ([ "fast"; "intuitive"; ],
       (fun rotation_mode ->
        let rotation_mode =
          match rotation_mode with
          | "fast" -> Config.Fast
          | "intuitive" -> Config.Intuitive
          | _ -> assert false in
        Config.set_rotation_mode rotation_mode)) in
  Arg.align
    [
      "-d", set_debug, "<level> Set debug level";
      "--mode", set_rotation_mode, "<rotation-mode> Set rotation mode";
    ]
;;

let anon_fun s =
  raise (Arg.Bad (Printf.sprintf "unknown argument %S" s))
;;

let usage_msg =
  Printf.sprintf "%s [options]" (Filename.basename Sys.argv.(0))
;;

let main () =
  try
    Arg.parse speclist anon_fun usage_msg;
    with_graphics main_loop;
    0
  with
  | x ->
    Printf.eprintf "# error: %s" (Printexc.to_string x);
    1
;;

let () = exit (main ());;
