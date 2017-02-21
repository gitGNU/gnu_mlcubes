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

type locked =
  {
    locked_hl : int;
    locked_x : int;
    locked_y : int;
    locked_angle : float;
  }
;;

type mouse_state =
  | Free of int option
  | Locked of locked
;;

type dimension =
  {
    max_x : float;
    max_y : float;
    min_x : float;
    min_y : float;
  }
;;

type state =
  {
    cube : Cube.t;
    dimension : dimension;
    mouse_state : mouse_state;
  }
;;

type event_kind =
  | Mouse_move
  | Mouse_down
  | Mouse_up
  | Key of char
;;

type event =
  {
    mouse_x : int;
    mouse_y : int;
    event_kind : event_kind;
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

let draw_cube hl_angle_opt = function
  | { Cube.
      skeleton = skeleton;
      tiles = tiles;
    } ->
    let positions, center_hl_tiles_angle_opt =
      match skeleton with
      | { Cube.
          positions = positions;
          rotations = rotations;
        } ->
        let center_hl_tiles_angle_opt =
          match hl_angle_opt with
          | None -> None
          | Some (hl, angle) ->
            let rotation = Maps.Int.find hl rotations in
            let tiles =
              List.fold_left
                (fun tiles cycle ->
                 List.rev_append (Array.to_list cycle) tiles)
                []
                rotation.Cube.cycles in
            Some (rotation.Cube.center, tiles, angle) in
        positions, center_hl_tiles_angle_opt in
    let fg_tiles, bg_tiles =
      let predictate i _ =
        match center_hl_tiles_angle_opt with
        | None -> true
        | Some (_, hl_tiles, _) -> List.mem i hl_tiles in
      Maps.Int.partition predictate tiles in
    Maps.Int.iter
      (fun i tile ->
       let position = Maps.Int.find i positions in
       draw_tile position false tile)
      bg_tiles;
    Graph.with_proj
      (fun () ->
       begin
         match center_hl_tiles_angle_opt with
         | None -> ()
         | Some (center, _, angle) ->
           let cx, cy = center in
           let cx, cy = Expr.eval cx, Expr.eval cy in
           Graph.translate cx cy;
           Graph.rotate angle;
           Graph.translate (-. cx) (-. cy);
       end;
       Maps.Int.iter
         (fun i tile ->
          let position = Maps.Int.find i positions in
          Debug.fdebug
            2
            (fun epf ->
             Format.fprintf
               epf "drawing tile %d at %f %f"
               i
               (Expr.eval (fst position)) (Expr.eval (snd position)));
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
      dimension = _;
      mouse_state = mouse_state;
    } ->
    let hl_angle_opt =
      match mouse_state with
      | Free hl ->
        begin
          match hl with
          | None -> None
          | Some hl -> Some (hl, 0.0)
        end
      | Locked locked -> Some (locked.locked_hl, locked.locked_angle) in
    draw_cube hl_angle_opt cube
;;

let find_hl mx my = function
  | {
      cube = cube;
      dimension = dimension;
      mouse_state = _;
    } ->
    let min_x, min_y =
      Graph.project [| dimension.min_x; dimension.min_y; 1.0; |] in
    let max_x, max_y =
      Graph.project [| dimension.max_x; dimension.max_y; 1.0; |] in
    Debug.fdebug
      3
      (fun epf ->
       Format.fprintf
         epf "PROJRECT@ %d@ %d@ %d@ %d@ %d@ %d"
         min_x min_y max_x max_y mx my);
    if min_x <= mx && mx <= max_x
       && min_y <= my && my <= max_y then
      let rotations = cube.Cube.skeleton.Cube.rotations in
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
      Some hl
    else
      None
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
  match state.mouse_state with
  | Free _ ->
    let hl = find_hl event.mouse_x event.mouse_y state in
    let mouse_state =
      match event.event_kind with
      | Mouse_move -> Free hl
      | Mouse_down ->
        begin
          match hl with
          | None -> Free None
          | Some hl ->
            let locked =
              {
                locked_hl = hl;
                locked_x = event.mouse_x;
                locked_y = event.mouse_y;
                locked_angle = 0.0;
              } in
            Locked locked
        end
      | Mouse_up -> Free hl
      | Key _ -> Free hl in
    {
      state with mouse_state = mouse_state;
    }
  | Locked locked ->
    let rotation =
      Maps.Int.find
        locked.locked_hl
        state.cube.Cube.skeleton.Cube.rotations in
    let angle =
      let cx, cy = rotation.Cube.center in
      get_angle
        cx cy
        locked.locked_x locked.locked_y
        event.mouse_x event.mouse_y in
    match event.event_kind with
    | Mouse_move ->
      let locked =
        {
          locked with
          locked_angle = angle;
        } in
      let mouse_state = Locked locked in
      { state with mouse_state = mouse_state; }
    | Mouse_down -> assert false
    | Mouse_up ->
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
      let hl = find_hl event.mouse_x event.mouse_y state in
      {
        state with
        cube = cube;
        mouse_state = Free hl;
      }
    | Key _ ->
      let locked =
        {
          locked with
          locked_angle = angle;
        } in
      let mouse_state = Locked locked in
      { state with mouse_state = mouse_state; }
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
               (fun epf -> Format.fprintf epf "KEY@ %C" k))))
;;

let wait_next_event =
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

let rec loop state =
  Graph.mk_proj ();
  Graph.clear 0 0 0;
  draw_state state;
  Graph.swap ();
  let event = wait_next_event () in
  if event.event_kind <> Key 'q' then
    let state = handle_event state event in
    loop state
;;

let merge_dimension dimension x y =
  match dimension with
  | {
      max_x = max_x;
      max_y = max_y;
      min_x = min_x;
      min_y = min_y;
    } ->
    {
      max_x = max max_x x;
      max_y = max max_y y;
      min_x = min min_x x;
      min_y = min min_y y;
    }
;;

let get_dimension = function
  | { Cube.
      skeleton = skeleton;
      tiles = tiles;
    } ->
    let min_float = -. max_float in
    let dimension =
      {
        max_x = min_float;
        max_y = min_float;
        min_x = max_float;
        min_y = max_float;
      } in
    let positions = skeleton.Cube.positions in
    Maps.Int.fold
      (fun i tile dimension ->
       let cx, cy = Maps.Int.find i positions in
       let cx, cy = Expr.eval cx, Expr.eval cy in
       let z =
         List.fold_left
           (fun z polygon ->
            List.fold_left
              (fun z (px, py) ->
               let px, py = Expr.eval px, Expr.eval py in
               let z = max z (abs_float px) in
               let z = max z (abs_float py) in
               z)
              z
              polygon.Cube.points)
           0.0
           tile.Cube.polygons in
       let dimension = merge_dimension dimension (cx -. z) (cy -. z) in
       let dimension = merge_dimension dimension (cx +. z) (cy +. z) in
       dimension)
      tiles
      dimension
;;

let main_loop () =
  Graphics.set_window_title "mlcubes v0";
  let mx, my = Graphics.mouse_pos () in
  let cube = mk_square_minx_4_3 () in
  let dimension = get_dimension cube in
  begin
    match dimension with
    | {
        max_x = max_x;
        max_y = max_y;
        min_x = min_x;
        min_y = min_y;
      } ->
      let cx = (min_x +. max_x) /. 2.0 in
      let cy = (min_y +. max_y) /. 2.0 in
      let dx = max_x -. min_x in
      let dy = max_y -. min_y in
      Graph.scale (0.9 *. sqrt 2.0 /. max dx dy);
      Graph.translate (-. cx) (-. cy)
  end;
  Debug.fdebug
    1
    (fun epf ->
     Debug.debug_sexp
       epf
       (fun epf ->
        Format.fprintf
          epf "DIMENSION@ %f@ %f@ %f@ %f"
          dimension.min_x dimension.min_y
          dimension.max_x dimension.max_y));
  let state =
    {
      cube = cube;
      dimension = dimension;
      mouse_state = Free None;
    } in
  let hl = find_hl mx my state in
  let state = { state with mouse_state = Free hl; } in
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
    prerr_newline ();
    1
;;

let () = exit (main ());;
