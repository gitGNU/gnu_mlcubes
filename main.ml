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

type dimension =
  {
    max_x : float;
    max_y : float;
    min_x : float;
    min_y : float;
  }
;;

type menu_entry =
  {
    entry_text : string;
    entry_rect : Graph.rect;
    entry_text_left : int;
    entry_text_bottom : int;
  }
;;

type menu =
  {
    menu_scale : int;
    menu_rect : Graph.rect;
    menu_entries : menu_entry array;
    menu_selected : int option;
  }
;;

type locked =
  {
    locked_hl : int;
    locked_x : int;
    locked_y : int;
    locked_angle : float;
  }
;;

type cube_state =
  | Free of int option
  | Locked of locked
;;

type game_state =
  | Cube of cube_state
  | Menu of menu
;;

type cube_data =
  {
    cube : Cube.t;
    dimension : dimension;
  }
;;

type state =
  {
    size_x : int;
    size_y : int;
    cube_name : string;
    cube_data : cube_data;
    cubes : cube_data Maps.String.t;
    game_state : game_state;
  }
;;

type action_kind =
  | Continue
  | Quit
;;

type action =
  {
    action_kind : action_kind;
    state : state;
  }
;;

let mk_action action_kind state =
  {
    action_kind = action_kind;
    state = state;
  }
;;

let continue = mk_action Continue
and quit = mk_action Quit
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

let rect_contains rect mx my =
  mx >= rect.Graph.left && mx < rect.Graph.left + rect.Graph.width
  && my >= rect.Graph.bottom
  && my < rect.Graph.bottom + rect.Graph.height
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
  Graph.rgb r g b
;;

let green = Graph.rgb 0 255 0
and black = Graph.rgb 0 0 0
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
    Graph.draw_poly points black
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

let draw_cube_state cube_data cube_state =
  let hl_angle_opt =
    match cube_state with
    | Free hl ->
      begin
        match hl with
        | None -> None
        | Some hl -> Some (hl, 0.0)
      end
    | Locked locked -> Some (locked.locked_hl, locked.locked_angle) in
  draw_cube hl_angle_opt cube_data.cube
;;

let mk_menu size_x size_y entries =
  let hpad = 21 in
  let vpad = 5 in
  let w, h =
    Array.fold_left
      (fun (w, h) t ->
       let tw, th = Graph.text_size t in
       max w tw, max h th)
      (0, 0)
      entries in
  let fw = w + 2 * hpad + 2 in
  let fh = Array.length entries * (h + 2 * vpad) + 2 in
  let scale =
    let scale_x = size_x / fw in
    let scale_y = size_y / fh in
    min scale_x scale_y in
  let scale = max 1 (scale / 2) in
  let fw = scale * fw in
  let fh = scale * fh in
  let x0 = (size_x - fw) / 2 in
  let y0 = (size_y - fh) / 2 in
  let menu_rect =
    { Graph.
      left = x0;
      bottom = y0;
      width = fw;
      height = fh;
    } in
  let menu_entries =
    Array.mapi
      (fun ai text ->
       let gi = Array.length entries - ai - 1 in
       let rect =
         { Graph.
           left = x0 + scale;
           bottom = y0 + scale + gi * scale * (h + 2 * vpad);
           width = fw - 2 * scale;
           height = scale * (h + 2 * vpad);
         } in
       {
         entry_text = text;
         entry_rect = rect;
         entry_text_left = x0 + scale * (1 + hpad);
         entry_text_bottom =
           y0 + scale * (1 + vpad + gi * (h + 2 * vpad));
       })
      entries in
  {
    menu_scale = scale;
    menu_rect = menu_rect;
    menu_entries = menu_entries;
    menu_selected = None;
  }
;;

let menu_fg_color = green
and menu_bg_color = black
;;

let draw_menu _size_x _size_y = function
  | {
      menu_scale = menu_scale;
      menu_rect = menu_rect;
      menu_entries = menu_entries;
      menu_selected = menu_selected;
    } ->
    Graph.fill_rect menu_fg_color menu_rect;
    let inner_rect =
      { Graph.
        left = menu_rect.Graph.left + menu_scale;
        bottom = menu_rect.Graph.bottom + menu_scale;
        width = menu_rect.Graph.width - 2 * menu_scale;
        height = menu_rect.Graph.height - 2 * menu_scale;
      } in
    Graph.fill_rect menu_bg_color inner_rect;
    Array.iteri
      (fun ai ->
       function
       | {
           entry_text = text;
           entry_rect = rect;
           entry_text_left = x;
           entry_text_bottom = y;
         } ->
         let bg, fg =
           if Some ai = menu_selected then
             begin
               Graph.fill_rect menu_fg_color rect;
               menu_fg_color, menu_bg_color
             end
           else
             menu_bg_color, menu_fg_color in
         Graph.draw_string x y bg fg menu_scale text)
      menu_entries
;;

let draw_state = function
  | {
      size_x = size_x;
      size_y = size_y;
      cube_name = _;
      cube_data = cube_data;
      cubes = _;
      game_state = game_state;
    } ->
    Graph.mk_proj size_x size_y;
    Graph.clear black;
    begin
      match game_state with
      | Cube cube_state -> draw_cube_state cube_data cube_state
      | Menu menu ->
        draw_cube None cube_data.cube;
        draw_menu size_x size_y menu
    end;
    Graph.swap ();
;;

let find_hl_menu mx my menu_entries =
  let len = Array.length menu_entries in
  let rec loop i =
    if i = len then
      None
    else if rect_contains menu_entries.(i).entry_rect mx my then
      Some i
    else
      loop (succ i) in
  loop 0
;;

let find_hl_cube mx my = function
  | {
      cube = cube;
      dimension = dimension;
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
      if hl = -1 then
        None
      else
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

let handle_event_cube_state size_x size_y cube_data cube_state event =
  match cube_state with
  | Free _ ->
    let hl =
      find_hl_cube event.Graph.mouse_x event.Graph.mouse_y cube_data in
    let game_state =
      match event.Graph.event_kind with
      | Graph.Mouse_move -> Cube (Free hl)
      | Graph.Mouse_down ->
        begin
          match hl with
          | None -> Cube (Free None)
          | Some hl ->
            let locked =
              {
                locked_hl = hl;
                locked_x = event.Graph.mouse_x;
                locked_y = event.Graph.mouse_y;
                locked_angle = 0.0;
              } in
            Cube (Locked locked)
        end
      | Graph.Mouse_up -> Cube (Free hl)
      | Graph.Key '\t' ->
        let entries =
          [|
            "RESUME";
            "SHUFFLE";
            "SQUAREMINX 4-3";
            "TRIMINX 3-2";
            "TRIMINX 4-3";
            "QUIT";
           |] in
        let menu = mk_menu size_x size_y entries in
        let hl =
          find_hl_menu
            event.Graph.mouse_x event.Graph.mouse_y menu.menu_entries in
        let menu = { menu with menu_selected = hl; } in
        Menu menu
      | Graph.Key _ -> Cube (Free hl)
      | Graph.Window_resize (_, _) -> Cube (Free hl) in
    cube_data, game_state
  | Locked locked ->
    let rotation =
      Maps.Int.find
        locked.locked_hl
        cube_data.cube.Cube.skeleton.Cube.rotations in
    let angle =
      let cx, cy = rotation.Cube.center in
      get_angle
        cx cy
        locked.locked_x locked.locked_y
        event.Graph.mouse_x event.Graph.mouse_y in
    let cube_data, cube_state =
      match event.Graph.event_kind with
      | Graph.Mouse_move ->
        let locked =
          {
            locked with
            locked_angle = angle;
          } in
        let cube_state = Locked locked in
        cube_data, cube_state
      | Graph.Mouse_down -> assert false
      | Graph.Mouse_up ->
        let order = rotation.Cube.order in
        let angle = float order *. angle /. (2.0 *. pi) in
        let angle = Common.int angle in
        let angle =
          if angle < 0 then
            angle + order
          else
            angle in
        let tiles =
          apply_rotation cube_data.cube.Cube.tiles rotation angle in
        let cube = { cube_data.cube with Cube.tiles = tiles; } in
        let hl =
          find_hl_cube
            event.Graph.mouse_x event.Graph.mouse_y cube_data in
        let cube_data = { cube_data with cube = cube; } in
        let cube_state = Free hl in
        cube_data, cube_state
      | Graph.Key _ ->
        let locked =
          {
            locked with
            locked_angle = angle;
          } in
        cube_data, Locked locked
      | Graph.Window_resize _ ->
        let locked =
          {
            locked with
            locked_angle = angle;
          } in
        cube_data, Locked locked in
    cube_data, Cube cube_state
;;

let shuffle = function
  | { Cube.
      skeleton = skeleton;
      tiles = tiles;
    } as cube ->
    let rotations = Maps.Int.bindings skeleton.Cube.rotations in
    let rotations = Array.of_list (Common.map snd rotations) in
    let l = Array.length rotations in
    let rec loop tiles = function
      | 0 -> tiles
      | k ->
        let rotation = rotations.(Random.int l) in
        let count = Random.int rotation.Cube.order in
        let tiles = apply_rotation tiles rotation count in
        loop tiles (pred k) in
    let tiles = loop tiles (500 + Random.int 500) in
    { cube with Cube.tiles = tiles; }
;;

let adjust_to_dimension = function
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
    Graph.load_id ();
    Graph.scale (0.9 *. sqrt 2.0 /. max dx dy);
    Graph.translate (-. cx) (-. cy)
;;

let switch_to_cube cube_name state =
  let cubes =
    Maps.String.add state.cube_name state.cube_data state.cubes in
  let cube_data = Maps.String.find cube_name cubes in
  let cubes = Maps.String.remove cube_name cubes in
  adjust_to_dimension cube_data.dimension;
  {
    state with
    cube_name = cube_name;
    cube_data = cube_data;
    cubes = cubes;
  }
;;

let handle_event_menu state menu event =
  let hl =
    find_hl_menu
      event.Graph.mouse_x event.Graph.mouse_y menu.menu_entries in
  let menu = { menu with menu_selected = hl; } in
  let state = { state with game_state = Menu menu; }in
  match event.Graph.event_kind with
  | Graph.Mouse_move -> continue state
  | Graph.Mouse_down -> continue state
  | Graph.Mouse_up ->
    begin
      match menu.menu_selected with
      | None -> continue { state with game_state = Menu menu; }
      | Some index ->
        let entry = menu.menu_entries.(index).entry_text in
        Debug.fdebug
          1
          (fun epf ->
           Format.fprintf epf "entry %S selected" entry);
        match entry with
        | "RESUME" ->
          continue { state with game_state = Cube (Free hl); }
        | "SHUFFLE" ->
          let cube = shuffle state.cube_data.cube in
          let cube_data = { state.cube_data with cube = cube; } in
          let hl =
            find_hl_cube
              event.Graph.mouse_x event.Graph.mouse_y cube_data in
          let state =
            {
              state with
              cube_data = cube_data;
              game_state = Cube (Free hl);
            } in
          continue state
        | "SQUAREMINX 4-3"
        | "TRIMINX 3-2"
        | "TRIMINX 4-3" as cube_name ->
          let cube_name =
            match cube_name with
            | "SQUAREMINX 4-3" -> "Squareminx-4-3"
            | "TRIMINX 3-2" -> "Triminx-3-2"
            | "TRIMINX 4-3" -> "Triminx-4-3"
            | _ -> assert false in
          let state = switch_to_cube cube_name state in
          let hl =
            find_hl_cube
              event.Graph.mouse_x event.Graph.mouse_y state.cube_data in
          continue { state with game_state = Cube (Free hl); }
        | "QUIT" -> quit state
        | _ -> assert false
    end
  | Graph.Key '\t' ->
    let hl =
      find_hl_cube
        event.Graph.mouse_x event.Graph.mouse_y state.cube_data in
    continue { state with game_state = Cube (Free hl); }
  | Graph.Key _ -> continue state
  | Graph.Window_resize _ ->
    let menu =
      mk_menu
        state.size_x state.size_y
        (Array.map (fun entry -> entry.entry_text) menu.menu_entries) in
    let hl =
      find_hl_menu
        event.Graph.mouse_x event.Graph.mouse_y menu.menu_entries in
    let menu = { menu with menu_selected = hl; } in
    continue { state with game_state = Menu menu; }
;;

let handle_event state event =
  match state.game_state with
  | Cube cube_state ->
    let cube_data, game_state =
      handle_event_cube_state
        state.size_x state.size_y
        state.cube_data cube_state event in
    let state =
      {
        state with
        cube_data = cube_data;
        game_state = game_state;
      } in
    continue state
  | Menu menu -> handle_event_menu state menu event
;;

let rec loop state =
  draw_state state;
  let event = Graph.wait_next_event () in
  if
    event.Graph.event_kind <> Graph.Key 'q'
    && event.Graph.event_kind <> Graph.Key '\027'
  then
    let state =
      match event.Graph.event_kind with
      | Graph.Mouse_move
      | Graph.Mouse_down
      | Graph.Mouse_up
      | Graph.Key _ -> state
      | Graph.Window_resize (size_x, size_y) ->
        {
          state with
          size_x = size_x;
          size_y = size_y;
        } in
    let action = handle_event state event in
    match action.action_kind with
    | Continue -> loop action.state
    | Quit -> ()
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

let main_loop size_x size_y mouse_x mouse_y =
  let mk_cube cube =
    let dimension = get_dimension cube in
    {
      cube = cube;
      dimension = dimension;
    } in
  let cubes =
    let list =
      [
        "Squareminx-4-3", Cubes.mk_square_minx_4_3;
        "Triminx-3-2", Cubes.mk_triminx_3_2;
        "Triminx-4-3", Cubes.mk_triminx_4_3;
      ] in
    List.fold_left
      (fun cubes (name, cube) ->
       Maps.String.add name (mk_cube (cube ())) cubes)
      Maps.String.empty
      list in
  let cube_name = "Squareminx-4-3" in
  let cube_data = Maps.String.find cube_name cubes in
  let state =
    {
      size_x = size_x;
      size_y = size_y;
      cube_name = cube_name;
      cube_data = cube_data;
      cubes = cubes;
      game_state = Cube (Free None);
    } in
  let state = switch_to_cube cube_name state in
  let hl = find_hl_cube mouse_x mouse_y state.cube_data in
  let state = { state with game_state = Cube (Free hl); } in
  loop state
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
    Random.self_init ();
    Graph.with_graph "mlcubes v0" main_loop;
    0
  with
  | x ->
    Printf.eprintf "# error: %s" (Printexc.to_string x);
    prerr_newline ();
    1
;;

let () = exit (main ());;
