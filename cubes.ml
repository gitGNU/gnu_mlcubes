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

let ( +/ ) = Expr.add
and ( -/ ) = Expr.sub
and ( */ ) = Expr.mult
and ( // ) = Expr.div
and ( **/ ) = Expr.pow
and int = Expr.int
;;

let sqrt n = n **/ (int 1 // int 2);;

let mk_triminx_3_2 () =
  let h = sqrt (int 3) // int 2 in
  let mk_pos r c =
    int 1 // int 2
    +/ int r // int 2
    +/ int c,
    sqrt (int 3) // int 6
    +/ int r */ h in
  let mk_ipos r c =
    int 1
    +/ int r // int 2
    +/ int c,
    sqrt (int 3) // int 3
    +/ int r */ h in
  let indices =
    [
      0, true, 2, 0, 0;
      1, true, 1, 0, 1;
      2, false, 1, 0, 0;
      3, true, 1, 1, 0;
      4, true, 0, 0, 2;
      5, false, 0, 0, 1;
      6, true, 0, 1, 1;
      7, false, 0, 1, 0;
      8, true, 0, 2, 0;
    ] in
  let mk_polygon color =
    { Cube.
      points =
        [
          int 0 -/ int 1 // int 2, int 0 -/ h // int 3;
          int 1 // int 2, int 0 -/ h // int 3;
          int 0, int 2 */ h // int 3;
        ];
      color = color;
    } in
  let colors =
    [|
      255, 0, 0;
      0, 255, 0;
      255, 255, 0;
     |] in
  let mk_tile i color_index =
    let color = colors.(color_index) in
    let polygon = mk_polygon color in
    { Cube.
      orientation = (if i then 0 else 3), 6;
      polygons = [ polygon; ];
    } in
  let positions, tiles =
    List.fold_left
      (fun (positions, tiles) (i, direct, r, c, color_index) ->
       let position =
         if direct then
           mk_pos r c
         else
           mk_ipos r c in
       let tile = mk_tile direct color_index in
       Maps.Int.add i position positions,
       Maps.Int.add i tile tiles)
      (Maps.Int.empty, Maps.Int.empty)
      indices in
  let rotations =
    let rotation0 =
      { Cube.
        center = mk_ipos 1 0;
        cycles =
          [
            [| 0; 1; 3; |];
            [| 2; |]
          ];
        order = 3;
      } in
    let rotation1 =
      { Cube.
        center = mk_ipos 0 0;
        cycles =
          [
            [| 1; 4; 6; |];
            [| 5; |]
          ];
        order = 3;
      } in
    let rotation2 =
      { Cube.
        center = mk_ipos 0 1;
        cycles =
          [
            [| 3; 6; 8; |];
            [| 7; |]
          ];
        order = 3;
      } in
    List.fold_left
      (fun rotations (i, rotation) -> Maps.Int.add i rotation rotations)
      Maps.Int.empty
      [ 0, rotation0; 1, rotation1; 2, rotation2; ] in
  let center_rotation =
    { Cube.
      center = int 3 // int 2, h;
      cycles =
        [
          [| 1; 5; 6; 7; 3; 2; |];
        ];
      order = 6;
    } in
  let rotations = Maps.Int.add 3 center_rotation rotations in
  let skeleton =
    { Cube.
      positions = positions;
      rotations = rotations;
    } in
  { Cube.
    skeleton = skeleton;
    tiles = tiles;
  }
;;

let mk_triminx_4_3 () =
  let h = sqrt (int 3) // int 2 in
  let mk_pos r c =
    int 1 // int 2
    +/ int r // int 2
    +/ int c,
    sqrt (int 3) // int 6
    +/ int r */ h in
  let mk_ipos r c =
    int 1
    +/ int r // int 2
    +/ int c,
    sqrt (int 3) // int 3
    +/ int r */ h in
  let indices =
    [
      0, true , 3, 0, 0;
      1, true , 2, 0, 0;
      2, false, 2, 0, 0;
      3, true , 2, 1, 0;
      4, true , 1, 0, 1;
      5, false, 1, 0, 2;
      6, true , 1, 1, 2;
      7, false, 1, 1, 2;
      8, true , 1, 2, 3;
      9, true , 0, 0, 1;
      10, false, 0, 0, 1;
      11, true , 0, 1, 1;
      12, false, 0, 1, 2;
      13, true , 0, 2, 3;
      14, false, 0, 2, 3;
      15, true , 0, 3, 3;
    ] in
  let mk_polygon color =
    { Cube.
      points =
        [
          int 0 -/ int 1 // int 2, int 0 -/ h // int 3;
          int 1 // int 2, int 0 -/ h // int 3;
          int 0, int 2 */ h // int 3;
        ];
      color = color;
    } in
  let colors =
    [|
      255, 0, 0;
      0, 255, 0;
      255, 255, 0;
      0, 0, 255;
     |] in
  let mk_tile i color_index =
    let color = colors.(color_index) in
    let polygon = mk_polygon color in
    { Cube.
      orientation = (if i then 0 else 3), 6;
      polygons = [ polygon; ];
    } in
  let positions, tiles =
    List.fold_left
      (fun (positions, tiles) (i, direct, r, c, color_index) ->
       let position =
         if direct then
           mk_pos r c
         else
           mk_ipos r c in
       let tile = mk_tile direct color_index in
       Maps.Int.add i position positions,
       Maps.Int.add i tile tiles)
      (Maps.Int.empty, Maps.Int.empty)
      indices in
  let rotations =
    let rotation0 =
      { Cube.
        center = Expr.int 2, Expr.int 2 */ h;
        cycles =
          [
            [| 0; 4; 8; |];
            [| 2; 5; 7; |];
            [| 1; 6; 3; |];
          ];
        order = 3;
      } in
    let rotation1 =
      { Cube.
        center = Expr.int 3 // Expr.int 2, h;
        cycles =
          [
            [| 1; 9; 13; |];
            [| 5; 10; 12; |];
            [| 4; 11; 6; |];
          ];
        order = 3;
      } in
    let rotation2 =
      { Cube.
        center = Expr.Int 5 // Expr.Int 2, h;
        cycles =
          [
            [| 3; 11; 15; |];
            [| 6; 13; 8; |];
            [| 7; 12; 14; |];
          ];
        order = 3;
      } in
    List.fold_left
      (fun rotations (i, rotation) -> Maps.Int.add i rotation rotations)
      Maps.Int.empty
      [ 0, rotation0; 1, rotation1; 2, rotation2; ] in
  let skeleton =
    { Cube.
      positions = positions;
      rotations = rotations;
    } in
  { Cube.
    skeleton = skeleton;
    tiles = tiles;
  }
;;
