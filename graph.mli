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

type color;;

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

val with_graph : string -> (int -> int -> int -> int -> 'a) -> 'a;;

val rgb : int -> int -> int -> color;;

val mk_proj : int -> int -> unit;;

val mk_hud : unit -> unit;;

val clear : color -> unit;;

val swap : unit -> unit;;

val with_proj : (unit -> unit) -> unit;;

val load_id : unit -> unit;;

val mult : Geometry.matrix -> unit;;

val translate : float -> float -> unit;;

val rotate : float -> unit;;

val scale : float -> unit;;

val project : Geometry.vector -> int * int;;

val draw_poly : Geometry.vector array -> color -> unit;;

val fill_poly : Geometry.vector array -> color -> unit;;

val wait_next_event : unit -> event;;

val fill_rect : color -> rect -> unit;;

val text_size : string -> int * int;;

val draw_string :
  int -> int -> color -> color -> int -> string -> unit
;;
