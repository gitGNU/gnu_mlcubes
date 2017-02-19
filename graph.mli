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

val mk_proj : unit -> unit;;

val clear : int -> int -> int -> unit;;

val swap : unit -> unit;;

val with_proj : (unit -> unit) -> unit;;

val mult : Geometry.matrix -> unit;;

val translate : float -> float -> unit;;

val rotate : float -> unit;;

val scale : float -> unit;;

val project : Geometry.vector -> int * int;;

val draw_poly : Geometry.vector array -> Graphics.color -> unit;;

val fill_poly : Geometry.vector array -> Graphics.color -> unit;;
