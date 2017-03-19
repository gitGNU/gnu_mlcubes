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

type vector = float array;;

type matrix = float array array;;

val init_v : int -> (int -> float) -> vector;;

val init_m : int -> int -> (int -> int -> float) -> matrix;;

val identity : int -> matrix;;

val x : vector;;

val y : vector;;

val z : vector;;

val dim_v : vector -> int;;

val dim_m : matrix -> int * int;;

val add_v : vector -> vector -> vector;;

val add_m : matrix -> matrix -> matrix;;

val sub_v : vector -> vector -> vector;;

val sub_m : matrix -> matrix -> matrix;;

val mult_s_v : float -> vector -> vector;;

val mult_v_v : vector -> vector -> float;;

val mult_s_m : float -> matrix -> matrix;;

val mult_m_v : matrix -> vector -> vector;;

val mult_m_m : matrix -> matrix -> matrix;;

val div_v : vector -> float -> vector;;

val div_m : matrix -> float -> matrix;;

val norm_v : vector -> (unit -> 'a) -> (vector -> 'a) -> 'a;;

val debug_v : Format.formatter -> vector -> unit;;

val debug_m : Format.formatter -> matrix -> unit;;
