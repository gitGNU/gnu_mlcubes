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

type unary_operator =
  | Inv
  | Opp
;;

type binary_operator =
  | Add
  | Div
  | Mult
  | Pow
  | Sub
;;

type t =
  | Binary_operator of binary_operator * t * t
  | Int of int
  | Unary_operator of unary_operator * t
;;

val inv : t -> t;;

val opp : t -> t;;

val add : t -> t -> t;;

val div : t -> t -> t;;

val mult : t -> t -> t;;

val pow : t -> t -> t;;

val sub : t -> t -> t;;

val int : int -> t;;

val eval : t -> float;;
