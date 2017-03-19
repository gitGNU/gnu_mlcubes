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

let inv x = Unary_operator (Inv, x);;

let opp x = Unary_operator (Opp, x);;

let add x y = Binary_operator (Add, x, y);;

let div x y = Binary_operator (Div, x, y);;

let mult x y = Binary_operator (Mult, x, y);;

let pow x y = Binary_operator (Pow, x, y);;

let sub x y = Binary_operator (Sub, x, y);;

let int x = Int x;;

let eval_unary_op op x =
  match op with
  | Inv -> 1.0 /. x
  | Opp -> 0.0 -. x
;;

let eval_binary_op op x y =
  match op with
  | Add -> x +. y
  | Div -> x /. y
  | Mult -> x *. y
  | Pow -> x ** y
  | Sub -> x -. y
;;

let rec eval = function
  | Binary_operator (op, x, y) -> eval_binary_op op (eval x) (eval y)
  | Int n -> float n
  | Unary_operator (op, x) -> eval_unary_op op (eval x)
;;
