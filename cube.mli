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

type point = Expr.t * Expr.t;;

type cycle = int array;;

type rotation =
  {
    center : point;
    cycles : cycle list;
    order : int;
  }
;;

type skeleton =
  {
    positions : point Maps.Int.t;
    rotations : rotation Maps.Int.t;
  }
;;

type polygon =
  {
    points : point list;
    color : int * int * int;
  }
;;

type tile =
  {
    orientation : int * int;
    polygons : polygon list;
  }
;;

type t =
  {
    skeleton : skeleton;
    tiles : tile Maps.Int.t;
  }
;;
