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

let init_v = Array.init;;

let init_m h w f =
  Array.init h (fun r -> Array.init w (fun c -> f r c))
;;

let identity n = init_m n n (fun r c -> if r = c then 1.0 else 0.0);;

let mk_base n d = init_v n (fun i -> if i = d then 1.0 else 0.0);;

let x = mk_base 3 0;;

let y = mk_base 3 1;;

let z = mk_base 3 2;;

let dim_v = Array.length;;

let dim_m m = Array.length m, Array.length m.(0);;

let add_v v1 v2 =
  let l = dim_v v1 in
  assert (l = dim_v v2);
  init_v l (fun i -> v1.(i) +. v2.(i))
;;

let add_m m1 m2 =
  let h1, w1 = dim_m m1 in
  let h2, w2 = dim_m m2 in
  assert (h1 = h2 && w1 = w2);
  init_m h1 w1 (fun r c -> m1.(r).(c) +. m2.(r).(c))
;;

let mult_s_v k v = init_v (dim_v v) (fun i -> k *. v.(i));;

let mult_v_v v1 v2 =
  let l = dim_v v1 in
  assert (l = dim_v v2);
  let rec loop sum i =
    if i = l then
      sum
    else
      loop (sum +. v1.(i) *. v2.(i)) (succ i) in
  loop 0.0 0
;;

let mult_s_m k m =
  let h, w = dim_m m in
  init_m h w (fun r c -> k *. m.(r).(c))
;;

let mult_m_v m v =
  let h, w = dim_m m in
  let l = dim_v v in
  assert (w = l);
  init_v
    h
    (fun i ->
     let rec loop accu j =
       if j = w then
         accu
       else
         loop (m.(i).(j) *. v.(j) +. accu) (succ j) in
     loop 0.0 0)
;;

let mult_m_m m1 m2 =
  let h1, w1 = dim_m m1 in
  let h2, w2 = dim_m m2 in
  assert (w1 = h2);
  init_m
    h1 w2
    (fun i j ->
     let rec loop accu k =
       if k = w1 then
         accu
       else
         loop (m1.(i).(k) *. m2.(k).(j) +. accu) (succ k) in
     loop 0.0 0)
;;

let sub_v v1 v2 = add_v v1 (mult_s_v (-1.0) v2);;

let sub_m m1 m2 = add_m m1 (mult_s_m (-1.0) m2);;

let div_v v k = mult_s_v (1.0 /. k) v;;

let div_m m k = mult_s_m (1.0 /. k) m;;

let norm_v v f_null f =
  let n2 = mult_v_v v v in
  if n2 = 0.0 then
    f_null ()
  else
    f (div_v v (sqrt n2))
;;

let debug_v ppf v =
  Debug.debug_sexp
    ppf
    (fun ppf ->
     Array.iteri
       (fun i k ->
        if i > 0 then
          Format.fprintf ppf "@ ";
        Format.fprintf ppf "%f" k)
       v)
;;

let debug_m ppf m =
  Debug.debug_sexp
    ppf
    (fun ppf ->
     Array.iteri
       (fun i v ->
        if i > 0 then
          Format.fprintf ppf "@ ";
        Format.fprintf ppf "%a" debug_v v)
       m)
;;
