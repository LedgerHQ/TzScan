(************************************************************************)
(*                                TzScan                                *)
(*                                                                      *)
(*  Copyright 2017-2018 OCamlPro                                        *)
(*                                                                      *)
(*  This file is distributed under the terms of the GNU General Public  *)
(*  License as published by the Free Software Foundation; either        *)
(*  version 3 of the License, or (at your option) any later version.    *)
(*                                                                      *)
(*  TzScan is distributed in the hope that it will be useful,           *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

let list_init n f =
  let rec aux n acc =
    if n < 0 then raise (Invalid_argument "Misc.list_init")
    else if n = 0 then acc
    else aux (n - 1) (f (n - 1) :: acc)
  in
  aux n []


(* Remove empty strings *)
let list_trim l = List.filter (fun s -> s <> "") l

let rec list_find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else list_find_opt p l

let sublist length l =
  let rec iter length l rev =
    if length = 0 then List.rev rev
    else
      match l with
      | [] -> List.rev rev
      | hd :: tl ->
         iter (length-1) tl (hd :: rev)
  in
  iter length l []

let rec list_assoc_opt x = function
    [] -> None
  | (a,b)::l -> if compare a x = 0 then Some b else list_assoc_opt x l

let rec compare_lengths l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _ :: l1, _ :: l2 -> compare_lengths l1 l2

let unopt_list f l =
  List.rev @@ List.fold_left (fun acc elt -> match elt with
      | None -> acc
      | Some elt -> f elt :: acc) [] l

let unoptf def f = function
  | None -> def
  | Some x -> f x

let unopt def = function
  | None -> def
  | Some x -> x

let list_index f =
  List.fold_left (fun acc x ->
      let i = fst acc in
      let l2 = match snd acc with
      | [] -> [f x, [i]]
      | (h, li) :: t when h = f x -> (h, i :: li) :: t
      | l3 -> (f x, [i]) :: l3 in
      i + 1, l2) (0, [])

let remove_duplicate f =
  List.fold_left (fun acc x -> if List.mem (f x) acc then acc else (f x) :: acc) []
