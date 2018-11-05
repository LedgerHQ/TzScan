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

open Tezos_types

let unopt ~default = function None -> default | Some value -> value

let string_of_ballot_vote = function
  | Yay -> "Yay"
  | Nay -> "Nay"
  | Pass -> "Pass"

let ballot_of_string = function
  | "Yay" | "yay" -> Yay
  | "Pass"| "pass" -> Pass
  | "Nay" | "nay" -> Nay
  | _ -> Nay
