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

val timestamps : unit -> (string * string option) list Lwt.t

val account_dec :
  string ->
  error:(int -> unit) ->
  (
    string option *
    (
      string * int64 * bool * (bool * string option) *
      (Tezos_types.script_expr_t * Tezos_types.script_expr_t) option *
      Tezos_types.script_expr_t option * Z.t
    )
    -> unit
  ) -> unit

val delegate_details :
  string ->
  error:(int -> unit) ->
  (Tezos_types.delegate_details -> unit) -> unit


val to_lwt :
  ('a -> error:(int -> unit) -> ('b -> unit) -> 'c) -> 'a -> 'b Lwt.t
