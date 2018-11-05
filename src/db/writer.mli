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
open Data_types

(* DB Internals *)
val dbh : (string, bool) Hashtbl.t PGOCaml.t PGOCaml.monad
val pg_lock : (unit -> unit) -> unit

(* Writer functions *)

val head : unit -> block option

val is_block_registered : block_hash -> bool

val block_hash : int -> block_hash

val register_tezos_user : string -> unit

val register_protocol : string -> unit

val register_genesis : node_block -> unit

val register_pending :
  CalendarLib.Calendar.t -> pending_operation_parsed list -> unit

val register_operations :
  node_block -> node_operation list -> unit

val register_all :
  node_block -> node_level -> node_operation list -> unit

val register_main_chain : bool -> node_block -> unit

val register_network_stats : network_stats list -> unit

val register_crawler_activity : string -> int -> unit

val update_alias : account_hash -> string option -> unit

val counts_downup : int -> int -> unit

val register_cycle_count_baker : int64 -> string -> unit
