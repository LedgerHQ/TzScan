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

(** [level url hash] returns the level of the block with the
    corresponding [hash]. *)
val level : url -> block_hash -> node_level

(** [predecessor url hash] returns the hash of the predecessor of
    the block with the hash [hash]. *)
val predecessor : url -> block_hash -> block_hash

(** [block url hash] returns a value [Data_types.block] which contains
    all the information about the block with the hash [hash]. *)
val block : url -> block_hash -> node_block

(** [operations url block_hash] gets all the operations from the block
    with the hash [hash]. *)
val operations : url -> string -> node_operation list list


(** Shortcuts on the block [head] *)
val genesis : url -> Tezos_types.node_block
val head_block : ?block_hash:string -> url -> Tezos_types.node_block
val get_head_hash : ?block:string -> url -> Tezos_types.block_hash

val get_alternative_heads_hashes : url -> Tezos_types.block_hash list list

val current_level : ?block:string -> url -> Tezos_types.node_level
val pending_operations : url -> Tezos_types.pending_operation


val request : ?post:bool -> cachable:bool -> Data_types.url -> string -> string
