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

val exec :
  ?verbose:bool -> (* print commands, true by default *)
  'a PGOCaml.t -> (* database handler *)
  ?callback: (* function called with results, None = error *)
    (string list list option -> unit) ->
  string -> (* Query *)
  unit

val execs : (* same as exec, but with a list of queries *)
  ?verbose:bool ->
  'a PGOCaml.t ->
  string list ->
  unit

val update :
  ?verbose:bool -> (* print commands, false by default *)
  versions: (* migration scripts *)
    (int * ('a PGOCaml.t -> unit)) list ->
  ?target:int -> (* target version *)
  ?witness:string -> (* a file modified if the db is modified *)
  'a PGOCaml.t -> (* database handler *)
  unit

val touch_witness : ?witness:string -> int -> unit
val init_version1 : 'a PGOCaml.t -> unit

(* Useful functions to create the initial database *)
val createdb : ?verbose:bool -> string -> unit PGOCaml.monad
val dropdb : ?verbose:bool -> string -> unit PGOCaml.monad

val begin_tr : 'a PGOCaml.t -> unit
val end_tr : 'a PGOCaml.t -> unit
val abort_tr : 'a PGOCaml.t -> unit

val in_tr : 'a PGOCaml.t -> ('a PGOCaml.t -> 'b) -> unit
