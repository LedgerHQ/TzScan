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

val add_alias : Data_types.account_name -> unit

val reset : unit -> unit

val to_name : ?alias:string -> Tezos_types.account_hash -> Data_types.account_name

val change_aliases_from_json :
  ?with_table:bool ->
  ?update_db:(bool -> Data_types.account_name -> unit) ->
  Data_types.service list Json_encoding.encoding -> Json_repr.ezjsonm -> unit
