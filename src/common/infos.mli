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

(* This value is statically known at build time *)
val versions : Data_types.versions

(* These values are only available after downloading them from the
   API server and the WWW server *)
val api : Data_types.api_server_info
val www : Data_types.www_server_info

(* `constants ~cycle` returns the constants for `cycle` *)
val constants : cycle:int -> Tezos_types.constants

(* `rampup ~cycle deposit` computes the deposit when there is a rampup,
   i.e. an progressive increase of the deposit during the first cycles. *)
val rampup : cycle:int -> int64 -> int64


(* `save_api_config filename` save current configuration into `filename` *)
val save_api_config : string -> unit
