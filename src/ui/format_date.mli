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

module TEXT : sig

  val s_day : Lang.text
  val s_days : Lang.text

end

val auto_updating_timespan :
  ?refresh:(float -> unit) -> (* called with the diff at every refresh *)
  ?future:bool ->
  string ->
  [> Html_types.span ] Tyxml_js.Html5.elt

val auto_updating_timespan_float :
  ?refresh:(float -> unit) -> (* called with the diff at every refresh *)
  ?future:bool ->
  float ->
  [> Html_types.span ] Tyxml_js.Html5.elt

val set_server_date : float -> unit
val time_before_level : cst:Tezos_types.constants -> int -> string

val get_now : unit -> float

val ago_str : ?future:bool -> float -> string

val float_of_iso : string -> float
