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

module UpdateOnFocus : sig
  (* tell the updaters that the page changed *)
  val incr_page : unit -> unit

  val update_every : ?always:bool -> int -> (unit -> unit) -> unit

  (* remove all timers *)
  val clear_timers : unit -> unit
end

val input :
  ([< Html_types.input_attrib ], [> Html_types.input ])
    Ocp_js.Html.nullary
val get_input_value : string -> string
val set_input_value : string -> string -> unit
