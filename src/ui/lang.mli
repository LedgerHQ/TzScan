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

val init : (unit -> unit) -> unit


type text
val ss_ : string -> text
val t_ : text -> string
val pcdata_t : text -> [> `PCDATA ] Tyxml_js.Html5.elt
(* val pcdata_s : text -> [> `PCDATA ] Tyxml_js.Html5.elt *)

val id_ : text -> string

val pcdata_s : string -> [> `PCDATA ] Tyxml_js.Html5.elt
val s_ : string -> string
