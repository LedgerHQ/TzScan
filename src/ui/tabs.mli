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

(** Active: tab open by default
    Disabled: tab cannot be accessed *)
type state = Active | Disabled | Inactive

(**
   A tab is represented by its title and the content associated.
   - title: function that takes an integer (a number of operations) that
     is used to generate or update the tab displayed title.
   - id: refers to the <a> component of the tab ("li-<id>" is the identifier of
     the <li> component, if that can be useful).
   - content_id: the container displayed by the tab
   - _class: the class applied on the <li> element
*)
type t = {
  title: int option -> Html_types.flow5_without_interactive Tyxml_js.Html5.elt;
  id: string;
  content_id: string;
  _class: string list;
}

(* Takes a function for `title` and a list of string for `_class`. 
   Generates fresh ids. *) 
val make :
  (int option -> Html_types.flow5_without_interactive Tyxml_js.Html5.elt) ->
  string list ->
  t

val update_tab_title : t -> int option -> unit

val disable : t -> unit
  
val enable : t -> unit

val set_on_show : t -> (unit -> unit) -> unit 

type tabs_kind = Pills | Tabs

(* Generates a nav-tabs component from a list of tab descriptor and their
   respective state. *)
val make_tabs :
  ?fills:bool -> ?_class:string list -> tabs_kind -> (t * state) list
  -> [> Html_types.ul ] Tyxml_js.Html5.elt

(* Generates the container associated to the tab *)
val make_content_panel :
  ?_class:string list -> t -> state -> [< Html_types.div_content_fun ] Tyxml_js.Html5.elt
  -> [> Html_types.div ] Tyxml_js.Html5.elt
