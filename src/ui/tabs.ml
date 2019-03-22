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

open Ocp_js
open Html
open Js_utils

type state = Active | Disabled | Inactive | Hidden

type t = {
  title: int option -> Html_types.flow5_without_interactive elt;
  id: string;
  content_id: string;
  _class: string list;
}

let make =
  let id = ref 0 in
  fun title _class ->
    let curr = incr id; !id in
    { title; _class;
      id = Printf.sprintf "tab-%d" curr;
      content_id = Printf.sprintf "tab-content-%d" curr;
    }

let make_state_class = function
    Active -> [ "active" ]
  | Disabled -> [ "disabled" ]
  | Inactive | Hidden -> [ ]

let make_state_content_class = function
    Active -> [ "active"; "in" ]
  | Disabled | Inactive | Hidden -> [ ]

let disable tab =
  let tab_li_id = Common.make_id "li" tab.id in
  Manip.addClass (find_component @@ tab_li_id) "disabled";
  let comp = To_dom.of_element @@ find_component tab.id in
  comp##removeAttribute(Js.string "data-toggle");
  comp##removeAttribute(Js.string "href")

let enable tab =
  let tab_li_id = Common.make_id "li" tab.id in
  Manip.removeClass (find_component @@ tab_li_id) "disabled";
  let comp = To_dom.of_element @@ find_component tab.id in
  comp##setAttribute(Js.string "data-toggle",Js.string "tab");
  comp##setAttribute(Js.string "href",
                     Js.string @@ Printf.sprintf "#%s" tab.content_id)

let show tab =
  let tab_li_id = Common.make_id "li" tab.id in
  show (find_component tab_li_id)

let hide tab =
  let tab_li_id = Common.make_id "li" tab.id in
  hide (find_component tab_li_id)

let make_tab tab ?nb state =
  let is_active_class =
    if state = Disabled then [] else
      [ a_user_data "toggle" "tab";
        a_href  @@ "#" ^ tab.content_id]
  in
  let is_hidden_attr =
    if state <> Hidden then [] else [ a_style "display:none" ] in
  li ~a:([ a_class ("nav_item" :: make_state_class state);
          a_id @@ Common.make_id "li" tab.id ] @ is_hidden_attr) [
    a ~a:([ a_id tab.id;
            a_class ("nav-link" :: tab._class);
            Bootstrap_helpers.Attributes.a_role "tab";
            Bootstrap_helpers.Attributes.a_aria "controls" tab.content_id;
            Bootstrap_helpers.Attributes.a_aria "selected" "true"
          ] @ is_active_class) [
      tab.title nb
    ]
  ]

let update_tab_title tab nb =
  try
    let elt = find_component tab.id in
    Manip.replaceChildren elt @@ [ tab.title nb ]
  with Failure _ -> ()

let set_on_show tab f =
  let elt = Js.Unsafe.eval_string (Printf.sprintf "jQuery(\'#%s')" tab.id) in
  Js.Unsafe.meth_call elt "on"
    [| Js.Unsafe.inject (Js.string "shown.bs.tab");
       Js.Unsafe.inject (fun _ -> f (); Js._true);
    |]

type tabs_kind = Pills | Tabs

let string_of_kind = function
  Pills -> "nav-pills"
| Tabs -> "nav-tabs"

let make_tabs ?(fills = false) ?(_class = []) kind tabs =
  let fills = if fills then "nav-fills" :: _class else _class in
  ul ~a:[ a_class ("nav" :: string_of_kind kind :: fills);
          Bootstrap_helpers.Attributes.a_role "tablist" ] @@
  List.map (fun (t, state) -> make_tab t state) tabs

let make_content_panel ?(_class = []) tab state content =
  div ~a:[ a_id tab.content_id;
           a_class ([ "tab-pane"; "fade" ] @ _class @
                    make_state_content_class state);
           Bootstrap_helpers.Attributes.a_role "tabpanel";
           Bootstrap_helpers.Attributes.a_aria "labelledby" tab.id
         ] [ content ]
