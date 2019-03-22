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
open Bootstrap_helpers.Button
open Bootstrap_helpers.Form
open Bootstrap_helpers.Icon

let make () =
  div ~a:[ a_class [form_inline] ] [
    div ~a:[ a_class [form_group] ] [
      label ~a:[ a_class [Bootstrap_helpers.Misc.lead] ] [
        txt "Simple webview app for android: " ];
      space_icon ();
      button ~a:[
        a_button_type `Submit;
        a_onclick (fun _e ->
            Dom_html.window##location##href <- Js.string ("/tzscan.apk"); true);
        a_class [btn; btn_primary] ] [
        txt "Download" ]
    ]
  ]
