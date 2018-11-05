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

open Tyxml_js.Html5
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel

let rewards_summary_id hash = Common.make_id "rewards-summary-id" hash

let update_rewards_summary hash =
  let _container = find_component @@ rewards_summary_id hash in
  ()

let make_page hash =
  div ~a:[ a_class [ row ] ] [
    div ~a:[ a_id @@ rewards_summary_id hash;
             a_class [ panel; panel_primary ] ] [
      div ~a:[ a_class [ panel_body ] ] [ ]
    ]
  ]
