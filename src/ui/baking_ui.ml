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

open Ocp_js.Html
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel

let chart_id = "baking-roll-chart-id"

let charts_div_id hash = Common.make_id "charts-div-id" hash

let update_chart_div hash rolls =
  let container = find_component @@ charts_div_id hash in
  if rolls = [] then hide container
  else
    let data = Baking_charts_ui.rolls_to_data @@ List.rev rolls in
    let to_update_content =
      div ~a:[ a_class [ panel_body ] ] [
        div ~a:[ a_class [ row ] ] [
          div ~a:[ a_id chart_id; a_class [ "baking-charts" ] ] [ ]
        ]
      ]
    in
    Manip.removeChildren container ;
    Manip.appendChild container to_update_content ;
    Baking_charts_ui.make_chart
      chart_id
      "Rolls over time (by cycle)"
      "Number of rolls"
      "testrightaxis" data;
    let div = find_component chart_id in
    Manip.SetCss.width div "100%";
    Manip.SetCss.height div "300px"

let make_page hash =
  div ~a:[ a_class [ row ] ] [
    div ~a:[ a_id @@ charts_div_id hash; a_class [ panel; panel_primary ] ] [
      div ~a:[ a_class [ panel_body ] ] [ ]
    ]
  ]
