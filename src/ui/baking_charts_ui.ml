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

let amcharts3_ready = Amcharts3.ready "/amcharts3"

let rolls_to_data data =
  Array.of_list @@
  List.map (fun (cycle, rolls, total) ->
      Int64.to_string cycle,
      Int32.to_float rolls,
      Int32.to_float total) data

let make_chart chart_id graph_name left_axis_title _right_axis_title data =
  amcharts3_ready (fun () ->
      let chart = Amcharts3.serial2 () in

      chart##dataProvider <- Amcharts3.Serial.dataProvider2 data;
      chart##marginLeft <- 10;
      chart##addClassNames <- true;

      let categoryAxis = chart##categoryAxis in
      categoryAxis##parseDates <- false;

      categoryAxis##dashLength <- 3;
      categoryAxis##minorGridEnabled <- true;
      categoryAxis##minorGridAlpha <- 0.1;
      categoryAxis##title <- Js.string graph_name ;

      let leftAxis = Amcharts3.valueAxis() in
      leftAxis##axisAlpha <- 0.;
      leftAxis##inside <- true;
      leftAxis##dashLength <- 3;
      leftAxis##position <- Js.string "left";
      leftAxis##title <- Js.string left_axis_title ;

      chart##addValueAxis(leftAxis);

      let graph = Amcharts3.graph1_2 "smoothedLine" in
      graph##lineColor <- Js.string "#d1655d";
      (* this line makes the graph to change color when it drops below 0 *)
      graph##negativeLineColor <- Js.string "#637bb6";
      graph##bullet <- Js.string "round";
      graph##bulletSize <- 8;
      graph##bulletBorderColor <- Js.string "#FFFFFF";
      graph##bulletBorderAlpha <- 1;
      graph##bulletBorderThickness <- 2;
      graph##lineThickness <- 2;
      graph##valueAxis <- leftAxis;
      graph##balloonText <- Js.string
          (Printf.sprintf "Cycle [[category]]<br><b><span style='font-size:14px;'> [[y1]] / [[y2]]</span></b>");
      chart##addGraph( graph );

      let chartCursor = Amcharts3.chartCursor () in
      chartCursor##cursorAlpha <- 0;
      chartCursor##cursorPosition <- Js.string "mouse";
      chart##addChartCursor(chartCursor);

      let chartScrollbar = Amcharts3.chartScrollbar() in
      chart##addChartScrollbar(chartScrollbar);
      chart##creditsPosition <- Js.string "bottom-right";
      chart##write (Js.string chart_id)
    )
