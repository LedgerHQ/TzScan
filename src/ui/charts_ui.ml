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
open Bootstrap_helpers.Grid
open Data_types
open Text

let amcharts3_ready = Amcharts3.ready "/amcharts3"

(* cut float at 2 digits after dot *)
let simplify_float f =
  float_of_int (int_of_float (f *. 100.)) /. 100.

let array_fold f t x =
  let x = ref x in
  for i = 0 to Array.length t-1 do
    x := f !x t.(i)
  done;
  !x




let chart_id = "bakers-pie-id"
let chart_div = div ~a:[ a_id chart_id; a_class [ cxs12 ] ] [] ;

module Bakers =
  Panel.MakePageTable(
      struct
        let name = "bakers"
        let theads =
          Panel.theads_of_strings [
            s_baker, 3;
            s_blocks, 1;
            s_volume, 1;
            s_fees, 1;
            s_endorsements, 1;
          ]
        let title_span = Panel.title_nb s_bakers ~help:Glossary_doc.HBaker
        let table_class = "default-table"
        let page_size = 20
      end)

let make_bakers_panel () =
  div [
    div ~a:[ a_id "bakers-cycle-cmd";
             a_class [ clgoffset10; clg2; cxsoffset8; cxs4; "bakers-cmd" ] ] [] ;
    chart_div;
    Bakers.make_clg12 ~footer:true ()
  ]

let update_bakers_cmd nb_cycle update =
  let arr = Array.init (nb_cycle + 1)
      (fun i ->
         if nb_cycle - i -1 >= 0 then string_of_int @@ (nb_cycle - i - 1)
         else "All") in
  Common.make_options "bakers-cycle-cmd" arr update

let update_bakers_chart bakers =
  Array.sort (fun b1 b2 -> compare b2.nb_blocks b1.nb_blocks) bakers;
  let rows =
    Array.map (fun b ->
        tr [
          Common.account_w_blockies ~args:[ "default", "baking"] b.baker_hash;
          td [ txt @@ string_of_int b.nb_blocks ];
          td [ Tez.pp_amount ~precision:2 ~width:6 b.volume_total ];
          td [ Tez.pp_amount ~precision:2 ~width:6 b.fees_total ];
          td [ txt @@ string_of_int b.nb_endorsements ]
        ]) bakers
  in
  Bakers.paginate_all rows;

  (* Display a pie *)

  let data = Array.map (fun b ->
                 Common.choose_name b.baker_hash, b.nb_blocks;
               ) bakers in
  let balloon =  "[[title]]<br><span style='font-size:14px'><b>Baked [[value]] blocks</b> ([[percents]]%)</span>" in

  amcharts3_ready (fun () ->
      let pie = Amcharts3.pie () in
      let len = Array.length data in
      let max_len = 10 in
      let legend = Amcharts3.legend () in
      let data = if len <= max_len then data
                 else
                   let data2 = Array.init max_len (fun i -> data.(i)) in
                   let x = ref 0 in
                   let n = ref 0 in
                   for i = max_len-1 to len-1 do
                     x :=  !x + snd data.(i);
                     incr n;
                   done;
                   let others = Printf.sprintf "Others (%d)" !n in
                   data2.(max_len-1) <- (others, !x);
                   data2
      in

      pie##dataProvider <- Amcharts3.Pie.dataProvider data;
      pie##outlineColor <- Js.string "#FFFFFF";
      pie##outlineAlpha <- 0.8;
      pie##outlineThickness <- 2;
      pie##balloonText <- Js.string balloon;

      (* this makes the chart 3D *)
      pie##depth3D <- 15;
      pie##angle <- 30;

      if Dom_html.window##screen##width <= 500 then
        begin
          pie##labelsEnabled <- false;
          legend##valueText <- Js.string "[[percents]]%";
          pie##legend <- legend
        end;

      pie##export <- Amcharts3.export ();
      pie##write (Js.string chart_id);

      let div = Tyxml_js.To_dom.of_div @@ Js_utils.find_component chart_id in

      div##style##width <- Js.string "100%";
      div##style##height <- Js.string "600px";
    );
  ()

let make_chart_panel () = div [
    div ~a:[ a_id "export-div"; a_class [cxs12] ] [];
    chart_div ]

let make_blocks_per_day_panel = make_chart_panel

let chart_value_per_day data ytitle =
    amcharts3_ready (fun () ->

      let chart = Amcharts3.serial () in

      chart##dataProvider <- Amcharts3.Serial.dataProvider data;
      chart##marginLeft <- 10;
      chart##dataDateFormat <- Js.string "YYYY-MM-DD";
      chart##addClassNames <- true;

      let categoryAxis = chart##categoryAxis in
      categoryAxis##parseDates <- true; (* as our data is date-based, we set parseDates to true *)

      categoryAxis##minPeriod <- Js.string "DD";
      categoryAxis##dashLength <- 3;
      categoryAxis##minorGridEnabled <- true;
      categoryAxis##minorGridAlpha <- 0.1;

      let valueAxis = Amcharts3.valueAxis() in
      valueAxis##axisAlpha <- 0.;
      valueAxis##inside <- true;
      valueAxis##dashLength <- 3;
      valueAxis##title <- Js.string ytitle;
      chart##addValueAxis(valueAxis);

      let graph = Amcharts3.graph "smoothedLine" in
      graph##lineColor <- Js.string "#d1655d";
      (* this line makes the graph to change color when it drops below 0 *)
      graph##negativeLineColor <- Js.string "#637bb6";
      graph##bullet <- Js.string "round";
      graph##bulletSize <- 8;
      graph##bulletBorderColor <- Js.string "#FFFFFF";
      graph##bulletBorderAlpha <- 1;
      graph##bulletBorderThickness <- 2;
      graph##lineThickness <- 2;
      graph##balloonText <- Js.string "[[category]]<br><b><span style='font-size:14px;'>[[value]]</span></b>";
      chart##addGraph( graph );

      chart##export <- Amcharts3.export ~divId:(Js.string "export-div") ();

      let chartCursor = Amcharts3.chartCursor () in
      chartCursor##cursorAlpha <- 0;
      chartCursor##cursorPosition <- Js.string "mouse";
      chartCursor##categoryBalloonDateFormat <- Js.string "YYYY-MM-DD";
      chart##addChartCursor(chartCursor);

      let chartScrollbar = Amcharts3.chartScrollbar() in
      chart##addChartScrollbar(chartScrollbar);
      chart##creditsPosition <- Js.string "bottom-right";
      chart##write (Js.string chart_id);

      let div = Tyxml_js.To_dom.of_div @@ Js_utils.find_component chart_id in

      div##style##width <- Js.string "100%";
      div##style##height <- Js.string "600px";

      (*
                // listen for "dataUpdated" event (fired when chart is inited) and call zoomChart method when it happens
       chart.addListener("dataUpdated", zoomChart);

       // this method is called when chart is first inited as we listen for "dataUpdated" event

            function zoomChart() {
                // different zoom methods can be used - zoomToIndexes, zoomToDates, zoomToCategoryValues
                chart.zoomToDates(new Date(1972, 0), new Date(1984, 0));
            }
                     *)
      ()
      )

let update_int_per_day legend =
  fun pd ->
  let ndays = Array.length pd.pd_days in
  let data = Array.init ndays (fun i ->
                          pd.pd_days.(i),
                          float_of_int pd.pd_value.(i))
  in
  chart_value_per_day data legend

let update_float_per_day legend =
  fun pd ->
  let ndays = Array.length pd.pd_days in
  let data = Array.init ndays (fun i ->
                              pd.pd_days.(i),
                              simplify_float pd.pd_value.(i)) in
  chart_value_per_day data legend

let update_xtz_per_day legend factor pd =
  let ndays = Array.length pd.pd_days in
  let data = Array.init ndays
      (fun i -> pd.pd_days.(i),
                simplify_float
                  Int64.(to_float (div pd.pd_value.(i) (mul factor 1000000L)))) in
  chart_value_per_day data legend


let update_blocks_per_day =
  update_int_per_day "Number of Blocks per Day"



let make_delay_per_day_panel = make_chart_panel

let update_delay_per_day pd =
  let ndays = Array.length pd.pd_days in
  let nsecs_per_day = 3600. *. 24. in
  let data = Array.init ndays (fun i ->
                              let nblocks = pd.pd_value.(i) in
                              let delay =
                                if nblocks = 0 then nsecs_per_day
                                else nsecs_per_day /. float_of_int nblocks
                              in
                              pd.pd_days.(i),
                              simplify_float delay) in
  chart_value_per_day data "Average Delay Between Blocks (secs)"




let make_priorities_per_day_panel = make_chart_panel

let update_priorities_per_day =
  update_float_per_day  "Average Priority/Rank of Baker"






let make_bakers_per_day_panel = make_chart_panel

let update_bakers_per_day pd =
  let ndays = Array.length pd.pd_days in
  let data = Array.init ndays (fun i ->
                              pd.pd_days.(i),
                              float_of_int pd.pd_value.(i)) in
  chart_value_per_day data "Number of Bakers per Day"






let make_operations_per_day_panel = make_chart_panel

let update_operations_per_day =
  update_int_per_day "Number of Operations per Day"


let make_operations_per_block_per_day_panel = make_chart_panel

let update_operations_per_block_per_day =
  update_int_per_day "Number of Operations per Block per Day"



let update_fees_per_day =
  update_xtz_per_day "Total Fees per Day (XTZ)" 1L

let update_volume_per_day =
  update_xtz_per_day "Total Volume per Day (k XTZ)" 1000L

let update_market_prices data =
  amcharts3_ready (fun () ->
      let chart = Amcharts3.serialN () in
      chart##dataProvider <- Amcharts3.DataItem.dataProvider data;
      chart##marginLeft <- 10;

      let categoryAxis = chart##categoryAxis in
      categoryAxis##parseDates <- true;
      categoryAxis##minPeriod <- Js.string "hh";
      categoryAxis##dashLength <- 3;
      categoryAxis##minorGridEnabled <- true;
      categoryAxis##minorGridAlpha <- 0.1;

      let leftAxis = Amcharts3.valueAxis() in
      leftAxis##axisAlpha <- 0.;
      leftAxis##inside <- false;
      leftAxis##dashLength <- 3;
      leftAxis##title <- Js.string "BTC";
      leftAxis##titleColor <- Js.string "#0000ff";
      leftAxis##color <- Js.string "#0000ff";
      leftAxis##position <- Js.string "left";
      chart##addValueAxis(leftAxis);

      let rightAxis = Amcharts3.valueAxis() in
      rightAxis##axisAlpha <- 0.;
      rightAxis##inside <- false;
      rightAxis##dashLength <- 3;
      rightAxis##title <- Js.string "USD";
      rightAxis##titleColor <- Js.string "#ff0000";
      rightAxis##color <- Js.string "#ff0000";
      rightAxis##position <- Js.string "right";
      chart##addValueAxis(rightAxis);

      let graph1 = Amcharts3.graphN "price_usd" "smoothedLine" in
      graph1##lineColor <- Js.string "#ff0000";
      graph1##negativeLineColor <- Js.string "#00ffff";
      graph1##lineThickness <- 2;
      graph1##balloonText <- Js.string "[[category]]<br><b><span style='font-size:14px;'>[[price_usd]]</span></b>";
      graph1##title <- Js.string "XTZ to USD";
      graph1##valueAxis <- rightAxis;
      chart##addGraph( graph1 );

      let graph2 = Amcharts3.graphN "price_btc" "smoothedLine" in
      graph2##lineColor <- Js.string "#0000ff";
      graph2##negativeLineColor <- Js.string "#ffff00";
      graph2##lineThickness <- 2;
      graph2##balloonText <- Js.string "[[category]]<br><b><span style='font-size:14px;'>[[price_btc]]</span></b>";
      graph2##title <- Js.string "XTZ to BTC";
      graph2##valueAxis <- leftAxis;
      chart##addGraph( graph2 );

      chart##export <- Amcharts3.export ~divId:(Js.string "export-div") ();

      let legend = Amcharts3.legend () in
      legend##useGraphSettings <- true;
      legend##valueAlign <- Js.string "left";
      legend##valueWidth <- 200;
      chart##legend <- legend;

      let chartCursor = Amcharts3.chartCursor () in
      chartCursor##cursorAlpha <- 0;
      chartCursor##cursorPosition <- Js.string "mouse";
      chartCursor##categoryBalloonDateFormat <- Js.string "YYYY-MM-DD HHh";
      chart##addChartCursor(chartCursor);

      let chartScrollbar = Amcharts3.chartScrollbar() in
      chart##addChartScrollbar(chartScrollbar);
      chart##creditsPosition <- Js.string "bottom-right";
      chart##write (Js.string chart_id);

      let div = Tyxml_js.To_dom.of_div @@ Js_utils.find_component chart_id in
      div##style##width <- Js.string "100%";
      div##style##height <- Js.string "600px"
    )
