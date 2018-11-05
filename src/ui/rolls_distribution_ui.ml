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
open Bootstrap_helpers.Grid
open Lang
open Text

let spf = Printf.sprintf

let rolls_distrib_chart_id = "rolls-distrib-pie-id"
let rolls_cmd_id = "rolls-cmd-id"
let rolls_cmd_select_id = "rolls-cmd-select-id"

let amcharts3_ready = Amcharts3.ready "/amcharts3"

module Rolls_distribution =
  Panel.MakePageTable(
      struct
        let name = "rolls_distribution"
        let theads =
          Panel.theads_of_strings [
            s_roll_owner, 3;
            s_rolls, 1;
            s_percent, 1;
          ]
        let title_span = Panel.title_nb s_rolls_distribution
        let table_class = "blocks-table"
        let page_size = 20
      end)

let update_rolls_distrib cycle rolls =

  let total, rolls2 =
    List.fold_left (fun (acc, rolls) (tz1, count) ->
        acc + count,
        (tz1, count) :: rolls)
      (0, []) rolls in
  let rolls2, other_rolls, _ =
    List.fold_left (fun (acc_rolls, acc_other, acc_count) (tz1, count) ->
        if acc_count < 10 then
          (Common.choose_name tz1, count) :: acc_rolls , acc_other, acc_count + 1
        else acc_rolls, (Common.choose_name tz1, count) :: acc_other, acc_count
      ) ([], [], 0) (List.rev rolls2) in
  let others_number, others_rolls_count =
    List.fold_left (fun (acc_number, acc_count) (_tz1, count) ->
        acc_number + 1, acc_count + count
      ) (0, 0) (List.rev other_rolls) in
  let rolls2 =
    if others_rolls_count > 0 then
      let str = Printf.sprintf "%s (%d)" (t_ s_others) others_number in
      (str, others_rolls_count) :: rolls2
    else rolls2 in
  let data = Array.of_list rolls2 in
  let total_str =
    Js.to_string
      (Js.number_of_float @@ float @@ total)##toLocaleString()  in

  (* Table Panel *)
  let rows = Array.map (fun r ->
      let owner_hash = fst r
      and nb_rolls = snd r in
      tr [
        Common.account_w_blockies owner_hash;
        td [ pcdata @@ spf "%i" nb_rolls ];
        td [ pcdata @@ spf "%.2f %%" (
            (float nb_rolls) /. (float total) *. 100. )];
      ]) (Array.of_list rolls) in
  Rolls_distribution.paginate_all rows;

  (* Display a pie *)
  let balloon =
    "[[title]]<br><span style='font-size:14px'><b>[[value]] rolls</b> ([[percents]]%)</span>" in

  amcharts3_ready (fun () ->
      (* Hack : pie##theme not working *)
      ignore @@ Js.Unsafe.eval_string "AmCharts.theme = AmCharts.themes.light";
      let pie = Amcharts3.pie () in
      let export = Amcharts3.export () in
      let title = Amcharts3.title () in
      let subtitle = Amcharts3.title () in
      let legend = Amcharts3.legend () in
      subtitle##bold <- false;
      title##text <- Js.string @@
        Printf.sprintf "Rolls use for cycle %d" cycle;
      subtitle##text <-
        Js.string @@ Printf.sprintf "Total : %s rolls " total_str ;
      export##enabled <- false;
      pie##theme <- Js.string "light";
      pie##dataProvider <- Amcharts3.Pie.dataProvider data;
      pie##outlineColor <- Js.string "#FFFFFF";
      pie##balloonText <- Js.string balloon;
      pie##titles <- Js.array [| title; subtitle |] ;
      if Dom_html.window##screen##width <= 600 then
        begin
          pie##labelsEnabled <- false;
          legend##valueText <- Js.string "[[percents]]%";
          pie##legend <- legend
        end;

      pie##export <- export ;
      pie##write (Js.string rolls_distrib_chart_id);
      let div = Common.get_div_by_id rolls_distrib_chart_id in
      div##style##width <- Js.string "100%";
      if Dom_html.window##screen##width <= 600 then
        div##style##height <- Js.string "800px"
      else
        div##style##height <- Js.string "600px"
    )

let update_cmd last_cycle update =
  let arr = Array.init (last_cycle+1)
      (fun i -> string_of_int @@ last_cycle - i) in
  Common.make_options rolls_cmd_id arr update

let make_page () =
  div [
    div
      ~a:[ a_id rolls_cmd_id;
           a_class [ clgoffset10; clg2; cxsoffset8; cxs4; "rolls-cmd" ] ] [] ;
    div ~a:[ a_id rolls_distrib_chart_id; a_class [ cxs12 ] ] [] ;
    Rolls_distribution.make ~footer:true ()
  ]
