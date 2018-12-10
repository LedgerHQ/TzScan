(************************************************************************)
(*                                TzScan                                *)
(*                                                                      *)
(*  Copyright 2017-2018 OCamlPro                                        *)
(*                                                                      *)
(*  This file is accountsuted under the terms of the GNU General Public  *)
(*  License as published by the Free Software Foundation; either        *)
(*  version 3 of the License, or (at your option) any later version.    *)
(*                                                                      *)
(*  TzScan is accountsuted in the hope that it will be useful,           *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*  GNU General Public License for more details.                        *)
(*                                                                      *)
(************************************************************************)

open Data_types
open Tyxml_js.Html5
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Form
open Text

let spf = Printf.sprintf

let tops_accounts_chart_id = "tops-accounts-pie-id"
let tops_cmd_id = "tops-cmd-id"
let tops_cmd_select_id = "tops-cmd-select-id"

let amcharts3_ready = Amcharts3.ready "/amcharts3"

module Balance_ranking_table =
  Panel.MakePageTable(
      struct
        let name = "balance_ranking"
        let theads =
          Panel.theads_of_strings [ s_ranking, 1; s_account, 2; s_balance, 1 ]
        let title_span = Panel.title_nb s_top_balances
        let table_class = "blocks-table"
        let page_size = 20
      end)

let make_table ranking_list =
  List.map
    (fun ((pos : int),owner_hash,balance) ->
      tr [td [Lang.pcdata_s @@ spf "%i" pos];
          Common.account_w_blockies owner_hash;
          td [ Tez.pp_amount balance ];
    ]) ranking_list

let update_ranking_table xhr =
  let container = find_component "top_accounts_table" in
  let content = Balance_ranking_table.make ~footer:true () in
  Manip.removeChildren container ;
  Manip.appendChild container content ;

  Balance_ranking_table.paginate_fun
    ~urlarg_page:"" ~urlarg_size:""
    make_table
    (fun page page_size cb ->
       xhr page page_size
         (fun d -> cb d;
           ignore (Js.Unsafe.eval_string
                     "jQuery('[data-toggle=\"popover\"]').popover();")))

let selection_to_kind = function
  | "Balances" -> "balances"
  | "Frozen balances" -> "frozen_balances"
  | "Frozen deposits" -> "frozen_deposits"
  | "Frozen rewards" -> "frozen_rewards"
  | "Paid bytes" -> "paid_bytes"
  | "Staking balances" -> "staking_balances"
  | "Total balances" -> "total_balances"
  | "Total delegated" -> "total_delegated"
  | "Total delegators" -> "total_delegators"
  | "Total frozen fees" -> "total_frozen_fees"
  | "Used bytes" -> "used_bytes"
  | _ -> "balances"

let selection_to_total context = function
  | "Balances" -> context.context_current_balances
  | "Frozen balances" -> context.context_frozen_balances
  | "Frozen deposits" -> context.context_frozen_deposits
  | "Frozen rewards" -> context.context_frozen_rewards
  | "Paid bytes" -> context.context_paid_bytes
  | "Staking balances" -> context.context_staking_balances
  | "Total balances" -> context.context_full_balances
  | "Total delegated" -> context.context_delegated
  | "Total delegators" -> Int64.of_int context.context_delegators
  | "Total frozen fees" -> context.context_frozen_fees
  | "Used bytes" -> context.context_used_bytes
  | _ -> context.context_current_balances

let top_kind_to_title = function
  | "balances" -> s_top_balances
  | "frozen_balances" -> s_top_frozen_balances
  | "frozen_deposits" -> s_top_frozen_deposits
  | "frozen_rewards" -> s_top_frozen_rewards
  | "paid_bytes" -> s_top_paid_bytes
  | "staking_balances" -> s_top_staking_balances
  | "total_balances" -> s_top_total_balances
  | "total_delegated" -> s_top_total_delegated
  | "total_delegators" -> s_top_total_delegators
  | "total_frozen_fees" -> s_top_total_frozen_fees
  | "used_bytes" -> s_top_used_bytes
  | _ -> s_top_balances

let top_kind_to_thname = function
  | "balances" -> s_balances
  | "frozen_balances" -> s_frozen_balances
  | "frozen_deposits" -> s_frozen_deposits
  | "frozen_rewards" -> s_frozen_rewards
  | "paid_bytes" -> s_paid_bytes
  | "staking_balances" -> s_staking_balances
  | "total_balances" -> s_total_balances
  | "total_delegated" -> s_total_delegated
  | "total_delegators" -> s_total_delegators
  | "total_frozen_fees" -> s_total_frozen_fees
  | "used_bytes" -> s_used_bytes
  | _ -> s_balances

let top_kind_to_printer = function
  | "balances"
  | "frozen_balances"
  | "frozen_deposits"
  | "frozen_rewards"
  | "staking_balances"
  | "total_balances"
  | "total_delegated"
  | "total_frozen_fees" -> Tez.pp_amount
  | "paid_bytes"
  | "total_delegators"
  | "used_bytes"
  | _ ->
    (fun ?(precision=6) ?(width=15) ?(order=0) ?icon i ->
       ignore (precision, width, order, icon) ;
       span [ pcdata @@ Int64.to_string i ])

(* mika : add piechart on top of table  *)
(* let update_tops_accounts kind nrows total tops =
 *   let container = find_component "top_accounts_table" in
 *   let title_str = top_kind_to_title kind in
 *   let module Top =
 *     Panel.MakePageTable(
 *     struct
 *       let name = "top_accounts"
 *       let theads =
 *         Panel.theads_of_strings [
 *           s_account, 3;
 *           top_kind_to_thname kind, 1;
 *           s_percent, 1;
 *         ]
 *       let title_span = Panel.title_nb title_str
 *       let table_class = "blocks-table"
 *       let page_size = 20
 *     end) in
 *   let content = Top.make ~footer:true () in
 *   Manip.removeChildren container ;
 *   Manip.appendChild container content ;
 * 
 *   let tops2, tops_total, _ =
 *     List.fold_left (fun (acc_tops, acc_total, acc_count) (tz1, value) ->
 *         if acc_count < 10 then
 *           (tz1, Int64.to_int value) :: acc_tops ,
 *           Int64.add acc_total value,
 *           acc_count + 1
 *         else acc_tops, acc_total, acc_count
 *       ) ([], 0L, 0) tops.top_list in
 * 
 *   let tops2 =
 *     let remaining = Int64.sub total tops_total in
 *     if remaining > 0L then
 *       let str = Printf.sprintf "%s" (t_ s_others) in
 *       (str, Int64.to_int remaining) :: tops2
 *     else tops2 in
 * 
 *   let data = Array.of_list tops2 in
 * 
 *   (\* Table Panel *\)
 *   let rows = Array.map (fun r ->
 *       let owner_hash = fst r
 *       and nb_tops = snd r in
 *       tr [
 *         td [ pcdata owner_hash ] ;
 *         td [ pcdata @@ spf "%Ld" nb_tops ];
 *         td [ pcdata @@ spf "%.2f %%" (
 *             (Int64.to_float nb_tops) /. (Int64.to_float total) *. 100. )];
 *       ]) (Array.of_list tops.top_list) in
 *   Top.paginate_all rows;
 * 
 *   (\* Display a pie *\)
 *   let balloon =
 *     "[[title]]<br><span style='font-size:14px'><b>[[value]] tops</b> ([[percents]]%)</span>" in
 * 
 *   amcharts3_ready (fun () ->
 *       (\* Hack : pie##theme not working *\)
 *       ignore @@ Js.Unsafe.eval_string "AmCharts.theme = AmCharts.themes.light";
 *       let pie = Amcharts3.pie () in
 *       let export = Amcharts3.export () in
 *       let title = Amcharts3.title () in
 *       let subtitle = Amcharts3.title () in
 *       let legend = Amcharts3.legend () in
 *       subtitle##bold <- false;
 *       title##text <- Js.string @@ Lang.t_ title_str ;
 *       subtitle##text <- Js.string "" ;
 *       export##enabled <- false;
 *       pie##theme <- Js.string "light";
 *       pie##dataProvider <- Amcharts3.Pie.dataProvider data;
 *       pie##outlineColor <- Js.string "#FFFFFF";
 *       pie##balloonText <- Js.string balloon;
 *       pie##titles <- Js.array [| title; subtitle |] ;
 *       if Dom_html.window##screen##width <= 600 then
 *         begin
 *           pie##labelsEnabled <- false;
 *           legend##valueText <- Js.string "[[percents]]%";
 *           pie##legend <- legend
 *         end;
 * 
 *       pie##export <- export ;
 *       pie##write (Js.string tops_accounts_chart_id);
 *       let div = Common.get_div_by_id tops_accounts_chart_id in
 *       div##style##width <- Js.string "100%";
 *       if Dom_html.window##screen##width <= 600 then
 *         div##style##height <- Js.string "800px"
 *       else
 *         div##style##height <- Js.string "600px"
 *     ) *)

let update nrows kind level total xhr_request =
  let container = find_component "top_accounts_table" in
  let title_str = top_kind_to_title kind in

  Manip.removeChildren container ;

  begin match level with
    | None -> ()
    | Some level ->
      let date =
        div ~a:[ a_class [ "alert"; "alert-info" ] ] [
          strong [ pcdata "Data collected at " ] ;
          Common.make_link @@ string_of_int level.Tezos_types.lvl_level
        ] in

      Manip.appendChild container date
  end;

  let module Top =
    Panel.MakePageTable(
    struct
      let name = "top_accounts"
      let theads =
        Panel.theads_of_strings [
          s_account, 3;
          top_kind_to_thname kind, 1;
          s_percent, 1;
        ]
      let title_span = Panel.title_nb title_str
      let table_class = "blocks-table"
      let page_size = 20
    end) in
  let content = Top.make ~footer:true () in

  Manip.appendChild container content ;

  let printer = top_kind_to_printer kind in

  let to_rows accs =
    List.map (fun (hash, value) ->
        tr [
          Common.account_w_blockies hash;
          td [ printer value ];
          td [ pcdata @@ spf "%.2f %%" (
              (Int64.to_float value) /. (Int64.to_float total) *. 100. )]
      ]) accs.top_list
  in
  Top.paginate_fun to_rows ~nrows xhr_request

let update_cmd update =
  let optg1 = optgroup ~label:"Real Time" [] in
  let opt1 = option ~a:[ a_selected () ] (pcdata "Balances") in
  let optg2 = optgroup ~label:"Context Data" [] in
  let opt2 = option (pcdata "Frozen balances" ) in
  let opt3 = option (pcdata "Frozen deposits" ) in
  let opt4 = option (pcdata "Frozen rewards" ) in
  let opt5 = option (pcdata "Paid bytes" ) in
  let opt6 = option (pcdata "Staking balances" ) in
  let opt7 = option (pcdata "Total balances" ) in
  let opt8 = option (pcdata "Total delegated" ) in
  let opt9 = option (pcdata "Total delegators" ) in
  let opt10 = option (pcdata "Total frozen fees" ) in
  let opt11 = option (pcdata "Used bytes" ) in
  let id_container = tops_cmd_id in
  let container = find_component id_container in
  let n = 11 in
  let options = [
    optg1 ;
    opt1 ;
    optg2 ;
    opt2 ;
    opt3 ;
    opt4 ;
    opt5 ;
    opt6 ;
    opt7 ;
    opt8 ;
    opt9 ;
    opt10 ;
    opt11 ;
  ] in
  let select_elt =
    select ~a:[ a_class [form_control] ] options in
  Manip.Ev.onchange_select select_elt (fun _e ->
      let select_eltjs = Tyxml_js.To_dom.of_select select_elt in
      let opt = select_eltjs##options##item(select_eltjs##selectedIndex) in
      let selection =
        Js.Opt.case opt
          (fun () -> "")
          (fun opt ->
             try Js.to_string opt##value
             with _ -> "") in
      update selection;
      true);
  let content = form ~a:[ a_class [ form_inline ] ] [
      div ~a:[ a_class [ form_group ] ] [
        label [ pcdata "Kind :"; Bootstrap_helpers.Icon.space_icon () ] ;
        select_elt ] ] in
  Manip.removeChildren container ;
  Manip.appendChild container content ;
  if n <> 0 then update "Balances"

let make_page () =
  div [
    div
      ~a:[ a_id tops_cmd_id;
           a_class [ clgoffset10; clg2; cxsoffset8; cxs4; "tops-cmd" ] ] [] ;
    div ~a:[ a_id tops_accounts_chart_id; a_class [ cxs12 ] ] [] ;
    div ~a:[ a_id "top_accounts_table"; a_class [ cxs12 ] ] []
  ]
