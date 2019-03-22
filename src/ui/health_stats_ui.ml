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
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Tezos_types
open Data_types

let health_content_id = "health-content-id"
let health_cmd_id = "health-cmd-id"
let health_cmd_select_id = "health-cmd-select-id"

let loading_fan_number = ref 0
(* We should have a place to store all protocol variables *)

let colors () =
  match Jsloc.find_arg "theme" with
  | Some "slate" -> "#008000", "#e50000"
  | _ -> "#00ff00", "#ff0000"

let get_progress_class i =
  if i < 50 then "red"
  else "green"

let get_progress_color i =
  let colors = colors () in
  if i < 50 then (snd colors)
  else (fst colors)

let get_reverse_progress_class i =
  if i < 50 then "green"
  else "red"

let get_reverse_progress_color i =
  let colors = colors () in
  if i < 50 then (fst colors)
  else (snd colors)

let init_fan () =
  for i = 0 to !loading_fan_number - 1 do
    let fan_class = Printf.sprintf ".fanbar-%d" i in
    let init_str = Printf.sprintf "var bar=new ldBar(%S,{});" fan_class in
    ignore (Js.Unsafe.eval_string init_str)
  done

let make_fan percent =
  let color = get_progress_color percent in
  let fan_init_class = Printf.sprintf "fanbar-%d" !loading_fan_number in
  let fan_class = get_progress_class percent in
  let set_fan bar =
    let barjs = To_dom.of_element bar in
    barjs##setAttribute(Js.string "data-value", Js.string @@ string_of_int percent) ;
    barjs##setAttribute(Js.string "data-stroke", Js.string color) ;
    barjs##setAttribute(Js.string "data-preset", Js.string "fan") in
  let pb = div ~a:[ a_class [ fan_class; fan_init_class; "ldBar"; "label-center"; "auto" ] ] [] in
  ignore (set_fan pb);
  incr loading_fan_number ;
  pb

let make_reverse_fan percent =
  let color = get_reverse_progress_color percent in
  let fan_init_class = Printf.sprintf "fanbar-%d" !loading_fan_number in
  let fan_class = get_reverse_progress_class percent in
  let set_fan bar =
    let barjs = To_dom.of_element bar in
    barjs##setAttribute(Js.string "data-value", Js.string @@ string_of_int percent) ;
    barjs##setAttribute(Js.string "data-stroke", Js.string color) ;
    barjs##setAttribute(Js.string "data-preset", Js.string "fan") in
  let pb = div ~a:[ a_class [ fan_class; fan_init_class; "ldBar"; "label-center"; "auto" ] ] [] in
  ignore (set_fan pb);
  incr loading_fan_number ;
  pb

let make_progress_bar ~cst cycle_position =
  let percent = (cycle_position + 1)* 100 / cst.blocks_per_cycle in
  let percent_str = Printf.sprintf "width: %d%%" percent in
  let percent_span = Printf.sprintf "%d%% : %d/%d" percent (cycle_position + 1) cst.blocks_per_cycle in
  div ~a:[ a_class [ row ] ] [
    div ~a:[ a_class [ clg8; clgoffset2; cxs12 ] ] [
      div ~a:[ a_class ["progress"]; a_id "health-progress"] [
        div ~a:[ a_id "bar"; a_class ["progress-bar"; "progress-bar-striped"];
                 a_role ["progressbar"];
                 a_style percent_str;
               ] [
          span ~a:[a_id "bar-span"] [txt percent_span]]]]]


let calculate_health_score stats =
  let endorsements_score = 10. *. stats.endorsements_rate in
  let priority_score = 5. *. stats.score_priority in
  int_of_float @@
  (endorsements_score +. priority_score) /. 15.

let make_panel_value title value color_class desc =
  div ~a:[ a_class [ clg3; csm6; cxs12 ] ] [
    div ~a:[ a_class [ panel; panel_default ] ] [
      div ~a:[ a_class [ panel_body ] ] [
        h5 ~a:[ a_class [ "text-center" ] ] [ title ] ;
        h2 ~a:[ a_class [ "text-center"; color_class ] ] [ value ] ;
      ] ;
      div ~a:[ a_class [ "text-center"; panel_footer ] ] [ desc ]
    ]
  ]

let make_panel_fan title percent desc =
  div ~a:[ a_class [ clg3; csm6; cxs12 ] ] [
    div ~a:[ a_class [ panel; panel_default ] ] [
      div ~a:[ a_class [ panel_body ] ] [
        h5 ~a:[ a_class [ "text-center" ] ] [ title ] ;
        make_fan @@ int_of_float percent ;
      ] ;
      div ~a:[ a_class [ "text-center"; panel_footer ] ] [ desc ]
    ]
  ]

let make_panel_reverse_fan title percent desc =
  div ~a:[ a_class [ clg3; csm6; cxs12 ] ] [
    div ~a:[ a_class [ panel; panel_default ] ] [
      div ~a:[ a_class [ panel_body ] ] [
        h5 ~a:[ a_class [ "text-center" ] ] [ title ] ;
        make_reverse_fan @@ int_of_float percent ;
      ] ;
      div ~a:[ a_class [ "text-center"; panel_footer ] ] [ desc ]
    ]
  ]

let make_centered_panel_fan title percent desc =
  div ~a:[ a_class [ clgoffset3; clg6; csmoffset3; csm6; cxs10; cxsoffset1 ] ] [
    div ~a:[ a_class [ panel; panel_default ] ] [
      div ~a:[ a_class [ panel_body ] ] [
        h3 ~a:[ a_class [ "text-center" ] ] [ title ] ;
        make_fan percent ;
        p ~a:[ a_class [ "small"; "text-muted"; "text-center" ] ] [ desc ]
      ] ;
    ]
  ]

let make_panel_info title value desc =
  div ~a:[ a_class [ clg3; csm6; cxs12 ] ] [
    div ~a:[ a_class [ panel; panel_default ] ] [
      div ~a:[ a_class [ panel_body ] ] [
        h5 ~a:[ a_class [ "text-center" ] ] [ title ] ;
        h3 ~a:[ a_class [ "text-center" ] ] [ value ] ;
      ] ;
      div ~a:[ a_class [ "text-center"; panel_footer; "no-overflow" ] ] [ desc ]
    ]
  ]

let update_health_page cycle stats =
  (* We need a binding for loadingjs if we dont want to deal with this :
     - We need to make an unsafe call to initialize each loading bars
     - We need to uniquely identify each loading bars *)
  loading_fan_number := 0 ;
  let cycle_position = (stats.cycle_end_level - stats.cycle_start_level) in
  let container = find_component health_content_id in
  let cst = Infos.constants ~cycle in
  let cycle_str = Printf.sprintf "Cycle #%i" cycle in

  let level_title = txt "Cycle Levels" in
  let level_value =
    txt @@
    Printf.sprintf "%d - %d" stats.cycle_start_level stats.cycle_end_level in
  let level_desc =
    if stats.cycle_end_level - stats.cycle_start_level + 1 =
       cst.blocks_per_cycle
    then
      let year_start, month_start, day_start = stats.cycle_date_start in
      let year_end, month_end, day_end = stats.cycle_date_end in
      txt @@ Printf.sprintf "%02d/%02d/%d - %02d/%02d/%d"
       day_start month_start year_start day_end month_end year_end
    else begin
      let levels_left =
        cst.blocks_per_cycle - (stats.cycle_end_level - stats.cycle_start_level) in
      txt @@ Printf.sprintf "Cycle in progress: %s left"
        (Format_date.time_before_level ~cst levels_left)
    end in
  let volume_title = txt "Cycle Volume" in
  let volume_value = Tez.pp_amount ~width:12 stats.cycle_volume in
  let volume_desc =
    let hash, level = stats.biggest_block_volume in
    span [
      txt "Highest Volume Block " ;
      Common.make_link (string_of_int level) ~path:hash
    ] in

  let fees_title = txt "Cycle Fees" in
  let fees_value = Tez.pp_amount ~width:12 stats.cycle_fees in
  let fees_desc =
    let hash, level = stats.biggest_block_fees in
    span [
      txt "Highest Fees Block " ;
      Common.make_link (string_of_int level) ~path:hash
    ] in

  let bakers_endorsers_title = txt "Uniquer bakers / endorsers" in
  let bakers_endorsers_value =
    txt @@
    Printf.sprintf "%d / %d" stats.cycle_bakers stats.cycle_endorsers in
  let bakers_endorsers_desc =
    span ~a:[a_class ["no-overflow"; cxs12]] [
      txt "Top Baker ";
      Common.make_link_account stats.top_baker] in
  let health_score =
    let health_title = txt "Health Score" in
    let health_percent = calculate_health_score stats in
    let health_desc = txt "" in
    div ~a:[ a_class [ row ] ] [
      make_centered_panel_fan health_title health_percent health_desc ;
    ] in
  let summary = div ~a:[ a_class [ "health-summary"; row] ] [
      h1 [ txt cycle_str ] ;
      health_score ;
      make_panel_info level_title level_value level_desc ;
      make_panel_info volume_title volume_value volume_desc ;
      make_panel_info fees_title fees_value fees_desc ;
      make_panel_info
        bakers_endorsers_title
        bakers_endorsers_value
        bakers_endorsers_desc ;
    ] in
  let progress_bar = make_progress_bar ~cst cycle_position in
  let endorsements_rate_title = txt "Endorsement Rate" in
  let endorsements_rate_desc = txt "Non empty slots" in
  let main_endorsements_rate_title = txt "Endorsement Distribution" in
  let main_endorsements_rate_desc = txt "Endorsements on the main chain" in
  let alt_endorsements_rate_title = txt "Alt Chain Slot Rate" in
  let alt_endorsements_rate_desc = txt "Endorsements on an alt chain" in
  let empty_endorsements_rate_title = txt "Empty Slot Rate" in
  let empty_endorsements_rate_desc = txt "Empty endorsement slots" in

  let double_endorsement_title = txt "Double Endorsements" in
  let double_endorsement_value = txt @@ string_of_int stats.double_endorsements in
  let double_endorsement_class =
    if stats.double_endorsements = 0 then "good-value" else "bad-value" in
  let double_endorsement_desc = txt "Number of double endorsements" in

  let alt_heads_number_title = txt "Alternative Heads" in
  let alt_heads_number_value =
    txt @@ string_of_int stats.alternative_heads_number in
  let alt_heads_number_class =
    if stats.alternative_heads_number < 5 then "good-value" else "bad-value" in
  let alt_heads_number_desc = txt "Number of alternative blocks" in

  let tzscan_stats = txt "TzScan Nodes Stats" in
  let switch_number_title = txt "Number of Long Chains Received" in
  let switch_number_value =
    txt @@ Printf.sprintf "%d" stats.switch_number in
  let switch_number_class =
    if stats.switch_number = 0 then "good-value" else "bad-value" in
  let switch_number_desc = txt "Number of Long Chains Received By TzScan" in

  let longest_switch_depth_title = txt "Longest Chain Received" in
  let longest_switch_depth_value =
    txt @@ Printf.sprintf "%d" stats.longest_switch_depth in
  let longest_switch_depth_class =
    if stats.longest_switch_depth = 0 then "good-value" else "bad-value" in
  let longest_switch_depth_desc = txt "Longest Chain Crawled by TzScan" in

  let main_revelation_rate_title = txt "Nonce Revelations Rate" in
  let main_revelation_rate_desc = txt "Nonce revealed in this cycle" in

  let score_priority_title = txt "Priority Score" in
  let score_priority_desc =
    txt @@
    Printf.sprintf "Average priority is %.2f" stats.mean_priority in

  let details =
    div ~a:[ a_class [ row; "details" ] ] [
      make_panel_fan
        endorsements_rate_title
        stats.endorsements_rate
        endorsements_rate_desc ;

      make_panel_fan
        main_endorsements_rate_title
        stats.main_endorsements_rate
        main_endorsements_rate_desc ;

      make_panel_reverse_fan
        alt_endorsements_rate_title
        stats.alt_endorsements_rate
        alt_endorsements_rate_desc ;

      make_panel_reverse_fan
        empty_endorsements_rate_title
        stats.empty_endorsements_rate
        empty_endorsements_rate_desc ;

      make_panel_value
        double_endorsement_title
        double_endorsement_value
        double_endorsement_class
        double_endorsement_desc ;

      make_panel_value
        alt_heads_number_title
        alt_heads_number_value
        alt_heads_number_class
        alt_heads_number_desc ;

      make_panel_fan
        main_revelation_rate_title
        stats.main_revelation_rate
        main_revelation_rate_desc ;

      make_panel_fan
        score_priority_title
        stats.score_priority
        score_priority_desc ;

      h2 ~a:[ a_class [ "center" ] ] [ tzscan_stats ] ;

      make_panel_value
        switch_number_title
        switch_number_value
        switch_number_class
        switch_number_desc ;

      make_panel_value
        longest_switch_depth_title
        longest_switch_depth_value
        longest_switch_depth_class
        longest_switch_depth_desc
    ] in
  Manip.removeChildren container ;
  Manip.appendChild container summary ;
  Manip.appendChild container progress_bar ;
  Manip.appendChild container details ;
  init_fan ()


let update_cmd last_cycle update =
  let arr = Array.init (last_cycle+1) (fun i -> string_of_int @@ last_cycle - i) in
  Common.make_options health_cmd_id arr update

let make_page () =
  let div_cmd =
    div ~a:[ a_id health_cmd_id;
             a_class [ clgoffset10; clg2; cxsoffset8; cxs4; "health-cmd" ] ]
      [] in
  let div_content =
    div ~a:[ a_id health_content_id ;
             a_class [ clg12; cxs12; "health-content" ] ] [] in
  div ~a:[ a_class [ row ] ] [
    div_cmd ;
    div_content ]
