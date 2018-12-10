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
open Data_types
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel

let context_content_id = "context-content-id"
let context_cmd_id = "context-cmd-id"
let context_cmd_select_id = "context-cmd-select-id"

let update_context_empty () = ()

let make_panel_info title value value_diff =
  let color_class =
    if value_diff = 0. then ""
    else if value_diff > 0. then "green"
    else "red" in
  div ~a:[ a_class [ clg3; csm6; cxs12 ] ] [
    div ~a:[ a_class [ panel; panel_default ] ] [
      div ~a:[ a_class [ panel_body ] ] [
        h5 ~a:[ a_class [ "text-center" ] ] [ title ] ;
        h3 ~a:[ a_class @@ color_class :: [ "text-center" ] ] [ value ] ;
        h5 ~a:[ a_class @@ color_class :: [ "text-center" ] ] [
          pcdata @@ Printf.sprintf "%+.1f%%" value_diff ] ;
      ] ;
    ]
  ]

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


let update_cmd days update =
  let arr = Array.of_list days in
  Common.make_options ~title:"Days" context_cmd_id arr update

let update_context_page context =
  let container = find_component context_content_id in

  let title_content = match context.context_level with
    | None ->
      span [ pcdata "" ]
    | Some lvl ->
      span [
        pcdata "Data collected at " ;
        Common.make_link @@ string_of_int lvl.Tezos_types.lvl_level ]
  in

  let bakings_str = "Bakings" in
  let rolls_title = pcdata "# rolls" in
  let rolls_value = pcdata @@ Printf.sprintf "%d" context.context_rolls in
  let rolls_diff = context.context_rolls_diff in
  let roll_owners_title = pcdata "# roll owners" in
  let roll_owners_value =
    pcdata @@ Printf.sprintf "%d" context.context_roll_owners in
  let roll_owners_diff = context.context_roll_owners_diff in
  let delegated_title = pcdata "Total delegated tz" in
  let delegated_value = Tez.pp_amount ~width:12 context.context_delegated in
  let delegated_diff = context.context_delegated_diff in
  let staking_balance_title = pcdata "Total staking balance" in
  let staking_balance_value =
    Tez.pp_amount ~width:12 context.context_staking_balances in
  let staking_balance_diff = context.context_staking_balances_diff in
  let delegates_title = pcdata "# delegates" in
  let delegates_value =
    pcdata @@ Printf.sprintf "%d" context.context_deleguees in
  let delegates_diff = context.context_deleguees_diff in
  let multi_delegates_title = pcdata "# multi delegates" in
  let multi_delegates_value =
    pcdata @@ Printf.sprintf "%d" context.context_multi_deleguees in
  let multi_delegates_diff = context.context_multi_deleguees_diff in
  let self_delegates_title = pcdata "# self delegates" in
  let self_delegates_value =
    pcdata @@ Printf.sprintf "%d" context.context_self_delegates in
  let self_delegates_diff = context.context_self_delegates_diff in
  let delegators_title = pcdata "# delegators" in
  let delegators_value =
    pcdata @@ Printf.sprintf "%d" context.context_delegators in
  let delegators_diff = context.context_delegators_diff in

  let balances_str = "Balances" in
  let current_balances_title = pcdata "Total current balances" in
  let current_balances_value =
    Tez.pp_amount ~width:12 context.context_current_balances in
  let current_balances_diff = context.context_current_balances_diff in
  let frozen_balances_title = pcdata "Total frozen balances" in
  let frozen_balances_value =
    Tez.pp_amount ~width:12 context.context_frozen_balances in
  let frozen_balances_diff = context.context_frozen_balances_diff in
  let full_balances_title = pcdata "Total full balances" in
  let full_balances_value =
    Tez.pp_amount ~width:12 context.context_full_balances in
  let full_balances_diff = context.context_full_balances_diff in
  let frozen_deposits_title = pcdata "Total frozen deposits" in
  let frozen_deposits_value =
    Tez.pp_amount ~width:12 context.context_frozen_deposits in
  let frozen_deposits_diff = context.context_frozen_deposits_diff in
  let frozen_rewards_title = pcdata "Total frozen rewards" in
  let frozen_rewards_value =
    Tez.pp_amount ~width:12 context.context_frozen_rewards in
  let frozen_rewards_diff = context.context_frozen_rewards_diff in
  let frozen_fees_title = pcdata "Total frozen fees" in
  let frozen_fees_value =
    Tez.pp_amount ~width:12 context.context_frozen_fees in
  let frozen_fees_diff = context.context_frozen_fees_diff in

  let accounts_str = "Accounts" in
  let addresses_title = pcdata "# addresses" in
  let addresses_value = pcdata @@ Printf.sprintf "%d" context.context_addresses in
  let revealed_title = pcdata "# revealed addresses" in
  let addresses_diff = context.context_addresses_diff in
  let revealed_value = pcdata @@ Printf.sprintf "%d" context.context_revealed in
  let originated_title = pcdata "# originated addresses" in
  let revealed_diff = context.context_revealed_diff in
  let originated_value = pcdata @@ Printf.sprintf "%d" context.context_originated in
  let contracts_title = pcdata "# smart contracts" in
  let originated_diff = context.context_originated_diff in
  let contracts_value =
    pcdata @@ Printf.sprintf "%d" context.context_contracts in
  let contracts_diff = context.context_contracts_diff in
  let paid_bytes_title = pcdata "Total paid bytes" in
  let paid_bytes_value = Tez.pp_amount ~width:12 context.context_paid_bytes in
  let used_bytes_title = pcdata "Total used bytes" in
  let paid_bytes_diff = context.context_paid_bytes_diff in
  let used_bytes_value = Tez.pp_amount ~width:12 context.context_used_bytes in
  let used_bytes_diff = context.context_used_bytes_diff in

  let summary = div ~a:[ a_class [ "health-summary"; row] ] [
      h1 [ title_content ] ;
      h2 [ pcdata bakings_str ] ;
      make_panel_info rolls_title rolls_value rolls_diff ;
      make_panel_info roll_owners_title roll_owners_value roll_owners_diff ;
      make_panel_info delegated_title delegated_value delegated_diff;
      make_panel_info staking_balance_title staking_balance_value staking_balance_diff ;
      make_panel_info delegates_title delegates_value delegates_diff ;
      make_panel_info multi_delegates_title multi_delegates_value multi_delegates_diff ;
      make_panel_info self_delegates_title self_delegates_value self_delegates_diff ;
      make_panel_info delegators_title delegators_value delegators_diff ;
      div ~a:[ a_class [ "clearfix" ] ] [] ;

      h2 [ pcdata balances_str ] ;
      make_panel_info current_balances_title current_balances_value current_balances_diff ;
      make_panel_info full_balances_title full_balances_value full_balances_diff ;
      make_panel_info frozen_balances_title frozen_balances_value frozen_balances_diff ;
      make_panel_info frozen_deposits_title frozen_deposits_value frozen_deposits_diff ;
      make_panel_info frozen_rewards_title frozen_rewards_value frozen_rewards_diff ;
      make_panel_info frozen_fees_title frozen_fees_value frozen_fees_diff ;
      div ~a:[ a_class [ "clearfix" ] ] [] ;

      h2 [ pcdata accounts_str ] ;
      make_panel_info addresses_title addresses_value addresses_diff ;
      make_panel_info revealed_title revealed_value revealed_diff ;
      make_panel_info originated_title originated_value originated_diff ;
      make_panel_info contracts_title contracts_value contracts_diff ;
      make_panel_info paid_bytes_title paid_bytes_value paid_bytes_diff ;
      make_panel_info used_bytes_title used_bytes_value used_bytes_diff ;

    ] in

  Manip.removeChildren container ;
  Manip.appendChild container summary

let make_page () =
  let div_cmd =
    div ~a:[ a_id context_cmd_id;
             a_class [ clgoffset10; clg2; cxsoffset8; cxs4; "context-cmd" ] ]
      [] in
  let div_content =
    div ~a:[ a_id context_content_id ;
             a_class [ clg12; cxs12; "context-content" ] ] [] in
  div ~a:[ a_class [ row ] ] [
    div_cmd ;
    div_content ]
