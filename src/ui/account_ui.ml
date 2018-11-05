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

open Tezos_types
open Data_types
open Tyxml_js.Html5
open Js_utils
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Bootstrap_helpers.Color
open Bootstrap_helpers.Align
open Bootstrap_helpers.Button
open Lang
open Common
open Text

let div64 = Int64.div

let details_id hash = Common.make_id "account-details" hash
let details_roll_id hash = Common.make_id "account-details-roll" hash

let bakings_stats_id hash = Common.make_id "account-bakings-stats" hash

let tabs_id hash = Common.make_id "account-tabs" hash

let deleguees_id hash = Common.make_id "account-deleguees" hash
let nb_transactions_id hash = Common.make_id "nb-transactions" hash

let balance_id hash = Common.make_id "account-balance" hash
let staking_balance_id hash = Common.make_id "account-staking-balance" hash
let balance_label_id hash = Common.make_id "account-balance-label" hash
let advanced_baking_id hash = Common.make_id "account-advanced-balance" hash

let del_balance_id hash del_hash =
  Common.make_id "del" (Printf.sprintf "%s-%s" hash del_hash)

let tz1_details_id hash = Common.make_id "tz1-details" hash

let transactions_view_shown_once = ref false
let delegations_view_shown_once = ref false
let originations_view_shown_once = ref false
let endorsements_view_shown_once = ref false
let bakings_view_shown_once = ref false
let rewards_view_shown_once = ref false

let level_id op_hash op_block_hash index =
  let id = Printf.sprintf "%s-%s-%i" op_hash op_block_hash index in
  Common.make_id "level" id

let timestamp_id op_hash op_block_hash index =
  let id = Printf.sprintf "%s-%s-%i" op_hash op_block_hash index in
  Common.timestamp_id id

let update_timestamp op_hash op_block_hash index timestamp =
  let ts_span = find_component @@ timestamp_id op_hash op_block_hash index in
  Manip.replaceChildren ts_span [Format_date.auto_updating_timespan timestamp]

let update_level op_hash op_block_hash index level =
  let lvl_span = find_component @@ level_id op_hash op_block_hash index in
  Manip.replaceChildren lvl_span
    [ a ~a:( Common.a_link op_block_hash ) [ pcdata @@ string_of_int level ]  ]

let update_balance hash del_hash details =
  let bl_span = find_component @@ del_balance_id hash del_hash in
  Manip.replaceChildren bl_span [ Tez.pp_amount details.acc_balance ]

let tz1_details name =
  let div_alias, div_hash = match name.alias with
    | None -> div [], strong ~a:[ a_class [cxs12; "no-overflow"] ] [pcdata name.tz]
    | Some alias -> strong [pcdata alias], strong [pcdata name.tz] in
  let data_container = to_attrib @@ Tyxml_js.Xml.string_attrib "container" "body" in
  let template =
    Printf.sprintf "<div id=\"%s\" class=\"%s\"><strong>%s</strong></div>" (tz1_details_id name.tz)
      (String.concat "\" \"" [ ]) name.tz in
  div ~a:[ a_class [ row ] ] [
    div ~a:[ a_class [cxs12; text_center] ] [ div_alias ];
    div ~a:[ a_class [cxs1] ] [
      a ~a:[ a_href "#"; a_onclick (fun _ ->
          Js_utils.Clipboard.copy name.tz;
          false) ] [clipboard_icon ()]
    ];
    div ~a:[ a_class [cxs10; text_center ; "no-overflow"] ] [
      a ~a:[ Bootstrap_helpers.Attributes.a_data_toggle "popover";
              Bootstrap_helpers.Attributes.a_data_placement `Top;
              Bootstrap_helpers.Attributes.a_data_html true;
              Bootstrap_helpers.Attributes.a_data_trigger "focus";
              Bootstrap_helpers.Attributes.a_role "button";
              a_tabindex 0;
              data_container;
              (* Bootstrap_helpers.Attributes.a_data_content hash; *)
              Bootstrap_helpers.Attributes.a_data_content template;
              a_id @@ tz1_details_id name.tz;
           ]
        [div_hash]
    ] ;
  ]

type filter =
  | Def_Txs | Def_Del | Def_Ori
  | Def_Endt | Def_Bk | Def_Rew | Def_Code

let string_of_filter = function
    Def_Txs -> "transaction"
  | Def_Del -> "delegation"
  | Def_Ori -> "origination"
  | Def_Endt -> "endorsement"
  | Def_Bk -> "baking"
  | Def_Rew -> "rewards"
  | Def_Code -> "code"

let filter_of_string = function
    "transaction" -> Def_Txs
  | "delegation" -> Def_Del
  | "origination" -> Def_Ori
  | "endorsement" -> Def_Endt
  | "baking" -> Def_Bk
  | "rewards" -> Def_Rew
  | "code" -> Def_Code
  | _ -> Def_Txs

let default_filter filters =
  match Misc.list_assoc_opt "default" filters with
  | Some filter -> filter_of_string filter
  | _ -> Def_Txs

let is_active default exp =
  if default = exp then Tabs.Active else Tabs.Inactive

let mk_title icon title nb =
  span ( Common.responsive_title_fun icon
           (Panel.title_nb title)
           (match nb with Some nb -> nb | None -> -1)
       )

let tr_tab = Tabs.make
    (mk_title exchange_icon s_transactions) [ "account-tab" ]

let del_tab = Tabs.make
    (mk_title handshake_icon s_delegations) [ "account-tab" ]

let ori_tab = Tabs.make
    (mk_title link_icon s_originations) [ "account-tab" ]

let endt_tab = Tabs.make
    (mk_title stamp_icon s_endorsements) [ "account-tab" ]

let baking_tab = Tabs.make
    (mk_title cookie_icon s_bakings) [ "account-tab" ]

let rewards_tab = Tabs.make
    (mk_title bill_icon s_rewards) [ "account-tab" ]

let code_tab = Tabs.make
    (mk_title code_icon s_code) [ "account-tab" ]

let michelson_tab = Tabs.make
    (mk_title Tez.icon s_michelson) [ "code-tab" ]

let liquidity_tab = Tabs.make
    (mk_title liquidity_icon s_liquidity) [ "code-tab" ]

let storage_tab = Tabs.make
    (mk_title database_icon s_storage) [ "code-tab" ]

let dapp_tab = Tabs.make
    (mk_title Bootstrap_helpers.Icon.database_icon s_view_dapp) [ "code-tab" ]

module AccountOperationsPanel = struct
  let title_span _ = span []
  let table_class = "transactions-table"
  let page_size = Common.big_panel_number
end

module TransactionsPanel = Panel.MakePageTable(
  struct
    include AccountOperationsPanel
    let name = "transactions"
    let theads () =
      tr [
        th ~a:[ a_class [ cxs1 ] ] @@ cl_icon exchange_icon (t_ s_txn_hash);
        th ~a:[ a_class [ cxs1 ] ] @@ cl_icon cube_icon (t_ s_block);
        th ~a:[ a_class [ cxs1 ] ] @@ cl_icon clock_icon (t_ s_date);
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon account_icon (t_ s_from);
        th ~a:[ a_class [ cxs1; "arrow" ] ] [ pcdata "" ] ;
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon account_icon (t_ s_to);
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon Tez.icon (t_ s_amount);
        th ~a:[ a_class [ cxs1 ] ] @@ cl_icon bill_icon (t_ s_fee);
        th ~a:[ a_class [ cxs1 ] ] @@ cl_icon params_icon (t_ s_param);
      ]
  end)

let make_account_transactions hash transactions =
  match transactions with
  | [] -> [ tr [ td ~a: [ a_colspan 9 ] [ pcdata_t s_no_transaction ]]]
  | _ ->
    (* let ops = *)
    List.map (fun (op_hash, op_block_hash, src, transaction) ->
        let src, arrow, dst =
          if transaction.tr_failed then
            Common.account_w_blockies src,
            td ~a:[ a_title (t_ s_failed) ;
                    a_class [ red ] ] [ cross_icon () ],
            Common.account_w_blockies transaction.tr_dst
          else if src.tz = hash then begin
            Common.account_w_blockies src,
            td ~a:[ a_class [ red ] ] [ right_icon () ],
            Common.account_w_blockies transaction.tr_dst
          end else begin
            Common.account_w_blockies src,
            td ~a:[ a_class [ green ] ] [ right_icon () ],
            Common.account_w_blockies transaction.tr_dst
          end  in
        let param = match transaction.tr_parameters with
            None -> td [ pcdata_t s_no ]
          | Some p ->
            let template =
              Printf.sprintf "<div class=\"%s\">%s</div>"
                (String.concat "\" \"" [ ])
                (Common.html_escaped p) in
            td [
              a ~a:[ Bootstrap_helpers.Attributes.a_data_toggle "popover";
                     Bootstrap_helpers.Attributes.a_data_placement `Top;
                     Bootstrap_helpers.Attributes.a_data_html true;
                     Bootstrap_helpers.Attributes.a_data_trigger "focus";
                     Bootstrap_helpers.Attributes.a_role "button";
                     a_tabindex 0;
                     to_attrib @@ Tyxml_js.Xml.string_attrib "container" "body" ;
                     (* Bootstrap_helpers.Attributes.a_data_content hash; *)
                     Bootstrap_helpers.Attributes.a_data_content template;
                   ] [
                pcdata_t s_yes ]
            ] in
        let tr_class =
          if transaction.tr_failed then
            [ a_class @@ Common.failed_class transaction.tr_failed ]
          else if op_block_hash = Utils.pending_block_hash then
            [ a_class [ "operation-pending" ] ]
          else if op_block_hash = Utils.orphan_block_hash then
            [ a_class [ "operation-orphan" ] ]
          else if transaction.tr_internal then
            [ a_class [ "warning" ];
              a_title (t_ s_include_internal_operations) ]
          else [] in
        let block_hash =
          if op_block_hash = Utils.pending_block_hash
          then td [ pcdata_t s_pending ]
          else td [ Common.make_link ~path:op_block_hash
                    @@ string_of_int transaction.tr_op_level] in
        tr ~a:tr_class  [
          td [ Common.make_link ~crop_len:8 op_hash ] ;
          block_hash ;
          td [ Format_date.auto_updating_timespan transaction.tr_timestamp ] ;
          src ;
          arrow ;
          dst ;
          td [ Tez.pp_amount transaction.tr_amount ] ;
          td [ if transaction.tr_internal
               then Common.pcdata_ () else Tez.pp_amount transaction.tr_fee ] ;
          param
        ]) @@ Common.get_transactions transactions

module DelegationsPanel = Panel.MakePageTable(
  struct
    include AccountOperationsPanel
    let name = "delegations"
    let theads () =
      tr [
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon handshake_icon (t_ s_del_hash);
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon cube_icon (t_ s_block);
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon account_icon (t_ s_account);
        th ~a:[ a_class [ cxs2 ] ] @@ [ pcdata "" ] ;
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon astronaut_icon (t_ s_delegate);
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon clock_icon (t_ s_age);
        th ~a:[ a_class [ cxs2 ] ] @@ cl_icon bill_icon (t_ s_fee);
      ]
    let title_span nb = Panel.title_nb s_delegations nb
  end)

module DelegueesPanel = Panel.MakePageTable(
  struct
    let title_span nb = Panel.title_nb s_delegators nb
    let table_class = "transactions-table"
    let page_size = Common.small_panel_number
    let name = "Delegators"
    let theads () =
      tr [
        th [ pcdata_t s_hash ] ;
        th [ pcdata_t s_balance ] ;
      ]
  end)

let make_account_delegations hash dels =
  match dels with
  | [] -> [ tr [ td ~a:[ a_colspan 7 ] [ pcdata_t s_no_delegations ]]]
  | _ ->
    List.mapi (fun i (op_hash, op_block_hash, src, del) ->
        Common.timestamp op_block_hash (update_timestamp op_hash op_block_hash i);
        Common.level op_block_hash (update_level op_hash op_block_hash i);
        let src, arrow, delegate =
          if src.tz = hash then begin
            Common.account_w_blockies src,
            td ~a:[ a_class [ red ] ] [ right_icon () ],
            Common.account_w_blockies del.del_delegate
          end else begin
            Common.account_w_blockies src,
            td ~a:[ a_class [ green ] ] [ right_icon () ],
            Common.account_w_blockies del.del_delegate
          end  in
        let tr_class =
          if op_block_hash = Utils.pending_block_hash
          then [ a_class [ "operation-pending" ] ]
          else if op_block_hash = Utils.orphan_block_hash
          then [ a_class [ "operation-orphan" ] ]
          else if del.del_internal then
            [ a_class [ "warning" ];
              a_title (t_ s_include_internal_operations) ]
          else [] in
        let block_hash =
          if op_block_hash = Utils.pending_block_hash
          then td [ pcdata_t s_pending ]
          else td ~a:[ a_id @@ level_id op_hash op_block_hash i] [
              Common.pcdata_ () ] in
        tr ~a:tr_class [
          td [ Common.make_link ~crop_len:8 op_hash ] ;
          block_hash ;
          src ;
          arrow ;
          delegate ;
          td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ]
            [ Common.pcdata_ () ] ;
          td [ if del.del_internal
               then Common.pcdata_ () else Tez.pp_amount del.del_fee ] ;
        ]) @@ Common.get_delegations dels

let make_account_deleguees hash dels =
  match dels with
  | [] -> [ tr [ td ~a:[ a_colspan 7 ] [ pcdata_t s_no_delegators ]]]
  | _ ->
    List.mapi (fun _i del ->
        Common.balance del (update_balance hash del);
        tr [
          Common.account_w_blockies {tz=del;alias=None} ;
          td ~a:[ a_id @@ del_balance_id hash del ]
            [ pcdata @@ Common.bullshit_s ] ;
        ]) dels

let theads_originations () =
  tr [
    th ~a:[ a_class [ cxs1 ] ] @@ cl_icon link_icon (t_ s_operation_hash);
    th ~a:[ a_class [ cxs1 ] ] @@ cl_icon cube_icon (t_ s_block);
    th ~a:[ a_class [ cxs1 ] ] @@ cl_icon clock_icon (t_ s_age);
    th ~a:[ a_class [ cxs2 ] ] @@ cl_icon account_icon (t_ s_new_account);
    th ~a:[ a_class [ cxs1 ] ] @@ cl_icon Tez.icon (t_ s_new_balance);
    th ~a:[ a_class [ cxs2 ] ] @@ cl_icon originator_icon (t_ s_originator);
    th ~a:[ a_class [ cxs2 ] ] @@ cl_icon manager_icon (t_ s_manager);
    th ~a:[ a_class [ cxs1 ] ] @@ cl_icon bill_icon (t_ s_fee);
    th ~a:[ a_class [ cxs1 ] ] @@ cl_icon burn_icon (t_ s_burn);
  ]

module OriginationsPanel = Panel.MakePageTable(
  struct
    include AccountOperationsPanel
    let name = "originations"
    let theads = theads_originations
  end)

module OriginationsDelegatePanel = Panel.MakePageTable(
  struct
    include AccountOperationsPanel
    let name = "originations_delegate"
    let theads = theads_originations
    let title_span nb = Panel.title_nb s_originations nb
  end)

let make_account_originations hash origs =
  match origs with
  | [] -> [ tr [ td ~a:[ a_colspan 8 ] [ pcdata_t s_no_origination ]]]
  | _ ->
    List.mapi (fun i (op_hash, op_block_hash, src, orig) ->
        Common.timestamp op_block_hash (update_timestamp op_hash op_block_hash i);
        Common.level op_block_hash (update_level op_hash op_block_hash i);
        let tr_class =
          if orig.or_failed then
            [ a_class @@ Common.failed_class orig.or_failed ]
          else if op_block_hash = Utils.pending_block_hash
          then [ a_class [ "operation-pending" ] ]
          else if op_block_hash = Utils.orphan_block_hash
          then [ a_class [ "operation-orphan" ] ]
          else if orig.or_internal then
            [ a_class [ "warning" ];
              a_title (t_ s_include_internal_operations) ]
          else [] in
        let block_hash =
          if op_block_hash = Utils.pending_block_hash
          then td [ pcdata_t s_pending ]
          else td ~a:[ a_id @@ level_id op_hash op_block_hash i ] [
              Common.pcdata_ () ] in
        let burn = orig.or_burn in
        let td_burn = td [ Tez.pp_amount burn ] in
        let new_acc, src, manager =
          if orig.or_tz1.tz = hash then begin
            Common.account_w_blockies ~crop_len:10 orig.or_tz1,
            td [ Common.make_link_account ~crop_len:10 src ],
            td [ Common.make_link_account ~crop_len:10 orig.or_manager ]
          end else if src.tz = hash then begin
            Common.account_w_blockies ~crop_len:10 orig.or_tz1,
            td [ pcdata_account src ],
            td [ Common.make_link_account ~crop_len:10 orig.or_manager ]
          end else begin
            Common.account_w_blockies ~crop_len:10 orig.or_tz1,
            td [ Common.make_link_account ~crop_len:10 src ],
            td [ pcdata_account orig.or_manager ]
          end in
        tr ~a:tr_class [
          td [ Common.make_link ~crop_len:8 op_hash ] ;
          block_hash ;
          td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ]
            [ Common.pcdata_ () ] ;
          new_acc ;
          td [ Tez.pp_amount orig.or_balance ] ;
          src ;
          manager ;
          td [ if orig.or_internal
               then Common.pcdata_ () else Tez.pp_amount orig.or_fee ] ;
          td_burn ;
        ]) @@ Common.get_originations origs

module EndorsementsPanel = Panel.MakePageTable(
  struct
    include AccountOperationsPanel
    let name = "endorsements"
    let theads () =
      tr [
        th @@ cl_icon stamp_icon (t_ s_end_hash);
        th @@ cl_icon cube_icon (t_ s_block);
        th @@ cl_icon clock_icon (t_ s_age);
        th @@ cl_icon account_icon (t_ s_endorser) ;
        th @@ cl_icon slots_icon (t_ s_slots);
        th @@ cl_icon check_icon (t_ s_endorsed_block);
        th @@ cl_icon priority_icon (t_ s_priority) ;
      ]
  end)

let make_account_endorsements ents =
  match ents with
  | [] -> [ tr [ td ~a:[ a_colspan 5 ] [ pcdata_t s_no_endorsement ]]]
  | _ ->
    List.map (fun (op_hash, op_block_hash, endorse) ->
        let tr_class =
          if op_block_hash = Utils.pending_block_hash
          then [ a_class [ "operation-pending" ] ]
          else if op_block_hash = Utils.orphan_block_hash
          then [ a_class [ "operation-orphan" ] ]
          else [] in
        let block_hash =
          if op_block_hash = Utils.pending_block_hash
          then td [ pcdata_t s_pending ]
          else td [ Common.make_link ~path:op_block_hash @@
                    string_of_int endorse.endorse_op_level ] in
        tr ~a:tr_class [
          td [ Common.make_link ~crop_len:8 op_hash ] ;
          block_hash ;
          td [ Format_date.auto_updating_timespan endorse.endorse_timestamp] ;
          Common.account_w_blockies endorse.endorse_src;
          td [ pcdata @@
               String.concat ", " @@
               List.map string_of_int endorse.endorse_slot ] ;
          td [ Common.make_link endorse.endorse_block_hash ] ;
          td [ pcdata @@ string_of_int endorse.endorse_priority ] ;
        ]) @@ Common.get_endorsements ents


(* Bakings Tab *)

let toggle = a_user_data "toggle" "collapse"

module BakingsPanel = Panel.MakePageTable(
  struct
    let title_span _nb = span [pcdata_t s_cycle_details]
    let name = "bakings"
    let theads () =
       tr [
         th @@ cl_icon cube_icon (t_ s_level);
         th @@ cl_icon priority_icon (t_ s_priority);
         th @@ cl_icon bill_icon (t_ s_rewards);
         th @@ cl_icon deposit_icon (t_ s_deposits);
         th @@ cl_icon clock_icon (t_ s_bake_time);
         th @@ cl_icon spinner_icon (t_ s_status)
       ]
    let page_size = 10
    let table_class = "bakings-table"
  end)

let make_account_bakings bks =
  match bks with
  | [] -> [ tr [ td ~a:[ a_colspan 6 ] [
      pcdata_t s_no_block_baked_for_this_cycle ]]]
  | _ ->
    List.map (fun bk ->
        let cycle = bk.bk_cycle in
        let cst = Infos.constants ~cycle in
        let rewards =
          Tez.pp_amount @@ Int64.add bk.bk_fees cst.block_reward in
        let deposits =
          Tez.pp_amount (Infos.rampup ~cycle cst.block_security_deposit) in
        let str_bktime = Printf.sprintf "%ds" bk.bk_bktime in
        let str_level = string_of_int bk.bk_level in
        let div_level =
          if bk.bk_distance_level = 0 then Common.make_link str_level
          else Common.make_link str_level ~path:bk.bk_block_hash in
        let div_rewards, div_deposit, div_bake_time =
          if not bk.bk_baked || bk.bk_distance_level <> 0 then
            Common.pcdata_ (), Common.pcdata_ (), Common.pcdata_ ()
          else rewards, deposits, pcdata str_bktime in
        let div_status, tr_class =
          if not bk.bk_baked then pcdata_t s_missed, [ a_class ["danger"] ]
          else if bk.bk_distance_level <> 0 then
            pcdata_t s_alternative_branch, [ a_class ["bg-block-alt-chain"] ]
          else pcdata_t s_good, [] in
        tr ~a:tr_class [
          td [ div_level ] ;
          td [ pcdata @@ string_of_int @@ bk.bk_priority ] ;
          td [ div_rewards ] ;
          td [ div_deposit ] ;
          td [ div_bake_time ] ;
          td [ div_status ]
        ]) bks

module BakingRightsPanel = Panel.MakePageTable(
  struct
    let title_span nb = Panel.title_nb s_baking_rights nb
    let table_class = "bakings-table"
    let page_size = 5
    let name = "pending_bake_priorities"
    let theads () =
      tr [
        th @@ cl_icon cube_icon (t_ s_level);
        th @@ cl_icon priority_icon (t_ s_priority);
        th @@ cl_icon bill_icon (t_ s_rewards);
        th @@ cl_icon deposit_icon (t_ s_deposits);
        th @@ cl_icon clock_icon (t_ s_eta)
      ]
  end)

let make_account_baking_rights prs =
  match prs with
  | [] -> [ tr [ td ~a:[ a_colspan 5 ] [
      pcdata_t s_no_baking_rights_for_this_cycle ]]]
  | _ ->
    List.map (fun pr ->
        let cycle = pr.br_cycle in
        let cst = Infos.constants ~cycle in
        let security_deposit = Infos.rampup cst.block_security_deposit ~cycle in
        let div_deposit, div_reward =
          if pr.br_priority = 0 then
            [Tez.pp_amount security_deposit], [Tez.pp_amount cst.block_reward]
          else
            [pcdata "("; Tez.pp_amount security_deposit; pcdata ")"],
            [pcdata "("; Tez.pp_amount cst.block_reward; pcdata ")"] in
        tr [
          td [ pcdata @@ string_of_int pr.br_level ] ;
          td [ pcdata @@ string_of_int pr.br_priority ] ;
          td div_reward ;
          td div_deposit ;
          td [ pcdata @@ Format_date.time_before_level ~cst pr.br_depth ] ;
        ]) prs


module CycleBakingsPanel = Panel.MakePageTable(
  struct
    let name = "cycle-bakings"
    let table_class = "bakings-table"
    let title_span _nb = span [ pcdata_t s_baking_history ]
    let page_size = 15
    let theads () =
      tr [
        th @@ cl_icon cycle_icon (t_ s_cycle);
        th @@ cl_icon (number_icon cube_icon) (t_ s_nbblocks);
        th @@ cl_icon priority_icon (t_ s_av_priority);
        th @@ cl_icon exclamation_icon (t_ s_miss_steal);
        th @@ cl_icon bill_icon (t_ s_rewards);
        th @@ cl_icon deposit_icon (t_ s_tot_deposits);
        th @@ cl_icon clock_icon (t_ s_bake_time);
        th @@ cl_icon spinner_icon (t_ s_rew_status);
       ]
  end)

let make_baking_history_table xhr_rights xhr_1cycle hash
    (total_bk, cbks_r, cbks) =
  match cbks, cbks_r with
  | [], [] -> [ tr [ td ~a:[ a_colspan 8 ] [
      pcdata_t s_no_block_baked_and_no_baking_rights ]]]
  | _ ->
    let future_rows = if cbks_r = [] then [] else
        List.flatten @@ List.map (fun cbk_r ->
            let cycle = cbk_r.cr_cycle in
            let cst = Infos.constants ~cycle in
            let id = "bakings-future" in
            let suf_id = Printf.sprintf "-%d" cbk_r.cr_cycle in
            let id_cycle = id ^ suf_id in
            let target = a_user_data "target" ("#" ^ id_cycle) in
            let data_href = a_user_data "href" ("#" ^ id_cycle) in
            let href =  a_href ("#" ^ id_cycle) in
            let security_deposit =
              Infos.rampup ~cycle cst.block_security_deposit in
            let deposit = Int64.(mul security_deposit (of_int cbk_r.cr_nblocks)) in
            let rewards =
              Int64.(mul (of_int cbk_r.cr_nblocks) cst.block_reward) in
            [ tr ~a:[toggle; target; data_href] [
                  td [ pcdata @@ string_of_int cbk_r.cr_cycle ];
                  td [ pcdata @@ string_of_int cbk_r.cr_nblocks ];
                  td [ pcdata @@ Printf.sprintf "%.2f" cbk_r.cr_priority ];
                  td [ pcdata_ () ];
                  td [ Tez.pp_amount rewards ];
                  td [ Tez.pp_amount deposit ];
                  td [ pcdata_ () ];
                  td [ pcdata_ () ;
                       a ~a:[ a_class ["caret"; "pull-right"]; toggle; href] [] ]];
              tr ~a:[ a_id id_cycle ; a_class ["collapse"];
                      a_onshow (fun _ev ->
                          xhr_rights ?cycle:(Some cbk_r.cr_cycle) hash; true)] [
                td ~a:[ a_colspan 8 ] [
                  BakingRightsPanel.make ~suf_id ~panel_class:["bakings-div"] ();
                ]]]) cbks_r in
    let passed_rows = List.flatten @@ List.map (fun cbk ->
        let cycle = cbk.cbk_cycle in
        let id = "bakings" in
        let suf_id = Printf.sprintf "-%d" cbk.cbk_cycle in
        let id_cycle = id ^ suf_id in
        let target =  a_user_data "target" ("#" ^ id_cycle) in
        let data_href = a_user_data "href" ("#" ^ id_cycle) in
        let href =  a_href ("#" ^ id_cycle) in
        let priority_str = Misc.unoptf Common.bullshit_s
            (Printf.sprintf "%.2f") cbk.cbk_priority in
        let bktime_str = Misc.unoptf Common.bullshit_s
            (Printf.sprintf "%ds") cbk.cbk_bktime in
        let rewards =
          Int64.add cbk.cbk_tez.tez_fee cbk.cbk_tez.tez_reward in
        let cst = Infos.constants ~cycle in
        let color, str_status =
          if cbk.cbk_depth <= cst.preserved_cycles then
            a_class ["warning"], s_pending
          else
            a_class ["success"], s_delivered
        in [
          tr ~a:[toggle; target; data_href; color]
            [
              td [ pcdata @@ string_of_int cbk.cbk_cycle ];
              td [ pcdata @@ Int64.to_string cbk.cbk_count.cnt_all ] ;
              td [ pcdata priority_str ] ;
              td [ pcdata @@ Printf.sprintf "%Ld/%Ld" cbk.cbk_count.cnt_miss
                     cbk.cbk_count.cnt_steal];
              td [ Tez.pp_amount rewards ] ;
              td [ Tez.pp_amount cbk.cbk_tez.tez_deposit ] ;
              td [ pcdata bktime_str ] ;
              td [ pcdata_t str_status;
                   a ~a:[ a_class ["caret"; "pull-right"]; toggle; href] [] ] ;
            ];
          tr ~a:[ a_id id_cycle; a_class ["collapse"];
                  a_onshow (fun _ev ->
                      xhr_1cycle ?cycle:(Some cbk.cbk_cycle) hash; true) ] [
            td ~a:[ a_colspan 8 ] [
               BakingsPanel.make ~suf_id ~panel_class:["bakings-div"] () ]]]
      ) cbks in
    let total_rows = match total_bk with
      | [ total_bk ] ->
        let total_rewards =
          Int64.add total_bk.cbk_tez.tez_fee total_bk.cbk_tez.tez_reward in
        let priority_str = Misc.unoptf Common.bullshit_s
            (Printf.sprintf "%.2f") total_bk.cbk_priority in
        let bktime_str = Misc.unoptf Common.bullshit_s
            (Printf.sprintf "%ds") total_bk.cbk_bktime in
        [ tr ~a:[ a_class ["info"] ] [
              td [ pcdata_t s_total ];
              td [ pcdata @@  Printf.sprintf "%Ld" total_bk.cbk_count.cnt_all ] ;
              td [ pcdata priority_str ] ;
              td [ pcdata @@ Printf.sprintf "%Ld/%Ld" total_bk.cbk_count.cnt_miss
                     total_bk.cbk_count.cnt_steal];
              td [ Tez.pp_amount total_rewards ] ;
              td [ Tez.pp_amount total_bk.cbk_tez.tez_deposit ] ;
              td [ pcdata bktime_str ] ;
              td [ pcdata_ () ]
            ]]
      | _ -> [] in
    future_rows @ total_rows @ passed_rows


module BakingsEndorsementPanel = Panel.MakePageTable(
  struct
    let title_span _nb = span [pcdata_t s_cycle_detail]
    let page_size = 10
    let table_class = "bakings-table"
    let name = "endorsements"
    let theads () =
      tr [
         th @@ cl_icon cube_icon (t_ s_level);
         th @@ cl_icon slots_icon (t_ s_slots );
         th @@ cl_icon priority_icon (t_ s_priority);
         th @@ cl_icon bill_icon (t_ s_rewards);
         th @@ cl_icon deposit_icon (t_ s_deposits);
         th @@ cl_icon spinner_icon (t_ s_status)
       ]
  end)

let make_account_bakings_endorsement ebks =
  match ebks with
  | [] -> [ tr [ td ~a:[ a_colspan 6 ] [
      pcdata_t s_no_block_endorsed_for_this_cycle ]]]
  | _ ->
    List.map (function
        | {ebk_block = Some ebk_block; ebk_level; ebk_cycle = Some ebk_cycle;
           ebk_priority = Some ebk_priority; ebk_dist = Some ebk_dist;
           ebk_slots = Some ebk_slots; _} ->
          let cycle = ebk_cycle in
          let cst = Infos.constants ~cycle in
          let rewards = Tez.pp_amount @@
            if ebk_cycle > 6 then
              Int64.(div64 (mul cst.endorsement_reward  (of_int @@ List.length ebk_slots))
                            (of_int ( 1 + ebk_priority )))
            else 0L in
          let security_deposit = Infos.rampup ~cycle cst.endorsement_security_deposit in
          let deposits = Tez.pp_amount @@
            Int64.(mul security_deposit (of_int @@ List.length ebk_slots)) in
          let str_slots = String.concat ", " @@
            List.rev @@ List.map string_of_int ebk_slots in
          let str_level = string_of_int ebk_level in
          let div_level, div_rewards, div_deposits, div_status, tr_class =
            if ebk_dist = 0 then
              Common.make_link str_level, rewards, deposits, pcdata_t s_good, []
            else
              Common.make_link str_level ~path:ebk_block, pcdata_ (),
              pcdata_ (), pcdata_t s_alt, [ a_class ["bg-block-alt-chain"] ]
          in
          tr ~a:tr_class [
            td [ div_level ] ;
            td [ pcdata str_slots ] ;
            td [ pcdata @@ string_of_int ebk_priority ] ;
            td [ div_rewards ] ;
            td [ div_deposits ] ;
            td [ div_status ]]
        | {ebk_level; ebk_lr_nslot; _ } ->
          tr ~a:[ a_class ["danger"] ] [
            td [ Common.make_link @@ string_of_int ebk_level ] ;
            td [ span ~a:[ a_class [red] ] [cross_icon (); space_icon ()];
                 pcdata @@ string_of_int ebk_lr_nslot ] ;
            td [ Common.pcdata_ () ] ;
            td [ Tez.pp_amount 0L ] ;
            td [ Tez.pp_amount 0L ] ;
            td [ pcdata_t s_missed ] ]
      ) ebks

module EndorsementRightsPanel = Panel.MakePageTable(
  struct
    let title_span nb = Panel.title_nb s_endorsement_rights nb
    let table_class = "bakings-table"
    let page_size = 5
    let name = "pending_endorse_priorities"
    let theads () =
      tr [
        th @@ cl_icon cube_icon (t_ s_level);
        th @@ cl_icon slots_icon (t_ s_slots);
        th @@ cl_icon bill_icon (t_ s_rewards);
        th @@ cl_icon deposit_icon (t_ s_deposits);
        th @@ cl_icon clock_icon (t_ s_eta)
      ]
  end)

let make_account_endorsement_rights prs =
  match prs with
  | [] -> [ tr [ td ~a:[ a_colspan 5 ] [
      pcdata_t s_no_endorsement_rights_for_this_cycle ]]]
  | _ ->
    List.map (fun pr ->
        let cycle = pr.er_cycle in
        let cst = Infos.constants ~cycle in
        let security_deposit =
          Int64.(mul (of_int pr.er_nslot)
                   (Infos.rampup ~cycle cst.endorsement_security_deposit)) in
        let rewards =
            Int64.(mul (of_int pr.er_nslot) cst.endorsement_reward) in
        tr [
          td [ pcdata @@ string_of_int pr.er_level ] ;
          td [ pcdata @@ string_of_int pr.er_nslot ] ;
          td [ Tez.pp_amount rewards ] ;
          td [ Tez.pp_amount security_deposit ] ;
          td [ pcdata @@ Format_date.time_before_level ~cst pr.er_depth ] ;
        ]) prs

module CycleEndorsementsPanel = Panel.MakePageTable(
  struct
    let name = "cycle-endorsements"
    let table_class = "bakings-table"
    let title_span _nb = span [ pcdata_t s_endorsement_history ]
    let page_size = 15
    let theads () =
      tr [
        th @@ cl_icon cycle_icon (t_ s_cycle);
        th @@ cl_icon (number_icon slots_icon) (t_ s_nbslots_miss);
        th @@ cl_icon priority_icon (t_ s_av_priority);
        th @@ cl_icon bill_icon (t_ s_tot_rewards);
        th @@ cl_icon deposit_icon (t_ s_tot_deposits);
        th @@ cl_icon spinner_icon (t_ s_rew_status);
       ]
  end)

let make_endorsement_history_table xhr_rights xhr_1cycle hash (total_ed, ceds_r, ceds) =
  match ceds, ceds_r with
  | [], [] -> [ tr [ td ~a:[ a_colspan 6 ] [
      pcdata_t s_no_block_endorsed_and_no_endorsement_rights
    ]]]
  | _ ->
    let future_rows = if ceds_r = [] then [] else
        List.flatten @@ List.map (fun ced_r ->
            let cycle = ced_r.cr_cycle in
            let cst = Infos.constants ~cycle in
            let id = "endorsements-future" in
            let suf_id = Printf.sprintf "-%d" ced_r.cr_cycle in
            let id_cycle = id ^ suf_id in
            let target =  a_user_data "target" ("#" ^ id_cycle) in
            let data_href = a_user_data "href" ("#" ^ id_cycle) in
            let href =  a_href ("#" ^ id_cycle) in
            let security_deposit = Infos.rampup ~cycle
                cst.endorsement_security_deposit in
            let priority = Int64.of_float ced_r.cr_priority in
            let deposit = Int64.mul priority security_deposit in
            let rewards = Int64.mul priority cst.endorsement_reward in
            [ tr ~a:[toggle; target; data_href] [
                  td [ pcdata (string_of_int ced_r.cr_cycle) ];
                  td [ pcdata (string_of_int @@ int_of_float ced_r.cr_priority) ];
                  td [ pcdata_ () ];
                  td [ Tez.pp_amount rewards ];
                  td [ Tez.pp_amount deposit ];
                  td [ Common.pcdata_ ();
                       a ~a:[ a_class ["caret"; "pull-right"]; toggle; href] []]; ];
              tr ~a:[ a_id id_cycle; a_class ["collapse"];
                      a_onshow (fun _ev ->
                          xhr_rights ?cycle:(Some ced_r.cr_cycle) hash; true) ] [
                td ~a:[ a_colspan 6 ] [
                  EndorsementRightsPanel.make ~suf_id ~panel_class:["bakings-div"] () ]]]
          ) ceds_r in
    let passed_rows = List.flatten @@ List.map (fun ced ->
        let cycle = ced.ced_cycle in
        let cst = Infos.constants ~cycle in
        let id = "endorsements" in
        let suf_id = Printf.sprintf "-%d" ced.ced_cycle in
        let id_cycle = id ^ suf_id in
        let target =  a_user_data "target" ("#" ^ id_cycle) in
        let data_href = a_user_data "href" ("#" ^ id_cycle) in
        let href =  a_href ("#" ^ id_cycle) in
        let rewards = ced.ced_tez.tez_reward in
        let deposit = ced.ced_tez.tez_deposit in
        let priority_str = Printf.sprintf "%.2f" ced.ced_priority in
        let color, str_status =
          if ced.ced_depth <= cst.preserved_cycles then
            a_class ["warning"], s_pending
          else
            a_class ["success"], s_delivered
        in
        [ tr ~a:[toggle; target; color; data_href] [
              td [ pcdata @@ string_of_int ced.ced_cycle ] ;
              td [ pcdata @@ Printf.sprintf "%Ld (%Ld)" ced.ced_slots.cnt_all
                     ced.ced_slots.cnt_miss ] ;
              td [ pcdata priority_str ] ;
              td [ Tez.pp_amount rewards ] ;
              td [ Tez.pp_amount deposit ] ;
              td [ pcdata_t str_status;
                   a ~a:[ a_class ["caret"; "pull-right"]; toggle; href] [] ]; ];
          tr ~a:[ a_id id_cycle; a_class ["collapse"];
                  a_onshow (fun _ev ->
                      xhr_1cycle ?cycle:(Some ced.ced_cycle) hash; true) ] [
            td ~a:[ a_colspan 6 ] [
              EndorsementsPanel.make ~suf_id ~panel_class:["bakings-div"] () ]]]
      ) ceds in
    let total_rows = match total_ed with
      | [ total_ed ] ->
         let priority_str = Printf.sprintf "%.2f" total_ed.ced_priority in
        [ tr ~a: [ a_class ["info"] ] [
              td [ pcdata_t s_total ] ;
              td [ pcdata @@ Int64.to_string total_ed.ced_slots.cnt_all ] ;
              td [ pcdata priority_str ] ;
              td [ Tez.pp_amount total_ed.ced_tez.tez_reward ] ;
              td [ Tez.pp_amount total_ed.ced_tez.tez_deposit ] ;
              td [ pcdata_ () ]
            ] ]
      | _ -> [] in
    future_rows @ total_rows @ passed_rows

(* Rewards Panels *)
module DelegatorsPanel = Panel.MakePageTable(
  struct
    let title_span _nb = span [pcdata_t s_at_snapshot_time ]
    let name = "rewards"
    let theads () =
      tr [
        th @@ cl_icon account_icon (t_ s_delegator);
        th @@ cl_icon Tez.icon (t_ s_delegator_balance);
        th @@ cl_icon chart_pie_icon (t_ s_share);
        th ~a:[ a_class ["extra-infos"];
                a_title (t_ s_doesnot_include_services_fee) ] @@
        cl_icon (fun () -> span [chart_pie_icon (); space_icon (); bill_icon ()])
          (t_ s_rewards_share)
      ]
    let page_size = 10
    let table_class = "bakings-table"
  end)

let make_account_rewards sbalance rewards rs =
  match rs with
  | [] -> [ tr [ td ~a:[ a_colspan 5 ] [
      pcdata_t s_no_delegators_for_this_cycle ]]]
  | _ ->
    List.map (fun (del, del_balance) ->
        let del_balance_f = Int64.to_float del_balance in
        let staking_balance_f = Int64.to_float sbalance in
        let share = del_balance_f /. staking_balance_f *. 100. in
        let rewards = Int64.to_float rewards in
        let rewards_share = (share /. 100.) *. rewards in
        let percent_value =
          let value = Manip.value @@ find_component "reward-percentage-input" in
          if value = "" then 100. else float_of_string value in
        let rewards_share_updated = rewards_share *. percent_value /. 100. in
        tr [
          Common.account_w_blockies del ;
          td [ Tez.pp_amount ~precision:2 del_balance ] ;
          td [ pcdata @@ Printf.sprintf "%.2f%%" share ] ;
          td
            ~a:[ a_class [ "extra-infos" ] ;
                 a_title (t_ s_doesnot_include_services_fee);
                 a_user_data "value" (string_of_float rewards_share);
                 a_class ["reward-share-td"] ]
            [ Tez.pp_amount_float ~width:6 ~precision:2 rewards_share_updated ] ;
        ]) rs

module CycleRewardsPanel = Panel.MakePageTable(
  struct
    let name = "rewards"
    let table_class = "bakings-table"
    let title_span _nb = span [ pcdata_t s_delegate_rewards ]
    let page_size = 20
    let theads () =
      tr [
        th @@ cl_icon cycle_icon (t_ s_cycle);
        th @@ cl_icon (number_icon handshake_icon) (t_ s_nbdel);
        th @@ cl_icon Tez.icon (t_ s_staking_balance);
        th @@
        cl_icon (fun () -> span [cube_icon (); space_icon (); bill_icon ()])
          (t_ s_blocks_rewards);
        th @@
        cl_icon (fun () -> span [stamp_icon (); space_icon (); bill_icon ()])
          (t_ s_end_rewards);
        th @@
        cl_icon (fun () -> span [pcdata "+"; bill_icon ()])
          (t_ s_extra_rewards);
        th @@
        cl_icon (fun () -> span [pcdata "-"; bill_icon ()])
          (t_ s_losses);
        th @@ cl_icon bill_icon (t_ s_fees);
        th @@ cl_icon spinner_icon (t_ s_status);
       ]
  end)

let make_rewards_history_table xhr_1cycle hash rewards_splits =
  match rewards_splits with
  | [] -> [ tr [ td ~a:[ a_colspan 7 ] [
      pcdata_t s_no_data ]]]
  | _ ->
    List.flatten @@ List.map (fun ars ->
        let id = "rewards-split" in
        let suf_id = Printf.sprintf "-%d" ars.ars_cycle in
        let id_cycle = id ^ suf_id in
        let target =  a_user_data "target" ("#" ^ id_cycle) in
        let href =  a_href ("#" ^ id_cycle) in
        let brewards =
          Int64.(add ars.ars_block_rewards ars.ars_baking_rights_rewards) in
        let erewards =
          Int64.add
            ars.ars_endorsement_rewards
            ars.ars_endorsing_rights_rewards in
        let color, str_status, _tooltip_txt = match ars.ars_status with
          | Cycle_in_progress -> a_class ["info"], s_in_progress, None
          | Cycle_pending -> a_class [""], s_pending, Some ""
          | Rewards_pending -> a_class ["warning"], s_rew_pending, Some ""
          | Rewards_delivered -> a_class ["success"], s_rew_delivered, None in
        let losts = List.fold_left Int64.add 0L
            [ars.ars_lost_deposit; ars.ars_lost_rewards; ars.ars_lost_fees ;
             ars.ars_rv_lost_rewards; ars.ars_rv_lost_fees] in
        let extra_rw =
          Int64.add ars.ars_gain_from_denounciation ars.ars_rv_rewards in
        let total_rw =
          Int64.sub
            (List.fold_left Int64.add 0L [brewards; erewards; extra_rw]) losts in
        let td_extra =
          if extra_rw = 0L then td [ Tez.pp_amount 0L ]
          else (
            let extra_title =
              Printf.sprintf "(denounciation = %Ld uxtz) + (revelation = %Ld uxtz)"
                ars.ars_gain_from_denounciation ars.ars_rv_rewards in
            td ~a:[a_title extra_title ] [ Tez.pp_amount ~precision:2 extra_rw ]) in
        let td_lost =
          if losts = 0L then td [Tez.pp_amount 0L]
          else (
            let lost_title = Printf.sprintf
                "(denounciation deposits = %Ld uxtz) + (denounciation rewards = %Ld uxtz) + \
                 (denounciation fees = %Ld uxtz) + (revelation rewards = %Ld uxtz) + \
                 (revelation fees = %Ld uxtz)"
                ars.ars_lost_deposit ars.ars_lost_rewards ars.ars_lost_fees
                ars.ars_rv_lost_rewards ars.ars_rv_lost_fees in
            td ~a:[a_title lost_title ] [ Tez.pp_amount ~precision:2 losts ]
          ) in
        [
          tr ~a:[toggle; target; color] [
            td [ pcdata @@ string_of_int ars.ars_cycle ];
            td [ pcdata @@ string_of_int ars.ars_delegators_nb ];
            td [ Tez.pp_amount ~width:5 ars.ars_delegate_staking_balance ];
            td [ Tez.pp_amount brewards ];
            td [ Tez.pp_amount erewards ];
            td_extra;
            td_lost;
            td [ Tez.pp_amount ~width:5 ~precision:3 ars.ars_fees ];
            td [ pcdata_t str_status;
                 a ~a:[ a_class ["caret"; "pull-right"]; toggle; href] [] ] ;
          ];
          tr ~a:[ a_id id_cycle; a_class ["collapse"];
                  a_onshow (fun _ev ->
                      xhr_1cycle ?cycle:(Some ars.ars_cycle) hash
                        ars.ars_delegate_staking_balance total_rw; true) ] [
            td ~a:[ a_colspan 9 ] [
              DelegatorsPanel.make ~suf_id ~panel_class:["bakings-div"] () ]]]
      ) rewards_splits

module DelegatorPanel = Panel.MakePageTable(
  struct
    let name = "delegator"
    let table_class = "bakings-table"
    let title_span _nb = span [ pcdata_t s_delegator_rewards ]
    let page_size = 20
    let theads () =
      tr [
        th @@ cl_icon cycle_icon (t_ s_cycle);
        th @@ cl_icon astronaut_icon (t_ s_delegate);
        th @@ cl_icon
          (fun () -> span [astronaut_icon (); Tez.icon ()]) (t_ s_staking_balance);
        th @@ cl_icon Tez.icon (t_ s_balance);
        th @@ cl_icon chart_pie_icon (t_ s_share);
        th @@ cl_icon bill_icon (t_ s_rewards);
        th @@ cl_icon (fun () -> span [ pcdata "+"; bill_icon ()]) (t_ s_extras);
        th @@ cl_icon (fun () -> span [ pcdata "-"; bill_icon ()]) (t_ s_losses);
        th @@ cl_icon
          (fun () -> span [chart_pie_icon (); bill_icon ()])(t_ s_rewards_share);
        th @@ cl_icon spinner_icon (t_ s_status);
       ]
  end)

let make_delegator_history_table = function
  | [] -> [ tr [ td ~a:[ a_colspan 10 ] [ pcdata_t s_no_data ]]]
  | dors ->
    List.map (fun dor ->
        let share = Int64.to_float dor.dor_balance /.
                    Int64.to_float dor.dor_staking_balance *. 100. in
        let rewards_share = Int64.to_float dor.dor_rewards *. share /. 100. in
        let percent_value =
          let value = Manip.value @@ find_component "reward-percentage-input" in
          if value = "" then 100. else float_of_string value in
        let rewards_share_updated = rewards_share *. percent_value /. 100. in
        let color, str_status = match dor.dor_status with
          | Cycle_in_progress -> a_class ["info"], s_in_progress
          | Cycle_pending -> a_class [""], s_pending
          | Rewards_pending -> a_class ["warning"], s_rew_pending
          | Rewards_delivered -> a_class ["success"], s_rew_delivered in
        let share_title =
          if  dor.dor_extra_rewards > 0L || dor.dor_losses > 0L then
            s_doesnot_include_extras_nor_losses_nor_services_fee
          else s_doesnot_include_services_fee in
        tr ~a:[ color ] [
          td [pcdata @@ string_of_int dor.dor_cycle ];
          Common.account_w_blockies dor.dor_delegate;
          td [ Tez.pp_amount dor.dor_staking_balance ];
          td [ Tez.pp_amount dor.dor_balance ];
          td [ pcdata @@ Printf.sprintf "%.2f %%" share ];
          td [ Tez.amount dor.dor_rewards ];
          td [ Tez.pp_amount dor.dor_extra_rewards ];
          td [ Tez.pp_amount dor.dor_losses ];
          td ~a:[ a_class [ "extra-infos" ] ;
                  a_title (t_ share_title);
                  a_user_data "value" (string_of_float rewards_share);
                  a_class ["reward-share-td"] ]
            [ Tez.pp_amount_float ~width:6 ~precision:2 rewards_share_updated ] ;
          td [ pcdata_t str_status ] ]
      ) dors

let make_storage title storage_s ?(is_in = false) id =
  let collapse_class =
    if is_in then [ "collapse"; "in" ] else [ "collapse" ] in
  let storage_id = Common.make_id "storage" id in
  let title_id = Common.make_id "storage-title" id in
  let toggle = a_user_data "toggle" "collapse" in
  let storage_code =
    code ~a:[ a_class [ clg12; "storage" ] ] [
      textarea ~a:[ a_readonly (); ]
        (pcdata storage_s)
    ] in
  let title_div =
    a ~a:[ a_class [ ];
           toggle;
           Bootstrap_helpers.Attributes.a_aria "controls" storage_id;
           Bootstrap_helpers.Attributes.a_role "button" ;
           a_href ("#" ^ storage_id) ] [
      span ~a:[ a_class [ ] ] [
        pcdata_t title;
        span ~a:[ a_class [ "caret" ; "text-right" ] ] [ ]
      ]
    ] in
  div ~a:[ a_class [ ] ] [
    div ~a:[ a_class [ "storage-title" ] ;
             a_id title_id ;
             Bootstrap_helpers.Attributes.a_role "tab" ;
           ] [title_div ] ;
    div ~a:[ a_id storage_id ;
             a_class ( "panel-collapse" :: panel :: collapse_class) ;
             Bootstrap_helpers.Attributes.a_role "tabpanel" ;
             Bootstrap_helpers.Attributes.a_aria "labelledby" title_id;
             Bootstrap_helpers.Attributes.a_aria "expanded" "false";
           ] [
      div ~a:[ a_class [ panel_body ] ] [ storage_code ]
    ]
  ]

let make_account_code (code_s, storage_s) =
  let storage_code =
    div ~a:[ a_class [ ] ;
             Bootstrap_helpers.Attributes.a_role "tablist";
             Bootstrap_helpers.Attributes.a_aria "multiselectable" "true" ] [
      make_storage s_michelson_storage storage_s ~is_in:true "michelson-storage";
      make_storage s_liquidity_storage (t_ s_liquidity_storage_coming_soon) "liquidity-storage";
    ] in
  let michelson_code =
    div ~a:[  ] [
      code ~a:[ a_class [ clg12; "account-code" ]]
        [ textarea ~a:[ a_readonly () ]
            (pcdata code_s) ]
    ] in
  let liquidity_code =
    div ~a:[ a_class [ row ] ] [
      code ~a:[ a_class [ clg12; "account-code" ]]
        [ textarea ~a:[ a_readonly () ]
            (pcdata_t s_liquidity_code) ]
    ] in
  michelson_code, liquidity_code, storage_code

let update_account_no_code () =
  (* Technically, this div should never be accessible, but just in case *)
  let inner_div =
    div ~a:[ a_class [] ] [
      div ~a:[ a_class [ row ] ] [
        code ~a:[ a_class [ clg12 ]]
          [ pcdata_t s_no_code ]
      ] ] in
  try
    let container = find_component @@ code_tab.Tabs.content_id in
    Manip.removeChildren container;
    Manip.appendChild container inner_div
  with _ -> ()

let make_account_content ?price_usd node_infos
    details status grace_period revelations activations staking_balance =
  let name = node_infos.acc_name in
  let mk_boolean_link = function
      None -> pcdata_t s_false
    | Some hash -> Common.make_link "true" ~path:hash in
  let originated = mk_boolean_link details.account_status_origination in
  let balance_data = Tez.with_usd price_usd node_infos.acc_balance in
  let cst0 = Infos.constants ~cycle:0 in
  let staking_balance_data, staking_balance_info = match staking_balance with
    | None -> [], div []
    | Some sblce ->
      let nb_rolls =
        Int64.abs @@
        div64 sblce cst0.tokens_per_roll in
      let cycle = cst0.preserved_cycles + 2 in
      let alert_str =
        t_subst s_subst_evaluated_balance (function
            | "cycles" -> string_of_int cycle
            | "rolls" -> Int64.to_string nb_rolls
            | s -> "??" ^ s)
      in
      Tez.with_usd price_usd sblce,
      div ~a:[ a_class [ "alert"; "alert-info"; "evaluated-balance-info" ] ] [
        i [ pcdata alert_str ]
      ]  in
  let balance_div =
    div ~a:[ a_id @@ balance_id name.tz; a_class [ row; "advanced-balance" ] ] [
      h4 ~a:[ a_class [ cxs12 ] ] [
        pcdata_t s_balance ] ;
      h4 ~a:[ a_class [ cxs12 ] ] balance_data ;
      div ~a:[ a_class [ cxs12 ] ] [
        h4 ~a:[ a_class [ "force-text-left" ] ] [
          pcdata_t s_current_deposits ] ];
      div ~a:[ a_class [ cxs6 ] ] [
        pcdata_t s_baking ] ;
      div ~a:[ a_class [ cxs6; text_right ] ] [
        pcdata_ () ] ;
      div ~a:[ a_class [ cxs6 ] ] [
        pcdata_t s_endorsement ] ;
      div ~a:[ a_class [ cxs6; text_right ] ] [
        pcdata_ () ] ;
      div ~a:[ a_class [ cxs12 ] ] [
        h4 ~a:[ a_class [ "force-text-left" ] ]
          [ pcdata_t s_pending_rewards ] ] ;
      div ~a:[ a_class [ cxs6 ] ] [
        pcdata_t s_baking ] ;
      div ~a:[ a_class [ cxs6; text_right ] ] [
        pcdata_ () ] ;
      div ~a:[ a_class [ cxs6 ] ] [
        pcdata_t s_endorsement ] ;
      div ~a:[ a_class [ cxs6; text_right ] ] [
        pcdata_ () ] ;
      div ~a:[ a_class [ cxs12 ] ] [
         h4 [ pcdata_t s_evaluated_balance ; Glossary_doc.(help HBalance) ] ] ;
      div ~a:[ a_class [ cxs12; text_center ] ] [
        pcdata_ () ] ;
    ] in
  let staking_balance_div =
    div ~a:[ a_id @@ staking_balance_id name.tz; a_class [ row; "advanced-balance" ] ] [
      h4 ~a:[ a_class [ clg12; csm6; cxs12 ] ] [
        pcdata_t s_staking_balance; Glossary_doc.(help HSBalance) ] ;
      h4 ~a:[ a_class [ clg12; csm6; cxs12 ] ] staking_balance_data ;
      div ~a:[ a_class [cxs12] ] [
        staking_balance_info ] ;
    ] in
  let current_baking_info_div =
    div ~a:[ a_class [ row; "advanced-baking" ]; a_id "baking-info-div" ] [
      h4 ~a:[ a_class [ clg12; csm6; cxs12 ] ] [
        pcdata_t s_current_cycle ] ;
      div ~a:[ a_id @@ advanced_baking_id name.tz ] [
        div ~a:[ a_class [ csm6; cxs12 ] ] [
          pcdata_t s_tokens_required ] ;
        div ~a:[ a_class [ csm6; cxs12; text_right ] ] [
          pcdata_ () ] ;
        div ~a:[ a_class [ clg6; csm3; cxs6 ] ]
          [ pcdata_t s_grace_period ] ;
        div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [
          pcdata @@ Printf.sprintf "%s %d" (t_ s_cycle) grace_period
        ] ;
      ]
    ] in
  let row_nb_tx_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_txs_number ] in
  let row_nb_tx_value =
    div ~a:[ a_id @@ nb_transactions_id name.tz;
             a_class [ clg6; csm3; cxs6 ] ] [
      pcdata @@ string_of_int Common.bullshit_d ] in
  let row_manager_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_manager ] in
  let row_manager_value =
    div ~a:[ a_class [ clg6; csm3; cxs6; "no-overflow" ] ] [
      Common.make_link_account node_infos.acc_manager
    ] in
  (*  let row_revealed_label =
      div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_revealed ] in
      let row_revealed_value =
      div ~a:[ a_class [ clg6; csm3; cxs6; "no-overflow" ] ] [
        revealed
      ] in*)
  let row_originated_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_originated ] in
  let row_originated_value =
    div ~a:[ a_class [ clg6; csm3; cxs6; "no-overflow" ] ] [
      originated
    ] in
  let row_spendable_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_spendable ] in
  let row_spendable_value =
    div ~a:[ a_class [ clg6; csm3; cxs6; "no-overflow" ] ] [
      pcdata @@ string_of_bool node_infos.acc_spendable
    ] in
  let row_delegatable_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_delegatable ] in
  let row_delegatable_value =
    div ~a:[ a_class [ clg6; csm3; cxs6; "no-overflow" ] ] [
      pcdata @@ string_of_bool (fst node_infos.acc_dlgt)
    ] in
  let row_delegate_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t s_delegate ] in
  let row_delegate_value =
    div ~a:[ a_class [ clg6; csm3; cxs6; "no-overflow" ] ] [
      match (snd node_infos.acc_dlgt) with
      | Some d -> Common.make_link_account d
      | None -> Common.pcdata_ ()
    ] in
  let row_status_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [
      pcdata_t s_delegate_status
    ] in
  let row_status_value =
    let status_color = if status then "text-danger" else "text-success " in
    div ~a:[ a_class [ clg6; csm3; cxs6;  status_color ] ] [
      pcdata_t (if status then s_inactive else s_active) ] in
  let row_counter_label =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [ pcdata_t  s_counter ] in
  let row_counter_value =
    div ~a:[ a_class [ clg6; csm3; cxs6 ] ] [
      pcdata @@ Z.to_string node_infos.acc_counter
    ] in
  let info = Dapps.find_srv name.tz in
  let badge_website =
    match info with
    | Some { srv_sponsored = Some tsp; srv_url; _ } when Common.time_diff tsp > 0. ->
      span ~a:[ a_class [ "badge" ; "badge-primary" ] ] [
        a ~a:[ a_href srv_url; a_target "website" ] [ pcdata_t s_website ] ]
    | _ -> span []
  in
  let badge_revelation =
    let is_reveled =
      match revelations with
      | [] ->  pcdata_t s_not_revealed
      | hd :: _ -> a ~a:[ a_href hd.op_hash ] [ pcdata_t s_revealed ] in
    span ~a:[ a_class [ "badge" ; "badge-primary" ] ] [ is_reveled ] in
  let badge_activation =
    match activations with
    | [] -> span []
    | hd :: _ ->
      span ~a:[ a_class [ "badge" ; "badge-secondary" ] ] [
        a ~a:[ a_href hd.op_hash ] [ pcdata_t s_activation ] ] in
  let logo = match info with
    | None ->
      div ~a:[ a_class [
          cxs12; text_center; "lead"; "account-avatar" ] ] [
        div ~a:[ a_class [ row ] ] [
          div ~a:[ a_class [ cxs12 ] ] [ Base58Blockies.create ~scale:16 name.tz ] ;
          div ~a:[ a_class ["badge-reveal-activate"; cxs12] ]
            [ badge_activation ; badge_revelation ]
        ]
      ]
    | Some del ->
      (
        div ~a:[ a_class [ row ] ] [
          div ~a:[ a_class [
              cxs12; text_center; "lead"; "account-avatar" ] ]
            [ a ~a:[ a_href del.srv_url ]
                [ img ~src:(Common.img_path del.srv_logo)
                    ~alt:(del.srv_name) () ] ;
              div ~a:[ a_class ["badge-reveal-activate"; cxs12] ]
                [  badge_website; badge_activation ; badge_revelation ] ]
        ])
  in
  let display_activation_status =
    match (snd node_infos.acc_dlgt) with
    | None -> false
    | Some d -> d.tz = name.tz in
  [ logo;
    balance_div ] @
  begin match staking_balance with
    | None -> []
    | Some _ -> [ staking_balance_div ] end @
  [ current_baking_info_div ;
    row_nb_tx_label ; row_nb_tx_value ] @
  (if display_activation_status then
     [ row_status_label ; row_status_value ] else []) @
  [ row_manager_label ; row_manager_value ;
    (*row_revealed_label ; row_revealed_value ;*)
    row_originated_label ; row_originated_value ;
    row_spendable_label ; row_spendable_value ;
    row_delegatable_label ; row_delegatable_value ;
    row_delegate_label ; row_delegate_value ;
    row_counter_label ; row_counter_value ;
  ]

let update_account_michelson view =
  try
    let container = find_component michelson_tab.Tabs.content_id in
    Manip.removeChildren container;
    Manip.appendChild container view
  with _ -> ()

let update_account_liquidity view =
  try
    let container = find_component liquidity_tab.Tabs.content_id in
    Manip.removeChildren container;
    Manip.appendChild container view
  with _ -> ()

let update_account_storage view =
  try
    let container = find_component storage_tab.Tabs.content_id in
    Manip.removeChildren container;
    Manip.appendChild container view
  with _ -> ()

let update_baking_info ?grace_period hash balance required =
  let container = find_component @@ advanced_baking_id hash in
  let label_grace_period =
    div ~a:[ a_class [ clg8; csm6; cxs12 ] ] [ pcdata_t s_grace_period ] in
  let value_grace_period =
    match grace_period with
    | None -> span []
    | Some grace_period ->
      div ~a:[ a_class [ clg4; csm6; cxs12; text_right ] ] [
        pcdata @@ Printf.sprintf "%s %d" (t_ s_cycle) grace_period
      ] in
  let label_required =
    div ~a:[ a_class [ cxs12 ] ] [
      pcdata_t s_xtz_required ] in
  let value_required =
    div ~a:[ a_class [ cxs12 ] ] (
      (div ~a:[ a_class [ row ]] [
          div ~a:[ a_class [cxs2] ] [ pcdata_t s_cycle ];
          div ~a:[ a_class [cxs3] ] [ pcdata_t s_xtz ];
          div ~a:[ a_class [cxs3];
                   a_title (t_ s_sum_over_cycles_of)
                 ] [
            pcdata_t s_cumul ];
          div ~a:[ a_class [cxs4] ] [ pcdata_t s_rolls ];
        ]) ::
      List.map (fun (cycle, tez, _back, sum, roll, total) ->
          let class_required = if balance > sum then "required-good"
            else "required-bad" in
          let roll =
            if roll = -1 then Common.bullshit_s
            else Printf.sprintf "%i / %i" roll total in
          div ~a:[ a_class [row]] [
            div ~a:[ a_class [cxs2]] [
              pcdata @@ Printf.sprintf "%d" cycle ];
            div ~a:[ a_class [class_required; cxs3] ] [ Tez.pp_amount ~width:4 tez ];
            div ~a:[ a_class [class_required; cxs3] ] [
              Tez.pp_amount ~width:4 @@ if sum < 0L then 0L else sum
            ];
            div ~a:[ a_class [cxs4] ] [ pcdata roll ]
          ]) required)  in
  Manip.removeChildren container;
  Manip.appendChildren container ([
      label_required ; value_required ] @
      match grace_period with None -> [] | Some _ -> [ label_grace_period ; value_grace_period ])

let update_bonds_rewards ?price_usd hash br balance =
  let container = find_component @@ balance_id hash in
  let balance_data = Tez.with_usd price_usd balance in
  let blocks_deposits = Tez.pp_amount ~width:3 br.acc_b_deposits in
  let blocks_rewards = Tez.pp_amount ~width:3 Int64.(add br.acc_b_rewards br.acc_fees) in
  let endorsements_deposits = Tez.pp_amount ~width:5 br.acc_e_deposits in
  let endorsements_rewards = Tez.pp_amount ~width:5 br.acc_e_rewards in
  let total =
    Int64.(add balance
             (add br.acc_b_rewards
                (add br.acc_e_rewards
                   (add br.acc_b_deposits
                      (add br.acc_fees
                         br.acc_e_deposits))))) in
  let row_balance =
    h4 ~a:[ a_class [ cxs12 ] ] [ pcdata_t s_balance ] in
  let value_balance =
    h4 ~a:[ a_class [ cxs12 ] ] balance_data in
  let label_current_deposit =
    div ~a:[ a_class [ cxs12 ] ] [
      h4 ~a:[ a_class [ "force-text-left" ] ] [ pcdata_t s_current_deposits ] ] in
  let label_bdeposits =
    div ~a:[ a_class [ cxs6 ] ] [ pcdata_t s_baking ] in
  let value_bdeposits =
    div ~a:[ a_class [ cxs6; text_right ] ] [ blocks_deposits ] in
  let label_edeposits =
    div ~a:[ a_class [ cxs6 ] ] [ pcdata_t s_endorsement ] in
  let value_edeposits =
    div ~a:[ a_class [ cxs6; text_right ] ] [ endorsements_deposits ] in
  let label_pending_rewards =
    div ~a:[ a_class [ cxs12 ] ] [
       h4 ~a:[ a_class [ "force-text-left" ] ] [ pcdata_t s_pending_rewards ] ] in
  let label_brewards =
    div ~a:[ a_class [ cxs6 ] ] [ pcdata_t s_baking ] in
  let value_brewards =
    div ~a:[ a_class [ cxs6; text_right ] ] [ blocks_rewards ] in
  let label_erewards =
    div ~a:[ a_class [ cxs6 ] ] [ pcdata_t s_endorsement ] in
  let value_erewards =
    div ~a:[ a_class [ cxs6; text_right ] ] [ endorsements_rewards ] in
  let label_total =
    div ~a:[ a_class [ cxs12 ] ] [
      h4 [ pcdata_t s_evaluated_balance; Glossary_doc.(help HBalance) ] ] in
  let value_total =
    div ~a:[ a_class [ cxs12; "result"; text_center ] ]
      (Tez.with_usd price_usd total)  in
  Manip.removeChildren container;
  Manip.appendChildren container [
    row_balance ;
    value_balance ;
    label_current_deposit ;
    label_bdeposits ;
    value_bdeposits ;
    label_edeposits ;
    value_edeposits ;
    label_pending_rewards ;
    label_brewards ;
    value_brewards ;
    label_erewards ;
    value_erewards ;
    label_total ;
    value_total
  ];
  ignore (Js.Unsafe.eval_string
              "jQuery('[data-toggle=\"popover\"]').popover();")

let update_account_code name code =
  match code with
    None ->
    update_account_no_code ()
  | Some (code, storage) ->
    Tabs.enable code_tab;
    let code_s = Micheline_printer.to_string code in
    let storage_s = Micheline_printer.to_string storage in
    let michelson_view, liquidity_view, storage_view =
      make_account_code (code_s, storage_s) in
    update_account_michelson michelson_view;
    update_account_liquidity liquidity_view;
    update_account_storage storage_view ;
    if Hashtbl.mem Dapps.dapps name.tz then
      Dapps.display dapp_tab.Tabs.content_id name.tz

let update_details ?price_usd
     node_infos details ts status grace_period revelations activations staking_balance =
  let name = node_infos.acc_name in
  let container = find_component @@ details_id name.tz in
  let to_update_heading =
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ row] ] [
        h5 ~a:[ a_class [ cxs10; panel_title; "no-overflow" ] ] [
          pcdata_t s_account_details ] ;
        div ~a: [ a_class [ cxs2; "node-icon" ] ] [
          Node_state_ui.node_state_heading_icon ts
        ]
      ]
    ] in
  let to_update_body =
    div ~a:[ a_class [ panel_body ] ] @@
    (div [ tz1_details name ]) ::
    make_account_content ?price_usd
      node_infos
      details
      status
      grace_period
      revelations
      activations
      staking_balance
  in
  update_account_code name node_infos.acc_script;
  Manip.removeChildren container ;
  Manip.appendChild container to_update_heading ;
  Manip.appendChild container to_update_body;
  Js_utils.Clipboard.set_copy ()

let update_details_404 name =
  let container = find_component @@ details_id name.tz in
  let to_update_heading =
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ row] ] [
        h5 ~a:[ a_class [ cxs10; panel_title; "no-overflow" ] ] [
          pcdata_t s_account_details ] ;
        div ~a: [ a_class [ cxs2; "node-icon" ] ] [
          Node_state_ui.node_state_heading_icon None
        ]
      ]
    ] in
  let to_update_body =
    div ~a:[ a_class [ panel_body ] ] [
      div ~a:[ a_class [ cxs12; text_center; (* "no-overflow" *)  ] ] [
        tz1_details name
      ];
      div ~a:[ a_class [ cxs12;
                         "lead"; "account-avatar"; text_center ] ] [
        Base58Blockies.create ~scale:16 name.tz
      ];
      span ~a: [ a_class [ cxs12 ] ] [
        pcdata_t s_no_information_for_this_account ]
    ] in
  Manip.removeChildren container ;
  Manip.appendChild container to_update_heading ;
  Manip.appendChild container to_update_body;
  ignore @@ Js_utils.Clipboard.set_copy ()

(* let update_timestamp op_hash op_block_hash timestamp =
 *   let ts_span = find_component @@ timestamp_id op_hash op_block_hash in
 *   Manip.removeChildren ts_span;
 *   Manip.appendChild ts_span (Format_date.auto_updating_timespan timestamp)
 *
 * let update_level op level =
 *   let lvl_span = find_component @@ level_id op in
 *   Manip.replaceChildren lvl_span
 *     [ a ~a:[ Common.a_link op.op_block_hash] [ pcdata @@ string_of_int level ]  ] *)

let update_nb_transactions hash total =
  try
    let txs_td = find_component @@ nb_transactions_id hash in
    Manip.setInnerHtml txs_td (string_of_int total)
  with Failure _ -> ()

let update_account_transactions_number hash nb =
  update_nb_transactions hash nb;
  Tabs.update_tab_title tr_tab (Some nb)

let update_account_transactions hash ~nrows xhr =
  update_account_transactions_number hash nrows;
  TransactionsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" (make_account_transactions hash) ~nrows
    (fun page page_size cb ->
       xhr page page_size
         (fun d -> cb d;
           ignore (Js.Unsafe.eval_string
                     "jQuery('[data-toggle=\"popover\"]').popover();")))

let update_account_delegations_number nrows =
  Tabs.update_tab_title del_tab (Some nrows)

let update_account_delegations hash ~nrows xhr =
  DelegationsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" (make_account_delegations hash) ~nrows xhr

let update_account_deleguees hash ~nrows xhr =
  DelegueesPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" (make_account_deleguees hash) ~nrows xhr

let update_account_no_deleguees () =
  DelegueesPanel.paginate_all @@ Array.of_list []

let update_account_originations_delegate hash ~nrows xhr =
  OriginationsDelegatePanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" (make_account_originations hash) ~nrows xhr

let update_account_originations_number nrows =
  Tabs.update_tab_title ori_tab (Some nrows)

let update_account_originations hash ~nrows xhr =
  Tabs.update_tab_title ori_tab (Some nrows);
  OriginationsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" (make_account_originations hash) ~nrows xhr

let update_account_endorsements_number nrows =
  Tabs.update_tab_title endt_tab (Some nrows)

let update_account_endorsements ~nrows xhr =
  Tabs.update_tab_title endt_tab (Some nrows);
  EndorsementsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" make_account_endorsements ~nrows xhr

let update_account_bakings_status active =
  if active then
    Tabs.update_tab_title baking_tab None
  else (
    Tabs.disable baking_tab;
    hide @@ find_component "baking-info-div")

let update_account_bakings ?cycle ~nrows xhr =
  let suf_id = match cycle with
    | None -> ""
    | Some cycle -> Printf.sprintf "-%d" cycle in
  Tabs.update_tab_title baking_tab (Some nrows);
  BakingsPanel.paginate_fun
    ~page_sizer:false ~suf_id
    ~urlarg_page:"" ~urlarg_size:"" make_account_bakings ~nrows xhr

let update_account_rewards ?cycle sbalance rewards ~nrows xhr =
  let suf_id = match cycle with
    | None -> ""
    | Some cycle -> Printf.sprintf "-%d" cycle in
  DelegatorsPanel.paginate_fun
    ~page_sizer:false ~suf_id
    ~urlarg_page:"" ~urlarg_size:"" (make_account_rewards sbalance rewards) ~nrows xhr

let update_account_bakings_endorsement ?cycle ~nrows xhr =
  let suf_id = match cycle with
    | None -> ""
    | Some cycle -> Printf.sprintf "-%d" cycle in
  BakingsEndorsementPanel.paginate_fun
    ~page_sizer:false ~suf_id
    ~urlarg_page:"" ~urlarg_size:"" make_account_bakings_endorsement ~nrows xhr

let update_baking_history ~nrows xhr xhr_rights xhr_1cycle hash =
  CycleBakingsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:""
    (make_baking_history_table xhr_rights xhr_1cycle hash)
    ~nrows xhr

let update_endorsement_history ~nrows xhr xhr_rights xhr_1cycle hash =
  CycleEndorsementsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:""
    (make_endorsement_history_table xhr_rights xhr_1cycle hash)
    ~nrows xhr

let update_rewards_history ~nrows xhr xhr_1cycle hash =
  if nrows = 0 then Tabs.disable rewards_tab;
  hide @@ find_component "delegator-reward-container";
  CycleRewardsPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:""
    (make_rewards_history_table xhr_1cycle hash)
    ~nrows xhr

let update_delegator_rewards_history ~nrows xhr =
  hide @@ find_component "cycle-reward-container";
  DelegatorPanel.paginate_fun
    ~urlarg_page:"" ~urlarg_size:"" make_delegator_history_table
    ~nrows xhr

let update_account_baking_rights ?cycle ~nrows xhr =
  let suf_id = match cycle with
    | None -> ""
    | Some cycle -> Printf.sprintf "-%d" cycle in
  BakingRightsPanel.paginate_fun
    ~page_sizer:false ~suf_id
    ~urlarg_page:"" ~urlarg_size:"" make_account_baking_rights ~nrows xhr

let update_account_endorsement_rights ?cycle ~nrows xhr =
  let suf_id = match cycle with
    | None -> ""
    | Some cycle -> Printf.sprintf "-%d" cycle in
  EndorsementRightsPanel.paginate_fun
    ~page_sizer:false ~suf_id
    ~urlarg_page:"" ~urlarg_size:"" make_account_endorsement_rights ~nrows xhr

let make_account_code_tabs hash =
  Tabs.(make_tabs ~fills:true ~_class:[ clg9; "code-tabs" ] Pills ([
      michelson_tab, Active;
      liquidity_tab, Inactive;
      storage_tab, Inactive;
    ] @ (if Dapps.is_dapp hash then [ dapp_tab, Inactive ] else [] )))

let make_michelson () =
  Tabs.(make_content_panel ~_class:[ "code-panel" ] michelson_tab Active @@
        div ~a:[ a_class [ ] ] [  Common.make_fetching () ])

let make_liquidity () =
  Tabs.(make_content_panel ~_class:[ "code-panel" ] liquidity_tab Inactive @@
        div ~a:[ a_class [ ] ] [  Common.make_fetching () ])

let make_storage () =
  Tabs.(make_content_panel ~_class:[ "code-panel" ] storage_tab Inactive @@
        div ~a:[ a_class [] ] [  Common.make_fetching () ])

let make_dapp () =
  Tabs.(make_content_panel ~_class:[ "code-panel" ] dapp_tab Inactive @@
        div ~a:[ a_class [] ] [  Common.make_fetching () ])

let make_code_panel hash =
  div ~a:[ a_class [ panel; panel_primary; "account-div"; row ] ] [
    div ~a:[ a_class [ clg12 ] ] [
      div ~a:[ a_class [ panel_heading] ] [
        div ~a: [ a_class [ row ] ] [
          make_account_code_tabs hash;
          div ~a:[ a_class [ clg3 ] ] [
            a ~a:[ a_class [ "btn"; "btn-primary"  ];
                   a_href @@ Printf.sprintf
                     "http://www.liquidity-lang.org/edit?contract=%s" hash;
                   Bootstrap_helpers.Attributes.a_role "button";
                   a_target "_blank" ] [ pcdata_t s_open_in_try_liquidity ] ] ]
      ] ;
      div ~a:[ a_class [ "code-main-panel"; "tab-content" ] ] ([
        make_michelson ();

        make_liquidity ();

        make_storage () ;

      ] @ (if Dapps.is_dapp hash then [make_dapp ()] else []))

    ]
  ]

let make_transaction_view default =
  Tabs.make_content_panel tr_tab (is_active default Def_Txs) @@
  TransactionsPanel.make ~footer:true ()

let make_delegations_view default =
  Tabs.make_content_panel del_tab (is_active default Def_Del) @@
    div [
      DelegueesPanel.make ();
      DelegationsPanel.make ~footer:true ();
      OriginationsDelegatePanel.make ()
    ]

let make_originations_view default =
  Tabs.make_content_panel ori_tab (is_active default Def_Ori) @@
  OriginationsPanel.make ~footer:true ()

let make_endorsements_view default =
  Tabs.make_content_panel endt_tab (is_active default Def_Endt) @@
  EndorsementsPanel.make ~footer:true ()

let make_bakings_view hash default =
  Tabs.make_content_panel baking_tab (is_active default Def_Bk) @@
  div  [
    Baking_ui.make_page hash ;
    CycleBakingsPanel.make ~panel_class:[row] () ;
    CycleEndorsementsPanel.make ~panel_class:[row] ()
  ]

let make_rewards_view _hash default =
  let reward_input =
    Tyxml_js.Html5.input ~a:[ a_class ["form-control"; cxs2]; a_id "reward-percentage-input";
               a_input_type `Text; a_placeholder "100"] () in
  let update () =
    Common.update_by_class "reward-share-td"
      (fun s -> [ Tez.pp_amount_float ~width:6 ~precision:2 @@
                  (float_of_string s) *.
                  (float_of_string @@ Manip.value reward_input) /. 100. ]);
    true in
  Manip.Ev.onkeydown reward_input (fun e ->
      if e##keyCode = 13 then update () else true);
  Tabs.make_content_panel rewards_tab (is_active default Def_Rew) @@
  div  [
    div ~a:[ a_class [cxs12] ] [
      div ~a:[ a_class ["form-group"; "pull-right"]; a_id "reward-form" ] [
        span ~a:[ a_class [text_right; cxs6] ] [ pcdata "Reward Percentage:"];
        reward_input;
        span ~a:[ a_class [cxs1]; a_id "reward-percent-text" ] [pcdata "%"];
        button ~a:[
          a_class [btn; btn_default; cxs3];
          a_onclick (fun _e -> update ())] [ pcdata "Update"]
      ]];
    div ~a:[ a_id "cycle-reward-container" ] [
      CycleRewardsPanel.make ~footer:true () ];
    div ~a:[ a_id "delegator-reward-container" ] [
      DelegatorPanel.make ~footer:true () ]
  ]

let make_code_view hash =
  Tabs.make_content_panel code_tab Tabs.Disabled @@ make_code_panel hash

let make_tabs default =
  Tabs.(make_tabs Tabs [
      tr_tab, (is_active default Def_Txs);
      del_tab, (is_active default Def_Del);
      ori_tab, (is_active default Def_Ori);
      endt_tab, (is_active default Def_Endt);
      baking_tab, (is_active default Def_Bk);
      rewards_tab, (is_active default Def_Rew);
      code_tab, Disabled])


let update_on_show default filter shown_once tab hash update : unit =
  (* In case of auto-update of the page, sets it as "never shown yet" *)
  shown_once := false;
  if default then (shown_once := true; update hash);
  Tabs.set_on_show tab
    (fun _ ->
       Common.set_url_arg "default" ~default:(string_of_filter Def_Txs)
       @@ string_of_filter filter;
       if not !shown_once then
         begin
           shown_once := true;
           update hash
         end)

let update_transactions default hash update =
  let default = default = Def_Txs in
  update_on_show
    default Def_Txs transactions_view_shown_once tr_tab hash update

let update_delegations default hash update =
  let default = default = Def_Del in
  update_on_show
    default Def_Del delegations_view_shown_once del_tab hash update

let update_originations default hash update =
  let default = default = Def_Ori in
  update_on_show
    default Def_Ori originations_view_shown_once ori_tab hash update

let update_endorsements default hash update =
  let default = default = Def_Endt in
  update_on_show
    default Def_Endt endorsements_view_shown_once endt_tab hash update

let update_bakings default hash update =
  let default = default = Def_Bk in
  update_on_show
    default Def_Bk bakings_view_shown_once baking_tab hash update

let update_rewards default hash update =
  let default = default = Def_Rew in
  update_on_show
    default Def_Rew rewards_view_shown_once rewards_tab hash update

let amcharts3_ready = Amcharts3.ready "/amcharts3"

let pie_chart_id = "rolls-pie-id"
let pie_chart_div = div ~a:[ a_id pie_chart_id; a_class [ clg12 ] ] []

let additional_divs = ref
    ([] : (string -> Html_types.div_content_fun Tyxml_js.Html5.elt)
       list )

let make_page hash filters =
  let details =
    div ~a:[ a_id @@ details_id hash; a_class [ panel; panel_primary ] ] [
      div ~a:[ a_class [ panel_heading ] ] [
        div ~a:[ a_class [ row] ] [
          h5 ~a:[ a_class [ cxs9; panel_title; "no-overflow" ] ] [
            pcdata_t s_account_details ] ;
          Common.make_loading_gif [ "loading-heading"; clg3]
        ]
      ] ;
      div ~a:[ a_class [ panel_body ] ] [ Common.make_fetching () ]
    ] in

  let default = default_filter filters in

  let transactions = make_transaction_view default in
  let delegations = make_delegations_view default in
  let originations = make_originations_view default in
  let endorsements = make_endorsements_view default in
  let bakings = make_bakings_view hash default in
  let rewards = make_rewards_view hash default in
  let code = make_code_view hash in

  div ~a:[ a_class [ "account-div"; row ] ] [
    div ~a:[ a_class [ "mobile-reverser" ] ] [
      div ~a:[ a_class [ clg9; cxs12 ] ] [
        make_tabs default;
        div ~a:[ a_class [ "tab-content" ] ] [
          transactions;

          delegations;

          originations;

          endorsements;

          bakings;

          code;

          rewards
        ]
      ];
      div ~a:[ a_class [ clg3; cxs12 ] ]
        ([
          div ~a:[a_id "button-bookmark"; a_class ["btn"; "btn-default"];
                  a_style "display:none"] [];
          div ~a:[a_id "alert-bookmark"; a_style "block"] [];

          (* Account Details *)
          details ;
        ] @
          (List.map (fun make_div -> make_div hash) !additional_divs)
        )
    ]
  ]
