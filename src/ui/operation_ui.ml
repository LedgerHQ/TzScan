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

open Data_types
open Tyxml_js.Html5
open Js_utils
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Bootstrap_helpers.Table
open Bootstrap_helpers.Color
open Lang
open Tezos_types
open Text

module TransactionsPanel = Panel.MakePageTable(
  struct
    let name = "sub-transactions"
    let theads =
      Panel.theads_of_strings
        [
          s_source, 3;
          s_empty, 1;
          s_destination, 3;
          s_amount, 2;
          s_fees, 2;
        ]
    let title_span =
      Panel.title_nb s_transactions ~help:Glossary_doc.HTransaction
    let table_class = "transactions-table"
    let page_size = 20
  end)

type filter = Op_Txs | Op_Del | Op_Ori | Op_Rvl

let string_of_filter = function
    Op_Txs -> "transaction"
  | Op_Del -> "delegation"
  | Op_Ori -> "origination"
  | Op_Rvl -> "reveal"

let filter_of_string = function
    "transaction" -> Op_Txs
  | "delegation" -> Op_Del
  | "origination" -> Op_Ori
  | "reveal" -> Op_Rvl
  | _ -> Op_Txs

let default_filter filters =
  match Misc.list_assoc_opt "default" filters with
  | Some filter -> filter_of_string filter
  | _ -> Op_Txs

let is_active default exp =
  if default = exp then Tabs.Active else Tabs.Inactive


let mk_title icon title nb =
  span ( Common.responsive_title_fun icon
           (Panel.title_nb title)
           (match nb with Some nb -> nb | None -> -1)
       )

let tr_tab =
  Tabs.make
    (mk_title Bootstrap_helpers.Icon.exchange_icon s_transactions) [ "account-tab" ]

let del_tab =
  Tabs.make
    (mk_title Bootstrap_helpers.Icon.handshake_icon s_delegations) [ "account-tab" ]

let ori_tab =
  Tabs.make
    (mk_title Bootstrap_helpers.Icon.link_icon s_originations) [ "account-tab" ]

let rvl_tab =
  Tabs.make
    (mk_title Bootstrap_helpers.Icon.check_icon s_reveals) [ "account-tab" ]

let tabs = [
  tr_tab, Op_Txs;
  del_tab, Op_Del;
  ori_tab, Op_Ori;
  rvl_tab, Op_Rvl
]

let make_op_tabs default =
  List.map (fun (tr, filter) ->
      tr, is_active default filter) tabs |>
  Tabs.(make_tabs Tabs)

let on_show filter tab =
  Tabs.set_on_show tab (fun _ ->
      Common.set_url_arg "default" ~default:(string_of_filter Op_Txs)
      @@ string_of_filter filter
    )

let update_tabs () =
  List.iter (fun (t, filter) ->
      on_show filter t
    ) tabs

let operation_id hash = Common.make_id "operation-summary" hash
let timestamp_id hash = Common.make_id "operation-timestamp" hash
let slot i = Common.make_id "slot" (string_of_int i)

let make_sub_operation_div title content =
  div ~a:[ a_class [ "sub-operation-div"; row ] ] [
    div ~a:[ a_class [ cxs12 ] ] [
      div ~a:[ a_class [ panel; panel_primary ]] [
        div ~a:[ a_class [ panel_heading ] ] [
          h3 ~a:[ a_class [ panel_title ] ] [ pcdata title ]
        ] ;
        div ~a:[ a_class [ panel_body ] ] content
      ] ;
    ]
  ]

let label list = div ~a:[ a_class [ clg2; cxs12; "lbl" ] ] list
let label_t s = label [ pcdata_t s ]
let value ?(classes=[]) ?id list =
    let a = [ a_class ([ clg10; cxs12 ; "value" ] @ classes ) ] in
    let a = match id with
        None -> a
      | Some id -> a_id id :: a
    in
    div ~a list
let value_pcdata ?classes ?id s = value ?classes ?id [ pcdata s ]
let value_s ?classes ?id s = value ?classes ?id [ pcdata_t s ]


let make_seed_nonce_revelation_details index seed =

  let row_level_label = label_t s_level in
  let row_level_value = value_pcdata @@ string_of_int seed.seed_level in

  let row_nonce_label = label_t s_nonce in
  let row_nonce_value = value_pcdata seed.seed_nonce  in

  let title = Printf.sprintf "%i: %s" index (t_ s_seed_nonce_revelation) in
  let content =
    [ row_level_label ; row_level_value ;
      row_nonce_label ; row_nonce_value ] in
  make_sub_operation_div title content

let make_activation_details index act =

  let row_tz1_label = label_t s_tz1 in
  let row_tz1_value = value [ Common.make_link_account act.act_pkh ] in

  let row_secret_label = label_t s_secret in
  let row_secret_value = value_pcdata act.act_secret in

  let title = Printf.sprintf "%i: %s" index (t_ s_activation) in
  let content =
    [ row_tz1_label ; row_tz1_value ;
      row_secret_label ; row_secret_value ] in
  make_sub_operation_div title content

let make_endorsement_details ?endorse_info endorsement =
  let ops, lvl =
    match endorse_info with
    | None -> assert false
    | Some (ops, lvl) -> (ops,lvl)
  in
  let cycle = lvl.lvl_cycle in
  let cst = Infos.constants ~cycle in
  let row_src_label = label_t s_endorser in
  let row_src_value = value
      [ Common.make_link_account endorsement.endorse_src ] in

  let row_hash_label = label_t s_endorsed_block in
  let row_hash_value = value
      [ Common.make_link endorsement.endorse_block_hash ] in

  let row_blevel_label = label_t s_endorsed_level in
  let row_blevel_value = value
    [ Common.make_link
        ~path:(string_of_int endorsement.endorse_block_level)
        (string_of_int endorsement.endorse_block_level) ] in

  let slots = Common.make_slots cst.endorsers_per_block in
  let slots = Common.update_endorsements_slots endorsement.endorse_block_hash slots ops in
  List.iter (fun s ->
      Js_utils.Manip.addClass (List.nth slots s) "slot-greener") endorsement.endorse_slot  ;
  let rows = Common.make_endorsements_slots slots in
  [ row_src_label ; row_src_value ;
    row_hash_label ; row_hash_value ;
    row_blevel_label ; row_blevel_value ;
    div ~a:[ a_class [ cxs6; cxsoffset2; "endorse-tbl" ] ] [ rows ] ;
    div ~a:[ a_class [ cxs8; cxsoffset2; "lbl" ] ] [ Common.legend () ] ]

let make_proposals_details src prop =

  let row_src_label = label_t s_source in
  let row_src_value = value [ Common.make_link_account src ] in

  let row_voting_label = label_t s_voting_period in
  let row_voting_value = value_pcdata
    @@ string_of_int @@ Int32.to_int prop.prop_voting_period  in

  [ div ~a:[ a_class [ "operation-div"; row ] ] [
        div ~a:[ a_class [ cxs12 ] ] [
          div ~a:[  a_class [ panel; panel_primary ]] [
            div ~a:[ a_class [ panel_body ] ]
              [ row_src_label ; row_src_value ;
                row_voting_label ; row_voting_value ]
          ]
        ]
      ]
  ]

let make_proposals_details_table prop =
  let trs = List.map (fun prop_hash ->
      tr [
        td [ pcdata prop_hash ];
      ])
      prop.prop_proposals in
  tablex ~a:[ a_class [ btable; "transactions-table" ] ] [ tbody (List.rev trs) ]

let make_ballot_details src ballot =

  let row_src_label = label_t s_source in
  let row_src_value = value [ Common.make_link_account src ] in

  let row_voting_label = label_t s_voting_period in
  let row_voting_value = value_pcdata
    @@ Int32.to_string ballot.ballot_voting_period  in

  let row_hash_label = label_t s_protocol_hash  in
  let row_hash_value = value_pcdata ballot.ballot_proposal in

  let row_vote_label = label_t s_vote in
  let row_vote_value = value_pcdata
    @@ Tezos_utils.string_of_ballot_vote ballot.ballot_vote in

  [ div ~a:[ a_class [ "operation-div"; row ] ] [
        div ~a:[ a_class [ cxs12 ] ] [
          div ~a:[  a_class [ panel; panel_primary ]] [
            div ~a:[ a_class [ panel_body ] ]
              [ row_src_label ; row_src_value ;
                row_voting_label ; row_voting_value;
                row_hash_label ; row_hash_value;
                row_vote_label ; row_vote_value ]
          ]
        ]
      ]
  ]

let triplet ?(mc=42) ?fals lbl v1 v2 =
  let fals = match fals with
    | None -> v1 <> v2
    | Some fals -> fals in
  let cls1, cls2 =
    if mc = 0 && fals then ["success"], ["danger"]
    else if mc = 1 && fals then ["danger"], ["success"]
    else if mc = -1 && fals then ["danger"], ["danger"]
    else [],[] in
  if v1 = "" && v2 = "" then [] else
    [div ~a:[ a_class [csm2; clg2; cxs12; "lbl" ] ] [ pcdata_t lbl ];
     div ~a:[ a_class (cls1 @ [clg4; cxs12; "value"]) ] [ pcdata v1 ];
     div ~a:[ a_class ([clg4; cxs12; cxsoffset0; csmoffset1; "value" ] @ cls2) ] [ pcdata v2 ]]

let triplet_int ?(mc=42) lbl i1 i2 =
  triplet ~mc ?fals:(Some (i1<>i2)) lbl (string_of_int i1) (string_of_int i2)

let make_double_baking_evidence_details index dbe =
  let level = string_of_int dbe.double_baking_header1.header_level in
  let link_str = "heads?level=" ^ level in
  let link = Common.make_link level ~path:link_str in
  let row_dbe_label = label_t s_double_baking_at_level in
  let row_dbe_value = value [ link ] in
  let row_dbe_accused_label = label_t s_offender in
  let row_dbe_accused_value = value [
      Common.make_link_account dbe.double_baking_accused
    ] in
  let row_dbe_denouncer_label = label_t s_baker  in
  let row_dbe_denouncer_value = value [
      Common.make_link_account dbe.double_baking_denouncer
    ] in
  let row_dbe_gain_label = label_t s_baker_rewards  in
  let row_dbe_gain_value = value [
      Tez.pp_amount ~precision:2 dbe.double_baking_gain_rewards
    ] in
  let row_dbe_lost_deposit_label = label_t s_lost_deposit in
  let row_dbe_lost_deposit_value = value [
      Tez.pp_amount ~precision:2 dbe.double_baking_lost_deposit
    ] in
  let row_dbe_lost_rewards_label = label_t s_lost_rewards in
  let row_dbe_lost_rewards_value = value [
      Tez.pp_amount ~precision:2 dbe.double_baking_lost_rewards
    ] in
  let row_dbe_lost_fees_label = label_t s_lost_fees in
  let row_dbe_lost_fees_value = value [
      Tez.pp_amount ~precision:2 dbe.double_baking_lost_fees
    ] in
  let h1 = dbe.double_baking_header1 and h2 = dbe.double_baking_header2 in
  let mc = dbe.double_baking_main in
  let content_diff = List.flatten
      [ triplet_int ~mc s_protocol h1.header_proto h2.header_proto;
        triplet ~mc s_predecessor h1.header_predecessor h2.header_predecessor;
        triplet ~mc s_timestamp
          (Date.to_string h1.header_timestamp)
          (Date.to_string h2.header_timestamp);
        triplet_int ~mc s_validation_pass h1.header_validation_pass h2.header_validation_pass;
        triplet ~mc s_hash h1.header_operations_hash h2.header_operations_hash;
        triplet ~mc s_fitness h1.header_fitness h2.header_fitness;
        triplet ~mc s_context h1.header_context h2.header_context;
        triplet_int ~mc s_priority h1.header_priority h2.header_priority;
        triplet ~mc s_nonce_hash h1.header_seed_nonce_hash h2.header_seed_nonce_hash;
        triplet ~mc s_pow_nonce h1.header_proof_of_work_nonce h2.header_proof_of_work_nonce;
        triplet ~mc s_signature h1.header_signature h2.header_signature] in
  let title = Printf.sprintf "%i: %s" index (t_ s_double_baking_evidence) in
  let content =
    [ row_dbe_label ; row_dbe_value ;
      row_dbe_accused_label ; row_dbe_accused_value ;
      row_dbe_denouncer_label ; row_dbe_denouncer_value ;
      row_dbe_gain_label ; row_dbe_gain_value ;
      row_dbe_lost_deposit_label; row_dbe_lost_deposit_value ;
      row_dbe_lost_rewards_label; row_dbe_lost_rewards_value ;
      row_dbe_lost_fees_label; row_dbe_lost_fees_value ;

    ] @ content_diff in
  make_sub_operation_div title content

let make_double_endorsement_evidence_details index dee =
  let endorse1 = Common.get_endorsement_op_type dee.double_endorsement1 in
  let endorse2 = Common.get_endorsement_op_type dee.double_endorsement2 in
  match endorse1, endorse2 with
  | Some endorse1, Some endorse2 ->
    let title =
      Printf.sprintf "%i: %s" index (t_ s_double_endorsement_evidence) in
    let content = List.flatten
        [ triplet s_block_hash endorse1.endorse_block_hash endorse2.endorse_block_hash;
          triplet_int s_level endorse1.endorse_block_level endorse2.endorse_block_level;
          triplet s_slots
            (String.concat ", " @@ List.map string_of_int endorse1.endorse_slot)
            (String.concat ", " @@ List.map string_of_int endorse2.endorse_slot)] in
    make_sub_operation_div title content
  | _, _ -> assert false

let make_transactions_table ?price_usd ops =
  let theads =
    Panel.theads_of_strings
      [
        s_source, 1;
        s_empty, 1;
        s_destination, 1;
        s_amount, 2;
        s_counter, 1;
        s_fee, 1;
        s_gas_limit, 1;
        s_storage_limit, 1;
        s_parameter, 1;
        s_internal, 1;
      ] in
  let rows =
  List.map (function
      | Transaction transaction ->
        let td_src, td_arrow, td_dst =
          Common.account_w_blockies transaction.tr_src,
          (if transaction.tr_failed then
            td ~a:[ a_title (t_ s_fail) ; a_class [ red ] ]
              [ span ~a:[ a_class ["fa"; "fa-times" ] ] [] ]
          else
            td ~a:[ a_class [ green ] ] [ right_icon () ]),
          Common.account_w_blockies transaction.tr_dst in
        let td_amount = td @@ Tez.with_usd price_usd transaction.tr_amount in
        let td_counter = td [ pcdata @@ Int32.to_string transaction.tr_counter ] in
        let td_fee = td [ Tez.pp_amount ~precision:2 transaction.tr_fee ] in
        let td_gas_limit =
          td [ pcdata @@ Z.to_string transaction.tr_gas_limit ] in
        let td_storage_limit =
          td [ pcdata @@ Z.to_string transaction.tr_storage_limit ] in
        let td_param = match transaction.tr_parameters with
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
        let td_internal, cls_internal =
          if not transaction.tr_internal then td [ pcdata_t s_no ], []
          else td [ pcdata_t s_yes ], [ "warning" ] in
        tr ~a:[ a_class (Common.failed_class transaction.tr_failed @ cls_internal)] [
          td_src ;
          td_arrow ;
          td_dst ;
          td_amount ;
          td_counter ;
          td_fee ;
          td_gas_limit ;
          td_storage_limit ;
          td_param ;
          td_internal ;
        ]
      | _ -> assert false)
    ops in
  tablex
    ~a:[ a_class [ btable; "transactions-table"; btable_striped; ] ] [
    tbody (theads () :: rows) ]

let make_originations_table ?price_usd ops =
  let theads =
    Panel.theads_of_strings
      [
        s_new_account, 2 ;
        s_new_balance, 1 ;
        s_originator, 1 ;
        s_originator_manager, 1 ;
        s_delegate, 1;
        s_counter, 1;
        s_fee, 1;
        s_gas_limit, 1;
        s_storage_limit, 1;
        s_burn, 1;
        s_code, 1;
      ] in
  let rows =
    List.map (function
        | Origination ori ->
          let td_new_account = Common.account_w_blockies ori.or_tz1 in
          let td_originator = Common.account_w_blockies ori.or_src in
          let td_manager = Common.account_w_blockies ori.or_manager in
          let td_delegate = Common.account_w_blockies ori.or_delegate in
          let td_counter = td [ pcdata @@ Int32.to_string ori.or_counter ] in
          let td_fee = td [ pcdata @@ Int64.to_string ori.or_fee ] in
          let td_gas_limit = td [ pcdata @@ Z.to_string ori.or_gas_limit ] in
          let td_storage_limit =
            td [ pcdata @@ Z.to_string ori.or_storage_limit ] in
          let burn = Tez.with_usd price_usd @@ ori.or_burn in
          let code_link = match ori.or_script with
            | Some { sc_code ; sc_storage } when (sc_code <> "{}" || sc_storage <> "{}") ->
              td [ Common.make_link (t_ s_yes) ~path:ori.or_tz1.tz ]
            | _ -> td [ pcdata_t s_no ] in
          let balance = Tez.with_usd price_usd ori.or_balance in
          tr ~a:[ a_class (Common.failed_class ori.or_failed)] ([
              td_new_account ;
              td balance ;
              td_originator ;
              td_manager ] @
              (if ori.or_delegate.tz <> "" then
                 [ td_delegate ]
               else ([ td [ pcdata_t s_no_delegate ] ])) @
              [ td_counter ;
                td_fee ;
                td_gas_limit ;
                td_storage_limit ;
                td burn ;
                code_link
              ])
        | _ -> assert false)
      ops in
  tablex
    ~a:[ a_class [ btable; "transactions-table"; btable_striped; ] ] [
    tbody (theads() :: rows) ]

let make_delegations_table ops =
  let theads =
    Panel.theads_of_strings
      [
        s_account_to_delegate, 3;
        s_empty, 1;
        s_new_delegate, 3;
        s_counter, 1;
        s_fee, 1;
        s_gas_limit, 1;
        s_storage_limit, 1;
      ] in
  let rows =
    List.map (function
        | Delegation del ->

          let td_delegate, td_arrow = match del.del_delegate.tz with
            | "" -> td [ pcdata "unset" ],
                    td ~a:[ a_class [ blue ; "center" ] ]
                      [ span ~a:[ a_class [ "fa"; "fa-arrow-down" ] ] [] ]
            | _ -> Common.account_w_blockies del.del_delegate,
                   td ~a:[ a_class [ green ; "center" ] ] [ right_icon () ] in
          let td_counter = td [ pcdata @@ Int32.to_string del.del_counter ] in
          let td_fee = td [ pcdata @@ Int64.to_string del.del_fee ] in
          let td_gas_limit = td [ pcdata @@ Z.to_string del.del_gas_limit ] in
          let td_storage_limit =
            td [ pcdata @@ Z.to_string del.del_storage_limit ] in
          tr ~a:[ a_class (Common.failed_class del.del_failed)] [
            Common.account_w_blockies del.del_src ;
            td_arrow ;
            td_delegate ;
            td_counter ;
            td_fee ;
            td_gas_limit ;
            td_storage_limit ;
          ]
        | _ -> assert false)
      ops in
  tablex
    ~a:[ a_class [ btable; "transactions-table"; btable_striped; ] ] [
    tbody (theads() :: rows) ]

let make_reveals_table ops =
  let theads =
    Panel.theads_of_strings
      [
        s_source, 3;
        s_empty, 1;
        s_public_key, 3;
        s_counter, 1;
        s_fee, 2 ;
        s_gas_limit, 1;
        s_storage_limit, 1;
      ] in
  let rows =
    List.map (function
        | Reveal rvl ->
          let td_source = Common.account_w_blockies rvl.rvl_src in
          let td_icon =
            td ~a:[ a_class [ green ; "center" ] ] [ right_icon () ] in
          let td_public_key =
            Common.account_w_blockies {tz=rvl.rvl_pubkey;alias=None} in
          let td_counter =
            td [ pcdata @@ Int32.to_string rvl.rvl_counter ] in
          let td_fee =
            td [ pcdata @@ Int64.to_string rvl.rvl_fee ] in
          let td_gas_limit =
            td [ pcdata @@ Z.to_string rvl.rvl_gas_limit ] in
          let td_storage_limit =
            td [ pcdata @@ Z.to_string rvl.rvl_gas_limit ] in
          tr ~a:[ a_class (Common.failed_class rvl.rvl_failed)] [
            td_source ;
            td_icon ;
            td_public_key ;
            td_counter ;
            td_fee ;
            td_gas_limit ;
            td_storage_limit ;
          ]
        | _ -> assert false)
      ops in
  tablex
    ~a:[ a_class [ btable; "transactions-table"; btable_striped; ] ] [
    tbody (theads() :: rows) ]

let make_transactions_tab ?price_usd default tab ops =
  let table = make_transactions_table ?price_usd ops in
  Tabs.make_content_panel tab (is_active default Op_Txs) @@
  div ~a:[ a_class [ panel; panel_primary ] ] [
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ row] ] [
        h5 ~a:[ a_class [ cxs12; panel_title; "no-overflow" ] ] [
          pcdata_t s_transactions ]
      ]
    ] ;
    div ~a:[ a_class [ panel_body ] ] [ table ] ]

let make_delegations_details default tab ops =
  let table = make_delegations_table ops in
  Tabs.make_content_panel tab (is_active default Op_Del) @@
  div ~a:[ a_class [ panel; panel_primary ] ] [
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ row] ] [
        h5 ~a:[ a_class [ cxs12; panel_title; "no-overflow" ] ] [
          pcdata_t s_delegations ]
      ]
    ] ;
    div ~a:[ a_class [ panel_body ] ] [ table ]
  ]

let make_originations_details ?price_usd default tab ops =
  let table = make_originations_table ?price_usd ops in
  Tabs.make_content_panel tab (is_active default Op_Ori) @@
  div ~a:[ a_class [ panel; panel_primary ] ] [
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ row] ] [
        h5 ~a:[ a_class [ cxs12; panel_title; "no-overflow" ] ] [
          pcdata_t s_originations ]
      ]
    ] ;
    div ~a:[ a_class [ panel_body ] ] [ table ]
  ]

let make_reveals_details default tab ops =
  let table = make_reveals_table ops in
  Tabs.make_content_panel tab (is_active default Op_Rvl) @@
  div ~a:[ a_class [ panel; panel_primary ] ] [
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ row] ] [
        h5 ~a:[ a_class [ cxs12; panel_title; "no-overflow" ] ] [
          pcdata_t s_reveals ]
      ]
    ] ;
    div ~a:[ a_class [ panel_body ] ] [ table ]
  ]

let make_manager_details ?price_usd default ops =
  let transactions =
    List.filter (function Transaction _ -> true | _ -> false) ops in
  let originations =
    List.filter (function Origination _ -> true | _ -> false) ops in
  let delegations =
    List.filter (function Delegation _ -> true | _ -> false) ops in
  let reveals =
    List.filter (function Reveal _ -> true | _ -> false) ops in
  div ~a:[ a_class [ row ] ] [
    div ~a:[ a_class [ cxs12 ] ] [
      make_op_tabs default ;
      div ~a:[ a_class [ "tab-content" ] ] [

        make_transactions_tab ?price_usd default tr_tab transactions ;

        make_delegations_details default del_tab delegations ;

        make_originations_details ?price_usd default ori_tab originations ;

        make_reveals_details default rvl_tab reveals
      ]
    ];
  ],
  (fun () ->
     let transactions_nb = List.length transactions in
     Tabs.update_tab_title tr_tab (Some transactions_nb) ;
     let originations_nb = List.length originations in
     Tabs.update_tab_title ori_tab (Some originations_nb) ;
     let delegations_nb = List.length delegations in
     Tabs.update_tab_title del_tab (Some delegations_nb) ;
     let reveals_nb = List.length reveals in
     Tabs.update_tab_title rvl_tab (Some reveals_nb))

let make_operation_info_content ?price_usd operation =
  let row_block_label = label [ pcdata_t s_included_in_block ] in
  let row_block_value = value [ Common.make_link operation.op_block_hash ] in

  let row_hash_label = label [ pcdata_t s_operation_hash ] in
  let row_hash_value = value [ pcdata operation.op_hash ] in

  let row_level_label = label [ pcdata_t s_level ] in
  let row_level_value = value ~id:Common.confirmation_blocks_id
      [ Common.pcdata_ () ]
  in

  let row_success_label = label [ pcdata_t s_status ] in
  let row_success_value =
      if operation.op_block_hash = Utils.pending_block_hash then
        value ~classes:[grey] [ pcdata_t s_pending ]
    else if operation.op_block_hash = Utils.orphan_block_hash then
      value ~classes:[ red ] [ pcdata_t s_failed ]
    else
      value ~classes: [ green ] [ pcdata_t s_success ]
  in

  let row_timestamp_label = label [ pcdata_t s_timestamp ] in
  let row_timestamp_value =
    value ~id:(timestamp_id operation.op_hash) [ Common.pcdata_ () ] in

  let res = [ row_hash_label ; row_hash_value ;
              row_level_label ; row_level_value ;
              row_success_label ; row_success_value ;
              row_timestamp_label ; row_timestamp_value ] in
  let res = match operation.op_type with
    | Sourced sop -> begin match sop with
        | Consensus _ | Dictator _ | Amendment _ -> res
        | Manager (_k, _src, ops) ->
          let fees = Common.compute_fees ops in
          (* let row_counter_label = label [ pcdata_t s_counter ] in
           * let row_counter_value =
           *   value [ pcdata @@ Int32.to_string transaction.tr_counter ] in *)
          let row_fee_label = label  [ pcdata_t s_fee ] in
          let row_fee_value = value ( Tez.with_usd price_usd fees ) in
          res @
          [ (* row_counter_label ; row_counter_value ; *)
            row_fee_label ; row_fee_value ]
          (* @ make_operation_manager_info_rows
             * ?price_usd fee counter gas_limit st_limit *)
      end
    | Anonymous _ -> res in
  if operation.op_block_hash = Utils.pending_block_hash ||
     operation.op_block_hash = Utils.orphan_block_hash then res
  else row_block_label :: row_block_value :: res

let update_confirmation bhash level nb_confirm =
  let confirm = find_component @@ Common.confirmation_blocks_id in
  Manip.replaceChildren confirm
    [ a ~a:( Common.a_link bhash ) [ pcdata @@ string_of_int level ];
      pcdata @@
      Printf.sprintf " (%d %s)" nb_confirm (t_ s_blocks_confirmation)
    ]

let update_timestamp hash timestamp =
  let ts_h = find_component @@ timestamp_id hash in
  Manip.setInnerHtml ts_h timestamp

let update_operation_summary ?price_usd ?endorse_info operation filters =
  let div_id = operation_id operation.op_hash in
  let container = find_component div_id in
  let default = default_filter filters in
  let header, op_info, update =
    match operation.op_type with
    | Anonymous list ->
      [ pcdata_t s_anonymous_operations; Glossary_doc.(help HTransaction) ],
      List.mapi (fun i aop ->
          let i = i + 1 in
          match aop with
          | Seed_nonce_revelation seed ->
            make_seed_nonce_revelation_details i seed
          | Activation act ->
            make_activation_details i act
          | Double_baking_evidence dbe ->
            make_double_baking_evidence_details i dbe
          | Double_endorsement_evidence dee ->
            make_double_endorsement_evidence_details i dee)
        list,
      (fun () -> ())
    | Sourced sop ->
      begin match sop with
        | Consensus Endorsement e ->
          [ pcdata_t s_endorsement; Glossary_doc.(help HEndorsement) ],
          make_endorsement_details ?endorse_info e,
          (fun () -> ())
        | Amendment (src, aop) ->
          begin match aop with
            | Proposal prop ->
              [ pcdata_t s_proposals; Glossary_doc.(help HAmendment) ],
              make_proposals_details src prop,
              (fun () -> ())
            | Ballot ballot ->
              [ pcdata_t s_ballot; Glossary_doc.(help HAmendment) ],
              make_ballot_details src ballot,
              (fun () -> ())
          end
        | Manager (_k, _src, list) ->
          let details, update =
            make_manager_details ?price_usd default list in
          [ pcdata_t s_manager_operations; Glossary_doc.(help HTransaction) ],
          [ details ],
          update
        | Dictator dop ->
          begin match dop with
            | Activate ->
              [ pcdata_t s_activate ],
              [],
              (fun () -> ())
            | Activate_testnet ->
              [ pcdata_t s_activate_testnet ],
              [],
              (fun () -> ())
          end
      end
  in
  let content = make_operation_info_content ?price_usd operation in
  let generic =
    div ~a:[ a_class [ "operation-div"; row ] ] [
      div ~a:[ a_class [ cxs12 ] ] [
        div ~a:[ a_class [ panel; panel_primary ]] [
          div ~a:[ a_class [ panel_heading ] ] [
            h3 ~a:[ a_class [ panel_title ] ] header
          ] ;
          div ~a:[ a_class [ panel_body ] ] content
        ] ;
      ]
    ]
  in
  Manip.removeChildren container ;
  Manip.appendChild container generic ;
  List.iter (Manip.appendChild container) op_info;
  update ();
  ignore (Js.Unsafe.eval_string
            "jQuery('[data-toggle=\"popover\"]').popover();")


(* Maker (empty) *)
let make_block_fetching () =  pcdata_t s_fetching_data

let make_operation_info_panel hash =
  div ~a:[ a_class [ "operation-div"; row ] ; a_id @@ operation_id hash ] [
    div ~a:[ a_class [ cxs12 ] ] [
      div ~a:[ a_class [ panel; panel_primary ]] [
        div ~a:[ a_class [ panel_heading ] ] [
          h3 ~a:[ a_class [ panel_title ] ] [ pcdata hash ]
        ] ;
        div ~a:[ a_class [ panel_body ] ] [
          make_block_fetching ();
        ]
      ] ;
    ]
  ]

let make_page hash = make_operation_info_panel hash
