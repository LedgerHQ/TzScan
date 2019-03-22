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
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Bootstrap_helpers.Color
open Tezos_types
open Data_types
open Lang
open Text

let spf = Printf.sprintf

let cl_title = Common.responsive_column_title
let cl_icon = Common.responsive_title

type bl_filter = Bl_Txs | Bl_Del | Bl_Ori | Bl_Endt | Bl_Act | Bl_Rev

let string_of_filter = function
    Bl_Txs -> "transaction"
  | Bl_Del -> "delegation"
  | Bl_Ori -> "origination"
  | Bl_Endt -> "endorsement"
  | Bl_Act -> "activation"
  | Bl_Rev -> "revelation"

let filter_of_string = function
    "transaction" -> Bl_Txs
  | "delegation" -> Bl_Del
  | "origination" -> Bl_Ori
  | "endorsement" -> Bl_Endt
  | "activation" -> Bl_Act
  | "revelation" -> Bl_Rev
  | _ -> Bl_Txs

let default_filter filters =
  match Misc.list_assoc_opt "default" filters with
  | Some filter -> filter_of_string filter
  | _ -> Bl_Txs

let is_active default exp =
  if default = exp then Tabs.Active else Tabs.Inactive

let mk_title icon title nb =
  span @@ Common.responsive_title_fun icon
    (Panel.title_nb title)
    (match nb with Some nb -> nb | None -> -1)

let tr_tab = Tabs.make
    (mk_title exchange_icon s_transactions) [
    "block-tab" ]

let del_tab = Tabs.make
    (mk_title handshake_icon s_delegations) [
    "block-tab" ]

let ori_tab = Tabs.make
    (mk_title link_icon s_originations) [
    "block-tab" ]

let endt_tab = Tabs.make
    (mk_title stamp_icon s_endorsements) [
    "block-tab" ]

let revl_tab = Tabs.make
    (mk_title ghost_icon s_revelations) [
    "block-tab" ]

let act_tab = Tabs.make
    (mk_title arrow_up_icon s_activations) [
    "block-tab" ]

module OperationsPanel = struct
  let title_span _ = span []
  let table_class = "default-table"
  let page_size = Common.big_panel_number
end

module TransactionsPanel = Panel.MakePageTable(
  struct
    include OperationsPanel
    let name = "transactions"
    let theads () =
      tr [
        th @@ cl_icon exchange_icon @@ t_ s_txn_hash ;
        th @@ cl_icon account_icon @@ t_ s_from ;
        th ~a:[ a_class [ "arrow" ] ] @@ [ txt_t s_empty ] ;
        th @@ cl_icon account_icon @@ t_ s_to ;
        th @@ cl_icon Tez.icon @@ t_ s_amount ;
        th @@ cl_icon (fun () -> txt "#") @@ t_ s_counter ;
        th @@ cl_icon bill_icon @@ t_ s_fee ;
        th @@ cl_icon (fun () -> txt "gas") @@ t_ s_gas_limit ;
        th @@ cl_icon (fun () -> txt "storage") @@ t_ s_storage_limit ;
        th @@ cl_icon folder_icon @@ t_ s_internal ;
      ]
  end)

module DelegationsPanel = Panel.MakePageTable(
  struct
    include OperationsPanel
    let name = "delegations"
    let theads () =
      tr [
        th @@ cl_icon exchange_icon (s_ "Del Hash");
        th @@ cl_icon account_icon @@ t_ s_account ;
        th ~a:[ a_class [ "arrow" ] ] @@ [ txt "" ] ;
        th @@ cl_icon astronaut_icon @@ t_ s_delegate ;
        th @@ cl_icon (fun () -> txt "#") @@ t_ s_counter ;
        th @@ cl_icon bill_icon @@ t_ s_fee ;
        th @@ cl_icon folder_icon @@ t_ s_internal ;
      ]
  end)

module OriginationsPanel = Panel.MakePageTable(
  struct
    include OperationsPanel
    let name = "originations"
    let theads () =
      tr [
        th @@ cl_icon link_icon (s_ "Ori. Hash");
        th @@ cl_icon account_icon @@ t_ s_new_account ;
        th @@ cl_icon Tez.icon @@ t_ s_new_balance ;
        th @@ cl_icon originator_icon @@ t_ s_originator ;
        th @@ cl_icon manager_icon @@ t_ s_manager ;
        th @@ cl_icon astronaut_icon @@ t_ s_delegate ;
        th @@ cl_icon bill_icon @@ t_ s_fee ;
        th @@ cl_icon burn_icon @@ t_ s_burn ;
        th @@ cl_icon folder_icon @@ t_ s_internal ;
      ]
  end)

module EndorsementsPanel = Panel.MakePageTable(
  struct
    include OperationsPanel
    let name = "endorsements"
    let theads () =
      tr [
        th @@ cl_icon stamp_icon (s_ "End. Hash");
        th @@ cl_icon account_icon @@ t_ s_endorser ;
        th @@ cl_icon slots_icon @@ t_ s_slots ;
        th @@ cl_icon check_icon @@ t_ s_endorsed_block ;
        th @@ cl_icon check_icon @@ t_ s_endorsed_level ;
        th @@ cl_icon priority_icon @@ t_ s_priority ;
      ]
  end)

module ActivationsPanel = Panel.MakePageTable(
  struct
    include OperationsPanel
    let name = "activations"
    let theads () =
      tr [
        th @@ cl_icon arrow_up_icon (s_ "Act. Hash");
        th @@ cl_icon account_icon (s_ "Tz1");
      ]
  end)

module NonceRevelationsPanel = Panel.MakePageTable(
  struct
    include OperationsPanel
    let name = "revelations"
    let theads () =
      tr [
        th @@ cl_icon ghost_icon (s_ "Nonce Rvl. Hash");
        th @@ cl_icon cube_icon @@ t_ s_level ;
        th @@ cl_icon space_icon @@ t_ s_nonce ;
      ]
  end)

let baker_id hash = Common.make_id "block-baker" hash
let endorsements_id hash = Common.make_id "block-endorsements" hash
let succ_id hash = Common.make_id "block-succ" hash
let internal_succ_id hash = Common.make_id "internal-block-succ" hash

(* Maker (empty) *)
let make_block_fetching () =  Lang.txt_t s_fetching_data

let make_transaction_view default =
  Tabs.make_content_panel tr_tab (is_active default Bl_Txs) @@
  TransactionsPanel.make ()

let make_delegations_view default =
  Tabs.make_content_panel del_tab (is_active default Bl_Del) @@
  DelegationsPanel.make ()

let make_originations_view default =
  Tabs.make_content_panel ori_tab (is_active default Bl_Ori) @@
  OriginationsPanel.make ()

let make_endorsements_view default =
  Tabs.make_content_panel endt_tab (is_active default Bl_Endt) @@
  EndorsementsPanel.make ()

let make_activations_view default =
  Tabs.make_content_panel act_tab (is_active default Bl_Act) @@
  ActivationsPanel.make ()

let make_revelations_view default =
  Tabs.make_content_panel revl_tab (is_active default Bl_Rev) @@
  NonceRevelationsPanel.make ()

let make_summary_template =
  let mk_row_lbl4 lbl = Common.mk_row_lbl clg4 lbl in
  let mk_row_val8_pcd v = Common.mk_row_val clg8 [ txt v ] in
  let mk_row_val8_txtarea v =
    Common.mk_row_val clg8 [ textarea ~a:[ a_disabled () ] ( txt v ) ]
  in
  fun ?block ?level_details hash  ->
    let level, fitness, timestamp,
        _pred, link_pred, nonce, protocol, network, _baker, pow, validation_pass,
        priority, proto, data, signature, volume, fees, dist_lvl =
      match block with
      | None ->
        Common.bullshit_s, Common.bullshit_s,
        Common.bullshit_s, Common.bullshit_s,
        Common.txt_ (), Common.bullshit_s,
        Common.bullshit_s, Common.bullshit_s,
        Common.bullshit_s, Common.bullshit_s,
        Common.bullshit_s, Common.bullshit_s,
        Common.bullshit_s, Common.bullshit_s,
        Common.bullshit_s, Common.txt_ (),
        Common.txt_ (), None
      | Some block ->
        let link_pred =
          Common.make_link block.predecessor_hash
            ~path:(
               if block.distance_level = 0 && block.level > 0 then
                 string_of_int (block.level - 1)
               else
                 block.predecessor_hash
            ) in
        Common.safe_value @@ string_of_int block.level,
        Common.safe_value @@ string_of_int @@ Common.get_fitness block.fitness,
        Common.safe_value @@ Date.to_string block.timestamp,
        Common.safe_value @@ block.predecessor_hash,
        link_pred,
        Common.safe_value @@ block.commited_nonce_hash,
        Common.safe_value @@ block.protocol.proto_name,
        Common.safe_value @@ block.network,
        Common.safe_value @@ block.baker.tz,
        Common.safe_value @@ block.pow_nonce,
        Common.safe_value @@ string_of_int block.validation_pass,
        Common.safe_value @@ string_of_int block.priority,
        Common.safe_value @@ string_of_int block.proto,
        Common.safe_value @@ block.data,
        Common.safe_value @@ block.signature,
        Tez.pp_amount block.volume,
        Tez.pp_amount block.fees,
        Some block.distance_level in
    let row_level_label = mk_row_lbl4 "Level" in
    let row_level_value =
      div ~a:[ a_id Common.confirmation_blocks_id; a_class [ clg8; "value" ] ]
        [ txt level ] in
    let row_fitness_label = mk_row_lbl4 "Fitness" in
    let row_fitness_value = mk_row_val8_pcd fitness in
    let row_timestamp_label = mk_row_lbl4 "Timestamp" in
    let row_timestamp_value = mk_row_val8_pcd timestamp in
    let row_hash_label = Common.mk_row_lbl clg2 "Hash" in
    let cls = [ clg10; "value" ] in
    let cls =
      match dist_lvl with
        None -> cls
      | Some 0 -> "bg-block-main-chain" :: cls
      | Some _ -> "bg-block-alt-chain" :: cls
    in
    let row_hash_value = div ~a:[ a_class cls ] [ txt hash ] in
    let row_pred_label = Common.mk_row_lbl clg2 "Predecessor" in
    let row_pred_value = Common.mk_row_val clg10 [ link_pred  ] in
    let row_succ_label = Common.mk_row_lbl clg2 "Successor" in
    let row_succ_value =
      div ~a:[ a_class [ clg10; "value" ] ; a_id @@ internal_succ_id hash ]
        [ Common.txt_ () ] in
    let row_volume_label = mk_row_lbl4 "Volume" in
    let row_volume_value = Common.mk_row_val clg8 [ volume ] in
    let row_fees_label = mk_row_lbl4 "Fees"  in
    let row_fees_value = Common.mk_row_val clg8 [ fees ] in
    let row_uncles_label = mk_row_lbl4 "Uncles" in
    let row_uncles_value =
      div ~a:[ a_class [ clg8; "value" ] ] [
        span ~a:[ a_id @@ Common.block_uncles_id hash ]
          [ Common.txt_ () ] ] in
    let row_nonce_hash_label = mk_row_lbl4 "Nonce Hash" in
    let row_nonce_hash_value = mk_row_val8_txtarea nonce in
    let row_validation_pass_label = mk_row_lbl4 "Validation Pass" in
    let row_validation_pass_value = mk_row_val8_pcd validation_pass in
    let row_protocol_label = mk_row_lbl4 "Protocol" in
    let row_protocol_value = mk_row_val8_txtarea protocol in
    let row_network_label = mk_row_lbl4 "Network" in
    let row_network_value = mk_row_val8_pcd network in
    let row_pow_label = mk_row_lbl4 "POW Nonce" in
    let row_pow_value = mk_row_val8_pcd pow in
    let row_proto_label = mk_row_lbl4 "Proto Changes" in
    let row_proto_value = mk_row_val8_pcd proto in
    let row_priority_label = mk_row_lbl4 "Priority" in
    let row_priority_value = mk_row_val8_pcd priority in
    let row_data_label = mk_row_lbl4 "Data" in
    let row_data_value = mk_row_val8_txtarea data in
    let row_signature_label = mk_row_lbl4 "Signature" in
    let row_signature_value = mk_row_val8_txtarea signature in
    let row_cycle_label = mk_row_lbl4 "Cycle" in
    let row_cycle_position_label = mk_row_lbl4 "Cycle Position" in
    let row_cycle_value, row_cycle_position_value =
      match level_details with
      | None -> mk_row_val8_pcd Common.bullshit_s,
                mk_row_val8_pcd Common.bullshit_s
      | Some level_details ->
        let cycle = level_details.lvl_cycle in
        let cst = Infos.constants ~cycle in
        mk_row_val8_pcd @@ string_of_int level_details.lvl_cycle,
        mk_row_val8_pcd @@ Printf.sprintf
          "%d / %d" level_details.lvl_cycle_position
          (cst.blocks_per_cycle - 1) in
    let full =
      div ~a:[ a_class [ clg12 ] ] [
        div ~a:[ a_class [ row ] ]  [
          row_hash_label ; row_hash_value ;
          row_pred_label ; row_pred_value ;
          row_succ_label ; row_succ_value ] ] in
    let right =
      div ~a:[ a_class [ clg6 ] ] [
        div ~a:[ a_class [ row ] ]  [
          row_nonce_hash_label ; row_nonce_hash_value ;
          row_protocol_label ; row_protocol_value ;
          row_proto_label ; row_proto_value ;
          row_network_label ; row_network_value ;
          row_pow_label ; row_pow_value ;
          row_data_label ; row_data_value ;
          row_signature_label ; row_signature_value ] ] in
    let left =
      div ~a:[ a_class [ clg6 ] ] [
        div ~a:[ a_class [ row ] ]  [
          row_timestamp_label ; row_timestamp_value ;
          row_volume_label ; row_volume_value ;
          row_fees_label ; row_fees_value ;
          row_uncles_label ; row_uncles_value ;
          row_level_label ; row_level_value ;
          row_cycle_label ; row_cycle_value ;
          row_cycle_position_label ; row_cycle_position_value ;
          row_fitness_label ; row_fitness_value ;
          row_validation_pass_label ; row_validation_pass_value ;
          row_priority_label ; row_priority_value ] ] in
    [ full; left; right]

let make_block_summary ?block ?level_details hash =
  make_summary_template ?block ?level_details hash

let make_block_endorsements () =
  let slots = Common.make_slots 8 in
  let rows = Common.make_endorsements_slots slots in
  div [
    div ~a:[ a_class [ row; "endorsements-table" ] ]
      [ div ~a:[ a_class [ clg12 ] ] [ rows ]  ] ;
    Common.legend ()
  ]

(* Updater *)
let update_block_uncles block nb_uncles =
  let uncles = string_of_int nb_uncles in
  let to_update = find_component @@ Common.block_uncles_id block.hash in
  let content =
    if nb_uncles = 0 then txt uncles
    else
      let path = "heads" in
      let args = [ "level", string_of_int block.level ] in
      Common.make_link uncles ~args ~path
  in
  Manip.replaceChildren to_update [ content ]

let update_succ block succ_hash =

  let href_succ, link_succ =
    if block.distance_level = 0 then
      string_of_int @@ block.level + 1,
      Common.make_link succ_hash
        ~path:(string_of_int ( block.level + 1 ) )
    else
      succ_hash,
      Common.make_link ~path:succ_hash succ_hash
  in

  (* update hyperlink for succ_id *)
  let button = find_component @@ succ_id block.hash in
  (To_dom.of_a button)##href <- Js.string href_succ ;
  Manip.removeClass button "disabled" ;

  (* update hyperlink and txt for internal_succ_id *)
  let container = find_component @@ internal_succ_id block.hash in
  Manip.removeChildren container ;
  Manip.appendChild container link_succ

let update_block_baker = ref (fun (_block : block) -> ())

let update_block_transactions ~nrows xhr =
  let to_rows operations =
    List.map (fun (op_hash, op_block_hash, _src, transaction) ->
        let open Gen in
        let cols = from_transaction op_block_hash op_hash transaction in
        let cls_internal = is_internal transaction.tr_internal in
        tr ~a:[ a_class
                  (Common.failed_class transaction.tr_failed @ cls_internal) ] [
          cols.td_tx_op_hash ;
          cols.td_tx_src ;
          cols.td_tx_failed  ;
          cols.td_tx_dst ;
          cols.td_tx_amount ;
          cols.td_tx_counter ;
          cols.td_tx_fee ;
          cols.td_tx_gas_limit ;
          cols.td_tx_storage_limit ;
          cols.td_tx_internal ;
        ]) @@ Common.get_transactions operations
  in
  Tabs.update_tab_title tr_tab (Some nrows) ;
  TransactionsPanel.paginate_fun ~urlarg_page:"p_tr" to_rows ~nrows xhr

let update_block_originations ~nrows xhr =
  let to_rows operations =
    List.map (fun (op_hash, op_block_hash, src, orig) ->
        let open Gen in
        let cols = Gen.from_origination op_block_hash op_hash src orig in
        let cls_internal = is_internal orig.or_internal in
        tr ~a:[ a_class (Common.failed_class orig.or_failed @ cls_internal) ] [
          cols.td_or_op_hash ;
          cols.td_or_tz1 ;
          cols.td_or_balance ;
          cols.td_or_src ;
          cols.td_or_manager ;
          cols.td_or_delegate ;
          cols.td_or_fee ;
          cols.td_or_burn ;
          cols.td_or_internal ;
        ]) @@ Common.get_originations operations
  in
  Tabs.update_tab_title ori_tab (Some nrows);
  OriginationsPanel.paginate_fun ~urlarg_page:"p_ori" to_rows ~nrows xhr

let update_block_included_endorsements ~nrows xhr =
  let to_rows operations =
    List.map (fun (op_hash, _op_block_hash, endorse) ->
        let open Gen in
        let cols =
          from_endorsement ~crop_len_tz:36 ~crop_len_hash:10 op_hash endorse in
        tr [
          cols.td_endorse_op_hash ;
          cols.td_endorse_src ;
          cols.td_endorse_slot ;
          cols.td_endorse_block_hash ;
          cols.td_endorse_block_level ;
          cols.td_endorse_priority ;
        ]) @@ Common.get_endorsements operations
  in
  Tabs.update_tab_title endt_tab (Some nrows);
  EndorsementsPanel.paginate_fun ~urlarg_page:"p_end" to_rows ~nrows xhr

let update_block_delegations ~nrows xhr =
  let to_rows operations =
    List.map (fun (op_hash, op_block_hash, src, del) ->
        let open Gen in
        let cols =
          Gen.from_delegation
            ~crop_len_hash:8
            ~crop_limit:sm_size op_block_hash op_hash src del in
        let cls_internal = is_internal del.del_internal in
        let td_arrow = match del.del_delegate.tz with
          | "" ->
            td ~a:[ a_class [ blue ; "center" ] ]
              [ span ~a:[ a_class [ "fa"; "fa-arrow-down" ] ] [] ]
          | _ ->
            td ~a:[ a_class [ green ; "center" ] ] [ right_icon () ] in
        tr ~a:[ a_class (Common.failed_class del.del_failed @ cls_internal)] [
          cols.td_del_op_hash ;
          cols.td_del_src ;
          td_arrow ;
          cols.td_del_delegate ;
          cols.td_del_counter ;
          cols.td_del_fee ;
          cols.td_del_internal ;
        ]) @@ Common.get_delegations operations
  in
  Tabs.update_tab_title del_tab (Some nrows) ;
  DelegationsPanel.paginate_fun ~urlarg_page:"p_del" to_rows ~nrows xhr

let update_block_nonce_revelations ~nrows xhr =
  let to_rows operations =
    List.map (fun (op_hash, op_block_hash, r) ->
        let open Gen in
        let cols = Gen.from_seed_nonce_revelation op_block_hash op_hash r in
        tr [
          cols.td_seed_op_hash ;
          cols.td_seed_level ;
          cols.td_seed_nonce ;
        ]
      ) @@ Common.get_seed_nonce_revelations operations
  in
  Tabs.update_tab_title revl_tab (Some nrows) ;
  NonceRevelationsPanel.paginate_fun ~urlarg_page:"p_revl" to_rows ~nrows xhr

let update_block_activations ~nrows xhr =
  let to_rows operations =
    List.map (fun (op_hash, op_block_hash, act) ->
        let open Gen in
        let cols = Gen.from_activation op_block_hash op_hash act in
        tr [
          cols.td_act_op_hash ;
          cols.td_act_pkh ;
        ]) @@ Common.get_activations operations
  in
  Tabs.update_tab_title act_tab (Some nrows) ;
  ActivationsPanel.paginate_fun ~urlarg_page:"p_act" to_rows ~nrows xhr

let aria_hidden () = to_attrib @@ Xml.string_attrib "aria-hidden" "true"

let update_block_summary level_details block is_snapshot =
  let container = find_component @@ Common.summary_id block.hash in
  let link_pred =
    if block.level = 0 then
      a ~a:[ a_class [ "disabled" ] ] [
        span
          ~a:[
            a_class [ "glyphicon"; "glyphicon-triangle-left" ] ;
            aria_hidden () ] []
      ]
    else
      let href_pred =
        if block.distance_level = 0 && block.level <> 0 then
          Common.a_link (string_of_int ( block.level - 1 ))
        else
          Common.a_link block.predecessor_hash
      in
      a ~a:href_pred  [
        span
          ~a:[
            a_class [ "glyphicon"; "glyphicon-triangle-left" ] ;
            aria_hidden () ] []
      ] in
  let body_attr = if is_snapshot then
      [ a_class [ panel_body; "snapshot-block"];
        a_title "this block is a snapshot" ]
  else [ a_class [ panel_body ] ] in
  let to_update =
    div ~a:[ a_id @@ Common.summary_id block.hash;
             a_class [ panel; panel_primary ]] [
      div ~a:[ a_class [ panel_heading ] ] [
        div ~a:[ a_class [ row ] ] [
          h3 ~a:[ a_class [ clg12; panel_title; "no-overflow" ] ] ([
            span ~a:[ a_class [ "nav-level" ] ] [
              link_pred ;
              txt @@ string_of_int block.level ;
              a ~a:[ a_id @@ succ_id block.hash; a_class [ "disabled" ] ] [
                span
                  ~a:[
                    a_class [ "glyphicon"; "glyphicon-triangle-right" ] ;
                    aria_hidden () ] []
              ]
            ] ;
            Lang.txt_t s_block_information ;
            Glossary_doc.(help HBlock) ;
          ] @ (if is_snapshot then [space_icon (); camera_icon ()] else []))
        ]
      ] ;
      div ~a:body_attr
        (make_summary_template ~block ~level_details block.hash ) ] in
  Manip.removeChildren container ;
  Manip.appendChild container to_update

let update_block_endorsements ~cycle hash operations =
  let cst = Infos.constants ~cycle in
  let container = find_component @@ endorsements_id hash in
  let ends = Common.get_endorsements operations in
  let slots = Common.make_slots cst.endorsers_per_block in
  let slots = Common.update_endorsements_slots hash slots ends in
  let rows = Common.make_endorsements_slots slots in
  let endorsements_numbers =
    List.fold_left (fun acc (_op_hash, _op_block_hash, endorse) ->
        let nslot = List.length endorse.endorse_slot in
        if endorse.endorse_block_hash = hash then
          (nslot + fst acc, nslot + snd acc)
        else (fst acc, nslot + snd acc)) (0,0) ends in
  let to_update_heading =
    div ~a:[ a_class [ panel_heading ] ] [
      h3 ~a:[ a_class [ panel_title ] ] [
        txt @@ spf "%s (%d/%d)"
          (Lang.s_ "Endorsements of this block")
          (fst endorsements_numbers) (snd endorsements_numbers);
        Glossary_doc.(help HEndorsement)
      ] ] in
  let to_update_body =
    div [
      div ~a:[ a_id @@ endorsements_id hash;
               a_class [ row; "endorsements-table" ] ] [
        div ~a:[ a_class [ clg12 ] ] [ rows ]
      ] ;
      Common.legend () ] in
  Manip.removeChildren container ;
  Manip.appendChild container to_update_heading ;
  Manip.appendChild container to_update_body

let tabs = [
  tr_tab, Bl_Txs ;
  del_tab, Bl_Del ;
  ori_tab, Bl_Ori ;
  endt_tab, Bl_Endt ;
  act_tab, Bl_Act
  (* revl_tab, Inactive *)
]


let make_tabs default =
  List.map (fun (tr, filter) -> tr, is_active default filter) tabs
  |> Tabs.(make_tabs Tabs)

let on_show filter tab =
  Tabs.set_on_show tab (fun _ ->
      Common.set_url_arg "default" ~default:(string_of_filter Bl_Txs)
      @@ string_of_filter filter
    )

let update_tabs () =
  List.iter (fun (t, filter) ->
      on_show filter t
    ) tabs

let make_page hash filters =
  let default = default_filter filters in

  let summary =
    div ~a:[ a_id @@ Common.summary_id hash ; a_class [ clg12 ] ] [
      div ~a:[ a_class [ panel; panel_primary ]] [
        div ~a:[ a_class [ panel_heading ] ] [
          div ~a:[ a_class [ row ] ] [
            h3 ~a:[ a_class [ clg9; csm9; cxs9; panel_title ] ] [
              span ~a:[ a_class [ "nav-level" ] ] [
                a ~a:[ a_id @@ succ_id hash; a_class [ "disabled" ] ] [
                  span
                    ~a:[
                      a_class [ "glyphicon"; "glyphicon-triangle-left" ] ;
                      aria_hidden () ] []
                ] ;
                a ~a:[ a_class [ "disabled" ] ] [
                  span
                    ~a:[
                      a_class [ "glyphicon"; "glyphicon-triangle-right" ] ;
                      aria_hidden () ] []
                ]
              ] ;
            ] ;
            Common.make_loading_gif [ "loading-heading"; clg3; csm3; cxs3 ]
          ]
        ] ;
        div ~a:[ a_class [ panel_body ] ] (make_block_summary hash) ]
    ] in

  let transactions = make_transaction_view default in
  let delegations = make_delegations_view default in
  let originations = make_originations_view default in
  let endorsements = make_endorsements_view default in
  let activations = make_activations_view default in
  (* let revelations = make_revelations_view () in *)

  let baker =
    div ~a:[ a_class [ panel; panel_primary ]] [
      div ~a:[ a_class [ panel_heading ] ] [
        h3 ~a:[ a_class [ panel_title ] ] [
          Lang.txt_t s_baker
        ]
      ] ;
      p ~a:[ a_id @@ baker_id hash ; a_class [ "baker" ; "no-overflow" ]]
        [ Common.txt_ () ]
    ] in

  let bl_endorsements =
    div ~a:[ a_id @@ endorsements_id hash ;
             a_class [ panel; panel_primary ]] [
      div ~a:[ a_class [ panel_heading ] ] [
        div ~a:[ a_class [ row ] ] [
          h3 ~a:[ a_class [ clg9; csm9; cxs9; panel_title ] ] [
            txt @@ spf "%s (%s)"
              (Lang.s_ "Endorsements") Common.bullshit_s
          ] ;
          Common.make_loading_gif [ "loading-heading"; clg3; csm3; cxs3 ]
        ]
      ] ;
      make_block_endorsements ()
    ] in

  let content =
    div ~a:[ a_class [ "block-div"; row ] ] [
      summary ;

      div ~a:[ a_class [ "block-operations" ] ] [
        div ~a:[ a_class [ clg9; cxs12 ] ] [
          make_tabs default;
          div ~a:[ a_class [ "tab-content" ] ] [
            transactions;

            delegations;

            originations;

            endorsements;

            (* revelations *)
            activations
          ]
        ];
        div ~a:[ a_class [ clg3; csm12; cxs12 ] ] [

          (* Account Details *)
          baker ;

          (* Delegatation Services *)
          bl_endorsements ;

        ]
      ]
    ]
  in
  content

let update_confirmation _bhash level nb_confirm =
  let confirm = find_component @@ Common.confirmation_blocks_id in
  Manip.replaceChildren confirm
    [ txt @@ string_of_int level ;
      txt @@ Printf.sprintf " (%d %s)" nb_confirm
        (Lang.s_ "block confirmations")]
