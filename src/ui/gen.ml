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
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Color

open Data_types
open Lang
open Text

type col = Html_types.td Tyxml_js.Html5.elt

type cols_transaction =  {
  td_tx_block_hash : col ;
  td_tx_op_hash : col ;
  td_tx_src : col ;
  td_tx_dst : col ;
  td_tx_amount : col ;
  td_tx_counter : col ;
  td_tx_fee : col ;
  td_tx_gas_limit : col ;
  td_tx_storage_limit : col ;
  td_tx_parameters : col ;
  td_tx_failed : col ;
  td_tx_internal : col ;
  td_tx_burn : col ;
  td_tx_op_level : col ;
  td_tx_timestamp : col ;
}

type cols_origination = {
  td_or_block_hash : col ;
  td_or_op_hash : col ;
  td_or_src : col ;
  td_or_manager : col ;
  td_or_delegate : col ;
  (* td_or_script : col ; *)
  td_or_spendable : col ;
  td_or_delegatable : col ;
  td_or_balance : col ;
  td_or_counter : col ;
  td_or_fee : col ;
  td_or_gas_limit : col ;
  td_or_storage_limit : col ;
  td_or_tz1 : col ;
  td_or_failed : col ;
  td_or_internal : col ;
  td_or_burn : col ;
  td_or_op_level : col ;
  td_or_timestamp : col;
}

type cols_delegation = {
  td_del_block_hash : col ;
  td_del_op_hash : col ;
  td_del_src : col ;
  td_del_delegate : col ;
  td_del_counter : col ;
  td_del_fee : col ;
  td_del_gas_limit : col ;
  td_del_storage_limit : col ;
  td_del_failed : col ;
  td_del_internal : col ;
  td_del_op_level : col ;
  td_del_timestamp : col ;
}

type cols_seed_nonce_revelation = {
  td_seed_block_hash : col ;
  td_seed_op_hash : col ;
  td_seed_level : col ;
  td_seed_nonce : col ;
}

type cols_activation = {
  td_act_block_hash : col ;
  td_act_op_hash : col ;
  td_act_pkh : col ;
  td_act_secret : col ;
  td_act_op_level : col ;
  td_act_timestamp : col ;
}

type cols_endorsement = {
  td_endorse_op_hash : col ;
  td_endorse_src : col ;
  td_endorse_block_hash : col ;
  td_endorse_block_level : col;
  td_endorse_slot : col ;
  td_endorse_op_level : col ;
  td_endorse_priority : col ;
  td_endorse_timestamp : col ;
}

let crop_len_tz = 20
let crop_len_hash = 15

let from_string ?(link=false) ?crop_len ?crop_limit ?args ?aclass ?path s =
  if link then
    td [ Common.make_link ?crop_len ?crop_limit ?args ?aclass ?path s ]
  else
    td [ txt_s s ]

(* TODO faire un truc spécifique ? *)
let from_timestamp t =
  td [ Format_date.auto_updating_timespan t ]

let from_int i =
  td [ txt @@ string_of_int i ]

let from_int32 i32 =
  td [ txt @@ Int32.to_string i32 ]

let from_int64 i64 =
  td [ txt @@ Int64.to_string i64 ]

let from_z z =
  td [ txt @@ Z.to_string z ]

let from_bool cond =
  from_string @@ string_of_bool cond

let from_amount ?precision ?width ?order i64 =
  td [ Tez.pp_amount ?precision ?width ?order i64 ]

(* Blockies *)
let from_tz1 ?scale ?aclass ?before ?after ?crop_len ?crop_limit ?args account =
  Common.account_w_blockies
    ?scale ?aclass ?before ?after ?crop_len ?crop_limit ?args account

let from_michelson_parameter =
  function
  | None -> from_string "No"
  | Some param ->
    let template =
      Printf.sprintf "<div class=\"%s\">%s</div>"
        (String.concat "\" \"" [ ])
        (Common.html_escaped param) in
    td [
      a ~a:[ Bootstrap_helpers.Attributes.a_data_toggle "popover";
             Bootstrap_helpers.Attributes.a_data_placement `Top;
             Bootstrap_helpers.Attributes.a_data_html true;
             Bootstrap_helpers.Attributes.a_data_trigger "focus";
             Bootstrap_helpers.Attributes.a_role "button";
             a_tabindex 0;
             to_attrib @@ Tyxml_js.Xml.string_attrib "container" "body" ;
             Bootstrap_helpers.Attributes.a_data_content template;
           ] [
        txt_t s_yes ]
    ]

let is_internal internal = if not internal then [] else [ "warning" ]

let td_failed cond =
  if cond then
    td ~a:[ a_title "Fail" ; a_class [ red ] ] [ cross_icon () ]
  else
    td ~a:[ a_title "Success" ; a_class [ green ] ] [ right_icon () ]

let td_fee internal fee =
  if internal then
    td [ Common.txt_ () ]
  else
    from_amount ~precision:2 fee

let td_internal internal =
  from_string @@ if internal then "Yes" else "No"

let td_block_hash bhash level =
  if bhash = Utils.pending_block_hash then td [ txt_t s_pending ]
  else
    td [ Common.make_link ~path:bhash @@ string_of_int level]

(* Generates rows for all table *)
let from_transaction
    ?(crop_len_tz=crop_len_tz) ?(crop_len_hash=crop_len_hash)
    block_hash op_hash transaction =
  let td_tx_block_hash =
    td_block_hash block_hash transaction.tr_op_level in
  let td_tx_op_hash =
    from_string
      ~args:["block_hash", block_hash]
      ~link:true
      ~crop_len:crop_len_hash op_hash in
  let td_tx_src = from_tz1 ~crop_len:crop_len_tz transaction.tr_src in
  let td_tx_failed = td_failed transaction.tr_failed in
  let td_tx_dst = from_tz1 ~crop_len:crop_len_tz transaction.tr_dst in
  let td_tx_amount = from_amount transaction.tr_amount in
  let td_tx_counter = from_int32 transaction.tr_counter in
  let td_tx_fee = td_fee transaction.tr_internal transaction.tr_fee in
  let td_tx_gas_limit = from_z transaction.tr_gas_limit in
  let td_tx_storage_limit = from_z transaction.tr_storage_limit in
  let td_tx_internal = td_internal transaction.tr_internal in
  let td_tx_burn = from_amount transaction.tr_burn in
  let td_tx_parameters = from_michelson_parameter transaction.tr_parameters in
  let td_tx_op_level = from_int transaction.tr_op_level in
  let td_tx_timestamp = from_timestamp transaction.tr_timestamp in
  {
    td_tx_block_hash ;
    td_tx_op_hash ;
    td_tx_src ;
    td_tx_dst ;
    td_tx_amount ;
    td_tx_counter ;
    td_tx_fee ;
    td_tx_gas_limit ;
    td_tx_storage_limit ;
    td_tx_parameters ;
    td_tx_failed ;
    td_tx_internal ;
    td_tx_burn ;
    td_tx_op_level ;
    td_tx_timestamp ;
  }

let from_origination
    ?(crop_len_tz=crop_len_tz) ?(crop_len_hash=crop_len_hash)
    block_hash op_hash source orig =
  let td_or_block_hash = td_block_hash block_hash orig.or_op_level in
  let td_or_op_hash =
    from_string ~link:true ~crop_len:crop_len_hash op_hash in
  let td_or_tz1 = from_tz1 ~crop_len:crop_len_tz orig.or_tz1 in
  let td_or_balance = from_amount orig.or_balance in
  let td_or_src = from_tz1 ~crop_len:crop_len_tz source in
  let td_or_manager = from_tz1 ~crop_len:crop_len_tz orig.or_manager in
  let td_or_delegate =
    if orig.or_delegate.tz <> "" then
      from_tz1 ~crop_len:crop_len_tz orig.or_delegate
    else
      from_string "No delegate" in
  let td_or_fee = td_fee orig.or_internal orig.or_fee in
  let td_or_internal = td_internal orig.or_internal in
  let td_or_burn = from_amount orig.or_burn in
  let td_or_gas_limit = from_z orig.or_gas_limit in
  let td_or_storage_limit = from_z orig.or_storage_limit in
  let td_or_failed = td_failed orig.or_failed in
  let td_or_counter = from_int32 orig.or_counter in
  let td_or_spendable = from_bool orig.or_spendable in
  let td_or_delegatable = from_bool orig.or_delegatable in
  let td_or_op_level = from_int orig.or_op_level in
  let td_or_timestamp = from_timestamp orig.or_timestamp in
  {
    td_or_block_hash ;
    td_or_op_hash ;
    td_or_src ;
    td_or_manager ;
    td_or_delegate ;
    td_or_spendable ;
    td_or_delegatable ;
    td_or_balance ;
    td_or_counter ;
    td_or_fee ;
    td_or_gas_limit ;
    td_or_storage_limit ;
    td_or_tz1 ;
    td_or_failed ;
    td_or_internal ;
    td_or_burn ;
    td_or_op_level ;
    td_or_timestamp ;
  }

let from_delegation
    ?(crop_len_tz=crop_len_tz) ?(crop_len_hash=crop_len_hash)
    ?(crop_limit)
    block_hash op_hash source del =
  let td_del_block_hash =
    td_block_hash block_hash del.del_op_level in
  let td_del_op_hash = from_string ~link:true ~crop_len:crop_len_hash op_hash in
  let td_del_internal = td_internal del.del_internal in
  let td_del_delegate =
    if del.del_delegate.tz = "" then
      from_string "unset"
    else
      from_tz1 ~crop_len:crop_len_tz ?crop_limit del.del_delegate in
  let td_del_src = from_tz1 ~crop_len:crop_len_tz ?crop_limit source in
  let td_del_counter = from_int32 del.del_counter  in
  let td_del_fee = td_fee del.del_internal del.del_fee in
  let td_del_gas_limit = from_z del.del_gas_limit in
  let td_del_storage_limit = from_z del.del_storage_limit in
  let td_del_failed = td_failed del.del_failed in
  let td_del_op_level = from_int del.del_op_level in
  let td_del_timestamp = from_timestamp del.del_timestamp in
  {
    td_del_block_hash ;
    td_del_op_hash ;
    td_del_src ;
    td_del_delegate ;
    td_del_counter ;
    td_del_fee ;
    td_del_gas_limit ;
    td_del_storage_limit ;
    td_del_failed ;
    td_del_internal ;
    td_del_op_level ;
    td_del_timestamp ;
  }

let from_seed_nonce_revelation block_hash op_hash nonce =
  let td_seed_block_hash = from_string ~link:true block_hash in
  let td_seed_op_hash = from_string ~link:true op_hash in
  let td_seed_level = from_int nonce.seed_level in
  let td_seed_nonce = from_string nonce.seed_nonce in
  {
    td_seed_block_hash ;
    td_seed_op_hash ;
    td_seed_level ;
    td_seed_nonce ;
  }

let from_activation
    ?(crop_len_hash=crop_len_hash)
    block_hash op_hash act =
  let td_act_block_hash =
    from_string ~link:true ~crop_len:crop_len_hash block_hash in
  let td_act_op_hash = from_string ~link:true ~crop_len:crop_len_hash op_hash in
  let td_act_pkh = from_tz1 act.act_pkh in
  let td_act_secret = from_string act.act_secret in
  let td_act_op_level = from_int act.act_op_level in
  let td_act_timestamp = from_timestamp act.act_timestamp in
  {
    td_act_block_hash ;
    td_act_op_hash ;
    td_act_pkh ;
    td_act_secret ;
    td_act_op_level ;
    td_act_timestamp ;
  }

let from_endorsement
    ?(crop_len_tz=crop_len_tz) ?(crop_len_hash=crop_len_hash)
    ?(crop_limit)
    op_hash endorse =
  let td_endorse_op_hash =
    from_string ~link:true ~crop_len:crop_len_hash op_hash in
  let td_endorse_block_level = from_int endorse.endorse_block_level in
  let td_endorse_src =
    from_tz1 ?crop_limit ~crop_len:crop_len_tz endorse.endorse_src in
  let td_endorse_slot =
    td [ txt @@
         String.concat ", " @@
         List.map string_of_int endorse.endorse_slot ] in
  let td_endorse_block_hash =
    from_string
      ~link:true
      ~crop_len:crop_len_hash
      endorse.endorse_block_hash in
  let td_endorse_op_level = from_int endorse.endorse_op_level in
  let td_endorse_priority = from_int endorse.endorse_priority in
  let td_endorse_timestamp = from_timestamp endorse.endorse_timestamp in
  {
    td_endorse_op_hash ;
    td_endorse_src ;
    td_endorse_block_hash ;
    td_endorse_block_level ;
    td_endorse_slot ;
    td_endorse_op_level ;
    td_endorse_priority ;
    td_endorse_timestamp ;
  }
