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
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Icon
open Data_types
open Lang
open Text
open Common

let accounts_id = "accounts"
let accounts_node_id = "accounts-node"
let account_id hash = make_id "accounts-hash" hash

let node_state ts = Node_state_ui.node_state_panel accounts_node_id ts

let update_details hash details =
  match details with
  | None ->
    let tr = find_component @@ account_id hash in
    Manip.addClass tr "danger" ;
    let td_hash =
      account_w_blockies_no_link ~txtaclass:[ "text-danger" ] hash in
    let td_manager =
      td [ span ~a:[ a_class [ "text-danger" ] ] [
          txt "Account does not exist" ] ] in
    let td_delegate = td [ txt_ () ] in
    let td_spendable = td [ txt_ () ] in
    let td_balance = td [ txt_ () ] in
    Manip.removeChildren tr ;
    Manip.appendChild tr td_hash ;
    Manip.appendChild tr td_manager ;
    Manip.appendChild tr td_delegate ;
    Manip.appendChild tr td_spendable ;
    Manip.appendChild tr td_balance
  | Some details ->
    node_state details.acc_node_timestamp;
    let name = details.acc_name in
    let tr = find_component @@ account_id name.tz in
    let td_hash = account_w_blockies ~crop_len:20 ~crop_limit:md_size name in
    let td_manager = account_w_blockies ~crop_len:20 ~crop_limit:md_size details.acc_manager in
    let td_delegate = match (snd details.acc_dlgt) with
      | Some d -> account_w_blockies ~crop_len:20 d
      | None ->
        td [txt (if (fst details.acc_dlgt) then
                      "--"
                    else "forbidden")]
    in
    let td_spendable = td [
        txt_t
          (if details.acc_spendable then s_yes else s_no) ] in
    let balance_data = Tez.pp_amount details.acc_balance in
    let td_balance = td [ balance_data ] in
    Manip.removeChildren tr ;
    Manip.appendChild tr td_hash ;
    Manip.appendChild tr td_manager ;
    Manip.appendChild tr td_delegate ;
    Manip.appendChild tr td_spendable ;
    Manip.appendChild tr td_balance

let columns () = tr [
    td ~a:[ a_class [ cxs3 ] ] @@ cl_icon account_icon (t_ s_hash);
    td ~a:[ a_class [ cxs3 ] ] @@ cl_icon manager_icon (t_ s_manager);
    td ~a:[ a_class [ cxs3 ] ] @@ cl_icon astronaut_icon (t_ s_delegate);
    td ~a:[ a_class [ cxs1 ] ] @@ cl_icon wallet_icon (t_ s_spendable);
    td ~a:[ a_class [ cxs2 ] ] @@ cl_icon Tez.icon (t_ s_balance);
  ]

module AccountsPanel =
  Panel.MakePageTable(struct
                  let name = "accounts"
                  let page_size = big_panel_number
                  let theads = columns
                  let title_span = Panel.title_nb s_accounts
                                                  ~help:Glossary_doc.HAccount
                  let table_class =  "default-table"
                end)

module ContractsPanel =
  Panel.MakePageTable(struct
                  let name = "contracts"
                  let page_size = big_panel_number
                  let theads = columns
                  let title_span = Panel.title_nb s_contracts
                                                  ~help:Glossary_doc.HAccount
                  let table_class =  "default-table"
                end)


let make_accounts ?(contract=false) () =
  let node_state_panel =
    div ~a:[  a_id accounts_node_id; a_class [ clg12 ] ] [
      div ~a:[ a_class [ "alert" ] ] [
        strong [ txt_t s_loading ]
      ]
    ] in
  let acc_panel =
    if contract then
      ContractsPanel.make_clg12 ~footer:true ()
    else
      AccountsPanel.make_clg12 ~footer:true ()
  in
  div [ node_state_panel; acc_panel ]

let update ?(contract=false) xhr_details nrows xhr_request =
  let to_rows accs =
    let res =
      List.map (fun acc ->
          let td_spendable =
            td [
              txt_t (if acc.account_spendable then
                        s_yes else s_no) ] in
          tr ~a:[ a_id @@ account_id acc.account_hash.tz][
            account_w_blockies
              ~crop_len:20 ~crop_limit:md_size acc.account_hash;
            if acc.account_manager = acc.account_hash then
              td [ txt "--" ] else
              account_w_blockies
                ~crop_len:20 ~crop_limit:md_size acc.account_manager;
            td [ txt (if acc.account_delegatable then "--" else "forbidden")] ;
            td_spendable;
            td [ txt_ () ] ;
          ]) accs
    in
    List.iter (fun acc -> xhr_details acc.account_hash.tz) accs;
    res in
  if contract then
    ContractsPanel.paginate_fun to_rows ~nrows xhr_request
  else
    AccountsPanel.paginate_fun to_rows ~nrows xhr_request
