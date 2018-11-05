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
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Bootstrap_helpers.Table
open Bootstrap_helpers.Color
open Bootstrap_helpers.Align
open Lang (* s_ *)
open Tezos_types
open Text

let activation_alert_id = "activation_alert"

let level_id op_hash op_block_hash index =
  let id = Printf.sprintf "%s-%s-%i" op_hash op_block_hash index in
  Common.make_id "level" id

let timestamp_id op_hash op_block_hash index =
  let id = Printf.sprintf "%s-%s-%i" op_hash op_block_hash index in
  Common.timestamp_id id

let account_id tz1 = Common.make_id "account" tz1

let update_timestamp op_hash op_block_hash index timestamp =
  let ts_span = find_component @@ timestamp_id op_hash op_block_hash index in
  Manip.replaceChildren ts_span [Format_date.auto_updating_timespan timestamp]

let update_level op_hash op_block_hash index level =
  let lvl_span = find_component @@ level_id op_hash op_block_hash index in
  Manip.replaceChildren lvl_span
    [ a ~a:( Common.a_link op_block_hash ) [ pcdata @@ string_of_int level ]  ]

let cl_title = Common.responsive_column_title
let cl_icon = Common.responsive_title

(* GenericOpsDisplay: generic functor to display tables of differents
   operations (both 'pending' and finale *)
module GenericOpsDisplay
    (Arg : sig
       val name : string
       val help_doc : Glossary_doc.helpers
       val theads : unit -> [> Html_types.tr ] Tyxml_js.Html5.elt
       val to_rows : Data_types.operation list ->
         [> Html_types.tr ] Tyxml_js.Html5.elt list
     end) :
sig

  val make :
    ?pending:bool -> unit -> [> Html_types.div ] Tyxml_js.Html5.elt

  val update :
    pending:bool ->
    nrows:int ->
    (int -> int -> (Data_types.operation list -> unit) -> unit) -> unit

end = struct

  module Shared = struct
    let title_span name = Panel.title_nb name ~help:Arg.help_doc
    let theads = Arg.theads
    let page_size = Common.big_panel_number
    let table_class = "transactions-table"
  end

  module Final =
    Panel.MakePageTable
      (struct
        include Shared
        let name = Arg.name
        let s_name = ss_ name
        let title_span = title_span s_name
      end)

  module Pending =
    Panel.MakePageTable
      (struct
        include Shared
        let name = Printf.sprintf "Pending %s" Arg.name
        let s_name = ss_ name
        let title_span = title_span s_name
      end)

  let make ?(pending=false) () =
    if pending then Pending.make_clg12 ~footer:true ()
    else Final.make_clg12 ~footer:true ()

  let update ~pending ~nrows xhr =
    let xhr page page_size cb =
      xhr page page_size
        (fun d -> cb d;
          ignore (Js.Unsafe.eval_string
                    "jQuery('[data-toggle=\"popover\"]').popover();")) in
    if pending then Pending.paginate_fun Arg.to_rows ~nrows xhr
    else Final.paginate_fun Arg.to_rows ~nrows xhr

end
(* end of GenericOpsDisplay *)


(* Transactions panels *)
module Transactions =
  GenericOpsDisplay
    (struct
      let name = "Transactions"
      let help_doc = Glossary_doc.HTransaction

      let theads () =
        tr [
          th @@ cl_icon exchange_icon (s_ "Txn. Hash") ;
          th @@ cl_icon cube_icon (s_ "Block") ;
          th @@ cl_icon clock_icon (s_ "Age");
          th @@ cl_icon account_icon (s_ "From") ;
          th @@ cl_icon right_icon (s_ "") ;
          th @@ cl_icon account_icon (s_ "To") ;
          th @@ cl_icon Tez.icon (s_ "Amount") ;
          th @@ cl_icon bill_icon (s_ "Fee") ;
          th @@ cl_icon params_icon (s_ "Param.") ;
          th @@ cl_icon folder_icon (s_ "Internal") ;
        ]

      let to_rows txs =
        List.map
          (fun (op_hash, op_block_hash, src, transaction) ->
            let src, arrow, dst =
              Common.account_w_blockies src,
              (if transaction.tr_failed then
                 td ~a:[ a_title "Fail" ; a_class [ red; text_center ] ]
                   [ cross_icon () ]
               else
               if src <> transaction.tr_dst then
                 td ~a:[ a_class [ green; text_center ] ] [ right_icon () ]
               else
                 td ~a:[ a_class [ blue; text_center ] ] [ right_left_arrow_icon () ]),
              Common.account_w_blockies transaction.tr_dst
            in
            let param = match transaction.tr_parameters with
                None -> td [ pcdata_t s_no ]
              | Some p ->
                let template=
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
            let internal_td, cls_internal =
              if not transaction.tr_internal then td [ pcdata_t s_no ], []
              else td [ pcdata_t s_yes ], [ "warning" ] in
            tr ~a:[ a_class (Common.failed_class transaction.tr_failed @ cls_internal)] [
              td [ Common.make_link op_hash ] ;
              td [ Common.make_link ~path:op_block_hash
                   @@ string_of_int transaction.tr_op_level ] ;
              td [ Format_date.auto_updating_timespan transaction.tr_timestamp ] ;
              src ;
              arrow ;
              dst ;
              td [ Tez.pp_amount transaction.tr_amount ] ;
              td [ if transaction.tr_internal
                   then pcdata Common.bullshit_s
                   else Tez.pp_amount transaction.tr_fee ] ;
              param ;
              internal_td
            ]
          ) @@ Common.get_transactions txs

    end)
(* end of Transactions panels *)

(* Activations panels *)
module Activations =
  GenericOpsDisplay
    (struct
      let name = "Activations"
      let help_doc = Glossary_doc.HTransaction

      let theads() =
        tr [
          th @@ cl_icon arrow_up_icon (s_ "Activation Hash") ;
          th @@ cl_icon cube_icon (s_ "Block") ;
          th @@ cl_icon clock_icon (s_ "Age");
          th @@ cl_icon account_icon (s_ "Tz1");
          th @@ cl_icon secret_icon (s_ "Secret");
        ]

      let to_rows acts =
        List.mapi (fun i (op_hash, op_block_hash, activation) ->
            Common.timestamp
              op_block_hash (update_timestamp op_hash op_block_hash i);
            Common.level
              op_block_hash (update_level op_hash op_block_hash i);
            let pkh = Common.account_w_blockies activation.act_pkh in
            let secret = td [ pcdata activation.act_secret ] in
            let lvl, timestamp =
              if op_block_hash <> Utils.pending_block_hash then
                Text.s_bullshit, Text.s_bullshit
              else
                Text.s_pending, Text.s_pending
            in
            tr [
              td [ Common.make_link op_hash ] ;
              td ~a:[ a_id @@ level_id op_hash op_block_hash i ]
                [ pcdata_t lvl
                (* Common.make_link op.op_block_hash *) ] ;
              td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ]
                [ pcdata_t timestamp ] ;
              pkh ;
              secret
            ]) @@ Common.get_activations acts

    end)
(* end of Activations panels *)

(* Endorsements panels *)
module Endorsements =
  GenericOpsDisplay
    (struct
      let name = "Endorsements"
      let help_doc = Glossary_doc.HEndorsement

      let theads () =
        tr [
          th @@ cl_icon stamp_icon (s_ "Endorsement Hash") ;
          th @@ cl_icon cube_icon (s_ "Block") ;
          th @@ cl_icon clock_icon (s_ "Age") ;
          th @@ cl_icon account_icon (s_ "Endorser") ;
          th @@ cl_icon slots_icon (s_ "Slots") ;
          th @@ cl_icon check_icon (s_ "Endorsed Block") ;
          th @@ cl_icon priority_icon (s_ "Priority") ;
        ]

      let to_rows txs =
        List.map
          (fun (op_hash, op_block_hash, endorse) ->
             tr [
               td [ Common.make_link op_hash ] ;
               td [ Common.make_link ~path:op_block_hash
                    @@ string_of_int endorse.endorse_op_level] ;
               td [ Format_date.auto_updating_timespan endorse.endorse_timestamp] ;
               Common.account_w_blockies endorse.endorse_src;
               td [ pcdata @@
                    String.concat ", " @@
                    List.map string_of_int endorse.endorse_slot ] ;
               td [ Common.make_link endorse.endorse_block_hash ] ;
               td [ pcdata @@ string_of_int endorse.endorse_priority ] ;
               ]) @@ Common.get_endorsements txs
    end)
(* end of Endorsements panels *)

(* Delegations panels *)
module Delegations =
  GenericOpsDisplay
    (struct
      let name = "Delegations"
      let help_doc = Glossary_doc.HDelegation

      let theads () =
        tr [
          th @@ cl_icon handshake_icon (s_ "Del. Hash") ;
          th @@ cl_icon cube_icon (s_ "Block") ;
          th @@ cl_icon clock_icon (s_ "Age");
          th @@ cl_icon account_icon (s_ "Account To Delegate");
          th @@ cl_icon right_icon (s_ "") ;
          th @@ cl_icon astronaut_icon (s_ "New Delegate") ;
          th @@ cl_icon bill_icon (s_ "Fee") ;
          th @@ cl_icon folder_icon (s_ "Internal") ;
        ]

      let to_rows txs =
        List.mapi (fun i (op_hash, op_block_hash, src, del) ->
            Common.timestamp
              op_block_hash (update_timestamp op_hash op_block_hash i);
            Common.level
              op_block_hash (update_level op_hash op_block_hash i);
            let lvl, timestamp =
              if op_block_hash <> Utils.pending_block_hash then
                Text.s_bullshit, Text.s_bullshit
              else
                Text.s_pending, Text.s_pending
            in
            let td_delegate, td_arrow = match del.del_delegate with
              | {tz = ""; _ } -> td [ pcdata "unset" ],
                      td ~a:[ a_class [ blue ; "center" ] ]
                        [ span ~a:[ a_class [ "fa"; "fa-arrow-down" ] ] [] ]
              | _ -> Common.account_w_blockies del.del_delegate,
                     td ~a:[ a_class [ green ; text_center ] ] [ right_icon () ] in
            let internal_td, cls_internal =
              if not del.del_internal then td [ pcdata_t s_no ], []
              else td [ pcdata_t s_yes ], [] in
            tr ~a:[ a_class (Common.failed_class del.del_failed @ cls_internal)] [
              td [ Common.make_link op_hash ] ;
              td ~a:[ a_id @@ level_id op_hash op_block_hash i ]
                [ pcdata_t lvl ] ;
              td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ]
                [ pcdata_t timestamp ] ;
              Common.account_w_blockies src ;
              td_arrow ;
              td_delegate ;
              td [ if del.del_internal
                   then pcdata Common.bullshit_s else Tez.pp_amount del.del_fee ] ;
              internal_td
            ]) @@ Common.get_delegations txs

    end)
(* end of Delegations panels *)

(* Delegations panels *)
module Originations =
  GenericOpsDisplay
    (struct
      let name = "Originations"
      let help_doc = Glossary_doc.HOrigination

      let theads () =
        tr [
          th @@ cl_icon link_icon (s_ "Op. Hash") ;
          th @@ cl_icon cube_icon (s_ "Block") ;
          th @@ cl_icon clock_icon (s_ "Age");
          th @@ cl_icon account_icon "New Acc." ;
          th @@ cl_icon Tez.icon "New balance" ;
          th @@ cl_icon originator_icon "Origin" ;
          th @@ cl_icon manager_icon (s_ "Manager") ;
          th @@ cl_icon bill_icon (s_ "Fee") ;
          th @@ cl_icon burn_icon (s_ "Burn");
          th @@ cl_icon folder_icon (s_ "Internal") ;
        ]

      let to_rows txs =
        List.mapi (fun i (op_hash, op_block_hash, src, orig) ->
            Common.timestamp
              op_block_hash (update_timestamp op_hash op_block_hash i);
            Common.level
              op_block_hash (update_level op_hash op_block_hash i);
            let lvl, timestamp =
              if op_block_hash <> Utils.pending_block_hash then
                Text.s_bullshit, Text.s_bullshit
              else
                Text.s_pending, Text.s_pending
            in
            let burn = orig.or_burn in
            let internal_td, cls_internal =
              if not orig.or_internal then td [ pcdata_t s_no ], []
              else td [ pcdata_t s_yes ], [ "warning" ] in
            tr ~a:[ a_class (Common.failed_class orig.or_failed @ cls_internal)] [
              td [ Common.make_link op_hash ] ;
              td ~a:[ a_id @@ level_id op_hash op_block_hash i ] [
                pcdata_t lvl ] ;
              td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ] [
                pcdata_t timestamp ] ;
              Common.account_w_blockies orig.or_tz1 ;
              td [ Tez.pp_amount orig.or_balance ] ;
              Common.account_w_blockies src;
              Common.account_w_blockies orig.or_manager ;
              td [ if orig.or_internal
                   then pcdata_t Text.s_bullshit else Tez.pp_amount orig.or_fee ] ;
              td [ Tez.pp_amount burn ] ;
              internal_td
            ]) @@ Common.get_originations txs

    end)
(* end of Origination panels *)




module Nonces = struct

  module NoncesPanel =
    Panel.MakePageNoTable(struct
      let name = "nonces"
      let page_size = 10
      let title_span = Panel.title_nb s_nonces
    end)

  let nb_revelations ~cst =
    cst.blocks_per_cycle / cst.blocks_per_commitment

  let generate_nonces ~cst ops =
    let revealed i =
      let tmp = List.find_opt (fun (lvl, _op) ->
          ((lvl - 1) mod cst.blocks_per_cycle) / cst.blocks_per_commitment = i) ops in
      match tmp with
      | None ->
        td ~a:[a_class [cxs2; "slot-gray"]] [ div [ space_icon () ] ]
      | Some (_lvl, op) ->
        td ~a:[a_class [cxs2; "slot-green"]] [
          a ~a:( Common.a_link op ) [ div [ space_icon () ] ] ] in

    let nonces = Array.init (nb_revelations ~cst) revealed in
    div ~a:[ a_class [ row ; "nonces" ] ] [
      tablex ~a:[ a_class [ btable; btable_bordered; clg12 ] ]
        [ tbody [ tr (Array.to_list nonces) ] ]
    ]

  let make () =
    div ~a:[ a_id "nonces-div" ] [
      NoncesPanel.make ~footer:true ()
    ]

  let to_rows nonces_list =
    List.map (fun (cycle, nonces) ->
        let cst = Infos.constants ~cycle in
        let ops = List.fold_left (fun acc (op_hash, levels) ->
            acc @ List.map (fun lvl -> (lvl, op_hash)) levels) [] nonces in
        make_panel
          ~panel_title_content:(
            div ~a:[ a_class [ panel_title ] ] [
              pcdata @@ Printf.sprintf "%.0f%% %s %d"
                (float_of_int (List.length ops) /.
                 float_of_int (nb_revelations ~cst) *. 100.)
                (t_ s_nonces_revelation_for_cycle)
                cycle ])
          ~panel_body_content:[ generate_nonces ~cst ops ]
          ()
      ) nonces_list

  let update_nonces ?nrows xhr =
    NoncesPanel.paginate_fun to_rows ?nrows xhr

end

(* Double_Bakings panels *)
module Double_Bakings =
  GenericOpsDisplay
    (struct
      let name = "Double Bakings Evidence"
      let help_doc = Glossary_doc.HDouble_Baking

      let theads () =
        tr [
          th ~a:[ a_class [ ] ] @@ cl_icon cookie_icon (s_ "Operation Hash");
          th ~a:[ a_class [ ] ] @@ cl_icon cube_icon (s_ "Level");
          th ~a:[ a_class [ ] ] @@ cl_icon clock_icon (s_ "Age");
          th ~a:[ a_class [ ] ] @@ cl_icon cookie_icon (s_ "Baker");
          th ~a:[ a_class [ ] ] @@ cl_icon bill_icon (s_ "Baker Rewards");
          th ~a:[ a_class [ ] ] @@ cl_icon cookie_icon (s_ "Offender");
          th ~a:[ a_class [ ] ] @@ cl_icon cube_icon (s_ "Denounced Level");
          th ~a:[ a_class [ ] ] @@ cl_icon deposit_icon (s_ "Lost Deposits");
          th ~a:[ a_class [ ] ] @@ cl_icon cube_icon (s_ "Lost Rewards");
          th ~a:[ a_class [ ] ] @@ cl_icon bill_icon (s_ "Lost Fees");
        ]

      let to_rows txs =
        List.mapi (fun i (op_hash, op_block_hash, dbe) ->
            Common.timestamp op_block_hash (update_timestamp op_hash op_block_hash i);
            Common.level op_block_hash (update_level op_hash op_block_hash i);
            let lvl, timestamp =
              if op_block_hash <> Utils.pending_block_hash then
                Text.s_bullshit, Text.s_bullshit
              else
                Text.s_pending, Text.s_pending
            in
            let data =
              let level = string_of_int dbe.double_baking_header1.header_level in
              let path = "heads" in
              let args = [ "level", level ] in
              Common.make_link level ~path ~args in
            tr [
              td [ Common.make_link op_hash ] ;
              td ~a:[ a_id @@ level_id op_hash op_block_hash i ] [
                pcdata_t lvl] ;
              td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ] [
                pcdata_t timestamp ] ;
              Common.account_w_blockies ~crop_len:15 dbe.double_baking_denouncer ;
              td [ Tez.pp_amount ~precision:2 dbe.double_baking_gain_rewards ] ;
              Common.account_w_blockies ~crop_len:15 dbe.double_baking_accused ;
              td [ data ] ;
              td [ Tez.pp_amount ~precision:2 dbe.double_baking_lost_deposit ] ;
              td [ Tez.pp_amount ~precision:2 dbe.double_baking_lost_rewards ] ;
              td [ Tez.pp_amount ~precision:2 dbe.double_baking_lost_fees ]
            ]) @@ Common.get_double_baking_evidence txs

    end)
(* end of Double_Bakings panels *)

(* Double_Endorsements panels *)
module Double_Endorsements =
  GenericOpsDisplay
    (struct
      let name = "Double Endorsements"
      let help_doc = Glossary_doc.HDouble_Endorsement

      let theads () =
        tr [
          th ~a:[ a_class [ cxs3 ] ] @@ cl_icon stamp_icon "Hash"  ;
          th ~a:[ a_class [ cxs1 ] ] @@ cl_icon cube_icon "Block" ;
          th ~a:[ a_class [ cxs2 ] ] @@ cl_icon clock_icon "Age";
          th ~a:[ a_class [ cxs2 ] ] @@ cl_title "Level" "Level" ;
          th ~a:[ a_class [ cxs2 ] ] @@ cl_title "Block1" "Block1" ;
          th ~a:[ a_class [ cxs2 ] ] @@ cl_title "Block2" "Block2" ;
        ]

      let to_rows txs =
        List.mapi (fun i (op_hash, op_block_hash, dee) ->
            Common.timestamp
              op_block_hash (update_timestamp op_hash op_block_hash i);
            Common.level
              op_block_hash (update_level op_hash op_block_hash i);
            let lvl, timestamp =
              if op_block_hash <> Utils.pending_block_hash then
                Text.s_bullshit, Text.s_bullshit
              else
                Text.s_pending, Text.s_pending
            in
            let endorse1 = Common.get_endorsement_op_type dee.double_endorsement1 in
            let endorse2 = Common.get_endorsement_op_type dee.double_endorsement2 in
            match endorse1, endorse2 with
            | Some endorse1, Some endorse2 ->
              let level = string_of_int endorse1.endorse_block_level in
              let level_link =
                a ~a:( Common.a_link op_block_hash ) [ pcdata level ] in
              let block1_link =
                Common.make_link endorse1.endorse_block_hash in
              let block2_link =
                Common.make_link endorse2.endorse_block_hash in
              tr [
                td [ Common.make_link op_hash ] ;
                td ~a:[ a_id @@ level_id op_hash op_block_hash i ] [
                  pcdata_t lvl] ;
                td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ] [
                  pcdata_t timestamp ] ;
                td [ level_link ] ;
                td [ block1_link ] ;
                td [ block2_link ] ;
              ]
            | _, _ ->
              tr [
                td [ Common.make_link op_hash ] ;
                td ~a:[ a_id @@ level_id op_hash op_block_hash i ] [
                  pcdata_t s_cant_recover ] ;
                td ~a:[ a_id @@ timestamp_id op_hash op_block_hash i ] [
                  pcdata_t s_cant_recover ] ;
                td [ pcdata_t s_cant_recover ] ;
                td [ pcdata_t s_cant_recover ] ;
                td [ pcdata_t s_cant_recover ] ;
              ]) @@ Common.get_double_endorsements_evidence txs

    end)
(* end of Double_Endorsements panels *)

let make_activation_alert () =
  div ~a:[ a_id activation_alert_id; a_class [ clg12 ] ] [
    div ~a:[ a_class [ "alert" ] ] [
      strong [ pcdata_t s_loading ]
    ]
  ]

let update_activation_alert balances =
  let container = find_component activation_alert_id in
  let content =
    div ~a:[ a_class [ "alert"; "alert-info" ] ] [
      strong [ pcdata (Printf.sprintf "%s: "
                         (t_ s_total_activated_balances)) ] ;
      Tez.approx_amount balances ;
      pcdata @@ Printf.sprintf " (%Ld%% %s)"
          (Int64.div (Int64.mul balances 100L)
             Infos.api.api_config.conf_ico.ico_contributors_tokens)
          (t_ s_of_total_allocations);
    ] in
  Manip.removeChildren container;
  Manip.appendChild container content
