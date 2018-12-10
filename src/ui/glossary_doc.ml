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
open Js_utils
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Button
open Bootstrap_helpers.Modal

let make_doc () =
  match Manip.by_id "glossary" with
  | Some _ -> ()
  | None ->
    let left = Dom_html.createDiv Dom_html.window##document in
    left##className <- Js.string clg9 ;
    List.iter (fun (file, content) ->
        let section_title =
          Printf.sprintf "#%s\n"
            (String.concat " " @@
             (String.split_on_char '_' @@
              (String.capitalize_ascii (Filename.remove_extension file)))) in
        let content = section_title ^ content in
        let doc_html = Omd.(to_html @@ of_string content) in
        let doc = Dom_html.createDiv Dom_html.window##document in
        doc##id <- Js.string @@ Filename.remove_extension file ;
        doc##innerHTML <- Js.string (doc_html) ;
        Dom.appendChild left doc) Glossary.files ;
    let right =
      div ~a:[ a_class [ clg3 ] ] [
        ul ~a:[ a_class [ "list-group" ] ] @@
        List.map (fun (file, _ ) ->
            let section_title =
              (String.concat " " @@
               (String.split_on_char '_' @@
                (String.capitalize_ascii (Filename.remove_extension file)))) in
            let section_name = Filename.remove_extension file in
            li ~a:[ a_class [ "list-group-item" ] ] [
              a ~a:[ a_href ("#" ^ section_name) ] [
                pcdata section_title
              ]
            ])
          Glossary.files ] in
    let container =
      div ~a:[ a_id "glossary"; a_class [ row ] ] [
        Tyxml_js.Of_dom.of_div left ;
        right ] in

    Common.update_main_content container

type helpers =
  | HBlock | HOperation | HTransaction | HEndorsement | HOrigination
  | HDelegation | HNonce | HAccount | HAlternatives
  | HProtocol | HAmendment
  | HNetwork | HApi | HBaker | HBalance | HSBalance | HInject
  | HDouble_Baking | HDouble_Endorsement | HBalance_Updates
  | HBalance_Snapshot

let helpers_file = function
  | HBlock ->   "blocks.md"
  | HOperation -> "operations.md"
  | HTransaction -> "transactions.md"
  | HEndorsement -> "endorsements.md"
  | HOrigination -> "originations.md"
  | HDelegation -> "delegations.md"
  | HNonce -> "nonce_revelations.md"
  | HAccount -> "accounts.md"
  | HProtocol -> "protocols.md"
  | HAlternatives -> "alternative_heads.md"
  | HAmendment -> "amendments.md"
  | HNetwork -> "network.md"
  | HApi -> "api.md"
  | HBaker -> "baker.md"
  | HInject -> "inject.md"
  | HBalance -> "balance.md"
  | HSBalance -> "staking_balance.md"
  | HDouble_Baking -> "double_baking.md"
  | HDouble_Endorsement -> "double_endorsement.md"
  | HBalance_Updates -> "balance_updates.md"
  | HBalance_Snapshot -> "balance_snapshot.md"

let help t =
  let file = helpers_file t in
  let _, content =
    try
      List.find (fun (f, _content) -> file = f) Glossary.files
    with Not_found ->
      Printf.kprintf failwith "Glossary_doc: help for %s not found" file
  in

  let doc_html = Omd.(to_html @@ of_string content) in
  let section = Filename.remove_extension file in
  let id = section ^ "-help" in
  let id_title = section ^ "-title" in
  (* Now we can replace _ by space and capitalize the first char *)
  let section =
    String.concat " " @@ String.split_on_char '_' (String.capitalize_ascii section) in
  let link =
    let toggle = a_user_data "toggle" modal in
    let target = a_user_data "target" ("#" ^ id) in
    button ~a:[ a_class [ "glossary-help-icon" ]; toggle; target ] [ pcdata " ?" ] in
  let container =
    let dismiss = a_user_data "dismiss" modal in
    let aria_hidden = Bootstrap_helpers.Attributes.a_aria "hidden" "true" in
    let aria_label = Bootstrap_helpers.Attributes.a_aria "label" "Close" in
    let aria_labelledby = Bootstrap_helpers.Attributes.a_aria "labelledby" id_title in
    let main_content =
      let content = Dom_html.createDiv Dom_html.window##document in
      content##innerHTML <- Js.string doc_html ;
      Tyxml_js.Of_dom.of_div content in

    div ~a:[ a_class [ modal; "fade" ];
             a_id id; a_tabindex (-1);
             Bootstrap_helpers.Attributes.a_role "dialog";
             aria_labelledby; aria_hidden ]
      [ div ~a:[ a_class [ modal_dialog; modal_dialog_centered ];
                 Bootstrap_helpers.Attributes.a_role "document" ] [
            div ~a:[ a_class [ modal_content ] ] [
              div ~a:[ a_class [ modal_header ] ] [
                button ~a:[  a_button_type `Button; a_class [ "close" ]; dismiss; aria_label ] [
                  span ~a:[ aria_hidden ] [ times_icon () ]
                ] ;
                h4 ~a:[ a_class [ modal_title ]; a_id id_title ]
                  [ pcdata section ] ;
              ] ;
              div ~a:[ a_class [ modal_body ] ] [ main_content ];
              div ~a:[ a_class [ modal_footer ] ] [
                button ~a:[ a_button_type `Button;
                            a_class [ btn; btn_secondary ]; dismiss ]
                  [ pcdata "Close" ]
              ]
            ]
          ]
      ] in
  match Manip.by_id id with
  | Some _->
    link
  | None ->
    Manip.appendToBody container ;
    link
