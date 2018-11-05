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
open Bootstrap_helpers.Grid
open Lang (* s_ *)
open Common
open Data_types
open Tezos_types

let s_injection_failed = ss_ "Operation injection failed"
let s_injection_succeeded_at_address =
  ss_ "Operation successfully injected at address"
let s_rpc_error = ss_ "RPC call ended with return code"
let s_outdated_operation =
  ss_ "You are probably trying to inject an outdated operation"
let s_illformed_operation =
  ss_ "The operation you are trying to inject is probably illformed"
let s_generic_failure = ss_ ""
let s_inject_operation = ss_ "Inject operation"
let s_inject_signed_operation = ss_ "Inject a signed operation"

module Signed = struct

  (* different HTML elements *)
  let sending =
    img
      ~a:[a_id "injection-sending"]
      ~alt:"Request sent ! Waiting for an answer ..."
      ~src:"images/spinner.svg"
      ()

  let answer_ok = div ~a:[ a_id "injection-answer-ok" ] [ ]

  let answer_ko = div ~a:[ a_id "injection-answer-ko" ] [ ]

  let request_ko = div ~a:[ a_id "injection-request-ko" ] [ ]

  let answer_area =
    div
      ~a:[ a_id "injection-answer-area" ]
      [sending ; answer_ok ; answer_ko ; request_ko]

  let textarea =
    textarea
      ~a:[
        a_class [ clg2 ; "injection-textarea"] ;
        a_id "inject-op-content";
        a_placeholder (s_ "Insert a hexadecimal signed operation here")
      ]
      (pcdata "")

  let inject_button handler =
    let b =
      button
        ~a:[ a_class ["btn" ;"btn-default"] ; a_id "inject-op-button"]
        [ pcdata_t s_inject_operation ]
    in
    (Tyxml_js.To_dom.of_button b)##onclick <-
      Dom_html.handler handler;
    b


  (* auxiliary functions to show/hide html elements *)

  let show obj = Manip.SetCss.display obj "block"

  let hide obj = Manip.SetCss.display obj "none"

  let hide_all () =
    hide sending;
    hide answer_ok;
    hide answer_ko;
    hide request_ko


  (* update html if error is encountered *)
  let errors_handler status _content =
    hide_all ();
    show request_ko;
    Manip.replaceChildren request_ko
      [ div [ h3 [ pcdata_t s_injection_failed ];
              h4 [ pcdata_t s_rpc_error ; pcdata @@ Printf.sprintf " %d" status ]
            ] ]


  (* update html if RPC returned an answer *)
  let result_decoder res =
    hide_all ();
    Firebug.console##log (Js.string res);
    match EzEncoding.destruct Tezos_encoding.Encoding.Injection.result_encoding res with
    | Inj_ok op  ->
      show answer_ok;
      Manip.replaceChildren answer_ok
        [ div [ h3 [ pcdata_t s_injection_succeeded_at_address ];
                h4 ~a:[ a_class [ "no-overflow" ] ] [
                  Common.make_link op ];
              ] ]

    | Inj_ko l ->
      let most_precise =
        List.fold_left
          (fun acc e -> if acc < e then e else acc) Inj_other l in
      let msg_failure = match most_precise with
        | Inj_illformed -> pcdata_t s_illformed_operation
        | Inj_outdated  -> pcdata_t s_outdated_operation
        | Inj_other     -> pcdata_t s_generic_failure
        | Inj_generic _ -> pcdata_t s_generic_failure
      in
      show answer_ko;
      Manip.replaceChildren answer_ko
        [ div [ h3 [ pcdata_t s_injection_failed ]; h4 [ msg_failure ] ] ]

  let xhr_inject signed_op =
    let url = EzAPI.forge0 (api "inject") Service.V1.head [] in
    Xhr.get "Inject_ui.head" url (fun res ->
        let head = EzEncoding.destruct Api_encoding.V1.Block.encoding res in
        let data =
          EzEncoding.construct Tezos_encoding.Encoding.Injection.injection_encoding
            (signed_op, head.network, false) in
        let url = EzAPI.printf (api "inject")  "inject_operation" in
        Xhr.post
          ?content_type:(Some "application/json")
          ~content:data
          "Inject_ui.inject_signed_op" url
          ~error:errors_handler
          result_decoder
      )

  (* handler when clicking on injection button *)
  let inject_button_handler e =
    Dom.preventDefault e;
    let inject_input = find_component "inject-op-content" in
    let signed_op = Js.to_string (Tyxml_js.To_dom.of_input
                                    inject_input)##value in
    let signed_op = String.trim signed_op in
    hide_all ();
    if not (String.equal signed_op "") then
      begin
        show sending;
        xhr_inject signed_op;
      end;
    Js._true


  let make () =
    Bootstrap_helpers.Panel.make_panel
      ~panel_title_content:(div [ pcdata_t s_inject_signed_operation ;
                                  Glossary_doc.(help HInject) ])
      ~panel_body_content:[
        textarea ;
        (inject_button inject_button_handler);
        answer_area ]
      ()

end
