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
open Js

type last_block_diff = Big | Small | Good

let diff timestamp =
  let now = jsnew date_now () in
  let timestamp_f = date##parse (Js.string timestamp) in
  let diff_f = date##parse(now##toString()) -. timestamp_f in
  let diff =
    if diff_f < 0. then jsnew date_fromTimeValue(0.)
    else jsnew date_fromTimeValue(diff_f) in
  let year = diff##getUTCFullYear () in
  let month = diff##getUTCMonth () in
  let day = diff##getUTCDate () - 1 in
  let h = diff##getUTCHours () in
  let min = diff##getUTCMinutes () in
  if year <> 1970 then Big
  else if month <> 0 then Big
  else if day <> 0 then Big
  else if h <> 0 then Small
  else
  if min > 9 then Small
  else Good

let make_error_node_state () =
  div ~a:[ a_class [ "alert"; "alert-danger" ] ] [
    strong [ pcdata "Oups." ] ;
    pcdata "The node used for gathering balances information seems down... \
            Try again later."
  ]

let make_very_late_node_state ts =
  let msg1 = pcdata "Warning: last update " in
  div ~a:[ a_class [ "alert"; "alert-warning" ] ] [
    strong [ msg1; Format_date.auto_updating_timespan ts ] ;
    pcdata "The node used for gathering balances information is very late... \
            The balance may be outdated."
  ]

let make_bit_late_node_state ts =
  let msg1 = pcdata "Warning: last update " in
  div ~a:[ a_class [ "alert"; "alert-info" ] ] [
    strong [ msg1; Format_date.auto_updating_timespan ts ] ;
    pcdata "The node used for gathering balances information is a bit late... \
            The balance may be outdated."
  ]

let make_ok_node_state ts =
  let msg1 = pcdata "Last update " in
  div ~a:[ a_class [ "alert"; "alert-success" ] ] [
    strong [ msg1; Format_date.auto_updating_timespan ts ] ;
    pcdata " The node used for gathering balances information is up to date."
  ]

let node_state_panel id ts_opt =
  let container = find_component id in
  let div = match ts_opt with
    | None -> make_error_node_state ()
    | Some ts ->
      begin match diff ts with
        | Big -> make_very_late_node_state ts
        | Small -> make_bit_late_node_state ts
        | Good -> make_ok_node_state ts
    end in
    Manip.removeChildren container ;
    Manip.appendChild container div

let make_icon ts_opt =
  let alt, img_uri, tooltip = match ts_opt with
    | None ->
      "Node down",
      "images/icon_red.png",
      span ~a:[ a_class [ "css-tooltiptext"; "tt-error" ] ] [
        pcdata "Unable to gather balance" ]
    | Some ts ->
      begin match diff ts with
        | Big ->
          "Node is very late",
          "images/icon_yellow.png",
          span ~a:[ a_class [ "css-tooltiptext-right"; "tt-vlate" ] ] [
            pcdata "Node is outdated" ]
        | Small ->
          "Node is a bit late",
          "images/icon_blue.png",
          span ~a:[ a_class [ "css-tooltiptext-right"; "tt-late" ] ] [
            pcdata "Node is a bit outdated" ]
        | Good ->
          "Node is up to date",
          "images/icon_green.png",
          span ~a:[ a_class [ "css-tooltiptext-right"; "tt-ok" ] ] [
            pcdata "Node is up to date" ]
      end in
  div ~a:[ a_class [ "node-state-icon"; "css-tooltip" ] ] [
    img
      ~alt
      ~src:(uri_of_string img_uri) () ;
    tooltip
  ]

let make_heading_icon ts_opt =
  let alt, img_uri, tooltip = match ts_opt with
    | None ->
      "Node down",
      "images/icon_red.png",
      span ~a:[ a_class [ "css-tooltiptext"; "tt-error" ] ] [
        pcdata "Unable to gather infos... try again later" ]
    | Some ts ->
      begin match diff ts with
        | Big ->
          "Node very late",
          "images/icon_yellow.png",
          span ~a:[ a_class [ "css-tooltiptext"; "tt-vlate" ] ] [
            pcdata "Account infos are outdated... check again later" ]
        | Small ->
          "Node a bit late",
          "images/icon_blue.png",
          span ~a:[ a_class [ "css-tooltiptext"; "tt-late" ] ] [
            pcdata "Account infos are a bit outdated... check again later" ]
        | Good ->
          "Node up to date",
          "images/icon_green.png",
          span ~a:[ a_class [ "css-tooltiptext"; "tt-ok" ] ] [
            pcdata "Account infos are up to date" ]
      end in
  div ~a:[ a_class [ "node-state-heading-icon"; "css-tooltip" ] ] [
    img
      ~alt
      ~src:(uri_of_string img_uri) () ;
    tooltip
  ]

let node_state_heading_icon = make_heading_icon

let node_state_icon = make_icon
