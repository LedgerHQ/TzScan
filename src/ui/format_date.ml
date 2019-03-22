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
open Tezos_types
open Lang

module TEXT = struct
  let s_day = ss_ "day"
  let s_days = ss_ "days"
end
open TEXT

let days d = if d > 1 then s_days else s_day


let get_now () =
  let now = jsnew Js.date_now () in
  now##valueOf()

let diff_server = ref 0.
let set_server_date date =
  let date_ms = date *. 1000. in
  let now_ms = get_now () in
  diff_server := now_ms -. date_ms;
  ()

let get_now() =
  get_now() -. !diff_server

let parse_seconds t =
  if t < 60 then
    Printf.sprintf "%02d s" t
  else
    if t < 3600 then
      let secs = t mod 60 in
      let mins = t / 60 in
      if t < 600 then
        Printf.sprintf "%dm %02ds" mins secs
      else
        Printf.sprintf "%d min" mins
    else
      let t = t / 60 in
      if t < 60 * 24 then
        let mins = t mod 60 in
        let hours = t / 60 in
        Printf.sprintf "%dh %dm" hours mins
      else
        let t = t / 60 in
        if t < 72 then
          let hours = t mod 24 in
          let ds = t / 24 in
          Printf.sprintf "%d %s %dh" ds (t_ (days ds)) hours
        else
          let ds = t / 24 in
          Printf.sprintf "%d %s" ds (t_ (days ds))

let ago timestamp_f =
  (get_now () -. timestamp_f) /. 1000.

let ago_str ?(future=false) timestamp_f =
  let diff = int_of_float @@ ago timestamp_f in
  parse_seconds @@ if future then -1 * diff else diff

let time_before_level ~cst diff_level =
  let seconds_left = diff_level * List.hd cst.time_between_blocks in
  let divmod a b = a / b, a mod b in
  let days_left, hours_left = divmod seconds_left (24 * 60 * 60) in
  let hours_left, minutes_left = divmod hours_left (60 * 60) in
  let minutes_left, seconds_left = divmod minutes_left 60 in
  if days_left = 0 then
    if hours_left = 0 then
      if minutes_left = 0 then
        Printf.sprintf "%ds" seconds_left
      else
      Printf.sprintf "%dmin" minutes_left
    else
      Printf.sprintf "%dh %dm" hours_left minutes_left
  else
    Printf.sprintf "%dd %dh %dm" days_left hours_left minutes_left

let float_of_iso timestamp =
  Js.date##parse (Js.string timestamp)

let auto_updating_timespan_float ?refresh ?(future=false) timestamp_f =
  let timestamp_js = jsnew Js.date_fromTimeValue(timestamp_f) in
  let timestamp_str =
    try
      Js.to_string timestamp_js##toLocaleString()
    with exn ->
      Printf.eprintf "Error in toLocaleString: %s\n%!"
        (Printexc.to_string exn);
      string_of_float timestamp_f
  in
  let diff = ago timestamp_f in
  let diff = if future then -1. *. diff else diff in
  let delay = if abs_float diff < 600. then 1 else 60 in
  let ts_span =
    span ~a:[Bootstrap_helpers.Attributes.a_data_toggle "tooltip";
             a_title timestamp_str] [] in
  let update () =
    let diff = ago timestamp_f in
    let diff = if future then -1. *. diff else diff in
    begin match refresh with
      | None -> ()
      | Some refresh -> refresh diff
    end;
    let value = parse_seconds @@ int_of_float @@ diff in
    Manip.setInnerHtml ts_span value in
  Misc_js.UpdateOnFocus.update_every delay update;
  ts_span

let auto_updating_timespan ?refresh ?(future=false) timestamp =
  let timestamp_f = float_of_iso timestamp in
  auto_updating_timespan_float ?refresh ~future timestamp_f
