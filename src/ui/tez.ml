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

let tez_units = 1_000_000L
(* let total_supply_cents = 763_306_929.69 (* cents *) *)

let icon () =
  let sym = Infos.www.www_currency_symbol in
  span ~a:[a_class ["tz"]] [
    if sym.[0] = '#' then
      entity sym
    else
      pcdata sym
  ]

let dollar () =
  span ~a:[a_class ["tz"]] [
      entity "#x24"
  ]

let mu_icon () = entity "#956"

let sep =
  try
    match Js.to_string (Js.number_of_float 0.1)##toLocaleString() with
    | "0.1" -> '.'
    | "0,1" -> ','
    | _ -> '.'
  with _ -> '.'

let non_zeroes s =
  let len = String.length s in
  let rec iter s n len =
    match s.[n-1] with
    | '0' -> iter s (n-1) len
    | '.' | ',' -> n-1
    | _ -> n
  in
  iter s len len


let amount volumeL = (* in mutez *)
  if volumeL > 0L && volumeL < 1000L then
    let number =
      Js.number_of_float @@
      Int64.to_float volumeL
    in
    span
      [ pcdata @@ (Js.to_string number##toLocaleString()) ^ " ";
        mu_icon ();
        icon ()]
  else
    let tez = Int64.div volumeL tez_units in
    let mutez = Int64.sub volumeL (Int64.mul tez_units tez) in
    let tez = Js.to_string
        ((Js.number_of_float
           (Int64.to_float tez))##toLocaleString() ) in
    let s = Printf.sprintf "%s%c%06Ld" tez sep mutez in
    let n = non_zeroes s in
    let s = String.sub s 0 n in
    span [ pcdata (s ^ " "); icon () ]

let pp_amount ?(precision=6) ?(width=15) ?order ?(icon=icon) volumeL =
  if volumeL = 0L then span [ pcdata "0 "; icon ()]
  else
    let sign =
      if volumeL < 0L then "-"
      else "" in
    let volumeL = Int64.abs volumeL in
    let units = [|mu_icon (); pcdata " m"; pcdata ""; pcdata "K";
                  pcdata "M"; pcdata "B"; pcdata "T"|] in
    let ndecimal = String.length (Int64.to_string volumeL) in
    let diff_length = ndecimal - width in
    let order = match order with
      | None -> if ndecimal > 6 then
          max 2 ((ndecimal - width + 2) / 3)
        else if ndecimal > 3 then
          max 1 ((ndecimal - width + 2) / 3)
        else
          max 0 ((ndecimal - width + 2) / 3)
      | Some order -> order in
    let unit_float = if diff_length < 0 then 1L else
        Int64.(of_float (10. ** (float diff_length))) in
    let unit_int = if order < 0 then 1L else
        Int64.(of_float (10. ** (float (order * 3 )))) in
    let volume_float = Int64.div volumeL unit_float in
    let volume_int = Int64.div volumeL unit_int in
    let decimal = Int64.sub (Int64.mul volume_float unit_float)
        (Int64.mul volume_int unit_int) in
    let volume_int, decimal, precision =
      if order * 3 < precision then volume_int, decimal, order * 3 else
        let unit_precision = Int64.of_float (10. ** float (order * 3 - precision)) in
        let tmp = Int64.div decimal unit_precision in
        let unit_precision2 = Int64.div unit_precision 10L in
        if unit_precision2 = 0L || tmp = 0L ||
           Int64.div (Int64.sub decimal (Int64.mul tmp unit_precision))
             (Int64.div unit_precision 10L) < 5L then
          volume_int, tmp, precision
        else if Int64.succ tmp = Int64.of_float (10. ** float precision) then
          Int64.succ volume_int, Int64.zero, precision
        else
          volume_int, Int64.succ tmp, precision
    in
    let num = Js.to_string
        ((Js.number_of_float
            (Int64.to_float volume_int))##toLocaleString() ) in
    let s = Printf.sprintf "%s" num  in
    let decimal_str = Printf.sprintf "%c%0*Ld" sep precision decimal in
    let n = non_zeroes decimal_str in
    let decimal_str = String.sub decimal_str 0 n in
    span [
      span ~a:[ a_class ["pp-tez"] ] [ pcdata (sign ^ s) ] ;
      if decimal <> 0L then
        span ~a:[ a_class ["pp-tez-decimal"] ] [ pcdata decimal_str ]
      else
        span [ ] ;
      span [ pcdata " "; units.(max order 0); icon() ] ;
    ]

let pp_amount_float ?precision ?width ?order volumef =
  pp_amount ?precision ?width ?order (Int64.of_float volumef)

let approx_amount volumeL = (* in mutez *)
  let number =
    Js.number_of_float @@
    (Int64.to_float volumeL /. Int64.to_float tez_units) in
  span
    [ pcdata @@ Js.to_string number##toLocaleString() ^ " "; icon ()]

let amount_float volume = amount (Int64.of_float volume)

let approx_amount_float volume = approx_amount (Int64.of_float volume)

let amount_float_tez volume =
  let volumeL = Int64.of_float volume in
  let number =
    Js.number_of_float @@
    Int64.to_float volumeL in
  span
    [ pcdata @@ Js.to_string number##toLocaleString() ^ " "; icon ()]

(* To print statistics *)

(* amount expressed in 100_000 sub-unit of tz
    (alphanet = 1 kxtz, zeronet=0.1 xtz) *)
let amount_100000u f =
  let f = f /. 10_000. in
  if f < 0.01 then
    span [pcdata "0 "]
  else
    let m =
      if f < 10. then
        Printf.sprintf "%.2f " f
      else
      if f < 1_000. then
        Printf.sprintf "%.0f K" f
      else
        if f < 10_000. then
          Printf.sprintf "%.2f M" (f /. 1_000.)
        else
          if f < 100_000. then
            Printf.sprintf "%.1f M" (f /. 1_000.)
          else
            if f < 1_000_000. then
              Printf.sprintf "%.0f M" (f /. 1_000.)
            else
              if f < 10_000_000. then
                Printf.sprintf "%.2f B" (f /. 1_000_000.)
              else
                if f < 100_000_000. then
                  Printf.sprintf "%.1f B" (f /. 1_000_000.)
                else
                  Printf.sprintf "%.0f B" (f /. 1_000_000.)
    in
    span [pcdata m; icon()]

let with_usd price_usd xtz =
  match price_usd with
  | None -> [ amount xtz ]
  | Some price_usd ->
    let price_usd = float_of_string price_usd in
    let number =
      Js.number_of_float @@
      (Int64.to_float xtz *. price_usd /.
       Int64.to_float tez_units) in
    (* Display USD value only on mainnet *)
    pp_amount xtz ::
    match Tezos_constants.net with
    | Tezos_constants.Betanet ->
      [
        span ~a:[ a_class [ "usd-price" ] ] [
          pcdata " ($";
          pcdata @@ Js.to_string number##toLocaleString() ;
          pcdata ")" ]
      ]
    | _ -> []
