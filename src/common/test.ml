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

open Geoip

let unopt = function
  | None -> "None"
  | Some s -> s

let get_country ip =
  let gi = init_exn GEOIP_MEMORY_CACHE in
  let country_name = unopt @@ Geoip.country_name_by_name gi ip in
  let code = unopt @@ Geoip.country_code_by_name gi ip in
  Printf.eprintf "IP: %s\n  Country %s\n  Code %s\n%!" ip country_name code ;
  Geoip.close gi

let _ =
  let ips = [
    "195.189.143.147"; (* opera.com, NO *)
    "195.214.195.105"; (* ukr.net, UA *)
    "81.19.70.3";      (* rambler.ru, RU *)
    "212.112.119.232"
  ] in
  List.iter get_country ips
