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

open Js_utils
open Data_types

let dapps = Hashtbl.create 13

let is_dapp kt1 = Hashtbl.mem dapps kt1

(* Here we can add a specific function to display a version of the
   dapp. It would be displayed on 'View DApp' if an entry is added in
   services.json *)
let display container_id dapp_contract =
  try
    try
      let f = Hashtbl.find dapps (dapp_contract : string) in
      let container = find_component container_id in
      f (container : unit Ocp_js.Of_dom.elt)
    with Not_found -> ()
  with _ ->  ()

let services = Hashtbl.create 13

let find_srv ( kt1 : string ) =
  try
    Some ( Hashtbl.find services kt1 : service )
  with Not_found -> None
