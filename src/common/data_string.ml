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

let find_field header_all fields_all elt =
  let rec aux = function
    | [], _ | _, [] -> None
    | hh :: _, hf :: _ when hh = elt -> Some hf
    | _ :: th, _ :: tf -> aux (th, tf) in
  let res = aux (header_all, fields_all) in
  res

let choose_fields header_all fields_all header =
  List.rev @@ List.fold_left (fun acc elt ->
      match find_field header_all fields_all elt with
      | None -> acc
      | Some field -> field :: acc
    ) [] header

let transaction_header =
  ["transaction"; "block"; "network"; "source"; "destination"; "amount"; "fee";
   "parameters"; "failed"; "internal"; "burned tez"; "counter"; "gas limit";
   "storage limit"]

let transaction header o =
  List.rev @@
  match o.op_type with
  | Sourced sos ->
    begin match sos with
      | Manager (_, source, l) ->
        (List.fold_left (fun acc op -> match op with
             | Transaction tr ->
               ( choose_fields transaction_header
                   [ o.op_hash; o.op_block_hash; o.op_network_hash; source.tz ;
                     tr.tr_dst.tz;
                     Int64.to_string tr.tr_amount;
                     Int64.to_string tr.tr_fee;
                     Misc.unopt "" tr.tr_parameters;
                     string_of_bool tr.tr_failed;
                     string_of_bool tr.tr_internal;
                     Int64.to_string tr.tr_burn;
                     Int32.to_string tr.tr_counter;
                     Z.to_string tr.tr_gas_limit;
                     Z.to_string tr.tr_storage_limit ] header ) :: acc
             | _ -> acc) [] l)
      | _ -> [] end
  | _ -> []
