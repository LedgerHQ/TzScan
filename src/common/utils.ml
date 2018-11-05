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

open Tezos_types
open Data_types

let pending_block_hash = "prevalidation"

let orphan_block_hash = "Orphan"

let debug flag fmt =
  if flag then Printf.eprintf fmt
  else Printf.ifprintf stderr fmt

let unopt ~default = function None -> default | Some value -> value

let unopt_i64_slot =
  List.map (function
      | None -> assert false
      | Some s -> Int64.to_int s)


let opt_list list = List.map (fun elt -> Some elt) list

let get_op_from_double_endorsement_evidence = function
  | Anonymous _ -> assert false
  | Sourced op ->
    begin match op with
      | Consensus op ->
        begin match op with
          | Endorsement op -> op
        end
      | _ -> assert false
    end

let string_of_op_type = function
  | Anonymous _ -> "Anonymous"
  | Sourced op ->
    begin match op with
      | Consensus Endorsement _ -> "Endorsement"
      | Amendment (_ , op) ->
        begin match op with
          | Proposal _ -> "Proposals"
          | Ballot _ -> "Ballot"
        end
      | Manager (_, _, _) -> "Manager"
      | Dictator op ->
        begin match op with
          | Activate -> "Activate"
          | Activate_testnet -> "Activate_testnet"
        end
    end

let string_of_op_contents = function
  | NSeed_nonce_revelation _ | NDouble_baking_evidence _
  | NDouble_endorsement_evidence _ | NActivation _ ->
    "Anonymous"
  | NEndorsement _ -> "Endorsement"
  | NProposals _ -> "Proposals"
  | NBallot _ -> "Ballot"
  | NTransaction _ | NDelegation _
  | NOrigination _ | NReveal _ -> "Manager"
  | NActivate -> "Activate"
  | NActivate_testnet -> "Activate_testnet"

let string_of_manager_op_type = function
  | Sourced Manager (_, _, list) ->
    (if List.exists (function Transaction _ -> true | _ -> false) list then
       [ "Transaction" ]
     else []) @
    (if List.exists (function Origination _ -> true | _ -> false) list then
       [ "Origination" ]
     else []) @
    (if List.exists (function Reveal _ -> true | _ -> false) list then
       [ "Reveal" ]
     else []) @
    (if List.exists (function Delegation _ -> true | _ -> false) list then
       [ "Delegation" ]
     else [])
  | _ -> []

let string_of_manager_op_contents = List.fold_left (fun acc op -> match op with
    | NTransaction _ -> "Transaction" :: acc
    | NOrigination _ -> "Origination" :: acc
    | NReveal _ -> "Reveal" :: acc
    | NDelegation _ -> "Delegation" :: acc
    | _ -> acc) []

let string_of_anonymous_op_type = function
  | Anonymous list ->
    (if List.exists (function
         | Seed_nonce_revelation _ -> true
         | _ -> false) list then
       [ "Nonce" ]
     else []) @
    (if List.exists (function Activation _ -> true | _ -> false) list then
       [ "Activation" ]
     else []) @
    (if List.exists (function
         | Double_endorsement_evidence _ -> true
         | _ -> false) list then
       [ "Double_endorsement_evidence" ]
     else []) @
    (if List.exists (function
         | Double_baking_evidence _ -> true
         | _ -> false) list then
       [ "Double_baking_evidence" ]
     else [])
  | _ -> []

let string_of_anonymous_op_contents = List.fold_left (fun acc op -> match op with
    | NActivation _ -> "Activation" :: acc
    | NDouble_endorsement_evidence _ -> "Double_endorsement_evidence" :: acc
    | NDouble_baking_evidence _ -> "Double_baking_evidence" :: acc
    | NSeed_nonce_revelation _ -> "Nonce" :: acc
    | _ -> acc) []


let json_root = function
  | `O ctns -> `O ctns
  | `A ctns -> `A ctns
  | `Null -> `O []
  | oth -> `A [ oth ]

let string_of_balance_update = function
  (*
  | Debited (i1, i2) -> Printf.sprintf "Debited %Ld %Ld" i1 i2
  | Credited (i1, i2) -> Printf.sprintf "Credited %Ld %Ld" i1 i2
*)
  | Contract (s, i) -> Printf.sprintf "Contract %s %Ld" s i
  | Rewards (s, i1, i2) -> Printf.sprintf "Rewards %s %d %Ld" s i1 i2
  | Fees (s, i1, i2) -> Printf.sprintf "Fees %s %d %Ld" s i1 i2
  | Deposits (s, i1, i2) -> Printf.sprintf "Deposits %s %d %Ld" s i1 i2

let split_ymd_timestamp date_str =
  let l = String.split_on_char '-' date_str in
  match l with
  | [ year; month ; day ] ->
    int_of_string year,
    int_of_string month,
    int_of_string day
  | _ -> assert false
