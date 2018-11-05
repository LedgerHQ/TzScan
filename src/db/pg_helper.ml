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
open Tezos_types
open Db_intf

let string_of_cal t =
  CalendarLib.Printer.Calendar.sprint Date.format t
let cal_of_string s =
  CalendarLib.Printer.Calendar.from_fstring Date.format s

let date_of_cal t =
  Date.from_string (CalendarLib.Printer.Calendar.sprint Date.format t)
let cal_of_date s =
  CalendarLib.Printer.Calendar.from_fstring Date.format (Date.to_string s)

let rows_to_option = function
  | [] -> None
  | [x] -> Some x
  | _ -> assert false

let protocol_not_found = "Can't recover this protocol"

let option_to_protocol = function
  | None -> protocol_not_found
  | Some p -> p

let hash_selector_of_hash selector = match selector with
  | "pending" -> Pending
  | _ ->
    begin match String.get selector 0 with
      | 'b' | 'B' -> Block selector
      | 't' | 'T' | 'K' -> Account selector
      | _ -> Pending
    end

let mem_filter filter filters = List.mem filter filters

let anon_types filters =
  mem_filter "Nonce" filters,
  mem_filter "Activation" filters,
  mem_filter "Double_baking_evidence" filters,
  mem_filter "Double_endorsement_evidence" filters

let manager_types filters =
  mem_filter "Transaction" filters,
  mem_filter "Origination" filters,
  mem_filter "Delegation" filters,
  mem_filter "Reveal" filters

let filters_flag filters =
  let empty = filters = [] in
  mem_filter "Nonce" filters || empty,
  mem_filter "Activation" filters || empty,
  mem_filter "Transaction" filters || empty,
  mem_filter "Origination" filters || empty,
  mem_filter "Delegation" filters || empty,
  mem_filter "Endorsement" filters || empty,
  mem_filter "Proposal" filters || empty,
  mem_filter "Ballot" filters || empty,
  mem_filter "Double_baking_evidence" filters || empty,
  mem_filter "Double_endorsement_evidence" filters || empty,
  mem_filter "Reveal" filters || empty

let omap f = function None -> None | Some x -> Some (f x)
let noop = function None -> assert false | Some x -> x

let to_name_opt ?alias tz = match tz with
  | None -> None
  | Some tz -> Some {tz; alias}

let convert_opt f = function
  | None -> None
  | Some x -> Some (f x)

let block_of_tuple_noop
    (hash, predecessor_hash, fitness, baker, timestamp, protocol, test_protocol,
     network, test_network, test_network_expiration,
     level, _level_position, priority,
     _cycle, _cycle_position,
     _voting_period, _voting_period_position,
     commited_nonce_hash, pow_nonce,
     distance_level, nb_operations,
     validation_pass, proto, data, signature,
     volume, fees,
     protocol_name, test_protocol_name) =
  let block_protocol =
    { proto_name = protocol_name;
      proto_hash = protocol; } in
  let test_protocol =
    { proto_name = test_protocol_name;
      proto_hash = test_protocol; } in
  let level = Int64.to_int level in
  let timestamp = date_of_cal timestamp in
  { hash; predecessor_hash; fitness; baker = Alias.to_name baker;
    timestamp;
    validation_pass = Int64.to_int validation_pass;
    operations = [] ;
    protocol = block_protocol ;
    nb_operations = Int64.to_int nb_operations;
    test_protocol; network; test_network;
    test_network_expiration; priority = Int32.to_int priority; level;
    commited_nonce_hash; pow_nonce;
    proto = Int64.to_int proto;
    data;
    signature;
    volume;
    fees;
    distance_level = Int64.to_int distance_level }

let block_of_tuple_ignore_op
    (hash, predecessor_hash, fitness, baker, timestamp,
     protocol, test_protocol,
     network, test_network, test_network_expiration,
     level, level_position, priority,
     cycle, cycle_position,
     voting_period, voting_period_position,
     commited_nonce_hash, pow_nonce,
     distance_level, nb_operations,
     validation_pass, proto, data, signature,
     volume, fees,
     protocol_name, test_protocol_name,
     _op_hash, _op_type) =
  block_of_tuple_noop
    (hash, predecessor_hash, fitness, baker, timestamp,
     protocol, test_protocol,
     network, test_network, test_network_expiration,
     level, level_position, priority,
     cycle, cycle_position,
     voting_period, voting_period_position,
     commited_nonce_hash, pow_nonce,
     distance_level, nb_operations,
     validation_pass, proto, data, signature,
     volume, fees,
     protocol_name, test_protocol_name)

let op_hash_to_bop_hash op_hash = (op_hash, "", "")

let block_op_get_op_hash
    (_hash, _predecessor, _fitness, _baker, _timestamp,
     _protocol, _test_protocol,
     _network, _test_network, _test_network_expiration,
     _level, _level_position, _priority,
     _cycle, _cycle_position,
     _voting_period, _voting_period_position,
     _commited_nonce_hash, _pow_nonce,
     _distance_level, _nb_operations,
     _validation_pass, _proto, _data, _signature,
     _volume, _fees,
     _protocol_name, _test_protocol_name,
     op_hash, _op_type) = op_hash

let unoption_op_hash = function
  | (None,_,_) -> None
  | (Some a,b,c) -> Some (a,b,c)

let block_op_get_hash
    (hash, _predecessor, _fitness, _baker, _timestamp,
     _protocol, _test_protocol,
     _network, _test_network, _test_network_expiration,
     _level, _level_position, _priority,
     _cycle, _cycle_position,
     _voting_period, _voting_period_position,
     _commited_nonce_hash, _pow_nonce,
     _distance_level, _nb_operations,
     _validation_pass, _proto, _data, _signature,
     _volume, _fees,
     _protocol_name, _test_protocol_name,
     _op_hash, _op_type) = hash

let block_op_get_ops x =
  List.map (fun x -> op_hash_to_bop_hash (noop x))
    (noop (block_op_get_op_hash x))

let block_of_tuple_with_ops x =
  { (block_of_tuple_ignore_op x) with
    operations = block_op_get_ops x }

let activation_from_db rows =
  List.map (fun  (_, act_pkh, act_secret) ->
      let act_pkh = Alias.to_name act_pkh in
      let act = { act_pkh; act_secret } in
      Activation act)
    rows

let dee_from_db
    ((_, endorse1_block_hash, endorse1_block_level, endorse1_slots,
      endorse2_block_hash, endorse2_block_level, endorse2_slots), op_level) =
  let endorse1 = {
    endorse_src = Alias.to_name "" ;
    endorse_op_level = Int64.to_int op_level ;
    endorse_block_level = Int64.to_int endorse1_block_level ;
    endorse_block_hash = endorse1_block_hash ;
    endorse_slot = Misc.unopt_list Int32.to_int endorse1_slots ;
    endorse_priority = -1; endorse_timestamp = ""
  } in
  let endorse2 = {
    endorse_src = Alias.to_name "" ;
    endorse_op_level = Int64.to_int op_level ;
    endorse_block_level = Int64.to_int endorse2_block_level ;
    endorse_block_hash = endorse2_block_hash ;
    endorse_slot = Misc.unopt_list Int32.to_int endorse2_slots ;
    endorse_priority = -1; endorse_timestamp = ""
  } in
  let dee = {
    double_endorsement1 = Sourced (Consensus (Endorsement endorse1)) ;
    double_endorsement2 = Sourced (Consensus (Endorsement endorse2)) ;
  } in
  Double_endorsement_evidence dee

let dbe_from_db
    (signature, hash, _dbe_header1, _dbe_header2, accused,
     denouncer, lost_deposit, lost_rewards, lost_fees, gain_rewards,
     _h1_id , h1_level, h1_proto, h1_predecessor, h1_timestamp, h1_validation_pass,
     h1_operations_hash, h1_fitness, h1_context, h1_priority,
     h1_commited_nonce_hash, h1_pow_nonce, h1_signature,
     _h2_id , h2_level, h2_proto, h2_predecessor, h2_timestamp, h2_validation_pass,
     h2_operations_hash, h2_fitness, h2_context, h2_priority,
     h2_commited_nonce_hash, h2_pow_nonce, h2_signature) =
  let h1 = {
    header_level = Int64.to_int h1_level ;
    header_proto = Int64.to_int h1_proto ;
    header_predecessor = h1_predecessor ;
    header_timestamp = date_of_cal h1_timestamp ;
    header_validation_pass = Int64.to_int h1_validation_pass ;
    header_operations_hash = h1_operations_hash ;
    header_fitness = h1_fitness ;
    header_context = h1_context ;
    header_priority = Int32.to_int h1_priority ;
    header_seed_nonce_hash = h1_commited_nonce_hash ;
    header_proof_of_work_nonce = h1_pow_nonce ;
    header_signature = h1_signature } in
  let h2 = {
    header_level = Int64.to_int h2_level ;
    header_proto = Int64.to_int h2_proto ;
    header_predecessor = h2_predecessor ;
    header_timestamp = date_of_cal h2_timestamp ;
    header_validation_pass = Int64.to_int h2_validation_pass ;
    header_operations_hash = h2_operations_hash ;
    header_fitness = h2_fitness ;
    header_context = h2_context ;
    header_priority = Int32.to_int h2_priority ;
    header_seed_nonce_hash = h2_commited_nonce_hash ;
    header_proof_of_work_nonce = h2_pow_nonce ;
    header_signature = h2_signature } in
  let double_baking_main =
    if h1_signature = signature then 0
    else if h2_signature = signature then 1
    else (-1) in
  let double_baking_accused =
    match accused with None  -> Alias.to_name "unknown" | Some tz1 -> Alias.to_name tz1 in
  let double_baking_denouncer =
    match denouncer with None -> Alias.to_name "unknown" | Some tz1 -> Alias.to_name tz1 in
  (* let unopt = function None -> 0L | Some v -> v in *)
  let double_baking_lost_deposit = Utils.unopt ~default:Int64.zero lost_deposit in
  let double_baking_lost_rewards = Utils.unopt ~default:Int64.zero lost_rewards in
  let double_baking_lost_fees = Utils.unopt ~default:Int64.zero lost_fees in
  let double_baking_gain_rewards = Utils.unopt ~default:Int64.zero gain_rewards in
  let dbe =
    { double_baking_header1 = h1 ; double_baking_header2 = h2;
      double_baking_main ;
      double_baking_accused ;
      double_baking_denouncer ;
      double_baking_lost_deposit ;
      double_baking_lost_rewards ;
      double_baking_lost_fees ;
      double_baking_gain_rewards } in
  Printf.eprintf "%s\n%!" hash;
  Double_baking_evidence dbe

let nonce_from_db rows =
  List.map (fun (_, seed_level, seed_nonce) ->
      let seed_level = Int64.to_int seed_level in
      Seed_nonce_revelation { seed_level; seed_nonce }) rows

let transaction_from_db rows =
  match rows with
  | [] ->  "", []
  | _ ->
    match
      List.find_opt
        (fun (_, _source, _dst, _fee, _counter, _amount, _param, _gas, _storage,
              _failed, internal, _burn, _op_level, _tsp) -> not internal) rows with
    | Some (_, source, _dst, _fee, _counter, _amount, _param, _gas, _storage, _failed,
            _internal, _burn, _op_level, _tsp) ->
      source,
      List.map (fun (_, tr_src, tr_dst, tr_fee, tr_counter,
                     tr_amount, tr_parameters, gas_limit, storage_limit,
                     tr_failed, tr_internal, tr_burn, tr_op_level, tr_timestamp) ->
                 let tr_src = Alias.to_name tr_src in
                 let tr_dst = Alias.to_name tr_dst in
                 let tr_counter = Int64.to_int32 tr_counter in
                 let tr_gas_limit =
                   match gas_limit with
                   | None -> Z.zero
                   | Some gas_limit -> Z.of_int64  gas_limit in
                 let tr_storage_limit =
                   match storage_limit with
                   | None -> Z.zero
                   | Some storage_limit -> Z.of_int64 storage_limit in
                 let tr_op_level = Misc.unoptf (-1) Int64.to_int tr_op_level in
                 let tr_timestamp = string_of_cal tr_timestamp in
                 Transaction { tr_src; tr_dst; tr_amount; tr_counter; tr_fee;
                               tr_gas_limit; tr_storage_limit; tr_parameters;
                               tr_failed ; tr_internal; tr_burn; tr_op_level;
                               tr_timestamp}) rows
    | _ -> "", []

let origination_from_db rows =
  match rows with
  | [] -> "", []
  | _ ->
    try
      let (_, source, _tz1, _fee, _counter, _manager, _delegate,
           _script_code, _script_storage_ty, _spendable,
           _delegatable, _balance, _gas_limit, _st_limit,
           _failed, _internal, _burn) =
        List.find
          (fun (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
                internal, _) -> not internal)  rows in
      source,
      List.map
        (fun (_, or_src, or_tz1, or_fee, or_counter, or_manager, or_delegate,
              sc_code, sc_storage, or_spendable,
              or_delegatable, or_balance, gas_limit, storage_limit,
              or_failed, or_internal, or_burn) ->
          let or_delegate =
            match or_delegate with
            | None -> Alias.to_name or_src
            | Some del -> Alias.to_name del in
          let sc_code = match sc_code with
            | None -> "{}"
            | Some code -> code in
          let sc_storage = match sc_storage with
            | None -> "{}"
            | Some st -> st in
          let or_script = Some { sc_code; sc_storage }  in
          let or_src = Alias.to_name or_src in
          let or_tz1 = Alias.to_name or_tz1 in
          let or_counter = Int64.to_int32 or_counter in
          let or_manager = Alias.to_name or_manager in
          let or_gas_limit =
            match gas_limit with
            | None -> Z.zero
            | Some gas_limit -> Z.of_int64  gas_limit in
          let or_storage_limit =
            match storage_limit with
            | None -> Z.zero
            | Some storage_limit -> Z.of_int64 storage_limit in
          let origi = {
            or_src; or_tz1; or_manager; or_delegate; or_script;
            or_spendable; or_delegatable; or_balance ; or_counter ;
            or_fee ; or_gas_limit ; or_storage_limit ;
            or_failed ; or_internal ;
            or_burn } in
          Origination origi)
        rows
    with _ -> "", []

let delegation_from_db rows =
  match rows with
  | [] ->  "", []
  |  _ ->
    try
      let (_, source, _pubkey, _fee, _counter, _delegate, _gas_limit, _st_limit,
           _failed, _internal) = List.find
          (fun (_, _, _, _, _, _, _, _, _, internal) -> not internal) rows in
      source,
      List.map (fun (_, del_src, _del_pubkey, del_fee, del_counter,
                     del_delegate, gas_limit,
                     storage_limit, del_failed, del_internal) ->
                 let del_delegate = match del_delegate with
                   | None -> Alias.to_name ""
                   | Some tz -> Alias.to_name tz in
                 let del_src = Alias.to_name del_src in
                 let del_counter = Int64.to_int32 del_counter in
                 let del_gas_limit =
                   match gas_limit with
                   | None -> Z.zero
                   | Some gas_limit -> Z.of_int64  gas_limit in
                 let del_storage_limit =
                   match storage_limit with
                   | None -> Z.zero
                   | Some storage_limit -> Z.of_int64 storage_limit in
                 Delegation { del_src ; del_delegate ; del_counter ;
                              del_fee ; del_gas_limit ; del_storage_limit ;
                              del_failed ; del_internal }) rows
    with _ -> "", []

let reveal_from_db rows =
  match rows with
  | [] -> "", []
  | _ ->
    try
      let (_, source, _fee, _counter, _delegate,
           _gas_limit, _st_limit, _failed, _internal) =
        List.find
          (fun (_, _, _, _, _, _, _, _, internal) -> not internal) rows in
      source,
      List.map (fun (_, rvl_src, rvl_fee, rvl_counter, rvl_pubkey, gas_limit,
                     storage_limit, rvl_failed, rvl_internal ) ->
                 let rvl_pubkey = match rvl_pubkey with None -> ""| Some s -> s in
                 let rvl_src = Alias.to_name rvl_src in
                 let rvl_counter = Int64.to_int32 rvl_counter in
                 let rvl_gas_limit =
                   match gas_limit with
                   | None -> Z.zero
                   | Some gas_limit -> Z.of_int64  gas_limit in
                 let rvl_storage_limit =
                   match storage_limit with
                   | None -> Z.zero
                   | Some storage_limit -> Z.of_int64 storage_limit in
                 Reveal { rvl_src ; rvl_pubkey ; rvl_counter ;
                          rvl_fee ; rvl_gas_limit ; rvl_storage_limit ;
                          rvl_failed ; rvl_internal }) rows
    with _ -> "", []

let op_type_from_db_row row =
  let ( _op_hash, _op_block_hash, _op_network_hash, op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  op_type

let op_hash_from_db_row row =
  let ( op_hash, _op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  op_hash

let op_block_hash_from_db_row row =
  let ( _op_hash, op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  op_block_hash

let op_network_from_db_row row =
  let ( _op_hash, _op_block_hash, op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  op_network_hash

let op_src_from_db_row row =
  let ( _op_hash, _op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  match tr_src, or_src, del_src, rvl_src with
  | Some src, _, _, _ -> src
  | None, Some src, _, _ -> src
  | None, None, Some src, _ -> src
  | None, None, None, Some src -> src
  | _ -> assert false

let op_fee_from_db_row row =
  let ( _op_hash, _op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, del_fee, _del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  match tr_fee, or_fee, del_fee, rvl_fee with
  | Some fee, _, _, _ -> fee
  | None, Some fee, _, _ -> fee
  | None, None, Some fee, _ -> fee
  | None, None, None, Some fee -> fee
  | _ -> assert false

let op_counter_from_db_row row =
  let ( _op_hash, _op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, del_counter, _del_delegate,
        _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  match tr_counter, or_counter, del_counter, rvl_counter with
  | Some counter, _, _, _ -> counter
  | None, Some counter, _, _ -> counter
  | None, None, Some counter, _ -> counter
  | None, None, None, Some counter -> counter
  | _ -> assert false

let op_gas_from_db_row row =
  let ( _op_hash, _op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, tr_gas_limit, _tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        or_gas_limit, _or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  match tr_gas_limit, or_gas_limit, del_gas_limit, rvl_gas_limit with
  | Some gas_limit, _, _, _ -> Some gas_limit
  | None, Some gas_limit, _, _ -> Some gas_limit
  | None, None, Some gas_limit, _ -> Some gas_limit
  | None, None, None, Some gas_limit -> Some gas_limit
  | _ -> assert false

let op_storage_from_db_row row =
  let ( _op_hash, _op_block_hash, _op_network_hash, _op_type, _op_level,
        (* Nonce *)
        _, _s_level, _s_nonce,
        (* Activation *)
        _, _a_pkh, _a_secret,
        (* Transaction*)
        _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
        _tr_parameters, _tr_gas_limit, tr_storage_limit, _tr_failed,
        _tr_internal, _tr_burn,
        (* Origination *)
        _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
        _sc_code, _sc_storage, _or_spendable, _or_delegatable,
        _or_gas_limit, or_storage_limit,
        _or_balance, _or_failed, _or_internal, _or_burn,
        (* Delegation *)
        _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
        _del_gas_limit, del_storage_limit, _del_failed, _del_internal,
        (* Endorsement *)
        _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
        _endorse_block_level,
        (* Reveal *)
        _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
        _rvl_gas_limit, rvl_storage_limit, _rvl_failed, _rvl_internal) = row in
  match tr_storage_limit, or_storage_limit, del_storage_limit, rvl_storage_limit with
  | Some storage_limit, _, _, _ -> Some storage_limit
  | None, Some storage_limit, _, _ -> Some storage_limit
  | None, None, Some storage_limit, _ -> Some storage_limit
  | None, None, None, Some storage_limit -> Some storage_limit
  | _ -> assert false

let operation_from_db rows =
  Printf.eprintf "operation_from_db %d\n%!" @@ List.length rows ;
  match rows with
  | [] -> None
  | hd :: _ ->
    let op_type = op_type_from_db_row hd in
    match op_type with
    | Some "Manager" ->
      Printf.eprintf "Manager %d\n%!" @@ List.length rows ;
      let op_hash =
        match op_hash_from_db_row hd with
        | Some op_hash -> op_hash
        | _ -> assert false in
      let op_block_hash =
        match op_block_hash_from_db_row hd with
        | Some op_block_hash -> op_block_hash
        | _ -> Utils.pending_block_hash
      in
      let op_network_hash =
        match op_network_from_db_row hd with
        | Some op_network_hash -> op_network_hash
        | _ -> Utils.pending_block_hash in
      let src = op_src_from_db_row hd in
      let ops =
        List.fold_left (fun acc row -> match row with
            | ( _op_hash, _op_block_hash, _op_network_hash,
                _op_type, op_level,
                (* Nonce *)
                _, _s_level, _s_nonce,
                (* Activation *)
                _, _a_pkh, _a_secret,
                (* Transaction*)
                _, tr_src, tr_dst, tr_fee, tr_counter, tr_amount,
                tr_parameters, tr_gas_limit, tr_storage_limit,
                tr_failed, tr_internal, tr_burn,
                (* Origination *)
                _, or_src, or_tz1, or_fee, or_counter, or_manager,
                or_delegate, sc_code, sc_storage, or_spendable,
                or_delegatable, or_balance, or_gas_limit, or_storage_limit,
                or_failed, or_internal, or_burn,
                (* Delegation *)
                _, del_src, del_pubkey, del_fee, del_counter,
                del_delegate, del_gas_limit, del_storage_limit,
                del_failed, del_internal,
                (* Endorsement *)
                _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
                _endorse_block_level,
                (* Reveal *)
                _, rvl_src, rvl_fee, rvl_counter, rvl_pubkey, rvl_gas_limit,
                rvl_storage_limit, rvl_failed, rvl_internal
              ) ->
              let acc_tr =
                match tr_src, tr_dst, tr_fee, tr_gas_limit,
                      tr_storage_limit, tr_counter, tr_amount,
                      tr_parameters, tr_failed, tr_internal, tr_burn,
                      op_level with
                | Some tr_src, Some tr_dst, Some tr_fee,
                  Some gas_limit, Some storage_limit, Some tr_counter,
                  Some tr_amount, tr_parameters, Some tr_failed,
                  Some tr_internal, Some tr_burn, Some tr_op_level ->
                  let tr_parameters = match tr_parameters with
                    | Some "" -> None
                    | _ -> tr_parameters in
                  let tr_gas_limit = Z.of_int64 gas_limit in
                  let tr_storage_limit = Z.of_int64 storage_limit in
                  let tr_counter = Int64.to_int32 tr_counter in
                  Transaction { tr_src; tr_dst; tr_amount; tr_parameters ;
                                tr_counter ; tr_fee ;
                                tr_gas_limit ; tr_storage_limit ;
                                tr_failed ; tr_internal ; tr_burn;
                                tr_op_level = Int64.to_int tr_op_level;
                                tr_timestamp = "" } :: acc
                | _ -> acc in
              let acc_ori =
                match or_src, or_tz1, or_fee, or_gas_limit, or_storage_limit,
                      or_counter, or_manager,
                      or_delegate, sc_code, sc_storage, or_spendable,
                      or_delegatable, or_balance, or_failed,
                      or_internal, or_burn with
                | Some or_src, Some or_tz1, Some or_fee, Some gas_limit,
                  Some storage_limit, Some or_counter, Some or_manager,
                  or_delegate, sc_code, sc_storage,
                  Some or_spendable,
                  Some or_delegatable, Some or_balance,
                  Some or_failed, Some or_internal, Some or_burn ->
                  let or_delegate =
                    match or_delegate with None -> src | Some del -> del in
                  let sc_code = match sc_code with
                    | None -> "{}"
                    | Some code -> code in
                  let sc_storage = match sc_storage with
                    | None -> "{}"
                    | Some st -> st in
                  let or_script = Some { sc_code; sc_storage }  in
                  let or_gas_limit = Z.of_int64 gas_limit in
                  let or_storage_limit = Z.of_int64 storage_limit in
                  let or_counter = Int64.to_int32 or_counter in
                  let origi = {
                    or_src; or_tz1; or_manager; or_delegate; or_script;
                    or_spendable; or_delegatable; or_balance ;
                    or_counter ; or_fee ; or_gas_limit ; or_storage_limit ;
                    or_failed ; or_internal ; or_burn } in
                  Origination origi :: acc_tr
                | _ -> acc_tr in
              let acc_del =
                match del_src, del_pubkey, del_fee, del_gas_limit,
                      del_storage_limit, del_counter,
                      del_delegate, del_failed, del_internal with
                | Some del_src, Some _del_pubkey, Some del_fee,
                  Some gas_limit, Some storage_limit,
                  Some del_counter, del_delegate,
                  Some del_failed, Some del_internal ->
                  let del_delegate = Utils.unopt del_delegate ~default:(Alias.to_name "") in
                  let del_gas_limit = Z.of_int64 gas_limit in
                  let del_storage_limit = Z.of_int64 storage_limit in
                  let del_counter = Int64.to_int32 del_counter in
                  Delegation { del_src ; del_delegate ;
                               del_counter ; del_fee ; del_gas_limit ;
                               del_storage_limit ;
                               del_failed ; del_internal } :: acc_ori
                | _ -> acc_ori in
              let acc =
                match rvl_src, rvl_fee, rvl_gas_limit, rvl_storage_limit,
                      rvl_counter, rvl_pubkey, rvl_failed, rvl_internal with
                | Some rvl_src, Some rvl_fee, Some gas_limit,
                  Some storage_limit, Some rvl_counter,
                  rvl_pubkey, Some rvl_failed, Some rvl_internal ->
                  let rvl_pubkey = match rvl_pubkey with None -> ""| Some s -> s in
                  let rvl_gas_limit = Z.of_int64 gas_limit in
                  let rvl_storage_limit = Z.of_int64 storage_limit in
                  let rvl_counter = Int64.to_int32 rvl_counter in
                  Reveal { rvl_src ; rvl_pubkey ;
                           rvl_counter ; rvl_fee ; rvl_gas_limit ;
                           rvl_storage_limit ; rvl_failed ;
                           rvl_internal } :: acc_del
                | _ -> acc_del in
              acc)
          [] rows in
      Some
        { op_hash; op_block_hash;
          op_network_hash;
          op_type = Sourced (Manager ("manager", src, ops)) }

    | Some "Anonymous" ->
      let op_hash =
        match op_hash_from_db_row hd with
        | Some op_hash -> op_hash
        | _ -> assert false in
      let op_block_hash =
        match op_block_hash_from_db_row hd with
        | Some op_block_hash -> op_block_hash
        | _ -> Utils.pending_block_hash
      in
      let op_network_hash =
        match op_network_from_db_row hd with
        | Some op_network_hash -> op_network_hash
        | _ -> Utils.pending_block_hash in
      let ops =
        List.fold_left (fun acc row -> match row with
            | ( _op_hash, _op_block_hash, _op_network_hash,
                _op_type, _op_level,
                (* Nonce *)
                _, s_level, s_nonce,
                (* Activation *)
                _, a_pkh, a_secret,
                (* Transaction*)
                _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
                _tr_parameters, _tr_gas, _tr_st, _tr_failed, _tr_internal,
                _tr_burn,
                (* Origination *)
                _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager,
                _or_delegate, _sc_code, _sc_storage, _or_spendable,
                _or_delegatable, _or_balance, _or_gas, _or_st, _or_failed,
                _or_internal, _or_burn,
                (* Delegation *)
                _, _del_src, _del_pubkey, _del_fee, _del_counter,
                _del_delegate, _del_gas, _del_st, _del_failed, _del_internal,
                (* Endorsement *)
                _, _endorse_src, _, _endorse_block_hash, _endorse_slot,
                _endorse_block_level,
                (* Reveal *)
                _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
                _rvl_gas, _rvl_st, _rvl_failed, _rvl_internal) ->
              begin match s_level, s_nonce, a_pkh, a_secret with
                | Some seed_level, Some seed_nonce, None, None ->
                  let seed_level = Int64.to_int seed_level in
                  let seed = { seed_level; seed_nonce; } in
                  Seed_nonce_revelation seed :: acc
                | None, None, Some act_pkh, Some act_secret ->
                  let act = { act_pkh; act_secret } in
                  Activation act :: acc
                | _ -> acc
              end) [] rows in
      Some
        { op_hash; op_block_hash;
          op_network_hash;
          op_type = Anonymous ops }


    | Some "Endorsement" ->
      begin match rows with
        | ( Some op_hash, Some op_block_hash, Some op_network_hash, _op_type,
            Some op_level,
            (* Nonce *)
            _, _s_level, _s_nonce,
            (* Activation *)
            _, _a_pkh, _a_secret,
            (* Transaction*)
            _, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount, _tr_parameters,
            _tr_gas_limit, _tr_storage_limit, _tr_failed, _tr_internal, _tr_burn,
            (* Origination *)
            _, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager, _or_delegate,
            _sc_code, _sc_storage, _or_spendable, _or_delegatable,
            _or_balance, _or_gas_limit, _or_storage_limit, _or_failed, _or_internal,
            _or_burn,
            (* Delegation *)
            _, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
            _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
            (* Endorsement *)
            _, endorse_src, _, endorse_block_hash, endorse_slot,
            endorse_block_level,
            (* Reveal *)
            _, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
            _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal) :: [] ->
          begin match endorse_src, endorse_block_hash,
                      endorse_slot, endorse_block_level with
          | Some endorse_src, Some endorse_block_hash,
            Some endorse_slot, Some endorse_block_level ->
            let endorse_slot = Utils.unopt_i64_slot endorse_slot in
            let endorse_block_level = Int64.to_int endorse_block_level in
            let endorse =
              { endorse_src; endorse_block_hash; endorse_slot; endorse_block_level;
                endorse_op_level = Int64.to_int op_level; endorse_priority = -1;
                endorse_timestamp = "" } in
            Some {
              op_hash; op_block_hash;
              op_network_hash;
              op_type = Sourced (Consensus (Endorsement endorse)) }
          | _ -> None
          end
        | _ -> None
      end

    | Some "Dictator" | Some "Amendement" | _ -> None

let operation_from_db_list rows =
  Printf.eprintf "operation_from_db_list %d\n%!" @@ List.length rows ;
  let list_agg_by_hash =
    List.fold_left (fun acc row ->
        match row with
        | ( Some op_hash, Some _op_block_hash, _1, _2, _op_level,
            (* Nonce *)
            _3, _s_level, _s_nonce,
            (* Activation *)
            _4, _a_pkh, _a_secret,
            (* Transaction*)
            _5, _tr_src, _tr_dst, _tr_fee, _tr_counter, _tr_amount,
            _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
            _tr_internal, _tr_burn,
            (* Origination *)
            _6, _or_src, _or_tz1, _or_fee, _or_counter, _or_manager,
            _or_delegate,
            _sc_code, _sc_storage, _or_spendable, _or_delegatable,
            _or_balance, _or_gas_limit, _or_storage_limit, _or_failed,
            _or_internal, _or_burn,
            (* Delegation *)
            _7, _del_src, _del_pubkey, _del_fee, _del_counter, _del_delegate,
            _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
            (* Endorsement *)
            _8, _endorse_src, _9, _endorse_block_hash, _endorse_slot,
            _endorse_block_level,
            (* Reveal *)
            _10, _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
            _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal
          ) ->
          let row =
            (Some op_hash, Some _op_block_hash, _1, _2, _op_level,
             (* Nonce *)
             _3, _s_level, _s_nonce,
             (* Activation *)
             _4, to_name_opt _a_pkh, _a_secret,
             (* Transaction*)
             _5, to_name_opt _tr_src, to_name_opt _tr_dst,
             _tr_fee, _tr_counter, _tr_amount,
             _tr_parameters, _tr_gas_limit, _tr_storage_limit, _tr_failed,
             _tr_internal, _tr_burn,
             (* Origination *)
             _6, to_name_opt _or_src, to_name_opt _or_tz1,
             _or_fee, _or_counter, to_name_opt _or_manager,
             to_name_opt _or_delegate,
             _sc_code, _sc_storage, _or_spendable, _or_delegatable,
             _or_balance, _or_gas_limit, _or_storage_limit, _or_failed, _or_internal,
             _or_burn,
             (* Delegation *)
             _7, to_name_opt _del_src, _del_pubkey, _del_fee, _del_counter,
             to_name_opt _del_delegate,
             _del_gas_limit, _del_storage_limit, _del_failed, _del_internal,
             (* Endorsement *)
             _8, to_name_opt _endorse_src, _9, _endorse_block_hash, _endorse_slot,
             _endorse_block_level,
             (* Reveal *)
             _10, to_name_opt _rvl_src, _rvl_fee, _rvl_counter, _rvl_pubkey,
             _rvl_gas_limit, _rvl_storage_limit, _rvl_failed, _rvl_internal
            ) in
          begin try
              let old_list = List.assoc op_hash acc in
              (op_hash, row :: old_list) :: (List.remove_assoc op_hash acc)
            with Not_found ->
              (op_hash, [ row ]) ::  acc
          end
        | _ -> acc)
      [] rows in
  List.fold_left (fun acc (_hash, rows) ->
      match operation_from_db rows with
      | None -> acc
      | Some op -> op :: acc)
    [] list_agg_by_hash

let transaction_from_db_list rows =
  List.rev @@ List.fold_left
    (fun acc op -> match op with
       | ( op_hash, Some op_block_hash, Some op_network_hash,
           tr_src, tr_dst, tr_fee, tr_counter, tr_amount, tr_parameters,
           gas_limit, storage_limit, tr_failed, tr_internal,
           tr_burn, Some tr_op_level, Some tr_timestamp) ->
         let tr_src = Alias.to_name tr_src in
         let tr_counter = Int64.to_int32 tr_counter in
         let tr_parameters = match tr_parameters with
           | Some "" -> None
           | _ -> tr_parameters in
         let tr_dst = Alias.to_name tr_dst in
         let tr_gas_limit =
           match gas_limit with
           | None -> Z.zero
           | Some gas_limit -> Z.of_int64 gas_limit in
         let tr_storage_limit =
           match storage_limit with
           | None -> Z.zero
           | Some storage_limit -> Z.of_int64 storage_limit in
         let tr_op_level = Int64.to_int tr_op_level in
         let tr_timestamp = string_of_cal tr_timestamp in
         let trans =
           { tr_src ; tr_dst; tr_amount ; tr_parameters ;
             tr_counter ; tr_fee ; tr_gas_limit ; tr_storage_limit ;
             tr_failed ; tr_internal ; tr_burn; tr_op_level; tr_timestamp } in
         let ops = [ Transaction trans ] in
         { op_hash; op_block_hash;
           op_network_hash;
           op_type =
             Sourced (
               Manager
                 ("manager", tr_src, ops)) } :: acc
       | _ -> acc)
    [] rows

let delegation_from_db_list rows =
  List.rev @@
  List.fold_left
    (fun acc row ->
       match row with
       | ( op_hash, Some op_block_hash, Some op_network_hash,
           del_src, Some _del_pubkey, del_fee, del_counter, Some del_delegate,
           gas_limit, storage_limit, del_failed, del_internal ) ->
         let del_src = Alias.to_name del_src in
         let del_counter = Int64.to_int32 del_counter in
         let del_delegate = Alias.to_name del_delegate in
         let del_gas_limit =
           match gas_limit with
           | None -> Z.zero
           | Some gas_limit -> Z.of_int64 gas_limit in
         let del_storage_limit =
           match storage_limit with
           | None -> Z.zero
           | Some storage_limit -> Z.of_int64 storage_limit in
         let del =
           { del_src ; del_delegate ; del_counter ; del_fee ; del_gas_limit ;
             del_storage_limit; del_failed ; del_internal } in
         let ops = [ Delegation del ] in
         { op_hash; op_block_hash;
           op_network_hash;
           op_type =
             Sourced (
               Manager
                 ("manager", del_src, ops)) } :: acc
       | _ -> acc)
    [] rows

let reveal_from_db_list rows =
  List.rev @@
  List.fold_left
    (fun acc row ->
       match row with
       | ( op_hash, Some op_block_hash, Some op_network_hash,
           rvl_src, rvl_fee, rvl_counter, Some rvl_pubkey,
           gas_limit, storage_limit, rvl_failed, rvl_internal ) ->
         let rvl_src = Alias.to_name rvl_src in
         let rvl_gas_limit =
           match gas_limit with
           | None -> Z.zero
           | Some gas_limit -> Z.of_int64 gas_limit in
         let rvl_storage_limit =
           match storage_limit with
           | None -> Z.zero
           | Some storage_limit -> Z.of_int64 storage_limit in
         let rvl_counter = Int64.to_int32 rvl_counter in
         let ops = [ Reveal
                       { rvl_src ; rvl_pubkey ; rvl_counter ; rvl_fee ;
                         rvl_gas_limit ; rvl_storage_limit ;
                         rvl_failed ; rvl_internal } ] in
         { op_hash; op_block_hash;
           op_network_hash;
           op_type =
             Sourced (
               Manager ("manager", rvl_src, ops)) } :: acc
       | _ -> acc)
    [] rows

let activation_from_db_list rows =
  List.rev @@
  List.fold_left
    (fun acc row ->
       match row with
       | ( op_hash, Some op_block_hash, Some op_network_hash,
           act_pkh, act_secret ) ->
         let act_pkh = Alias.to_name act_pkh in
         let ops = [ Activation { act_pkh ; act_secret } ] in
         { op_hash; op_block_hash;
           op_network_hash;
           op_type =
             Anonymous ops } :: acc
       | _ -> acc)
    [] rows

let endorsement_from_db_list =
  List.map
    (fun ( op_hash, op_block_hash, op_network_hash, endorse_src,
           endorse_block_hash, endorse_slot, endorse_block_level,
           endorse_op_level, endorse_priority, endorse_timestamp ) ->
      let endorse_slot = Misc.unopt_list Int32.to_int endorse_slot in
      let endorse_block_level = Int64.to_int endorse_block_level in
      let endorse_op_level = Int64.to_int endorse_op_level in
      let endorse_priority = Int32.to_int endorse_priority in
      let endorse_src = Alias.to_name endorse_src in
      let endorse_timestamp = string_of_cal endorse_timestamp in
      let endorse =
        { endorse_src; endorse_block_hash; endorse_slot; endorse_block_level;
          endorse_op_level; endorse_priority; endorse_timestamp} in
      { op_hash; op_block_hash; op_network_hash;
        op_type = Sourced (Consensus (Endorsement endorse)) })

let origination_from_db_list rows =
  List.rev @@
  List.fold_left
    (fun acc row ->
       match row with
       | ( op_hash, Some op_block_hash, Some op_network_hash,
           or_src, or_tz1, or_fee, or_counter, or_manager, or_delegate,
           sc_code, sc_storage, or_spendable, or_delegatable, or_balance,
           gas_limit, storage_limit, or_failed, or_internal,
           or_burn) ->
         let or_src = Alias.to_name or_src in
         let or_counter = Int64.to_int32 or_counter in
         let or_delegate =
           match or_delegate with None -> or_src | Some del -> Alias.to_name del in
         let or_script = match sc_code, sc_storage with
           | None, None -> None
           | Some sc_code, None -> Some { sc_code ; sc_storage = "{}" }
           | None, Some sc_storage ->  Some { sc_code = "{}"; sc_storage }
           | Some sc_code, Some sc_storage ->
             Some { sc_code; sc_storage } in
         let or_tz1 = Alias.to_name or_tz1 in
         let or_manager = Alias.to_name or_manager in
         let or_gas_limit =
           match gas_limit with
           | None -> Z.zero
           | Some gas_limit -> Z.of_int64 gas_limit in
         let or_storage_limit =
           match storage_limit with
           | None -> Z.zero
           | Some storage_limit -> Z.of_int64 storage_limit in
         let ops = [ Origination {
             or_src; or_tz1; or_manager; or_delegate; or_script;
             or_spendable; or_delegatable; or_balance ;
             or_counter ; or_fee ; or_gas_limit ; or_storage_limit ; or_failed ;
             or_internal ; or_burn
           } ] in
         { op_hash; op_block_hash;
           op_network_hash;
           op_type =
             Sourced (
               Manager ("manager", or_src, ops))} :: acc
       | _ -> acc)
    [] rows

let bakings_from_db_list rows =
  List.rev @@
  List.fold_left
    (fun acc bk -> match bk with
       | (bk_block_hash, bk_baker_hash, bk_level, bk_cycle, bk_priority,
          bk_distance_level, bk_fees, Some bk_bktime, Some bk_baked) ->
         let bk_level = Int64.to_int bk_level in
         let bk_cycle = Int64.to_int bk_cycle in
         let bk_priority = Int32.to_int bk_priority in
         let bk_distance_level = Int64.to_int bk_distance_level in
         let bk_baker_hash = Alias.to_name bk_baker_hash in
         {bk_block_hash; bk_baker_hash; bk_level; bk_cycle ; bk_priority;
          bk_distance_level; bk_fees; bk_bktime = int_of_float bk_bktime;
          bk_baked} :: acc
       | _ -> acc ) [] rows

let bakings_endorsement_from_db_list rows =
  List.rev @@ List.fold_left (fun acc row -> match row with
      | (Some ebk_level, Some ebk_lr_nslot, ebk_block, ebk_source, ebk_cycle,
         ebk_priority, ebk_dist, ebk_slots) ->
        let ebk_level = Int64.to_int ebk_level in
        let ebk_cycle = convert_opt Int64.to_int ebk_cycle in
        let ebk_dist = convert_opt Int32.to_int ebk_dist in
        let ebk_source = convert_opt Alias.to_name ebk_source in
        let ebk_priority = convert_opt Int32.to_int ebk_priority in
        let ebk_slots = convert_opt (Misc.unopt_list Int32.to_int) ebk_slots in
        let ebk_lr_nslot = Int32.to_int ebk_lr_nslot in
        {ebk_block; ebk_source; ebk_level; ebk_cycle; ebk_priority;
         ebk_dist; ebk_slots; ebk_lr_nslot} :: acc
      | _ -> acc) [] rows

let cycle_bakings_from_db_list current_cycle rows =
  List.rev @@ List.fold_left
    (fun acc cbk -> match cbk with
       | (cbk_cycle, Some cnt_all, Some cnt_steal, cnt_miss, Some cbk_priority,
          tez_fee, cbk_bktime) ->
         let cbk_cycle = Int64.to_int cbk_cycle in
         let tez_reward =
           if cbk_cycle > 6 then
             Int64.mul Tezos_constants.Constants.block_reward cnt_all
           else 0L in
         let tez_deposit = Int64.mul cnt_all
             (Tezos_constants.cycle_deposits
                Tezos_constants.Constants.block_security_deposit cbk_cycle) in
         let cbk_priority, cbk_bktime = if cnt_all = 0L then None, None else
             Some cbk_priority,
             Some Int64.(to_int @@ div (of_float cbk_bktime) cnt_all) in
         {cbk_cycle; cbk_depth = current_cycle - cbk_cycle;
          cbk_count = {cnt_all; cnt_miss; cnt_steal};
          cbk_tez = {tez_fee; tez_reward; tez_deposit};
          cbk_priority; cbk_bktime} :: acc
       | _ -> acc ) [] rows

let cycle_endorsements_from_db_list current_cycle rows =
  List.rev @@ List.fold_left
    (fun acc ced -> match ced with
       | (ced_cycle, Some cnt_all, cnt_miss, Some ced_priority, Some tez_reward) ->
         let ced_cycle = Int64.to_int ced_cycle in
         let tez_deposit = Int64.mul cnt_all
             (Tezos_constants.cycle_deposits
                Tezos_constants.Constants.endorsement_security_deposit ced_cycle) in
         {ced_cycle; ced_depth = current_cycle - ced_cycle;
          ced_slots = {cnt_all; cnt_miss; cnt_steal=0L};
          ced_tez = {tez_fee=0L; tez_reward; tez_deposit};
          ced_priority} :: acc
       | _ -> acc) [] rows


let rights_from_db_list rows =
  List.rev @@
  List.fold_left
    (fun acc row -> match row with
       | ( Some pr_level, Some pr_bakers, Some pr_endorsers,
           Some pr_bakers_priority, baked_priority, baker ) ->
         let r_level = Int64.to_int pr_level in
         let r_bakers = List.rev @@
           List.fold_left (fun acc s ->
               match s with Some s -> (Alias.to_name s) :: acc | None -> acc) [] pr_bakers in
         let r_endorsers = List.rev @@
           List.fold_left (fun acc s ->
               match s with Some s -> (Alias.to_name s) :: acc | None -> acc) [] pr_endorsers in
         let r_bakers_priority = List.rev @@
           List.fold_left (fun acc s ->
               match s with Some s -> Int32.to_int s :: acc | None -> acc)
           [] pr_bakers_priority in
       let r_baked = match baked_priority, baker with
         | Some prio, Some baker -> Some (Alias.to_name baker, Int32.to_int prio)
         | _ -> None in
       { r_level; r_bakers; r_endorsers ; r_bakers_priority; r_baked } :: acc
       | _ -> acc)
    [] rows

let baker_rights_from_db_list rows =
  List.rev @@ List.fold_left
    (fun acc x -> match x with
       | ( pr_level, pr_cycle, Some pr_prio, Some pr_depth ) ->
         let br_level = Int64.to_int pr_level in
         let br_cycle = Int64.to_int pr_cycle in
         let br_priority = Int32.to_int pr_prio in
         let br_depth = Int64.to_int pr_depth in
         {br_level; br_cycle; br_priority; br_depth} :: acc
       | _ -> acc) [] rows

let endorser_rights_from_db_list rows =
  List.rev @@ List.fold_left
    (fun acc x -> match x with
       | ( pr_level, pr_cycle, Some pr_nslot, Some pr_depth) ->
         let er_level = Int64.to_int pr_level in
         let er_cycle = Int64.to_int pr_cycle in
         let er_nslot = Int32.to_int pr_nslot in
         let er_depth = Int64.to_int pr_depth in
         {er_level; er_cycle; er_nslot; er_depth} :: acc
       | _ -> acc) [] rows
