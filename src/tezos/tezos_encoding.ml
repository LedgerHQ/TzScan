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

open Json_encoding
open Tezos_types

let float = Json_encoding.float
let int64 = EzEncoding.int64
let tez = EzEncoding.int64

let conn_metadata_encoding =
  conv
    (fun { disable_mempool ; private_node } ->
       (disable_mempool, private_node) )
    (fun
      (disable_mempool, private_node) ->
      { disable_mempool ; private_node })
    (obj2
       (req "disable_mempool" bool)
       (req "private_node" bool))

let z_encoding =
  def "bignum" @@
  describe
    ~title: "Big number"
    ~description: "Decimal representation of a big number" @@
  conv Z.to_string Z.of_string string

module Micheline = struct

  (*
  let string_or_empty =
    union [
      case empty
        (fun _ -> None)
        (fun _ -> None);
      case string
        (fun s -> s)
        (fun s -> Some s)
    ]
*)

  let dummy_loc = (false, 0, None)

  let script_expr_encoding =
    let int_encoding =
      obj1 (req "int" z_encoding) in
    let string_encoding =
      obj1 (req "string" string) in
    let bytes_encoding =
      obj1 (req "bytes" bytes) in
    let application_encoding expr_encoding =
      obj3
        (req "prim" string)
        (dft "args" (list expr_encoding) [])
        (dft "annots" (list string) []) in
    let seq_encoding expr_encoding =
      list expr_encoding in
    let node_encoding = mu "tezosScriptExpression" (fun expr_encoding ->
        describe
          ~title: "Script expression (data, type or code)" @@
        union
          [ case int_encoding
              (function Int (_, v) -> Some v | _ -> None)
              (fun v -> Int (dummy_loc, v)) ;
            case string_encoding
              (function String (_, v) -> Some v | _ -> None)
              (fun v -> String (dummy_loc, v)) ;
            case bytes_encoding
              (function Bytes (_, v) -> Some v | _ -> None)
              (fun v -> Bytes (dummy_loc, v)) ;
            case (application_encoding expr_encoding)
              (function
                | Prim (_, v, args, annot) -> Some (v, args, annot)
                | _ -> None)
              (function (prim, args, annot) -> Prim (dummy_loc, prim, args, annot)) ;
            case (seq_encoding expr_encoding)
              (function Seq (_loc, v) -> Some v | _ -> None)
              (fun args -> Seq (dummy_loc, args)) ]) in
    node_encoding

  let encode script =
    let ezjson =
      (module Json_repr.Ezjsonm : Json_repr.Repr with type value = Json_repr.ezjsonm ) in
    Json_repr.pp
      ~compact:true
      ezjson
      Format.str_formatter
      (Json_encoding.construct script_expr_encoding script) ;
    Format.flush_str_formatter ()

  let decode str =
    let ezjson = Ezjsonm.from_string str in
    Json_encoding.destruct script_expr_encoding ezjson

  let script_encoding =
    (obj2
       (req "code" script_expr_encoding)
       (req "storage" script_expr_encoding))

  let script_str_encoding =
    (obj2
       (req "code" string)
       (req "storage" string))
end

  (* Encoding for Alphanet Protocol  *)
module Base = struct

  module Operation_hash = struct let encoding = string end
  module Context_hash = struct let encoding = string  end
  module Operation_list_list_hash = struct let encoding = string end
  module Fitness = struct let encoding = list string end
  module Signature = struct let encoding = string end


  module Block_hash = struct
    (*    type t = string *)
    let encoding = string
  end

  module Protocol_hash = struct
    (*    type t = string *)
    let encoding = string
  end

  module Chain_id = struct
    (*    type t = string *)
    let encoding = string
  end

  module Time = struct
    (*    type t = string *)
    let encoding = string
  end

  module Contracts = struct
    (*    type t = string list *)
    let encoding = list string
  end


  module Level = struct
    let encoding =
      conv
        (fun { node_lvl_level; node_lvl_level_position;
               node_lvl_cycle; node_lvl_cycle_position;
               node_lvl_voting_period; node_lvl_voting_period_position ;
               node_lvl_expected_commitment } ->
          (node_lvl_level, node_lvl_level_position, node_lvl_cycle,
           node_lvl_cycle_position, node_lvl_voting_period,
           node_lvl_voting_period_position, node_lvl_expected_commitment))
        (fun (node_lvl_level, node_lvl_level_position, node_lvl_cycle,
              node_lvl_cycle_position, node_lvl_voting_period,
              node_lvl_voting_period_position, node_lvl_expected_commitment) ->
          { node_lvl_level; node_lvl_level_position;
            node_lvl_cycle; node_lvl_cycle_position;
            node_lvl_voting_period; node_lvl_voting_period_position ;
            node_lvl_expected_commitment } )
        (obj7
           (req "level" int)
           (req "level_position" int)
           (req "cycle" int)
           (req "cycle_position" int)
           (req "voting_period" int)
           (req "voting_period_position" int)
           (req "expected_commitment" bool))
  end

  module Voting_period_repr = struct
    let kind_encoding =
      union  [
        case
          (constant "proposal")
          (function NProposal -> Some () | _ -> None)
          (fun () -> NProposal) ;
        case
          (constant "testing_vote")
          (function NTesting_vote -> Some () | _ -> None)
          (fun () -> NTesting_vote) ;
        case
          (constant "testing")
          (function NTesting -> Some () | _ -> None)
          (fun () -> NTesting) ;
        case
          (constant "promotion_vote")
          (function NPromotion_vote -> Some () | _ -> None)
          (fun () -> NPromotion_vote) ;
      ]
  end

  module Balance_updates = struct

    let balance_encoding =
      union
        [ case
            (obj3
               (req "kind" (constant "contract"))
               (req "contract" string)
               (req "change" tez))
            (function Contract (c, change) ->
               Some ((), c, change) | _ -> None )
            (fun ((), c, change) -> (Contract (c, change))) ;
          case
            (obj5
               (req "kind" (constant "freezer"))
               (req "category" (constant "rewards"))
               (req "delegate" string)
               (req "level" int)
               (req "change" tez))
            (function Rewards (d, l, change) ->
               Some ((), (), d, l, change) | _ -> None)
            (fun ((), (), d, l, change) -> Rewards (d, l, change)) ;
          case
            (obj5
               (req "kind" (constant "freezer"))
               (req "category" (constant "fees"))
               (req "delegate" string)
               (req "level" int)
               (req "change" tez))
            (function Fees (d, l, change) ->
               Some ((), (), d, l, change) | _ -> None)
            (fun ((), (), d, l, change) -> Fees (d, l, change)) ;
          case
            (obj5
               (req "kind" (constant "freezer"))
               (req "category" (constant "deposits"))
               (req "delegate" string)
               (req "level" int)
               (req "change" tez))
            (function Deposits (d, l, change) ->
               Some ((), (), d, l, change) | _ -> None)
            (fun ((), (), d, l, change) -> Deposits (d, l, change)) ]

    let encoding =
      list balance_encoding
  end

  module Proto = struct
    let block_header_data_encoding =
      let contents_encoding =
        (obj3
           (req "priority" int)
           (req "proof_of_work_nonce" string)
           (opt "seed_nonce_hash" string)) in
      (merge_objs
         contents_encoding
         (obj1 (req "signature" Signature.encoding)))

    let block_header_metadata_encoding =
      conv
        (fun { header_meta_baker ; header_meta_level ;
               header_meta_voting_period_kind  ;
               header_meta_nonce_hash ;
               header_meta_consumed_gas ;
               header_meta_deactivated ;
               header_meta_balance_updates } ->
          (header_meta_baker, header_meta_level,
           header_meta_voting_period_kind,
           header_meta_nonce_hash,
           header_meta_consumed_gas,
           header_meta_deactivated,
           header_meta_balance_updates))
        (fun (header_meta_baker, header_meta_level,
              header_meta_voting_period_kind,
              header_meta_nonce_hash,
              header_meta_consumed_gas,
              header_meta_deactivated,
              header_meta_balance_updates) ->
          { header_meta_baker ; header_meta_level ;
            header_meta_voting_period_kind ;
            header_meta_nonce_hash ;
            header_meta_consumed_gas ;
            header_meta_deactivated ;
            header_meta_balance_updates })
        (obj7
           (req "baker" string)
           (req "level" Level.encoding)
           (req "voting_period_kind" Voting_period_repr.kind_encoding)
           (req "nonce_hash" (option string))
           (req "consumed_gas" z_encoding)
           (req "deactivated" (list string))
           (opt "balance_updates" Balance_updates.encoding)
        )
  end

  module Block_header = struct

    let content_encoding =
      obj4
        (req "command" string)
        (req "hash" string)
        (req "fitness" Fitness.encoding)
        (req "protocol_parameters" string)

    let shell_header_encoding =
      obj8
        (req "level" int)
        (req "proto" int)
        (req "predecessor" Block_hash.encoding)
        (req "timestamp" Time.encoding)
        (req "validation_pass" int)
        (req "operations_hash" Operation_list_list_hash.encoding)
        (req "fitness" Fitness.encoding)
        (req "context" Context_hash.encoding)

    let genesis_shell_header_encoding =
      obj10
        (req "level" int)
        (req "proto" int)
        (req "predecessor" Block_hash.encoding)
        (req "timestamp" Time.encoding)
        (req "validation_pass" int)
        (req "operations_hash" Operation_list_list_hash.encoding)
        (req "fitness" Fitness.encoding)
        (req "context" Context_hash.encoding)
        (opt "content" content_encoding)
        (opt "signature" Signature.encoding)

    let encoding =
      conv
        (fun
          { header_level; header_proto; header_predecessor; header_timestamp;
            header_validation_pass; header_operations_hash; header_fitness;
            header_context; header_priority; header_seed_nonce_hash;
            header_proof_of_work_nonce; header_signature } ->
          let header_timestamp = Date.to_string header_timestamp in
          let header_seed_nonce_hash =
            if header_seed_nonce_hash = "" then None
            else Some header_seed_nonce_hash in
          ((header_level, header_proto, header_predecessor, header_timestamp,
            header_validation_pass, header_operations_hash, [ header_fitness ],
            header_context),
           ((header_priority, header_proof_of_work_nonce,
             header_seed_nonce_hash), header_signature)))
        (fun
          ((header_level, header_proto, header_predecessor, header_timestamp,
            header_validation_pass, header_operations_hash, header_fitness,
            header_context),
           ((header_priority, header_proof_of_work_nonce, seed),
            header_signature)) ->
          let header_timestamp = Date.from_string header_timestamp in
          let header_seed_nonce_hash = match seed with None -> "" | Some s -> s in
          { header_level;
            header_proto;
            header_predecessor;
            header_timestamp;
            header_validation_pass;
            header_operations_hash;
            header_fitness = String.concat " " header_fitness;
            header_context;
            header_priority;
            header_seed_nonce_hash ;
            header_proof_of_work_nonce;
            header_signature })
        (merge_objs
           shell_header_encoding
           Proto.block_header_data_encoding)

    let genesis_encoding =
      conv
        (fun
          { header_level; header_proto; header_predecessor; header_timestamp;
            header_validation_pass; header_operations_hash; header_fitness;
            header_context;
            header_signature = _ ;
            header_priority = _;
            header_seed_nonce_hash = _ ;
            header_proof_of_work_nonce = _
          } ->
          let header_timestamp = Date.to_string header_timestamp in
          ((header_level, header_proto, header_predecessor, header_timestamp,
            header_validation_pass, header_operations_hash, [ header_fitness ],
            header_context, None, None)))
        (fun
          ((header_level, header_proto, header_predecessor, header_timestamp,
            header_validation_pass, header_operations_hash, header_fitness,
            header_context, _, header_signature)) ->
          let header_timestamp = Date.from_string header_timestamp in
          let header_seed_nonce_hash = "No seed nonce hash for genesis" in
          let header_priority = ~-1 in
          let header_proof_of_work_nonce = "No pow nonce for genesis" in
          let header_signature = match header_signature with
            | None -> "No signature for genesis"
            | Some s -> s in
          { header_level;
            header_proto;
            header_predecessor;
            header_timestamp;
            header_validation_pass;
            header_operations_hash;
            header_fitness = String.concat " " header_fitness;
            header_context;
            header_priority;
            header_seed_nonce_hash ;
            header_proof_of_work_nonce;
            header_signature })
        genesis_shell_header_encoding
  end

  module Operation = struct

    let manager_encoding =
      (obj5
         (req "source" string)
         (req "fee" tez)
         (req "counter" z_encoding)
         (req "gas_limit" z_encoding)
         (req "storage_limit" z_encoding))

    let big_map_diff_item_encoding =
      (obj3
         (req "key_hash" string)
         (req "key" Micheline.script_expr_encoding)
         (opt "value" Micheline.script_expr_encoding))

    let op_metadata_encoding =
      conv
        (fun { meta_op_status ; meta_op_balance_updates ;
               meta_op_originated_contracts ; meta_op_consumed_gas ;
               meta_op_storage_size_diff ; meta_op_storage ;
               meta_op_delegate ; meta_op_slots ; meta_op_paid_storage_size_diff ;
               meta_op_big_map_diff ; meta_op_allocated_destination_contract} ->
          (meta_op_status, meta_op_balance_updates,
           meta_op_originated_contracts, meta_op_consumed_gas,
           meta_op_storage_size_diff,
           meta_op_storage, meta_op_delegate, meta_op_slots,
           meta_op_paid_storage_size_diff, None),
          (meta_op_big_map_diff, meta_op_allocated_destination_contract))
        (fun ((meta_op_status, meta_op_balance_updates,
               meta_op_originated_contracts, meta_op_consumed_gas,
               meta_op_storage_size_diff,
               meta_op_storage, meta_op_delegate, meta_op_slots,
               meta_op_paid_storage_size_diff,
               _meta_op_errors),
              (meta_op_big_map_diff, meta_op_allocated_destination_contract) ) ->
          { meta_op_status ;
            meta_op_balance_updates ;
            meta_op_originated_contracts ;
            meta_op_consumed_gas ;
            meta_op_storage_size_diff ;
            meta_op_storage ;
            meta_op_delegate ;
            meta_op_paid_storage_size_diff ;
            meta_op_slots ;
            meta_op_big_map_diff ;
            meta_op_allocated_destination_contract})
        (merge_objs
           (obj10
              (opt "status" string)
              (opt "balance_updates" Balance_updates.encoding)
              (opt "originated_contracts" (list string))
              (opt "consumed_gas" z_encoding)
              (opt "storage_size" int64)
              (opt "storage" Micheline.script_expr_encoding)
              (opt "delegate" string)
              (opt "slots" (list int))
              (opt "paid_storage_size_diff" z_encoding)
              (opt "errors" (list Json_encoding.any_value)))
           (obj2
              (opt "big_map_diff" (list big_map_diff_item_encoding))
              (opt "allocated_destination_contract" bool)) (* TODO not registered *)
        )

    let internal_transaction_result_encoding =
      (conv
         (fun { node_tr_src ;
                node_tr_fee = _ ;
                node_tr_counter = _ ;
                node_tr_amount ; node_tr_dst; node_tr_parameters ;
                node_tr_gas_limit = _ ; node_tr_storage_limit = _;
                node_tr_metadata } ->
           let node_tr_parameters = match node_tr_parameters with
             | None -> None
             | Some p -> Some p in
           let result = match node_tr_metadata with
             | None -> None
             | Some meta -> meta.manager_meta_operation_result in
           ((), node_tr_src, 0, node_tr_amount, node_tr_dst, None,
            node_tr_parameters, result))
         (fun ((), node_tr_src, _nonce, node_tr_amount, node_tr_dst, p,
               str_p, result) ->
           let node_tr_parameters =  match p, str_p with
             | None, None -> None
             | Some mic, None -> Some (Micheline.encode mic)
             | None, Some mic_str -> Some mic_str
             | _, _ -> assert false in
           let node_tr_metadata =
             Some
               { manager_meta_balance_updates = None ;
                 manager_meta_operation_result = result ;
                 manager_meta_internal_operation_results = [] } in
           { node_tr_src ; node_tr_fee = 0L ;
             node_tr_counter = Z.minus_one ;
             node_tr_amount ; node_tr_dst; node_tr_parameters ;
             node_tr_gas_limit = Z.minus_one ;
             node_tr_storage_limit = Z.minus_one;
             node_tr_metadata }))
        (obj8
           (req "kind" (constant "transaction"))
           (req "source" string)
           (req "nonce" int)
           (req "amount" tez)
           (req "destination" string)
           (opt "parameters" Micheline.script_expr_encoding)
           (opt "str_parameters" string)
           (opt "result" op_metadata_encoding))

    let internal_origination_result_encoding =
      (conv
         (fun { node_or_src ; node_or_fee = _; node_or_counter = _ ;
                node_or_gas_limit = _ ; node_or_storage_limit = _ ;
                node_or_manager ; node_or_delegate ; node_or_script ; node_or_spendable ;
                node_or_delegatable ; node_or_balance ; node_or_metadata } ->
           let str_script = match node_or_script with
             | None -> None
             | Some code ->
               Some (code.sc_code,
                     code.sc_storage) in
           let result = match node_or_metadata with
             | None -> None
             | Some meta -> meta.manager_meta_operation_result in
           (((), node_or_src, ~-1, Some node_or_manager, Some node_or_manager, node_or_balance,
             Some node_or_spendable, Some node_or_delegatable,
             node_or_delegate), (None, str_script, result)))
         (fun (((), node_or_src, _nonce, node_or_managerPubkey, node_or_manager_pubkey, node_or_balance,
                node_or_spendable, node_or_delegatable, node_or_delegate), (script, str_script, result)) ->
           let node_or_manager =
             match node_or_manager_pubkey with
             | Some m -> m
             | None ->
               match node_or_managerPubkey with
               | Some m -> m
               | None -> "No manager public key found" in
           let node_or_spendable = Tezos_utils.unopt node_or_spendable ~default:false in
           let node_or_delegatable = Tezos_utils.unopt node_or_delegatable ~default:false in
           let node_or_script = match script, str_script with
             | None, None -> None
             | Some (sc_code, sc_storage), None ->
               (* Orignation from Tezos Node *)
               let sc_code = Micheline.encode sc_code in
               let sc_storage = Micheline.encode sc_storage in
               Some { Tezos_types.sc_code ;
                      Tezos_types.sc_storage }
             | None, Some (sc_code, sc_storage) ->
               Some { Tezos_types.sc_code ;
                      Tezos_types.sc_storage }
             | _, _ -> assert false
           in
           let node_or_metadata =
             Some
               { manager_meta_balance_updates = None ;
                 manager_meta_operation_result = result ;
                 manager_meta_internal_operation_results = [] } in
           { node_or_src ; node_or_fee = 0L ;
             node_or_counter = Z.minus_one ;
             node_or_gas_limit = Z.minus_one ;
             node_or_storage_limit = Z.minus_one ;
             node_or_manager ; node_or_delegate ;
             node_or_script ; node_or_spendable ;
             node_or_delegatable ; node_or_balance ;
             node_or_metadata }))
           (merge_objs
              (obj9
                 (req "kind" (constant "origination"))
                 (req "source" string)
                 (req "nonce" int)
                 (* change during proto update, we need to do this to
                    be able to recrawl the chain from scratch *)
                 (opt "managerPubkey" string)
                 (opt "manager_pubkey" string)
                 (req "balance" tez)
                 (opt "spendable" bool)
                 (opt "delegatable" bool)
                 (opt "delegate" string))
              (obj3
                 (opt "script" Micheline.script_encoding)
                 (* ocp field *)
                 (opt "str_script" Micheline.script_str_encoding)
                 (opt "result" op_metadata_encoding)))

    let internal_delegation_result_encoding =
      (conv
         (fun { node_del_src ;
                node_del_fee = _ ;
                node_del_counter = _ ;
                node_del_gas_limit = _ ;
                node_del_storage_limit = _ ;
                node_del_delegate ;
                node_del_metadata} ->
           let node_del_delegate =
             if node_del_delegate = "" then None else Some node_del_delegate in
           let result = match node_del_metadata with
               None -> None
             | Some m -> m.manager_meta_operation_result
           in
           ((), node_del_src, ~-1, node_del_delegate, result))
         (fun ((), node_del_src, _nonce, node_del_delegate, result) ->
            let node_del_delegate = Tezos_utils.unopt node_del_delegate ~default:"" in
            { node_del_src ;
              node_del_fee = Int64.minus_one ;
              node_del_counter = Z.minus_one ;
              node_del_gas_limit = Z.minus_one ;
              node_del_storage_limit = Z.minus_one ;
              node_del_delegate ;
              node_del_metadata = Some
                  {manager_meta_balance_updates = None;
                   manager_meta_operation_result = result;
                   manager_meta_internal_operation_results = []}  }))
        (obj5
           (req "kind" (constant "delegation"))
           (req "source" string)
           (req "nonce" int)
           (opt "delegate" string)
           (opt "result" op_metadata_encoding))

    let internal_reveal_result_encoding =
      (conv
         (fun { node_rvl_src ;
                node_rvl_fee = _ ; node_rvl_counter = _ ;
                node_rvl_gas_limit = _ ; node_rvl_storage_limit = _ ;
                node_rvl_pubkey ;
                node_rvl_metadata} ->
           let result = match node_rvl_metadata with
               None -> None
             | Some m -> m.manager_meta_operation_result
           in
           ((), node_rvl_src, ~-1, node_rvl_pubkey, result))
         (fun ((), node_rvl_src, _none, node_rvl_pubkey, result) ->
            { node_rvl_src ;
              node_rvl_fee = Int64.minus_one ;
              node_rvl_counter = Z.minus_one ;
              node_rvl_gas_limit = Z.minus_one ;
              node_rvl_storage_limit = Z.minus_one ;
              node_rvl_pubkey;
              node_rvl_metadata = Some
                  {manager_meta_balance_updates = None;
                   manager_meta_operation_result = result;
                   manager_meta_internal_operation_results = []}}))
        (obj5
           (req "kind" (constant "reveal"))
           (req "source" string)
           (req "nonce" int)
           (req "public_key" string)
           (opt "result" op_metadata_encoding))

    let internal_operation_result_encoding =
      union [
        case internal_transaction_result_encoding
          (function NTransaction tr -> Some tr | _ -> None)
          (fun tr -> NTransaction tr) ;
        case internal_origination_result_encoding
          (function NOrigination orig -> Some orig | _ -> None)
          (fun orig -> NOrigination orig) ;
        case internal_delegation_result_encoding
          (function NDelegation del -> Some del | _ -> None)
          (fun del -> NDelegation del) ;
        case internal_reveal_result_encoding
          (function NReveal rvl -> Some rvl | _ -> None)
          (fun rvl -> NReveal rvl) ;
      ]

    let manager_metadata_encoding =
      conv
        (fun { manager_meta_balance_updates ;
               manager_meta_operation_result ;
               manager_meta_internal_operation_results } ->
          manager_meta_balance_updates,
          manager_meta_operation_result,
	  manager_meta_internal_operation_results)
        (fun (manager_meta_balance_updates,
              manager_meta_operation_result,
              manager_meta_internal_operation_results) -> (* not used ? *)
          { manager_meta_balance_updates ;
            manager_meta_operation_result ;
            manager_meta_internal_operation_results })
        (obj3
           (opt "balance_updates" Balance_updates.encoding)
           (opt "operation_result" op_metadata_encoding)
           (dft "internal_operation_results"
              (list internal_operation_result_encoding) []))

    let transaction_encoding =
      (conv
         (fun { node_tr_src ; node_tr_fee ; node_tr_counter ;
                node_tr_amount ; node_tr_dst; node_tr_parameters ;
                node_tr_gas_limit ; node_tr_storage_limit; node_tr_metadata } ->
           let node_tr_parameters = match node_tr_parameters with
             | None -> None
             | Some p -> Some p in
           ((node_tr_src, node_tr_fee, node_tr_counter, node_tr_gas_limit,
             node_tr_storage_limit),
            ((), node_tr_amount, node_tr_dst, node_tr_metadata, None,
             node_tr_parameters)))
         (fun (
            ((node_tr_src, node_tr_fee, node_tr_counter,
              node_tr_gas_limit, node_tr_storage_limit),
             ((), node_tr_amount, node_tr_dst, node_tr_metadata, p, str_p))) ->
            let node_tr_parameters =  match p, str_p with
              | None, None -> None
              | Some mic, None -> Some (Micheline.encode mic)
              | None, Some mic_str -> Some mic_str
              | _, _ -> assert false in
            { node_tr_src ; node_tr_fee ; node_tr_counter ;
              node_tr_amount ; node_tr_dst; node_tr_parameters ;
              node_tr_gas_limit ; node_tr_storage_limit ; node_tr_metadata }))
        (merge_objs
           manager_encoding
           (obj6
              (req "kind" (constant "transaction"))
              (req "amount" tez)
              (req "destination" string)
              (opt "metadata" manager_metadata_encoding)
              (opt "parameters" Micheline.script_expr_encoding)
              (opt "str_parameters" string)))

    let origination_encoding =
      (conv
         (fun { node_or_src ; node_or_fee ; node_or_counter ; node_or_gas_limit ; node_or_storage_limit ;
                node_or_manager ; node_or_delegate ; node_or_script ; node_or_spendable ;
                node_or_delegatable ; node_or_balance ; node_or_metadata } ->
           let str_script = match node_or_script with
             | None -> None
             | Some code ->
               Some (code.sc_code,
                     code.sc_storage) in
           ((node_or_src, node_or_fee, node_or_counter, node_or_gas_limit,
             node_or_storage_limit),
            ((), Some node_or_manager, Some node_or_manager, node_or_balance,
             Some node_or_spendable, Some node_or_delegatable,
             node_or_delegate, None, str_script, node_or_metadata )))
         (fun ((node_or_src, node_or_fee, node_or_counter, node_or_gas_limit, node_or_storage_limit),
               ((), node_or_managerPubkey, node_or_manager_pubkey,
                node_or_balance, node_or_spendable,
                node_or_delegatable, node_or_delegate, script, str_script,
                node_or_metadata)) ->
           let node_or_manager =
             match node_or_manager_pubkey with
             | Some m -> m
             | None ->
               match node_or_managerPubkey with
               | Some m -> m
               | None -> "No manager public key found" in
           let node_or_spendable = Tezos_utils.unopt node_or_spendable ~default:false in
           let node_or_delegatable = Tezos_utils.unopt node_or_delegatable ~default:false in
           let node_or_script = match script, str_script with
             | None, None -> None
             | Some (sc_code, sc_storage), None ->
               (* Orignation from Tezos Node *)
               let sc_code = Micheline.encode sc_code in
               let sc_storage = Micheline.encode sc_storage in
               Some { Tezos_types.sc_code ;
                      Tezos_types.sc_storage }
             | None, Some (sc_code, sc_storage) ->
               Some { Tezos_types.sc_code ;
                      Tezos_types.sc_storage }
             | _, _ -> assert false
           in
           { node_or_src ; node_or_fee ; node_or_counter ;
             node_or_gas_limit ; node_or_storage_limit ;
             node_or_manager ; node_or_delegate ;
             node_or_script ; node_or_spendable ;
             node_or_delegatable ; node_or_balance ; node_or_metadata }))
           (merge_objs
              manager_encoding
              (obj10
                 (req "kind" (constant "origination"))
                 (* change during proto update, we need to do this to
                    be able to recrawl the chain from scratch *)
                 (opt "managerPubkey" string)
                 (opt "manager_pubkey" string)
                 (req "balance" tez)
                 (opt "spendable" bool)
                 (opt "delegatable" bool)
                 (opt "delegate" string)
                 (opt "script" Micheline.script_encoding)
                 (* ocp field *)
                 (opt "str_script" Micheline.script_str_encoding)
                 (opt "metadata" manager_metadata_encoding)))

    let delegation_encoding =
      (conv
         (fun { node_del_src ; node_del_fee ; node_del_counter ; node_del_gas_limit ;
                node_del_storage_limit ; node_del_delegate ;
                node_del_metadata } ->
           let node_del_delegate =
             if node_del_delegate = "" then None else Some node_del_delegate in
           ((node_del_src, node_del_fee, node_del_counter, node_del_gas_limit,
             node_del_storage_limit), ((), node_del_delegate, node_del_metadata)))
         (fun ((node_del_src, node_del_fee, node_del_counter, node_del_gas_limit,
                node_del_storage_limit), ((), node_del_delegate, node_del_metadata)) ->
           let node_del_delegate = Tezos_utils.unopt node_del_delegate ~default:"" in
           { node_del_src ; node_del_fee ; node_del_counter ; node_del_gas_limit ;
             node_del_storage_limit ; node_del_delegate ; node_del_metadata }))
        (merge_objs
           manager_encoding
           (obj3
              (req "kind" (constant "delegation"))
              (opt "delegate" string)
              (opt "metadata" manager_metadata_encoding)))

    let reveal_encoding =
      (conv
         (fun { node_rvl_src ; node_rvl_fee ; node_rvl_counter ; node_rvl_gas_limit ;
                node_rvl_storage_limit ; node_rvl_pubkey ;
                node_rvl_metadata } ->
           ((node_rvl_src, node_rvl_fee, node_rvl_counter, node_rvl_gas_limit,
             node_rvl_storage_limit), ((), node_rvl_pubkey, node_rvl_metadata)))
         (fun ((node_rvl_src, node_rvl_fee, node_rvl_counter,
                node_rvl_gas_limit,
                node_rvl_storage_limit),
               ((), node_rvl_pubkey, node_rvl_metadata)) ->
           { node_rvl_src ; node_rvl_fee ; node_rvl_counter ; node_rvl_gas_limit ;
             node_rvl_storage_limit ; node_rvl_pubkey; node_rvl_metadata }))
        (merge_objs
           manager_encoding
           (obj3
              (req "kind" (constant "reveal"))
              (req "public_key" string)
              (opt "metadata" manager_metadata_encoding)))

    let endorsement_encoding =
      (* let endorse_metadata_encoding =
       *   obj2
       *     (req "delegate" string)
       *     (req "slots" (list int)) in *)
      (conv
         (fun { node_endorse_block_level ; node_endorse_metadata } ->
            ((), node_endorse_block_level, node_endorse_metadata))
         (fun ((), node_endorse_block_level, node_endorse_metadata) ->
            { node_endorse_block_level ; node_endorse_metadata }))
        (obj3
           (req "kind" (constant "endorsement"))
           (req "level" int)
           (opt "metadata" op_metadata_encoding))

    let double_endorsement_evidence_op_encoding =
      conv
        (fun { ndee_branch ; ndee_level ; ndee_signature } ->
           (ndee_branch, ((), ndee_level), ndee_signature))
        (fun (ndee_branch, ((), ndee_level), ndee_signature) ->
           { ndee_branch ; ndee_level ; ndee_signature })
        (obj3
           (req "branch" string)
           (req "operations"
              (obj2
                 (req "kind" (constant "endorsement"))
                 (req "level" int)))
           (req "signature" string))

    let activation_encoding =
      (conv
         (fun { node_act_pkh; node_act_secret ; node_act_metadata } ->
            ((), node_act_pkh, node_act_secret, node_act_metadata))
         (fun ((), node_act_pkh, node_act_secret, node_act_metadata) ->
            { node_act_pkh ; node_act_secret ; node_act_metadata }))
        (obj4
           (req "kind" (constant "activate_account"))
           (req "pkh" string)
           (req "secret" string)
           (opt "metadata" op_metadata_encoding))

    let proposal_encoding =
      (conv
         (fun { node_prop_src ; node_prop_voting_period ;
                node_prop_proposals ; node_prop_metadata } ->
           ((), node_prop_src, node_prop_voting_period, node_prop_proposals,
            node_prop_metadata ))
         (fun ((), node_prop_src, node_prop_voting_period, node_prop_proposals,
               node_prop_metadata) ->
           { node_prop_src ; node_prop_voting_period ; node_prop_proposals ;
             node_prop_metadata }))
        (obj5
           (req "kind" (constant "proposals"))
           (req "source" string)
           (req "period" int32)
           (req "proposals" (list string))
           (opt "metadata" op_metadata_encoding))

    let ballot_encoding =
      (conv
         (fun { node_ballot_src ; node_ballot_voting_period ;
                node_ballot_proposal ; node_ballot_vote ; node_ballot_metadata } ->
           let node_ballot_vote = Tezos_utils.string_of_ballot_vote node_ballot_vote in
           ((), node_ballot_src, node_ballot_voting_period,
            node_ballot_proposal, node_ballot_vote, node_ballot_metadata))
         (fun ((), node_ballot_src, node_ballot_voting_period,
               node_ballot_proposal, node_ballot_vote, node_ballot_metadata) ->
           let node_ballot_vote = Tezos_utils.ballot_of_string node_ballot_vote in
           { node_ballot_src ; node_ballot_voting_period ;
             node_ballot_proposal ; node_ballot_vote ; node_ballot_metadata }))
        (obj6
           (req "kind" (constant "ballot"))
           (req "source" string)
           (req "period" int32)
           (req "proposal" string)
           (req "ballot" string)
           (opt "metadata" op_metadata_encoding))

    let activate_encoding =
      (obj3
         (req "kind" (constant "activate_protocol"))
         (req "hash" string)
         (opt "metadata" op_metadata_encoding))

    let activate_testnet_encoding =
      (obj3
         (req "kind" (constant "activate_test_protocol"))
         (req "hash" string)
         (opt "metadata" op_metadata_encoding))

    let seed_nonce_revelation_encoding =
      (conv
         (fun { node_seed_level; node_seed_nonce ; node_seed_metadata } ->
            ((), node_seed_level, node_seed_nonce,
             node_seed_metadata ))
         (fun ((), node_seed_level, node_seed_nonce, node_seed_metadata) ->
            { node_seed_level; node_seed_nonce ; node_seed_metadata} ))
        (obj4
           (req "kind" (constant "seed_nonce_revelation"))
           (req "level" int)
           (req "nonce" string)
           (opt "metadata" op_metadata_encoding))

    let double_endorsement_evidence_encoding =
      (conv
         (fun { node_double_endorsement1; node_double_endorsement2 ;
                node_double_endorsement_metadata } ->
           ((), node_double_endorsement1, node_double_endorsement2,
            node_double_endorsement_metadata))
         (fun ((), node_double_endorsement1, node_double_endorsement2,
               node_double_endorsement_metadata) ->
           { node_double_endorsement1; node_double_endorsement2 ;
             node_double_endorsement_metadata}))
        (obj4
           (req "kind" (constant "double_endorsement_evidence"))
           (req "op1" double_endorsement_evidence_op_encoding)
           (req "op2" double_endorsement_evidence_op_encoding)
           (opt "metadata" op_metadata_encoding))

    let double_baking_evidence_encoding =
      (conv
         (fun { node_double_bh1; node_double_bh2 ; node_double_bh_metadata }  ->
            ((), node_double_bh1, node_double_bh2, node_double_bh_metadata))
         (fun ((), node_double_bh1, node_double_bh2, node_double_bh_metadata) ->
            { node_double_bh1; node_double_bh2; node_double_bh_metadata } ))
        (obj4
           (req "kind" (constant "double_baking_evidence"))
           (req "bh1" (Block_header.encoding))
           (req "bh2" (Block_header.encoding))
           (opt "metadata" op_metadata_encoding))

    let contents_and_result_encoding =
      union [
        (* Anonymous *)
        case seed_nonce_revelation_encoding
          (function
            | NSeed_nonce_revelation s -> Some s
            | _ -> None)
          (fun s -> NSeed_nonce_revelation s) ;
        case (activation_encoding)
          (function
            | NActivation a -> Some a
            | _ -> None)
          (fun act -> NActivation act) ;
        case double_baking_evidence_encoding
          (function
            | NDouble_baking_evidence dbe -> Some dbe
            | _ -> None)
          (fun evidence -> NDouble_baking_evidence evidence) ;
        case double_endorsement_evidence_encoding
          (function
            | NDouble_endorsement_evidence dee -> Some dee
            | _ -> None)
          (fun dbe -> NDouble_endorsement_evidence dbe) ;

        (*  Signed > Amendment *)
        case proposal_encoding
          (function
            | NProposals prop -> Some prop
            | _ -> None)
          (fun prop -> NProposals prop);
        case ballot_encoding
          (function
            | NBallot ballot -> Some ballot
            | _ -> None)
          (fun ballot -> NBallot ballot);
        (* Signed > Manager *)
        case reveal_encoding
          (function
            | NReveal rvl -> Some rvl
            | _ -> None)
          (fun rvl -> NReveal rvl) ;
        case transaction_encoding
          (function
            | NTransaction tr -> Some tr
            | _ -> None)
          (fun tr -> NTransaction tr) ;
        case origination_encoding
          (function
            | NOrigination ori -> Some ori
            | _ -> None)
          (fun ori -> NOrigination ori) ;
        case delegation_encoding
          (function
            | NDelegation del -> Some del
            | _ -> None)
          (fun del -> NDelegation del) ;
        (* signed > Consensus *)
        case endorsement_encoding
          (function
            | NEndorsement e -> Some e
            | _ -> None)
          (fun c -> NEndorsement c) ;
        (* Signed > Dictator *)
        case activate_encoding
          (function _ -> None)
          (fun _hash -> NActivate) ;
        case activate_testnet_encoding
          (function _ -> None)
          (fun _hash -> NActivate)
      ]

    let contents_and_result_list_encoding =
      (list contents_and_result_encoding)

    let operation_data_and_receipt_encoding =
      (obj2
         (req "contents" contents_and_result_list_encoding)
         (opt "signature" Signature.encoding))

    let shell_header_encoding = obj1 (req "branch" Block_hash.encoding)

    let operation_encoding =
      conv
        (fun { node_op_protocol ; node_op_chain_id ; node_op_hash ; node_op_branch ;
               node_op_contents ; node_op_signature } ->
          ((node_op_protocol, node_op_chain_id, node_op_hash),
           (node_op_branch, (node_op_contents, node_op_signature))))

        (fun ((node_op_protocol, node_op_chain_id, node_op_hash),
              (node_op_branch,
               (node_op_contents, node_op_signature))) ->
          { node_op_protocol ; node_op_chain_id ; node_op_hash ; node_op_branch ;
            node_op_contents ; node_op_signature  })
        (merge_objs
           (obj3
              (req "protocol" string)
              (req "chain_id" Chain_id.encoding)
              (req "hash" Operation_hash.encoding))
           (merge_objs
              shell_header_encoding
              operation_data_and_receipt_encoding))

    let encoding = list @@ list operation_encoding

    let pending_parsed_operation =
      conv
        (fun ops ->
           List.map
             (fun { pending_hash ; pending_branch ;
                    pending_contents ; pending_signature } ->
               (pending_branch, pending_hash), (pending_contents, pending_signature)) ops)
        (fun ops ->
           List.map
             (fun ((pending_branch, pending_hash), (pending_contents, pending_signature)) ->
                { pending_hash ; pending_branch ;
                  pending_contents ; pending_signature }) ops)
        (list @@
         merge_objs
           (obj2
              (req "branch" string)
              (req "hash" string))
           operation_data_and_receipt_encoding)

    let pending_operation =
      (* let error_encoding =
       *   list
       *     (obj4
       *        (req "kind" string)
       *        (req "id" string)
       *        (req "expected" string)
       *        (req "provided" string)) in *)
      (* let refused_encoding =
       *   merge_objs
       *     (obj1 (req "hash" string))
       *     (merge_objs
       *        pending_parsed_operation
       *        (obj1 (req "error" error_encoding))) in *)
      conv
        (fun { applied ; refused ; branch_refused ;
               branch_delayed ; unprocessed } ->
          applied, refused, branch_refused, branch_delayed, unprocessed)
        (fun (applied, refused, branch_refused,
              branch_delayed, unprocessed) ->
          { applied ; refused ; branch_refused ;
            branch_delayed ; unprocessed })
        (obj5
           (req "applied" pending_parsed_operation)
           (req "refused" (list Json_encoding.any_value))
           (req "branch_refused" (list Json_encoding.any_value))
           (req "branch_delayed" (list Json_encoding.any_value))
           (req "unprocessed" (list Json_encoding.any_value)))

  end

  (* module Pending_operation = struct
   *   let shell_header_encoding =
   *     (obj1 (req "branch" string))
   *
   *   let op_encoding =
   *       (merge_objs
   *          shell_header_encoding
   *          (obj2
   *             (req "contents" Operation.contents_and_result_list_encoding)
   *             (opt "signature" string))
   *       )
   *
   *   let preapply_encoding =
   *     let error_encoding =
   *       list
   *         (obj4
   *            (req "kind" string)
   *            (req "id" string)
   *            (req "expected" string)
   *            (req "provided" string)) in
   *     let operation_encoding =
   *     conv
   *       (fun { pending_hash ; pending_branch ;
   *              pending_contents ; pending_signature } ->
   *         pending_hash, (pending_branch,
   *                        (pending_contents, pending_signature)))
   *       (fun ( pending_hash,
   *              (pending_branch, (pending_contents, pending_signature))) ->
   *       { pending_branch ; pending_contents ; pending_signature })
   *       merge_objs
   *         (obj1 (req "hash" string))
   *         op_encoding in
   *     let refused_encoding =
   *       merge_objs
   *         (obj1 (req "hash" string))
   *         (merge_objs
   *            op_encoding
   *            (obj1 (req "error" error_encoding))) in
   *     conv
   *       (fun (applied, refused, branch_refused, branch_delayed) ->
   *          applied, refused, branch_refused, branch_delayed)
   *       (fun (applied, refused, branch_refused, branch_delayed) ->
   *          applied, refused, branch_refused, branch_delayed)
   *       (obj4
   *          (req "applied" (list operation_encoding))
   *          (req "refused" (list refused_encoding))
   *          (req "branch_refused" (list refused_encoding))
   *          (req "branch_delayed" (list refused_encoding)))
   *
   *   let encoding =
   *     (conv
   *        (fun { applied ; refused ; branch_refused ;
   *               branch_delayed ; unprocessed } ->
   *          ((applied, refused, branch_refused, branch_delayed), unprocessed))
   *        (fun ((applied, refused, branch_refused, branch_delayed),
   *              unprocessed) ->
   *          { applied ; refused ; branch_refused ;
   *            branch_delayed ; unprocessed })
   *        (merge_objs
   *           (preapply_encoding)
   *           (obj1
   *              (req
   *                 "unprocessed"
   *                 (list op_encoding)))))
   *
   * end *)

  module Delegate = struct
    let encoding =
      conv
        (fun { delegate_balance ; delegate_frozen_balance ;
               delegate_staking_balance ;
               delegate_delegated_contracts ;
               delegate_delegated_balance ;
               delegate_deactivated ;
               delegate_grace_period } ->
          (delegate_balance, delegate_frozen_balance,
           Json_repr.to_any `Null, delegate_staking_balance,
           delegate_delegated_contracts, delegate_delegated_balance,
           delegate_deactivated, delegate_grace_period))
        (fun (delegate_balance, delegate_frozen_balance,
              _delegate_frozen_balance_by_cycle, delegate_staking_balance,
              delegate_delegated_contracts, delegate_delegated_balance,
              delegate_deactivated, delegate_grace_period) ->
          { delegate_balance ;
            delegate_frozen_balance ;
            delegate_staking_balance ;
            delegate_delegated_contracts ;
            delegate_delegated_balance ;
            delegate_deactivated ;
            delegate_grace_period })
        (obj8
           (req "balance" tez)
           (req "frozen_balance" tez)
           (req "frozen_balance_by_cycle" any_value)
           (req "staking_balance" tez)
           (req "delegated_contracts" (list string))
           (req "delegated_balance" tez)
           (req "deactivated" bool)
           (req "grace_period" int))

    let delegated_contracts_encoding = list string

  end

  module Test_chain_status = struct

    let encoding =
      union [
        case
          (obj1 (req "status" (constant "not_running")))
          (function CSNot_running -> Some () | _ -> None)
          (fun () -> CSNot_running) ;
        case
          (obj3
             (req "status" (constant "forking"))
             (req "protocol" Protocol_hash.encoding)
             (req "expiration" Time.encoding))
          (function
            | CSForking { protocol ; expiration } ->
              Some ((), protocol, expiration)
            | _ -> None)
          (fun ((), protocol, expiration) ->
             CSForking { protocol ; expiration }) ;
        case
          (obj5
             (req "status" (constant "running"))
             (req "chain_id" Chain_id.encoding)
             (req "genesis" Block_hash.encoding)
             (req "protocol" Protocol_hash.encoding)
             (req "expiration" Time.encoding))
          (function
            | CSRunning { chain_id ; genesis ; protocol ; expiration } ->
              Some ((), chain_id, genesis, protocol, expiration)
            | _ -> None)
          (fun ((), chain_id, genesis, protocol, expiration) ->
             CSRunning { chain_id ; genesis ; protocol ; expiration }) ;
      ]

  end

  module Block = struct

    let operation_list_quota_encoding =
      (obj2
         (req "max_size" int)
         (opt "max_op" int))

    let block_metadata_encoding =
      conv
        (fun { meta_protocol ; meta_next_protocol ; meta_test_chain_status ;
               meta_max_operations_ttl ; meta_max_operation_data_length ;
               meta_max_block_header_length ; meta_max_operation_list_length ;
               meta_header}
          ->
            ((meta_protocol, meta_next_protocol, meta_test_chain_status,
              meta_max_operations_ttl, meta_max_operation_data_length,
              meta_max_block_header_length,
              meta_max_operation_list_length), meta_header))
        (fun ((meta_protocol, meta_next_protocol, meta_test_chain_status,
               meta_max_operations_ttl, meta_max_operation_data_length,
               meta_max_block_header_length,
               meta_max_operation_list_length), meta_header) ->
          { meta_protocol ; meta_next_protocol ; meta_test_chain_status ;
            meta_max_operations_ttl ; meta_max_operation_data_length ;
            meta_max_block_header_length ; meta_max_operation_list_length ;
            meta_header })
        (merge_objs
           (obj7
              (req "protocol" string)
              (req "next_protocol" string)
              (req "test_chain_status" Test_chain_status.encoding)
              (req "max_operations_ttl" int)
              (req "max_operation_data_length" int)
              (req "max_block_header_length" int)
              (req "max_operation_list_length"
                 (list operation_list_quota_encoding)))
           Proto.block_header_metadata_encoding)

    let genesis_block_metadata_encoding =
      conv
        (fun { meta_protocol ; meta_next_protocol ; meta_test_chain_status ;
               meta_max_operations_ttl ; meta_max_operation_data_length ;
               meta_max_block_header_length ; meta_max_operation_list_length ;
               _ }
          ->
            (meta_protocol, meta_next_protocol, meta_test_chain_status,
             meta_max_operations_ttl, meta_max_operation_data_length,
             meta_max_block_header_length,
             meta_max_operation_list_length))
        (fun (meta_protocol, meta_next_protocol, meta_test_chain_status,
              meta_max_operations_ttl, meta_max_operation_data_length,
              meta_max_block_header_length,
              meta_max_operation_list_length) ->
          { meta_protocol ; meta_next_protocol ; meta_test_chain_status ;
            meta_max_operations_ttl ; meta_max_operation_data_length ;
            meta_max_block_header_length ; meta_max_operation_list_length ;
            meta_header = { header_meta_baker = "God" ;
                            header_meta_level = {
                              node_lvl_level = 0 ;
                              node_lvl_level_position = 0 ;
                              node_lvl_cycle = 0 ;
                              node_lvl_cycle_position = 0 ;
                              node_lvl_voting_period = 0 ;
                              node_lvl_voting_period_position = 0 ;
                              node_lvl_expected_commitment = false ;
                            } ;
                            header_meta_nonce_hash = None;
                            header_meta_consumed_gas = Z.zero;
                            header_meta_deactivated = [];
                            header_meta_voting_period_kind = NProposal ;
                            header_meta_balance_updates = None
                          }
          })
        (obj7
           (req "protocol" string)
           (req "next_protocol" string)
           (req "test_chain_status" Test_chain_status.encoding)
           (req "max_operations_ttl" int)
           (req "max_operation_data_length" int)
           (req "max_block_header_length" int)
           (req "max_operation_list_length"
              (list operation_list_quota_encoding)))

    let encoding =
      conv
        (fun { node_protocol ; node_chain_id ; node_hash ; node_header ; node_metadata ; node_operations } ->
           (node_protocol, node_chain_id, node_hash, node_header, node_metadata, node_operations))
        (fun (node_protocol, node_chain_id, node_hash, node_header, node_metadata, node_operations) ->
           { node_protocol ; node_chain_id ; node_hash ; node_header ; node_metadata ; node_operations })
        (obj6
           (req "protocol" string)
           (req "chain_id" Chain_id.encoding)
           (req "hash" Block_hash.encoding)
           (req "header" Block_header.encoding)
           (req "metadata" block_metadata_encoding)
           (req "operations"
              (list (list Operation.operation_encoding))))

    let encoding_fallback node_metadata =
      (conv
         (fun _ -> assert false)
         (fun
           ((node_protocol, node_chain_id, node_hash), node_header) ->
           { node_protocol ; node_chain_id ; node_hash ;
             node_header ; node_metadata ; node_operations = [] }))
        (merge_objs
           (obj3
              (req "protocol" string)
              (req "chain_id" Chain_id.encoding)
              (req "hash" Block_hash.encoding))
           Block_header.encoding)

    let genesis_encoding =
      conv
        (fun { node_protocol ; node_chain_id ; node_hash ; node_header ; node_metadata ; node_operations } ->
           (node_protocol, node_chain_id, node_hash, node_header, node_metadata, node_operations))
        (fun (node_protocol, node_chain_id, node_hash, node_header, node_metadata, node_operations) ->
           { node_protocol ; node_chain_id ; node_hash ; node_header ; node_metadata ; node_operations })
        (obj6
           (req "protocol" string)
           (req "chain_id" Chain_id.encoding)
           (req "hash" Block_hash.encoding)
           (req "header" Block_header.genesis_encoding)
           (req "metadata" genesis_block_metadata_encoding)
           (req "operations"
              (list (list Operation.operation_encoding))))
  end

  module Rolls = struct

    let roll_encoding =
      union [
        case
          int32
          (fun i ->
             let j = Int64.to_int32 i in
             if Int64.equal (Int64.of_int32 j) i then Some j else None)
          Int64.of_int32 ;
        case
          string
          (fun i -> Some (Int64.to_string i))
          Int64.of_string
      ]

    let rolls_encoding =
      (conv
         (fun _ -> assert false)
         (fun (c, i, rolls) ->
            c, i,
            List.map (fun (roll_owner, (roll_count, roll_change)) ->
                { roll_owner ; roll_count; roll_change }) rolls)
         (obj3
            (req "cycle" int)
            (req "index" int)
            (req "rolls"
               (list
                  (tup2
                     string
                     (tup2
                        int
                        roll_encoding))))))

    (*
    let snapshot_level_encoding =
      (conv
         (fun _ -> assert false)
         (fun i -> i))
        (obj1 (req "level" int))
*)

    let deleguees =
      (conv
         (fun _ -> assert false)
         (fun deleguees -> deleguees))
        (obj1 (req "delegators" (list string)))

    let delegation_balances =
      (conv
         (fun _ -> assert false)
         (fun delegation_balances -> delegation_balances))
        (obj1 (req "balances" (list (tup2 string tez))))

  end

  module Baking_rights = struct

    let baking_right_encoding =
      conv
        (fun { node_br_level ; node_br_delegate ; node_br_priority ; node_br_timestamp } ->
           (node_br_level, node_br_delegate, node_br_priority, node_br_timestamp))
        (fun (node_br_level, node_br_delegate, node_br_priority, node_br_timestamp) ->
           { node_br_level ; node_br_delegate ; node_br_priority ; node_br_timestamp })
        (obj4
           (req "level" int)
           (req "delegate" string)
           (req "priority" int)
           (opt "estimated_time" string))

    let encoding = list baking_right_encoding
  end

  module Endorsing_rights = struct
    let endorsing_right_encoding =
      conv
        (fun { node_er_level ; node_er_delegate ; node_er_slots ; node_er_estimated_time } ->
           (node_er_level, node_er_delegate, node_er_slots, node_er_estimated_time))
        (fun (node_er_level, node_er_delegate, node_er_slots, node_er_estimated_time) ->
           { node_er_level ; node_er_delegate ; node_er_slots ; node_er_estimated_time })
        (obj4
           (req "level" int)
           (req "delegate" string)
           (req "slots" (list int))
           (opt "estimated_time" string))
    let encoding = list endorsing_right_encoding
  end

  module Predecessor = struct
    let encoding =
      conv
        (fun _ -> assert false)
        (fun (_level, _proto, predecessor, _timestamp, _vpass, _ophash,
              _fitness, _context) -> predecessor)
        Block_header.shell_header_encoding
  end

  module Alternative_blocks = struct
    let encoding = list @@ list string
  end

  module Network = struct

    let versions =
      conv
        ( fun { v_name ; v_major ; v_minor } -> ( v_name , v_major , v_minor ) )
        ( fun ( v_name , v_major , v_minor ) -> { v_name ; v_major ; v_minor } )
        (obj3
           (req "name" string)
           (req "major" int)
           (req "minor" int)
        )

    let state_encoding =
      string_enum [
        "accepted", Accepted ;
        "running", Running ;
        "disconnected", Disconnected ;
      ]

    let id_point_encoding =
      (obj2
         (req "addr" string)
         (dft "port" int 8732))

    let time_encoding =
      (union [
          case
            string
            (fun s -> Some s)
            (fun s -> s) ;
          case
            int64
            (fun _ -> None)
            (fun i -> string_of_int @@ Int64.to_int i) ;
        ])

    let data_stats =
      conv
        (fun { total_sent ; total_recv ; current_inflow ; current_outflow } ->
           (total_sent, total_recv, current_inflow, current_outflow))
        (fun (total_sent, total_recv, current_inflow, current_outflow) ->
           { total_sent ; total_recv ; current_inflow ; current_outflow })
        (obj4
           (req "total_sent" int64)
           (req "total_recv" int64)
           (req "current_inflow" int)
           (req "current_outflow" int))

    let point_to_string = function
      | None -> None
      | Some ((addr, port), timestamp) ->
        Some (Printf.sprintf "%s:%d" addr port, timestamp)

    let encoding =
      let peer_encoding =
        conv
          (fun _ -> assert false)
          (fun (peer_id, ((score, trusted, conn_metadata, _, state, id_point, stat),
                          (last_failed_connection, last_rejected_connection,
                           last_established_connection, last_disconnection,
                           last_seen, last_miss))) ->
            let country = "", "" in (* Recomputed in the database *)
            let id_point =
              match id_point with
              | None -> None
              | Some (ip, port) -> Some (Printf.sprintf "%s:%d" ip port) in
            let last_failed_connection = point_to_string last_failed_connection in
            let last_rejected_connection = point_to_string last_rejected_connection in
            let last_established_connection = point_to_string last_established_connection in
            let last_disconnection = point_to_string last_disconnection in
            let last_seen = point_to_string last_seen in
            let last_miss = point_to_string last_miss in
            { peer_id; country; score ; trusted ; conn_metadata ;
              state ; id_point; stat ;
              last_failed_connection ; last_rejected_connection ;
              last_established_connection ; last_disconnection ;
              last_seen ; last_miss })
          (tup2
             string
             (merge_objs
                (obj7
                   (req "score" float)
                   (req "trusted" bool)
                   (opt "conn_metadata" conn_metadata_encoding)
                   (opt "peer_metadata" Json_encoding.any_value)
                   (req "state" state_encoding)
                   (opt "reachable_at" id_point_encoding)
                   (req "stat" data_stats))
                (obj6
                   (opt "last_failed_connection" ((tup2 id_point_encoding time_encoding)))
                   (opt "last_rejected_connection" ((tup2 id_point_encoding time_encoding)))
                   (opt "last_established_connection" ((tup2 id_point_encoding time_encoding)))
                   (opt "last_disconnection" ((tup2 id_point_encoding time_encoding)))
                   (opt "last_seen" ((tup2 id_point_encoding time_encoding)))
                   (opt "last_miss" ((tup2 id_point_encoding time_encoding)))))) in
      (list peer_encoding)

    (*
    let peer_to_string peer =
      match peer with
      | None -> ""
      | Some s-> s
*)

    (*    let to_peer point_id  = point_id *)

    (*
    let last_connection =
      function
      | None -> "", ""
      | Some (point, date) -> peer_to_string (Some point), date
*)
  end

  module Injection = struct

    let injection_encoding =
      obj3
        (req "signedOperationContents" string)
        (req "net_id" string)
        (req "force" bool)

    let result_encoding =
      let ok =
        obj1 (req "ok"
                (obj1 (req "injectedOperation" string))) in
      let outdated =
        obj3
          (req "kind" string)
          (req "id" string)
          (req "ecoproto"
             (list (obj5
                      (req "kind" string)
                      (req "id" string)
                      (req "contract" string)
                      (req "expected" int)
                      (req "found" int)
                   )))
      in
      let maybe_illformed =
        obj2
          (req "kind" string)
          (req "error" string)
      in
      let maybe_parse_error =
        obj3
          (req "kind" string)
          (req "id" string)
          (req "ecoproto"
             (list (obj2
                      (req "kind" string)
                      (req "id" string)
                   )))
      in
      let errors =
        union [
          case outdated
            (fun _ -> None)
            (fun _ -> Inj_outdated) ;
          case maybe_illformed
            (fun _ -> None)
            (fun _ -> Inj_illformed) ;
          case maybe_parse_error
            (fun _ -> None)
            (fun _ -> Inj_illformed) ;
          case any_schema
            (fun _ -> None)
            (fun s -> Inj_generic s) ;

        ]
      in
      let errors_list =
        obj1 (req "error"
                (list errors)) in
      union [
        case ok
          (fun _ -> None)
          (fun op -> Inj_ok op) ;
        case errors_list
          (fun _ -> None)
          (fun errs -> Inj_ko errs) ;
      ]

  end

end

module Encoding = Base


let int_of_string =
  union [
    case
      int
      (fun i -> Some i)
      (fun i -> i) ;
    case
      string
      (fun i -> Some (string_of_int i))
      int_of_string
  ]

open Tezos_constants
let constants =
  conv
    (fun
      {
        proof_of_work_nonce_size ;
        nonce_length ;
        max_revelations_per_block ;
        max_operation_data_length ;
        preserved_cycles ;
        blocks_per_cycle ;
        blocks_per_commitment ;
        blocks_per_roll_snapshot ;
        blocks_per_voting_period ;
        time_between_blocks ;
        endorsers_per_block ;
        hard_gas_limit_per_operation ;
        hard_gas_limit_per_block ;
        proof_of_work_threshold ;
        tokens_per_roll ;
        michelson_maximum_type_size ;
        seed_nonce_revelation_tip ;
        origination_burn ;
        block_security_deposit ;
        endorsement_security_deposit ;
        block_reward ;
        endorsement_reward ;
        cost_per_byte ;
        hard_storage_limit_per_operation
      }
      ->
        (
          proof_of_work_nonce_size ,
          nonce_length ,
          max_revelations_per_block ,
          max_operation_data_length ,
          preserved_cycles ,
          blocks_per_cycle ,
          blocks_per_commitment ,
          blocks_per_roll_snapshot ,
          blocks_per_voting_period ,
          time_between_blocks ,
          endorsers_per_block ,
          hard_gas_limit_per_operation ,
          hard_gas_limit_per_block ,
          proof_of_work_threshold ,
          tokens_per_roll ,
          michelson_maximum_type_size ,
          seed_nonce_revelation_tip ,
          origination_burn ,
          block_security_deposit ,
          endorsement_security_deposit ,
          block_reward ,
          endorsement_reward ,
          cost_per_byte ,
          hard_storage_limit_per_operation
        )
    )
    (fun
      (
        proof_of_work_nonce_size ,
        nonce_length ,
        max_revelations_per_block ,
        max_operation_data_length ,
        preserved_cycles ,
        blocks_per_cycle ,
        blocks_per_commitment ,
        blocks_per_roll_snapshot ,
        blocks_per_voting_period ,
        time_between_blocks ,
        endorsers_per_block ,
        hard_gas_limit_per_operation ,
        hard_gas_limit_per_block ,
        proof_of_work_threshold ,
        tokens_per_roll ,
        michelson_maximum_type_size ,
        seed_nonce_revelation_tip ,
        origination_burn ,
        block_security_deposit ,
        endorsement_security_deposit ,
        block_reward ,
        endorsement_reward ,
        cost_per_byte ,
        hard_storage_limit_per_operation
      )
      ->
        {
          proof_of_work_nonce_size ;
          nonce_length ;
          max_revelations_per_block ;
          max_operation_data_length ;
          preserved_cycles ;
          blocks_per_cycle ;
          blocks_per_commitment ;
          blocks_per_roll_snapshot ;
          blocks_per_voting_period ;
          time_between_blocks ;
          endorsers_per_block ;
          hard_gas_limit_per_operation ;
          hard_gas_limit_per_block ;
          proof_of_work_threshold ;
          tokens_per_roll ;
          michelson_maximum_type_size ;
          seed_nonce_revelation_tip ;
          origination_burn ;
          block_security_deposit ;
          endorsement_security_deposit ;
          block_reward ;
          endorsement_reward ;
          cost_per_byte ;
          hard_storage_limit_per_operation
        }
    )
    (EzEncoding.obj24
       (dft "proof_of_work_nonce_size" int constants.proof_of_work_nonce_size)
       (dft "nonce_length" int constants.nonce_length)
       (dft "max_revelations_per_block" int constants.max_revelations_per_block)
       (dft "max_operation_data_length" int constants.max_operation_data_length)
       (dft "preserved_cycles" int constants.preserved_cycles)
       (dft "blocks_per_cycle" int constants.blocks_per_cycle)
       (dft "blocks_per_commitment" int constants.blocks_per_commitment)
       (dft "blocks_per_roll_snapshot" int constants.blocks_per_roll_snapshot)
       (dft "blocks_per_voting_period" int constants.blocks_per_voting_period)
       (dft "time_between_blocks" (list int_of_string) constants.time_between_blocks)
       (dft "endorsers_per_block" int constants.endorsers_per_block)
       (dft "hard_gas_limit_per_operation" int64 constants.hard_gas_limit_per_operation)
       (dft "hard_gas_limit_per_block" int64 constants.hard_gas_limit_per_block)
       (dft "proof_of_work_threshold" int64 constants.proof_of_work_threshold)
       (dft "tokens_per_roll" int64 constants.tokens_per_roll)
       (dft "michelson_maximum_type_size" int constants.michelson_maximum_type_size)
       (dft "seed_nonce_revelation_tip" int64 constants.seed_nonce_revelation_tip)
       (dft "origination_burn" int64 constants.origination_burn)
       (dft "block_security_deposit" int64 constants.block_security_deposit)
       (dft "endorsement_security_deposit" int64 constants.endorsement_security_deposit)
       (dft "block_reward" int64 constants.block_reward)
       (dft "endorsement_reward" int64 constants.endorsement_reward)
       (dft "cost_per_byte" int64 constants.cost_per_byte)
       (dft "hard_storage_limit_per_operation" int64 constants.hard_storage_limit_per_operation)
    )
