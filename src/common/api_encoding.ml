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
open Data_types
open Tezos_types

let float = Json_encoding.float
let int64 = EzEncoding.int64
let tez = EzEncoding.int64

let z_encoding = Tezos_encoding.z_encoding

let account_name_encoding =
  conv
    (fun {tz; alias} -> (tz, alias))
    (fun (tz, alias) -> {tz; alias})
    (obj2
       (req "tz" string)
       (opt "alias" string))

module Micheline = Tezos_encoding.Micheline

module V1 = struct

  module Op = struct

    let transaction_encoding =
      conv
        (fun { tr_src; tr_amount; tr_counter ; tr_fee ; tr_gas_limit ; tr_storage_limit ;
               tr_dst; tr_parameters ; tr_failed ;
               tr_internal ; tr_burn; tr_op_level; tr_timestamp } ->
          let tr_parameters = match tr_parameters with
            | None -> None
            | Some p -> Some p in
          ("transaction", tr_src, tr_amount, tr_dst, None, tr_parameters,
           tr_failed, tr_internal, tr_burn),
          (tr_counter, tr_fee, tr_gas_limit, tr_storage_limit, tr_op_level,
           tr_timestamp))
        (fun
          ((_k, tr_src, tr_amount, tr_dst, p, str_p,
            tr_failed, tr_internal, tr_burn),
           (tr_counter, tr_fee, tr_gas_limit, tr_storage_limit, tr_op_level,
            tr_timestamp)) ->
          let tr_parameters =  match p, str_p with
            | None, None -> None
            | Some mic, None -> Some (Micheline.encode mic)
            | None, Some mic_str -> Some mic_str
            | _, _ -> assert false in
          { tr_src; tr_amount; tr_counter ; tr_fee ; tr_gas_limit ; tr_storage_limit ;
            tr_dst; tr_parameters; tr_failed ; tr_internal ; tr_burn; tr_op_level;
            tr_timestamp })
        (merge_objs
           (obj9
              (req "kind" string)
              (req "src" account_name_encoding)
              (req "amount" tez)
              (req "destination" account_name_encoding)
              (opt "parameters" Micheline.script_expr_encoding)
              (opt "str_parameters" string)
              (req "failed" bool)
              (req "internal" bool)
              (req "burn" tez))
           (obj6
              (req "counter" int32)
              (req "fee" int64)
              (req "gas_limit" z_encoding)
              (req "storage_limit" z_encoding)
              (req "op_level" int)
              (req "timestamp" string)))

    let reveal_encoding =
      (conv
         (fun { rvl_src; rvl_pubkey ; rvl_counter ; rvl_fee ;
                rvl_gas_limit ; rvl_storage_limit ;
                rvl_failed ; rvl_internal } ->
           ("reveal", rvl_src, rvl_pubkey, rvl_counter, rvl_fee,
            rvl_gas_limit, rvl_storage_limit, rvl_failed, rvl_internal))
         (fun (_k, rvl_src, rvl_pubkey, rvl_counter, rvl_fee,
               rvl_gas_limit, rvl_storage_limit,
               rvl_failed, rvl_internal) ->
           { rvl_src ; rvl_pubkey ; rvl_counter ; rvl_fee ;
             rvl_gas_limit ; rvl_storage_limit ;
             rvl_failed ; rvl_internal}))
        (obj9
           (req "kind" string)
           (req "src" account_name_encoding)
           (req "public_key" string)
           (req "counter" int32)
           (req "fee" int64)
           (req "gas_limit" z_encoding)
           (req "storage_limit" z_encoding)
           (req "failed" bool)
           (req "internal" bool))

    let origination_encoding =
      (conv
         (fun { or_src ; or_manager ; or_delegate ; or_script ; or_spendable ;
                or_delegatable ; or_balance ; or_counter ; or_fee ;
                or_gas_limit ; or_storage_limit ; or_tz1 ;
                or_failed ; or_internal ; or_burn } ->
           let str_script = match or_script with
             | None -> None
             | Some code ->
               Some (code.sc_code,
                     code.sc_storage) in
           let or_delegate =
             if or_delegate.tz = "" then None else Some or_delegate in
           (("origination", or_src, or_manager, or_balance, Some or_spendable,
             Some or_delegatable, or_delegate, None, Some or_tz1, str_script),
            (or_failed, or_internal, or_burn,
             or_counter, or_fee, or_gas_limit, or_storage_limit)))
         (fun ((_k, or_src, or_manager, or_balance, or_spendable,
                or_delegatable,  or_delegate, script, or_tz1, str_script),
               (or_failed, or_internal, or_burn,
                or_counter, or_fee, or_gas_limit, or_storage_limit)) ->
           let or_tz1 = Utils.unopt or_tz1 ~default:{tz="";alias=None} in
           let or_delegate = Utils.unopt or_delegate ~default:{tz="";alias=None} in
           let or_spendable = Utils.unopt or_spendable ~default:false in
           let or_delegatable = Utils.unopt or_delegatable ~default:false in
           let or_script = match script, str_script with
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
           { or_src;
             or_manager ;
             or_delegate ;
             or_script ;
             or_spendable ;
             or_delegatable ;
             or_balance ;
             or_counter ;
             or_fee ;
             or_gas_limit ;
             or_storage_limit ;
             or_tz1 ;
             or_failed ;
             or_internal ;
             or_burn })
         (merge_objs
            (obj10
               (req "kind" string)
               (req "src" account_name_encoding)
               (req "managerPubkey" account_name_encoding)
               (req "balance" tez)
               (opt "spendable" bool)
               (opt "delegatable" bool)
               (opt "delegate" account_name_encoding)
               (opt "script" Micheline.script_encoding)
               (* ocp field *)
               (opt "tz1" account_name_encoding)
               (opt "str_script" Micheline.script_str_encoding)
            )
            (obj7
               (req "failed" bool)
               (req "internal" bool)
               (req "burn_tez" tez)
               (req "counter" int32)
               (req "fee" int64)
               (req "gas_limit" z_encoding)
               (req "storage_limit" z_encoding))))

    let delegation_encoding =
      (conv
         (fun { del_src ; del_delegate ; del_counter ; del_fee ;
                del_gas_limit ; del_storage_limit ;
                del_failed ; del_internal } ->
           let del_delegate =
             if del_delegate.tz = "" then None else Some del_delegate in
           ("delegation", del_src, del_delegate, del_counter, del_fee,
            del_gas_limit, del_storage_limit, del_failed, del_internal ))
         (fun (_k, del_src, del_delegate, del_counter, del_fee, del_gas_limit,
               del_storage_limit, del_failed, del_internal) ->
           let del_delegate = Utils.unopt del_delegate ~default:{tz="";alias=None} in
           { del_src ; del_delegate ; del_counter ; del_fee ; del_gas_limit ;
             del_storage_limit ; del_failed ; del_internal }))
        (obj9
           (req "kind" string)
           (req "src" account_name_encoding)
           (opt "delegate" account_name_encoding)
           (req "counter" int32)
           (req "fee" int64)
           (req "gas_limit" z_encoding)
           (req "storage_limit" z_encoding)
           (req "failed" bool)
           (req "internal" bool))

    let endorsement_encoding =
      conv
        (fun { endorse_src ; endorse_block_hash; endorse_block_level;
               endorse_slot; endorse_op_level; endorse_priority; endorse_timestamp }
          -> ("endorsement", endorse_block_hash, endorse_block_level,
              endorse_src, endorse_slot, endorse_op_level, endorse_priority,
              endorse_timestamp))
        (fun (_k,  endorse_block_hash, endorse_block_level, endorse_src,
              endorse_slot, endorse_op_level, endorse_priority, endorse_timestamp)
          -> { endorse_src; endorse_block_hash; endorse_block_level;
               endorse_slot; endorse_op_level; endorse_priority; endorse_timestamp })
        (obj8
           (req "kind" string)
           (req "block" string)
           (req "level" int)
           (* ocp field *)
           (req "endorser" account_name_encoding)
           (req "slots" (list int))
           (req "op_level" int)
           (req "priority" int)
           (req "timestamp" string))

    let proposal_encoding =
      (obj3
         (req "kind" string)
         (req "period" int32)
         (req "proposals" (list string)))

    let ballot_encoding =
      (obj4
         (req "kind" string)
         (req "period" int32)
         (req "proposal" string)
         (req "ballot" string))

    let seed_nonce_revelation_encoding =
      (conv
         (fun { seed_level; seed_nonce } -> ("nonce", seed_level, seed_nonce))
         (fun (_k, seed_level, seed_nonce) -> { seed_level; seed_nonce } ))
        (obj3
           (req "kind" string)
           (req "level" int)
           (req "nonce" string))

    let activation_encoding =
      (conv
         (fun { act_pkh; act_secret } ->
            ("activation", act_pkh, act_secret))
         (fun (_, act_pkh, act_secret) -> { act_pkh ; act_secret }))
        (obj3
           (req "kind" string)
           (req "pkh" account_name_encoding)
           (req "secret" string))

    let double_baking_evidence_encoding =
      conv
        (fun {double_baking_header1; double_baking_header2;
              double_baking_main ; double_baking_accused ;
              double_baking_denouncer ; double_baking_lost_deposit ;
              double_baking_lost_rewards ; double_baking_lost_fees;
              double_baking_gain_rewards
             } ->
          ("double_baking_evidence", double_baking_header1, double_baking_header2,
           double_baking_main, double_baking_accused,
           double_baking_denouncer, double_baking_lost_deposit,
           double_baking_lost_rewards, double_baking_lost_fees,
           double_baking_gain_rewards))
        (fun (_k, double_baking_header1, double_baking_header2,
              double_baking_main, double_baking_accused,
              double_baking_denouncer, double_baking_lost_deposit,
              double_baking_lost_rewards, double_baking_lost_fees,
              double_baking_gain_rewards) ->
          {double_baking_header1; double_baking_header2; double_baking_main ;
           double_baking_accused ; double_baking_denouncer ;
           double_baking_lost_deposit ; double_baking_lost_rewards ;
           double_baking_lost_fees; double_baking_gain_rewards})
        (obj10
           (req "kind" string)
           (req "op1" (Tezos_encoding.Encoding.Block_header.encoding))
           (req "op2" (Tezos_encoding.Encoding.Block_header.encoding))
           (req "main" int)
           (req "offender" account_name_encoding)
           (req "denouncer" account_name_encoding)
           (req "lost_deposit" int64)
           (req "lost_rewards" int64)
           (req "lost_fees" int64)
           (req "gain_rewards" int64))

    let double_endorsement_evidence_encoding op_encoding =
      conv
        (fun {double_endorsement1; double_endorsement2} ->
           ("double_endorsement_evidence", double_endorsement1, double_endorsement2))
        (fun (_k, double_endorsement1, double_endorsement2) ->
           {double_endorsement1; double_endorsement2})
        (obj3
           (req "kind" string)
           (req "op1" op_encoding)
           (req "op2" op_encoding))

    let manager_encoding =
      (obj3
         (req "kind" string)
         (req "source" account_name_encoding)
         (req "operations"
            (list (union [
                 case reveal_encoding
                   (function
                     | Reveal rvl -> Some rvl
                     | _ -> None)
                   (fun rvl -> Reveal rvl) ;
                 case transaction_encoding
                   (function
                     | Transaction tr -> Some tr
                     | _ -> None)
                   (fun tr -> Transaction tr) ;
                 case origination_encoding
                   (function
                     | Origination ori -> Some ori
                     | _ -> None)
                   (fun ori -> Origination ori) ;
                 case delegation_encoding
                   (function
                     | Delegation del -> Some del
                     | _ -> None)
                   (fun del -> Delegation del) ]))))

    let consensus_encoding = endorsement_encoding

    let amendment_encoding =
      merge_objs
        (obj1 (req "source" account_name_encoding))
        (union [
            case proposal_encoding
              (fun _ -> None)
              (fun (_k, prop_voting_period, prop_proposals) ->
                 Proposal { prop_voting_period ; prop_proposals }) ;
            case ballot_encoding
              (fun _ -> None)
              (fun (_k, ballot_voting_period,
                    ballot_proposal, ballot_vote) ->
                let ballot_vote = Tezos_utils.ballot_of_string ballot_vote in
                Ballot
                  {  ballot_voting_period ; ballot_proposal ; ballot_vote })
          ])

    let dictator_encoding =
      let activate_encoding =
        obj2
          (req "chain" string) (* needs to be "activate" *)
          (req "hash" string) in
      let activate_testnet_encoding =
        obj2
          (req "chain" string) (* needs to be "activate_testchain" *)
          (req "hash" string) in
      union [
        case activate_encoding
          (function _ -> None)
          (fun (_chain, _hash) -> Activate) ;
        case activate_testnet_encoding
          (function _ -> None)
          (fun (_chain, _hash) -> Activate)
      ]

    let signed_operation_encoding =
      union [
        case consensus_encoding
          (function
            | Consensus Endorsement e -> Some e
            | _ -> None)
          (fun c -> Consensus (Endorsement c)) ;
        case manager_encoding
          (function
            | Manager m -> Some m
            | _ -> None)
          (fun m -> Manager m) ;
        case amendment_encoding
          (fun _ -> None)
          (fun (source, am) -> Amendment (source, am)) ;
        case dictator_encoding
          (fun _ -> None)
          (fun d -> Dictator d)
      ]

    let unsigned_operation_encoding encoding =
      obj1
        (req "operations"
           (list
              (union [
                  case seed_nonce_revelation_encoding
                    (function
                      | Seed_nonce_revelation s -> Some s
                      | _ -> None)
                    (fun s -> Seed_nonce_revelation s) ;
                  case (activation_encoding)
                    (function
                      | Activation a -> Some a
                      | _ -> None)
                    (fun act -> Activation act) ;
                  case (double_baking_evidence_encoding)
                    (function
                      | Double_baking_evidence dbe -> Some dbe
                      | _ -> None)
                    (fun evidence -> Double_baking_evidence evidence) ;
                  case (double_endorsement_evidence_encoding encoding)
                    (function
                      | Double_endorsement_evidence dee -> Some dee
                      | _ -> None)
                    (fun evidence -> Double_endorsement_evidence evidence)])))


    let proto_operation_encoding encoding =
      union [
        case (unsigned_operation_encoding encoding)
          (function
            | Anonymous a -> Some a
            | _ -> None)
          (fun a -> Anonymous a) ;
        case signed_operation_encoding
          (function
            | Sourced s -> Some s
            | _ -> None)
          (fun s -> Sourced s)
      ]

    let operation_encoding =
      mu "operation"
        (fun encoding ->
           proto_operation_encoding encoding)

    let signed_proto_operation_encoding =
      merge_objs
        operation_encoding
        (obj1 (opt "signature" string))

    let operation_type =
      (list (list
               (merge_objs
                  signed_proto_operation_encoding
                  (obj3
                     (req "hash" string)
                     (opt "net_id" string)
                     (req "branch" string)))))

    let parsed_op =
      array
        (merge_objs
           signed_proto_operation_encoding
           (obj1
              (req "branch" string)))
  end



  module Protocol = struct
    let encoding =
      (conv
         (fun { proto_name; proto_hash } -> ( proto_name, proto_hash ))
         (fun ( proto_name, proto_hash ) -> { proto_name; proto_hash } )
         (obj2
            (req "name" string)
            (req "hash" string)))
  end

  module BakeOp = struct

    let encoding =
      conv
        (fun {bk_block_hash; bk_baker_hash; bk_level; bk_cycle ; bk_priority ;
              bk_distance_level; bk_fees; bk_bktime; bk_baked}
          -> (bk_block_hash, bk_baker_hash, bk_level, bk_cycle, bk_priority,
              bk_distance_level, bk_fees, bk_bktime, bk_baked))
        (fun (bk_block_hash, bk_baker_hash, bk_level, bk_cycle, bk_priority,
              bk_distance_level, bk_fees, bk_bktime, bk_baked)
          -> {bk_block_hash; bk_baker_hash; bk_level; bk_cycle ; bk_priority;
              bk_distance_level; bk_fees; bk_bktime; bk_baked})
        (obj9
           (req "block_hash" string)
           (req "baker_hash" account_name_encoding)
           (req "level" int)
           (req "cycle" int)
           (req "priority" int)
           (req "distance_level" int)
           (req "fees" tez)
           (req "bake_time" int)
           (req "baked" bool))
    let bakings = list encoding

  end

  module BakeEndorsementOp = struct

    let encoding =
      (conv
         (fun {ebk_block; ebk_source; ebk_level; ebk_cycle; ebk_priority;
               ebk_dist; ebk_slots; ebk_lr_nslot}
           -> (ebk_block, ebk_source, ebk_level, ebk_cycle, ebk_priority,
               ebk_dist, ebk_slots, ebk_lr_nslot))
         (fun (ebk_block, ebk_source, ebk_level, ebk_cycle, ebk_priority,
               ebk_dist, ebk_slots, ebk_lr_nslot)
           -> {ebk_block; ebk_source; ebk_level; ebk_cycle; ebk_priority;
               ebk_dist; ebk_slots; ebk_lr_nslot})
         (obj8
            (opt "block" string)
            (opt "source" account_name_encoding)
            (req "level" int)
            (opt "cycle" int)
            (opt "priority" int)
            (opt "distance_level" int)
            (opt "slots" (list int))
            (req "lr_nslot" int)))
    let bakings = list encoding

  end

  module CycleBakeOp = struct

    let count_encoding =
      conv
        (fun {cnt_all; cnt_miss; cnt_steal} ->
           (cnt_all, cnt_miss, cnt_steal))
        (fun (cnt_all, cnt_miss, cnt_steal) ->
           {cnt_all; cnt_miss; cnt_steal})
        (obj3
           (req "count_all" int64)
           (req "count_miss" int64)
           (req "count_steal" int64))

    let tez_encoding =
      conv
        (fun {tez_fee; tez_reward; tez_deposit} -> (tez_fee, tez_reward, tez_deposit))
        (fun (tez_fee, tez_reward, tez_deposit) -> {tez_fee; tez_reward; tez_deposit})
        (obj3
           (req "fee" int64)
           (req "reward" int64)
           (req "deposit" int64))

    let encoding =
      conv
        (fun {cbk_cycle; cbk_depth; cbk_count; cbk_tez; cbk_priority; cbk_bktime}
          -> (cbk_cycle, cbk_depth, cbk_count, cbk_tez, cbk_priority, cbk_bktime))
        (fun (cbk_cycle, cbk_depth, cbk_count, cbk_tez, cbk_priority, cbk_bktime)
          -> {cbk_cycle; cbk_depth; cbk_count; cbk_tez; cbk_priority; cbk_bktime})
        (obj6
           (req "cycle" int)
           (req "depth" int)
           (req "count" count_encoding)
           (req "tez" tez_encoding)
           (opt "priority" float)
           (opt "bake_time" int))
    let bakings = list encoding

  end

  module CycleEndorsementOp = struct

    let encoding =
      conv
        (fun {ced_cycle; ced_depth; ced_slots; ced_tez; ced_priority}
          -> (ced_cycle, ced_depth, ced_slots, ced_tez, ced_priority))
        (fun (ced_cycle, ced_depth, ced_slots, ced_tez, ced_priority)
          -> {ced_cycle; ced_depth; ced_slots; ced_tez; ced_priority})
        (obj5
           (req "cycle" int)
           (req "depth" int)
           (req "slots" CycleBakeOp.count_encoding)
           (req "tez" CycleBakeOp.tez_encoding)
           (req "priority" float))
    let bakings = list encoding

  end

  module Rights = struct

    let encoding =
      conv
        (fun ({ r_level; r_bakers; r_endorsers ; r_bakers_priority; r_baked})
          -> (r_level, r_bakers, r_endorsers, r_bakers_priority, r_baked))
        (fun (r_level, r_bakers, r_endorsers, r_bakers_priority, r_baked)
          -> ({ r_level; r_bakers; r_endorsers; r_bakers_priority; r_baked}))
        (obj5
           (req "level" int)
           (req "bakers" (list account_name_encoding))
           (req "endorsers" (list account_name_encoding))
           (req "bakers_priority" (list int))
           (opt "baked" (tup2 account_name_encoding int)))

    let rights = list encoding

  end

  module BakerRights = struct

    let encoding =
      (conv
         (fun ({ br_level; br_cycle; br_priority; br_depth }) ->
            ( br_level, br_cycle, br_priority, br_depth ))
         (fun ( br_level, br_cycle, br_priority, br_depth ) ->
            ({ br_level; br_cycle; br_priority; br_depth }))
         (obj4
            (req "level" int)
            (req "cycle" int)
            (req "priority" int)
            (req "depth" int)))

    let rights = list encoding

  end

  module EndorserRights = struct

    let encoding =
      (conv
         (fun ({ er_level; er_cycle; er_nslot; er_depth }) ->
            ( er_level, er_cycle, er_nslot, er_depth ))
         (fun ( er_level, er_cycle, er_nslot, er_depth ) ->
            ({ er_level; er_cycle; er_nslot; er_depth }))
         (obj4
            (req "level" int)
            (req "cycle" int)
            (req "nslot" int)
            (req "depth" int)))

    let rights = list encoding

  end

  module CycleRights = struct
    let encoding =
      (conv
         (fun { cr_cycle; cr_nblocks; cr_priority} ->
            ( cr_cycle, cr_nblocks, cr_priority ))
         (fun ( cr_cycle, cr_nblocks, cr_priority ) ->
            { cr_cycle; cr_nblocks; cr_priority})
         (obj3
            (req "cycle" int)
            (req "nblocks" int)
            (req "priority" float)))

    let rights = list encoding
  end

  module Block = struct

    let operation_encoding =
      obj3
        (req "hash" string)
        (req "branch" string)
        (req "data" string)

    let encoding =
      let block1 =
        obj10
          (req "hash" string)
          (req "predecessor_hash" string)
          (req "fitness" string)
          (req "timestamp" string)
          (req "validation_pass" int)
          (req "operations" (list (list operation_encoding)))
          (req "protocol" Protocol.encoding)
          (req "test_protocol" Protocol.encoding)
          (req "network" string)
          (req "test_network" string)
      in
      let block2 =
        obj10
          (req "test_network_expiration" string)
          (req "baker" account_name_encoding)
          (dft "nb_operations" int 0)
          (req "priority" int)
          (req "level" int)
          (req "commited_nonce_hash" string)
          (req "pow_nonce" string)
          (req "proto" int)
          (req "data" string)
          (req "signature" string)
      in
      let block3 =
        obj3
          (req "volume" tez)
          (req "fees" tez)
          (req "distance_level" int)
      in
      (conv
         (fun { hash; predecessor_hash; fitness; baker;
                timestamp; validation_pass; operations; protocol; nb_operations ;
                test_protocol; network; test_network;
                test_network_expiration; priority; level;
                commited_nonce_hash; pow_nonce; proto; data; signature;
                volume; fees; distance_level }
           ->
             let timestamp = Date.to_string timestamp in
             ((hash, predecessor_hash, fitness,
               timestamp, validation_pass, [ operations ], protocol,
               test_protocol, network, test_network),
              (test_network_expiration,
               baker, nb_operations, priority, level, commited_nonce_hash,
               pow_nonce, proto, data, signature)), (volume, fees, distance_level))
         (fun (((hash, predecessor_hash, fitness,
                 timestamp, validation_pass, operations, protocol,
                 test_protocol, network, test_network),
                (test_network_expiration,
                 baker, nb_operations, priority, level, commited_nonce_hash,
                 pow_nonce, proto, data, signature)),
               (volume, fees, distance_level))
           ->
             let timestamp = Date.from_string timestamp in
             { hash; predecessor_hash; fitness; baker ;
               nb_operations ;
               timestamp; validation_pass ;
               operations = List.flatten operations ; protocol;
               test_protocol; network; test_network;
               test_network_expiration; priority; level;
               commited_nonce_hash; pow_nonce; proto; data; signature;
               volume; fees; distance_level } ))
        (merge_objs
           (merge_objs block1 block2)
           block3)

    let blocks = list encoding

  end

  module Nonce_hash = struct
    let encoding =
      let op =
        obj2
          (req "operation_hash" string)
          (req "levels" (list int)) in
      obj2
        (req "cycle" int)
        (req "nonces" (list op))

  end

  module Level = struct
    let level_encoding =
      (obj7
         (req "level" int)
         (req "level_position" int)
         (req "cycle" int)
         (req "cycle_position" int)
         (req "voting_period" int)
         (req "voting_period_position" int)
         (req "expected_commitment" bool))


    let encoding =
      (conv
         (fun { lvl_level; lvl_level_position;
                lvl_cycle; lvl_cycle_position;
                lvl_voting_period; lvl_voting_period_position }
           -> (lvl_level, lvl_level_position, lvl_cycle, lvl_cycle_position,
               lvl_voting_period, lvl_voting_period_position, false))
         (fun (lvl_level, lvl_level_position, lvl_cycle, lvl_cycle_position,
               lvl_voting_period, lvl_voting_period_position, _) ->
           { lvl_level; lvl_level_position;
             lvl_cycle; lvl_cycle_position;
             lvl_voting_period; lvl_voting_period_position } ))
        level_encoding

  end

  module Health = struct
    let encoding =
      conv
        (fun ({ cycle_start_level ; cycle_end_level ;
                cycle_volume ; cycle_fees ;
                cycle_bakers ; cycle_endorsers ;
                cycle_date_start ; cycle_date_end ;
                endorsements_rate ; main_endorsements_rate ;
                alt_endorsements_rate ; empty_endorsements_rate ;
                double_endorsements ; main_revelation_rate ;
                alternative_heads_number ; switch_number ;
                longest_switch_depth ; mean_priority ; score_priority ;
                biggest_block_volume ; biggest_block_fees ; top_baker})
          -> (((cycle_start_level, cycle_end_level,
                cycle_volume, cycle_fees,
                cycle_bakers, cycle_endorsers,
                endorsements_rate, main_endorsements_rate,
                alt_endorsements_rate, empty_endorsements_rate),
               (double_endorsements, main_revelation_rate,
                alternative_heads_number, switch_number,
                longest_switch_depth, mean_priority, score_priority,
                biggest_block_volume, biggest_block_fees, top_baker )),
              (cycle_date_start, cycle_date_end)))
        (fun (((cycle_start_level, cycle_end_level,
                cycle_volume, cycle_fees,
                cycle_bakers, cycle_endorsers,
                endorsements_rate, main_endorsements_rate,
                alt_endorsements_rate, empty_endorsements_rate),
               (double_endorsements, main_revelation_rate,
                alternative_heads_number, switch_number,
                longest_switch_depth, mean_priority, score_priority,
                biggest_block_volume, biggest_block_fees, top_baker )),
              (cycle_date_start, cycle_date_end)) ->
          { cycle_start_level ; cycle_end_level ; cycle_volume ; cycle_fees ;
            cycle_bakers ; cycle_endorsers ; cycle_date_start ; cycle_date_end ;
            endorsements_rate ;
            main_endorsements_rate ; alt_endorsements_rate ;
            empty_endorsements_rate ; double_endorsements ;
            main_revelation_rate ; alternative_heads_number ;
            switch_number ; longest_switch_depth ; mean_priority ;
            score_priority ; biggest_block_volume ;
            biggest_block_fees; top_baker })
        (merge_objs
           (merge_objs
              (obj10
                 (req "cycle_start_level" int)
                 (req "cycle_end_level" int)
                 (req "cycle_volume" tez)
                 (req "cycle_fees" tez)
                 (req "cycle_bakers" int)
                 (req "cycle_endorsers" int)
                 (req "endorsements_rate" float)
                 (req "main_endorsements_rate" float)
                 (req "alt_endorsements_rate" float)
                 (req "empty_endorsements_rate" float))
              (obj10
                 (req "double_endorsements" int)
                 (req "main_revelation_rate" float)
                 (req "alternative_heads_number" int)
                 (req "switch_number" int)
                 (req "longest_switch_depth" int)
                 (req "mean_priority" float)
                 (req "score_priority" float)
                 (req "big_block_volume" (tup2 string int))
                 (req "big_block_fees" (tup2 string int))
                 (req "top_baker" account_name_encoding)))
           (obj2
              (req "cycle_date_start" (tup3 int int int))
              (req "cycle_date_end" (tup3 int int int))))

  end

  module Charts = struct

    let per_day_encoding kind =
      conv (fun { pd_days; pd_value } -> (pd_days, pd_value))
        (fun ( pd_days, pd_value) -> { pd_days; pd_value })
        (obj2
           (req "days" (array string))
           (req "value" (array kind)))

    let int_per_day_encoding = per_day_encoding int
    let float_per_day_encoding = per_day_encoding float
    let int64_per_day_encoding = per_day_encoding int64

    let mini_stats =
      conv
        (fun { ms_period; ms_nhours; ms_nblocks; ms_nops; ms_volume; ms_fees } ->
           ( ms_period, ms_nhours, ms_nblocks, ms_nops, ms_volume, ms_fees ) )
        (fun ( ms_period, ms_nhours, ms_nblocks, ms_nops, ms_volume, ms_fees ) ->
           { ms_period; ms_nhours; ms_nblocks; ms_nops; ms_volume; ms_fees}
        )
        (obj6
           (req "period" (array string))
           (req "nhours" (array int))
           (req "nblocks" (array int))
           (req "nops" (array int))
           (req "volume" (array int64))
           (req "fees" (array int64))
        )

  end

  module Network = struct

    let point_to_string = function
      | None -> None
      | Some ((addr, port), timestamp) ->
        Some (Printf.sprintf "%s:%d" addr port, timestamp)

    let peer_to_string peer =
      match peer with
      | None -> ""
      | Some s-> s

    let to_peer point_id  = point_id

    let last_connection =
      function
      | None -> "", ""
      | Some (point, date) -> peer_to_string (Some point), date

    let encoding =
      let peer_encoding =
        (conv
           (fun (
              { peer_id; country; score ; trusted ; conn_metadata ;
                state ; id_point ; stat ;
                last_failed_connection ; last_rejected_connection ;
                last_established_connection ; last_disconnection ;
                last_seen ; last_miss })
              ->
                let point_id = peer_to_string id_point in
                let state =
                  match state with
                    Accepted -> "accepted" | Running -> "running" | Disconnected -> "disconnected" in
                let last_failed_connection_point, last_failed_connection_date =
                  last_connection last_failed_connection in
                let last_rejected_connection_point, last_rejected_connection_date =
                  last_connection last_rejected_connection in
                let last_established_connection_point, last_established_connection_date =
                  last_connection last_established_connection in
                let last_disconnection_point, last_disconnection_date =
                  last_connection last_disconnection in
                let last_seen_point, last_seen_date = last_connection last_seen in
                let last_miss_point, last_miss_date = last_connection last_miss in
                (((peer_id, (fst country, snd country), point_id, trusted, conn_metadata,
                   score, state),
                  (stat.total_sent, stat.total_recv, stat.current_inflow, stat.current_outflow)),
                 ((last_failed_connection_point, last_failed_connection_date,
                   last_rejected_connection_point, last_rejected_connection_date,
                   last_established_connection_point, last_established_connection_date),
                  (last_disconnection_point, last_disconnection_date,
                   last_seen_point, last_seen_date, last_miss_point, last_miss_date))))
           (fun (((peer_id, (country_name, country_code), point_id, trusted, conn_metadata,
                   score, state),
                  (total_sent, total_recv, current_inflow, current_outflow)),
                 ((last_failed_connection_point, last_failed_connection_date,
                   last_rejected_connection_point, last_rejected_connection_date,
                   last_established_connection_point, last_established_connection_date),
                  (last_disconnection_point, last_disconnection_date,
                   last_seen_point, last_seen_date, last_miss_point, last_miss_date)))
             ->
               let country = country_name, country_code in
               let state =
                 match state with
                 | "accepted" -> Accepted
                 | "running"  -> Running
                 | "disconnected" -> Disconnected
                 | _ -> assert false in
               let id_point = Some (to_peer point_id) in
               let last_failed_connection =
                 Some (to_peer last_failed_connection_point, last_failed_connection_date) in
               let last_rejected_connection =
                 Some (to_peer last_rejected_connection_point, last_rejected_connection_date) in
               let last_established_connection =
                 Some (to_peer last_established_connection_point, last_established_connection_date) in
               let last_disconnection =
                 Some (to_peer last_disconnection_point, last_disconnection_date) in
               let last_seen = Some (to_peer last_seen_point, last_seen_date) in
               let last_miss = Some (to_peer last_miss_point, last_miss_date) in
               { peer_id; country; score ; trusted ; conn_metadata ;
                 state ; id_point ;
                 stat = { total_sent; total_recv; current_inflow; current_outflow } ;
                 last_failed_connection ; last_rejected_connection ;
                 last_established_connection ; last_disconnection ;
                 last_seen; last_miss } ))
          (merge_objs
             (merge_objs
                (obj7
                   (req "peer_id" string)
                   (req "country" (tup2 string string))
                   (req "point_id" string)
                   (req "trusted" bool)
                   (opt "conn_metadata" Tezos_encoding.conn_metadata_encoding)
                   (req "score" float)
                   (req "state" string))
                (obj4
                   (req "total_sent" int64)
                   (req "total_recv" int64)
                   (req "current_inflow" int)
                   (req "current_outflow" int)))
             (merge_objs
                (obj6
                   (req "last_failed_connection_peer" string)
                   (req "last_failed_connection_date" string)
                   (req "last_rejected_connection_peer" string)
                   (req "last_rejected_connection_date" string)
                   (req "last_established_connection_peer" string)
                   (req "last_established_connection_date" string))
                (obj6
                   (req "last_disconnection_peer" string)
                   (req "last_disconnection_date" string)
                   (req "last_seen_peer" string)
                   (req "last_seen_date" string)
                   (req "last_miss_peer" string)
                   (req "last_miss_date" string)))) in
      (list peer_encoding)

    let country_stats_encoding =
      let encoding =
        (conv
           (fun ({country_name; country_code; total})
             -> (country_name, country_code, total))
           (fun (country_name, country_code, total) ->
              {country_name; country_code; total}))
          (obj3
             (req "country_name" string)
             (req "country_code" string)
             (req "total" int)) in
      (list encoding)
  end

  module MarketCap = struct
    let encoding =
      (conv
         (fun { mc_id; name; symbol; rank; price_usd; price_btc;
                volume_usd_24; market_cap_usd; available_supply;
                total_supply; max_supply; percent_change_1;
                percent_change_24; percent_change_7; last_updated }
           ->
             ((mc_id, name, symbol, rank, price_usd, price_btc,
               volume_usd_24, market_cap_usd, available_supply),
              (total_supply, max_supply, percent_change_1,
               percent_change_24, percent_change_7, last_updated)))
         (fun ((mc_id, name, symbol, rank, price_usd, price_btc,
                volume_usd_24, market_cap_usd, available_supply),
               (total_supply, max_supply, percent_change_1,
                percent_change_24, percent_change_7, last_updated)) ->
           { mc_id; name; symbol; rank; price_usd; price_btc;
             volume_usd_24; market_cap_usd; available_supply;
             total_supply; max_supply; percent_change_1;
             percent_change_24; percent_change_7; last_updated }))
        (tup1
           (merge_objs
              (obj9
                 (req "id" string)
                 (req "name" string)
                 (req "symbol" string)
                 (req "rank" string)
                 (req "price_usd" string)
                 (req "price_btc" string)
                 (req "24h_volume_usd" (option string))
                 (req "market_cap_usd" (option string))
                 (req "available_supply" (option string)))
              (obj6
                 (req "total_supply" (option string))
                 (req "max_supply" (option string))
                 (req "percent_change_1h" (option string))
                 (req "percent_change_24h" (option string))
                 (req "percent_change_7d" (option string))
                 (req "last_updated" string))))

  end

  module Account = struct
    let encoding =
      (conv
         (fun { account_hash; account_manager;
                account_spendable; account_delegatable}
           -> ((account_hash, account_manager,
                account_spendable, account_delegatable)))
         (fun ( account_hash, account_manager,
                account_spendable, account_delegatable)
           -> { account_hash; account_manager;
                account_spendable; account_delegatable} ))
        (obj4
           (req "hash" account_name_encoding)
           (req "manager" account_name_encoding)
           (req "spendable" bool)
           (req "delegatable" bool))

    let accounts = list encoding
  end

  module Account_status = struct
    let encoding =
      (conv
         (fun { account_status_hash; account_status_revelation;
                account_status_origination;}
           -> ((account_status_hash, account_status_revelation,
                account_status_origination)))
         (fun ( account_status_hash, account_status_revelation,
                account_status_origination)
           -> { account_status_hash; account_status_revelation;
                account_status_origination;} ))
        (obj3
           (req "hash" account_name_encoding)
           (req "revelation" (option string))
           (req "origination" (option string)))
  end

  module Bonds_rewards = struct
    let priorities_encoding =
      (obj2
         (req "size" int)
         (req "priority" int))

    let encoding =
      (conv
         (fun { acc_b_rewards; acc_b_deposits; acc_fees;
                acc_e_rewards; acc_e_deposits }
           -> ( acc_b_rewards, acc_b_deposits, acc_fees,
                acc_e_rewards, acc_e_deposits ))
         (fun ( acc_b_rewards, acc_b_deposits, acc_fees,
                acc_e_rewards, acc_e_deposits )
           -> { acc_b_rewards; acc_b_deposits; acc_fees;
                acc_e_rewards; acc_e_deposits } )
         (obj5
            (req "block_rewards" tez)
            (req "block_deposits" tez)
            (req "block_acc_fees" tez)
            (req "endorsements_rewards" tez)
            (req "endorsement_deposits" tez)))

  end

  module Baker = struct
    type baker = BOk of string list | BError

    let encoding =
      (conv
         (fun ({ baker_hash; nb_blocks; volume_total; fees_total })
           -> (baker_hash, nb_blocks, volume_total, fees_total))
         (fun (baker_hash, nb_blocks, volume_total, fees_total) ->
            { baker_hash; nb_blocks; volume_total; fees_total }))
        (obj4
           (req "baker_hash" account_name_encoding)
           (req "nb_block" int)
           (req "volume_total" tez)
           (req "fees_total" tez))


    let bakers_encoding = (list encoding)

  end

  module Operation = struct
    include Op

    let encoding =
      (conv
         (fun { op_hash; op_block_hash; op_network_hash; op_type }
           -> ( op_hash, op_block_hash, op_network_hash, op_type ))
         (fun ( op_hash, op_block_hash, op_network_hash, op_type ) ->
            { op_hash; op_block_hash; op_network_hash; op_type } )
         (obj4
            (req "hash" string)
            (req "block_hash" string)
            (req "network_hash" string)
            (req "type" operation_encoding)))

    let operation = encoding
    let operations = list operation

  end

  module Account_details = struct

    let node_encoding =
      obj7
        (req "manager" string)
        (req "balance" tez)
        (req "spendable" bool)
        (req "delegate"
           (obj2
              (req "setable" bool)
              (opt "value" string)))
        (opt "script" Micheline.script_encoding)
        (opt "storage" Micheline.script_expr_encoding)
        (req "counter" z_encoding)

    let encoding =
      conv
        (fun {acc_name; acc_manager; acc_balance; acc_spendable; acc_dlgt;
              acc_script; acc_storage; acc_counter; acc_node_timestamp }
          -> (acc_name, acc_manager, acc_balance, acc_spendable, acc_dlgt,
              acc_script, acc_storage, acc_counter, acc_node_timestamp))
        (fun (acc_name, acc_manager, acc_balance, acc_spendable, acc_dlgt,
              acc_script, acc_storage, acc_counter, acc_node_timestamp)
          -> {acc_name; acc_manager; acc_balance; acc_spendable; acc_dlgt;
              acc_script; acc_storage; acc_counter; acc_node_timestamp})
        (obj9
           (req "name" account_name_encoding)
           (req "manager" account_name_encoding)
           (req "balance" tez)
           (req "spendable" bool)
           (req "delegate" (obj2
                              (req "setable" bool)
                              (opt "value" account_name_encoding)))
           (opt "script" Micheline.script_encoding)
           (opt "storage" Micheline.script_expr_encoding)
           (req "counter" z_encoding)
           (opt "node_timestamp" string)
        )
  end

  module Supply = struct
    let h_encoding =
      (conv
         (fun { h_activated_balance ; h_unfrozen_rewards ;
                h_revelation_rewards ; h_missing_revelations ;
                h_burned_tez_revelation ; h_burned_tez_origination ;
                h_tez_origination_recv ; h_tez_origination_send ;
                h_burned_tez_transaction ; h_tez_transaction_recv ;
                h_tez_transaction_send ; h_burned_tez_double_baking ;
                h_tez_dbe_rewards ; h_total } ->
           ( h_activated_balance, h_unfrozen_rewards,
             h_revelation_rewards, h_missing_revelations,
             h_burned_tez_revelation, h_burned_tez_origination,
             h_tez_origination_recv ),
           ( h_tez_origination_send,
             h_burned_tez_transaction, h_tez_transaction_recv,
             h_tez_transaction_send, h_burned_tez_double_baking,
             h_tez_dbe_rewards, h_total ))
         (fun (( h_activated_balance, h_unfrozen_rewards,
                 h_revelation_rewards, h_missing_revelations,
                 h_burned_tez_revelation, h_burned_tez_origination,
                 h_tez_origination_recv ),
               ( h_tez_origination_send,
                 h_burned_tez_transaction, h_tez_transaction_recv,
                 h_tez_transaction_send, h_burned_tez_double_baking,
                 h_tez_dbe_rewards, h_total )) ->
           { h_activated_balance ; h_unfrozen_rewards ;
             h_revelation_rewards ; h_missing_revelations ;
             h_burned_tez_revelation ; h_burned_tez_origination ;
             h_tez_origination_recv ; h_tez_origination_send ;
             h_burned_tez_transaction ; h_tez_transaction_recv ;
             h_tez_transaction_send ; h_burned_tez_double_baking ;
             h_tez_dbe_rewards ; h_total })
         (merge_objs
            (obj7
               (req "h_activated_balance" int64)
               (req "h_unfrozen_rewards" int64)
               (req "h_revelation_rewards" int64)
               (req "h_missing_revelations" int)
               (req "h_burned_tez_revelation" int64)
               (req "h_burned_tez_origination" int64)
               (req "h_tez_origination_recv" int64))
            (obj7
               (req "h_tez_origination_send" int64)
               (req "h_burned_tez_transaction" int64)
               (req "h_tez_transaction_recv" int64)
               (req "h_tez_transaction_send" int64)
               (req "h_burned_tez_double_baking" int64)
               (req "h_tez_dbe_rewards" int64)
               (req "h_total" int64))))

    let encoding =
      (conv
         (fun { dls ; foundation ; early_bakers ; contributors ;
                unfrozen_rewards ; missing_revelations ;
                revelation_rewards ; burned_tez_revelation ;
                burned_tez_origination ; burned_tez_double_baking ;
                total_supply_ico ; current_circulating_supply } ->
           ( dls, foundation, early_bakers, contributors),
           ( unfrozen_rewards, missing_revelations,
             revelation_rewards, burned_tez_revelation,
             burned_tez_origination, burned_tez_double_baking,
             total_supply_ico, current_circulating_supply ))
         (fun (( dls, foundation, early_bakers, contributors),
               ( unfrozen_rewards, missing_revelations,
                 revelation_rewards, burned_tez_revelation,
                 burned_tez_origination, burned_tez_double_baking,
                 total_supply_ico, current_circulating_supply )) ->
           { dls ; foundation ; early_bakers ; contributors
           ; unfrozen_rewards ; missing_revelations ;
             revelation_rewards ; burned_tez_revelation ;
             burned_tez_origination ; burned_tez_double_baking ;
             total_supply_ico ; current_circulating_supply }))
        (merge_objs
           (obj4
              (req "dls" int64)
              (req "foundation" int64)
              (req "early_bakers" int64)
              (req "contributors" int64))
           (obj8
              (req "unfrozen_rewards" int64)
              (req "missing_revelation" int)
              (req "revelation_rewards" int64)
              (req "burned_tez_revelation" int64)
              (req "burned_tez_origination" int64)
              (req "burned_tez_double_baking" int64)
              (req "total_supply_ico" int64)
              (req "circulating_supply" int64)))
  end

  module Rolls_distribution = struct
    let encoding = list (tup2 account_name_encoding int)
  end

  module Rewards_stats = struct
    let encoding =
      (conv
         (fun { rstats_staking_balance ; rstats_delegators_nb ;
                rstats_rewards ; rstats_pc_blocks ; rstats_pc_endorsements }
           ->
             ( rstats_staking_balance, rstats_delegators_nb,
               rstats_rewards, rstats_pc_blocks, rstats_pc_endorsements ))
         (fun ( rstats_staking_balance, rstats_delegators_nb,
                rstats_rewards, rstats_pc_blocks, rstats_pc_endorsements ) ->
           { rstats_staking_balance ; rstats_delegators_nb ;
             rstats_rewards ; rstats_pc_blocks ; rstats_pc_endorsements }))
        (obj5
           (req "staking_balance" tez)
           (req "delegators_nb" int)
           (req "rewards" tez)
           (req "pc_blocks" float)
           (req "pc_endorsements" float)
        )
  end

  module Rewards_split = struct
    let encoding =
      (conv
         (fun { rs_delegate_staking_balance ; rs_delegators_nb ;
                rs_delegators_balance ; rs_block_rewards ;
                rs_endorsement_rewards ; rs_fees ;
                rs_baking_rights_rewards ; rs_endorsing_rights_rewards ;
                rs_gain_from_denounciation ; rs_lost_deposit ;
                rs_lost_rewards ; rs_lost_fees;
                rs_rv_rewards; rs_rv_lost_rewards; rs_rv_lost_fees } ->
           (( rs_delegate_staking_balance, rs_delegators_nb,
              rs_delegators_balance, rs_block_rewards,
              rs_endorsement_rewards, rs_fees, rs_baking_rights_rewards,
              rs_endorsing_rights_rewards ),
            ( rs_gain_from_denounciation, rs_lost_deposit,
              rs_lost_rewards, rs_lost_fees,
              rs_rv_rewards, rs_rv_lost_rewards, rs_rv_lost_fees)))
         (fun (( rs_delegate_staking_balance, rs_delegators_nb,
                 rs_delegators_balance, rs_block_rewards,
                 rs_endorsement_rewards, rs_fees, rs_baking_rights_rewards,
                 rs_endorsing_rights_rewards ),
               ( rs_gain_from_denounciation, rs_lost_deposit,
                 rs_lost_rewards, rs_lost_fees,
                 rs_rv_rewards, rs_rv_lost_rewards, rs_rv_lost_fees)) ->
           { rs_delegate_staking_balance ; rs_delegators_nb ;
             rs_delegators_balance ; rs_block_rewards ;
             rs_endorsement_rewards ; rs_fees ;
             rs_baking_rights_rewards ; rs_endorsing_rights_rewards ;
             rs_gain_from_denounciation ; rs_lost_deposit ;
             rs_lost_rewards ; rs_lost_fees;
             rs_rv_rewards; rs_rv_lost_rewards; rs_rv_lost_fees}))
        (merge_objs
           (obj8
              (req "delegate_staking_balance" int64)
              (req "delegators_nb" int)
              (req "delegators_balance"
                 (list (tup2 account_name_encoding int64)))
              (req "blocks_rewards" int64)
              (req "endorsements_rewards" int64)
              (req "fees" int64)
              (req "future_blocks_rewards" int64)
              (req "future_endorsements_rewards" int64))
           (obj7
              (req "gain_from_denounciation" int64)
              (req "lost_deposit_from_denounciation" int64)
              (req "lost_rewards_denounciation" int64)
              (req "lost_fees_denounciation" int64)
              (req "revelation_rewards" tez)
              (req "lost_revelation_rewards" tez)
              (req "lost_revelation_fees" tez)))

    let status_encoding =
      (conv
         (function
           | Cycle_in_progress -> "cycle_in_progress"
           | Cycle_pending -> "cycle_pending"
           | Rewards_pending -> "rewards_pending"
           | Rewards_delivered -> "rewards_delivered")
         (function
           | "cycle_in_progress" -> Cycle_in_progress
           | "cycle_pending" -> Cycle_pending
           | "rewards_pending" -> Rewards_pending
           | "rewards_delivered" -> Rewards_delivered
           | _ -> assert false))
        (obj1
           (req "status" string))

    let all_encoding =
      conv
        (fun { ars_cycle ; ars_delegate_staking_balance ;
               ars_delegators_nb ; ars_delegate_delegated_balance ;
               ars_block_rewards ;
               ars_endorsement_rewards ; ars_fees ;
               ars_baking_rights_rewards ; ars_endorsing_rights_rewards ;
               ars_status ; ars_gain_from_denounciation ;
               ars_lost_deposit ; ars_lost_rewards ; ars_lost_fees;
               ars_rv_rewards; ars_rv_lost_rewards; ars_rv_lost_fees } ->
          ( (ars_cycle, ars_delegate_staking_balance,
             ars_delegators_nb, ars_delegate_delegated_balance,
             ars_block_rewards,
             ars_endorsement_rewards, ars_fees,
             ars_baking_rights_rewards, ars_endorsing_rights_rewards,
             ars_status),
            (ars_gain_from_denounciation,
             ars_lost_deposit, ars_lost_rewards, ars_lost_fees,
             ars_rv_rewards, ars_rv_lost_rewards, ars_rv_lost_fees)))
        (fun ( (ars_cycle, ars_delegate_staking_balance,
                ars_delegators_nb, ars_delegate_delegated_balance,
                ars_block_rewards,
                ars_endorsement_rewards, ars_fees,
                ars_baking_rights_rewards, ars_endorsing_rights_rewards,
                ars_status),
               (ars_gain_from_denounciation,
                ars_lost_deposit, ars_lost_rewards, ars_lost_fees,
                ars_rv_rewards, ars_rv_lost_rewards, ars_rv_lost_fees)) ->
          { ars_cycle ; ars_delegate_staking_balance;
            ars_delegators_nb ; ars_delegate_delegated_balance ;
            ars_block_rewards ;
            ars_endorsement_rewards ; ars_fees ;
            ars_baking_rights_rewards ; ars_endorsing_rights_rewards ;
            ars_status ; ars_gain_from_denounciation ;
            ars_lost_deposit ; ars_lost_rewards ; ars_lost_fees;
            ars_rv_rewards; ars_rv_lost_rewards; ars_rv_lost_fees})
        (merge_objs
           (obj10
              (req "cycle" int)
              (req "delegate_staking_balance" int64)
              (req "delegators_nb" int)
              (req "delegated_balance" int64)
              (req "blocks_rewards" int64)
              (req "endorsements_rewards" int64)
              (req "fees" int64)
              (req "future_baking_rewards" int64)
              (req "future_endorsing_rewards" int64)
              (req "status" status_encoding))
           (obj7
              (req "gain_from_denounciation" int64)
              (req "lost_deposit_from_denounciation" int64)
              (req "lost_rewards_denounciation" int64)
              (req "lost_fees_denounciation" int64)
              (req "revelation_rewards" tez)
              (req "lost_revelation_rewards" tez)
              (req "lost_revelation_fees" tez)
           ))

    let delegator_encoding =
      conv
        (fun {dor_cycle; dor_delegate; dor_staking_balance; dor_balance;
              dor_rewards; dor_extra_rewards; dor_losses; dor_status}
          -> (dor_cycle, dor_delegate, dor_staking_balance, dor_balance,
              dor_rewards, dor_extra_rewards, dor_losses, dor_status))
        (fun (dor_cycle, dor_delegate, dor_staking_balance, dor_balance,
              dor_rewards, dor_extra_rewards, dor_losses, dor_status)
          -> {dor_cycle; dor_delegate; dor_staking_balance; dor_balance;
              dor_rewards; dor_extra_rewards; dor_losses; dor_status})
        (obj8
           (req "cycle" int)
           (req "delegate" account_name_encoding)
           (req "staking_balance" int64)
           (req "balance" int64)
           (req "rewards" int64)
           (req "extra_rewards" int64)
           (req "losses" int64)
           (req "status" status_encoding))
    let delegator_encodings = list delegator_encoding
  end

  module Snapshot = struct
    let snapshot_encoding =
      (conv
         (fun { snap_cycle ; snap_index ; snap_level ; snap_rolls } ->
            ( snap_cycle, snap_index, snap_level, snap_rolls ))
         (fun ( snap_cycle, snap_index, snap_level, snap_rolls ) ->
            { snap_cycle ; snap_index ; snap_level ; snap_rolls })
         (obj4
            (req "snapshot_cycle" int)
            (req "snapshot_index" int)
            (req "snapshot_level" int)
            (req "snapshot_rolls" int)))

    let encoding = list snapshot_encoding
  end

  module Proto_details = struct
    let proto_encoding =
      conv
        (fun {prt_index; prt_hash; prt_name; prt_start; prt_end}
          -> (prt_index, prt_hash, prt_name, prt_start, prt_end))
        (fun (prt_index, prt_hash, prt_name, prt_start, prt_end)
          -> {prt_index; prt_hash; prt_name; prt_start; prt_end})
        (obj5
           (req "protocol_index" int)
           (req "protocol_hash" string)
           (req "protocol_name" string)
           (req "block_start" int)
           (req "block_end" int))
    let encoding = list proto_encoding
  end

  module Date_enc = struct
    let encoding =
      conv
        (fun d -> Date.to_string d)
        (fun (d: string) -> Date.from_string d)
        (obj1
           (req "date" string))
  end

  module Balance_update_info = struct
    let bu_encoding =
      conv
        (fun {bu_account; bu_diff; bu_date; bu_update_type;  bu_internal;
              bu_level;bu_frozen;bu_burn}
          -> (bu_account, bu_diff, bu_date, bu_update_type,bu_internal,
              bu_level,bu_frozen, bu_burn))
        (fun (bu_account, bu_diff, bu_date, bu_update_type,bu_internal,
              bu_level,bu_frozen, bu_burn)
          ->  {bu_account; bu_diff; bu_date; bu_update_type; bu_internal;
               bu_level; bu_frozen; bu_burn})
        (obj8
           (req "account" string)
           (req "diff" int64)
           (req "date" Date_enc.encoding)
           (req "update_type" string)
           (req "internal" bool)
           (req "level" int32)
           (req "frozen" bool)
           (req "burn" bool))
    let encoding = list bu_encoding
  end

  module H24_stats = struct
    let encoding =
      (conv
         (fun { h24_end_rate ; h24_block_0_rate ;
                h24_transactions ; h24_originations ;
                h24_delegations ; h24_activations ;
                h24_baking_rate ; h24_active_baker } ->
           ( h24_end_rate, h24_block_0_rate,
             h24_transactions, h24_originations,
             h24_delegations, h24_activations,
             h24_baking_rate, h24_active_baker))
         (fun ( h24_end_rate, h24_block_0_rate,
                h24_transactions, h24_originations,
                h24_delegations, h24_activations,
                h24_baking_rate, h24_active_baker ) ->
           { h24_end_rate ; h24_block_0_rate ;
             h24_transactions ; h24_originations ;
             h24_delegations ; h24_activations ;
             h24_baking_rate ; h24_active_baker}))
        (obj8
           (req "h24_endorsements_rate" float)
           (req "h24_block_0_rate" float)
           (req "h24_transactions" int)
           (req "h24_originations" int)
           (req "h24_delegations" int)
           (req "h24_activations" int)
           (req "h24_baking_rate" float)
           (req "h24_active_baker" int))
  end

  module Server = struct

    let versions =
      conv
        (fun { server_version; server_build; server_commit } ->
           ( server_version, server_build, server_commit ) )
        (fun ( server_version, server_build, server_commit ) ->
           { server_version; server_build; server_commit } )
        (obj3
           (req "version" string)
           (req "build" string)
           (req "commit" string))


    let ico_constants =
      conv
        (fun
          {
            ico_company_tokens ;
            ico_foundation_tokens ;
            ico_early_tokens ;
            ico_contributors_tokens ;
            ico_wallets
          }
          ->
            (
              ico_company_tokens ,
              ico_foundation_tokens ,
              ico_early_tokens ,
              ico_contributors_tokens ,
              ico_wallets
            )
        )
        (fun
            (
              ico_company_tokens ,
              ico_foundation_tokens ,
              ico_early_tokens ,
              ico_contributors_tokens ,
              ico_wallets
            )
          ->
          {
            ico_company_tokens ;
            ico_foundation_tokens ;
            ico_early_tokens ;
            ico_contributors_tokens ;
            ico_wallets
          }
        )
        (obj5
           (dft "company_tokens" int64 0L)
           (req "foundation_tokens" int64)
           (dft "early_tokens" int64 0L)
           (dft "contributors_tokens" int64 0L)
           (dft "wallets" int 0)
        )

    let api_server_config =
      conv
        (fun
          {
            conf_network ;
            conf_constants ;
            conf_rampup_cycles ;
            conf_ico ;
            conf_has_delegation ;
            conf_has_marketcap
          }
          ->
            (
              conf_network ,
              conf_constants ,
              conf_rampup_cycles ,
              conf_ico ,
              conf_has_delegation ,
              conf_has_marketcap
            )
        )
        (fun
          (
            conf_network ,
            conf_constants ,
            conf_rampup_cycles ,
            conf_ico ,
            conf_has_delegation ,
            conf_has_marketcap
          )
          ->
            {
              conf_network ;
              conf_constants ;
              conf_rampup_cycles ;
              conf_ico ;
              conf_has_delegation ;
              conf_has_marketcap
            }
        )
        (obj6
           (req "network" string)
           (req "constants" (list (tup2 int Tezos_encoding.constants)))
           (dft "rampup_cycles" int 0)
           (req "ico" ico_constants)
           (dft "has_delegation" bool false)
           (dft "has_marketcap" bool false)
        )

    let api_server_info =
      conv
        (fun
          {
            api_config ;
            api_date ;
            api_versions
          }
          ->
            (
              api_config ,
              api_date ,
              api_versions
            )
        )
        (fun
          (
            api_config ,
            api_date ,
            api_versions
          )
          ->
            {
              api_config ;
              api_date ;
              api_versions
          }
        )
        (obj3
           (req "config" api_server_config)
           (req "date" float)
           (req "versions" versions)
        )

  end

end

module Context_stats = struct

  let context_with_diff_encoding =
    conv
      (fun
        { context_level ;
          context_addresses ; context_addresses_diff ;
          context_keys ; context_keys_diff ; context_revealed ;
          context_revealed_diff ; context_originated ; context_originated_diff ;
          context_contracts ; context_contracts_diff ; context_roll_owners ;
          context_roll_owners_diff ; context_rolls ; context_rolls_diff ;
          context_delegated ; context_delegated_diff ; context_delegators ;
          context_delegators_diff ; context_deleguees ; context_deleguees_diff ;
          context_self_delegates ; context_self_delegates_diff ;
          context_multi_deleguees ; context_multi_deleguees_diff ;
          context_current_balances ; context_current_balances_diff ;
          context_full_balances ; context_full_balances_diff ;
          context_staking_balances ; context_staking_balances_diff ;
          context_frozen_balances ; context_frozen_balances_diff ;
          context_frozen_deposits ; context_frozen_deposits_diff ;
          context_frozen_rewards ; context_frozen_rewards_diff ;
          context_frozen_fees ; context_frozen_fees_diff ; context_paid_bytes ;
          context_paid_bytes_diff ; context_used_bytes ;
          context_used_bytes_diff } ->
        (( context_level,
           context_addresses, context_addresses_diff,
           context_keys, context_keys_diff, context_revealed,
           context_revealed_diff, context_originated ),
         ((context_originated_diff,
           context_contracts, context_contracts_diff, context_roll_owners,
           context_roll_owners_diff, context_rolls, context_rolls_diff,
           context_delegated, context_delegated_diff, context_delegators),
          ((context_delegators_diff, context_deleguees, context_deleguees_diff,
            context_self_delegates, context_self_delegates_diff,
            context_multi_deleguees, context_multi_deleguees_diff,
            context_current_balances, context_current_balances_diff,
            context_full_balances),
           ((context_full_balances_diff,
             context_staking_balances, context_staking_balances_diff,
             context_frozen_balances, context_frozen_balances_diff,
             context_frozen_deposits, context_frozen_deposits_diff,
             context_frozen_rewards, context_frozen_rewards_diff,
             context_frozen_fees),
            (context_frozen_fees_diff, context_paid_bytes,
             context_paid_bytes_diff, context_used_bytes,
             context_used_bytes_diff) )))))
      (fun
        (( context_level,
           context_addresses, context_addresses_diff,
           context_keys, context_keys_diff, context_revealed,
           context_revealed_diff, context_originated ),
         ((context_originated_diff,
           context_contracts, context_contracts_diff, context_roll_owners,
           context_roll_owners_diff, context_rolls, context_rolls_diff,
           context_delegated, context_delegated_diff, context_delegators),
          ((context_delegators_diff, context_deleguees, context_deleguees_diff,
            context_self_delegates, context_self_delegates_diff,
            context_multi_deleguees, context_multi_deleguees_diff,
            context_current_balances, context_current_balances_diff,
            context_full_balances),
           ((context_full_balances_diff,
             context_staking_balances, context_staking_balances_diff,
             context_frozen_balances, context_frozen_balances_diff,
             context_frozen_deposits, context_frozen_deposits_diff,
             context_frozen_rewards, context_frozen_rewards_diff,
             context_frozen_fees),
            (context_frozen_fees_diff, context_paid_bytes,
             context_paid_bytes_diff, context_used_bytes,
             context_used_bytes_diff) )))) ->
        { context_level ;
          context_addresses ; context_addresses_diff ;
          context_keys ; context_keys_diff ; context_revealed ;
          context_revealed_diff ; context_originated ; context_originated_diff ;
          context_contracts ; context_contracts_diff ; context_roll_owners ;
          context_roll_owners_diff ; context_rolls ; context_rolls_diff ;
          context_delegated ; context_delegated_diff ; context_delegators ;
          context_delegators_diff ; context_deleguees ; context_deleguees_diff ;
          context_self_delegates ; context_self_delegates_diff ;
          context_multi_deleguees ; context_multi_deleguees_diff ;
          context_current_balances ; context_current_balances_diff ;
          context_full_balances ; context_full_balances_diff ;
          context_staking_balances ; context_staking_balances_diff ;
          context_frozen_balances ; context_frozen_balances_diff ;
          context_frozen_deposits ; context_frozen_deposits_diff ;
          context_frozen_rewards ; context_frozen_rewards_diff ;
          context_frozen_fees ; context_frozen_fees_diff ; context_paid_bytes ;
          context_paid_bytes_diff ; context_used_bytes ;
          context_used_bytes_diff })
      (merge_objs
         (obj8
            (opt "level" V1.Level.encoding )
            (req "addresses"  int )
            (req "addresses_diff" float )
            (req "keys"  int  )
            (req "keys_diff" float )
            (req "revealed"  int  )
            (req "revealed_diff" float )
            (req "originated"  int  ))
         (merge_objs
            (obj10
               (req "originated_diff" float )
               (req "contracts"  int  )
               (req "contracts_diff" float )
               (req "roll_owners"  int  )
               (req "roll_owners_diff" float )
               (req "rolls"  int  )
               (req "rolls_diff" float )
               (req "delegated"  int64  )
               (req "delegated_diff" float )
               (req "delegators"  int  ))
            (merge_objs
               (obj10
                  (req "delegators_diff" float )
                  (req "deleguees"  int  )
                  (req "deleguees_diff" float )
                  (req "self_delegates"  int  )
                  (req "self_delegates_diff" float )
                  (req "multi_deleguees"  int  )
                  (req "multi_deleguees_diff" float )
                  (req "current_balances" int64)
                  (req "current_balances_diff" float )
                  (req "full_balances" int64))
               (merge_objs
                  (obj10
                     (req "full_balances_diff" float )
                     (req "staking_balances"  int64  )
                     (req "staking_balances_diff" float )
                     (req "frozen_balances"  int64  )
                     (req "frozen_balances_diff" float )
                     (req "frozen_deposits"  int64  )
                     (req "frozen_deposits_diff" float )
                     (req "frozen_rewards"  int64  )
                     (req "frozen_rewards_diff" float )
                     (req "frozen_fees"  int64  ))
                  (obj5
                     (req "frozen_fees_diff" float )
                     (req "paid_bytes"  int64  )
                     (req "paid_bytes_diff" float )
                     (req "used_bytes"  int64  )
                     (req "used_bytes_diff" float ))))))

end

module WWW = struct

  let chart_encoding =
    conv
      (fun
        { chart_period ; chart_period_kind ; chart_name ; chart_values }
        ->
          ( chart_period , chart_period_kind , chart_name , chart_values )
      )
      (fun
        ( chart_period , chart_period_kind , chart_name , chart_values )
        ->
          { chart_period ; chart_period_kind ; chart_name ; chart_values }
      )
      (obj4
         (req "last_period" string)
         (req "period_kind" string)
         (req "name" string)
         (req "values" (array (tup2 string float)))
      )

  let www_server_info =
    conv
      (fun
        { www_currency_name ; www_currency_short ; www_currency_symbol ;
          www_languages ; www_apis ; www_auth ;
          www_logo ; www_footer ; www_networks } ->
        ( www_currency_name , www_currency_short , www_currency_symbol ,
          www_languages , www_apis , www_auth ,
          www_logo , www_footer , www_networks )
      )
      (fun
        ( www_currency_name , www_currency_short , www_currency_symbol ,
          www_languages , www_apis , www_auth ,
          www_logo , www_footer , www_networks ) ->
        { www_currency_name ; www_currency_short ; www_currency_symbol ;
          www_languages ; www_apis ; www_auth ;
          www_logo ; www_footer ; www_networks }
      )
      (obj9
         (dft "currency" string "Tezos")
         (dft "currency_short" string "XTZ")
         (dft "currency_symbol" string "#xa729")
         (dft "languages" (list (tup2 string string)) [ "English", "en" ])
         (req "apis" (array string))
         (opt "auth" string)
         (dft "logo" string "tzscan-logo.png")
         (dft "footer" string "footer.html")
         (dft "networks" (list (tup2 string string)) [])
      )

end
