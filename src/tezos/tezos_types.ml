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

module Date = struct

  type t = DATE of string

  let format = "%Y-%m-%dT%H:%M:%SZ"
  let to_string (DATE s) = s
  let from_string s = DATE s
  let pretty_date = function
      DATE d ->
       match String.split_on_char 'T' d with
         [] -> "",""
       | hd :: [] -> hd,""
       | date :: time :: _ -> date,(String.sub time 0 (String.length time - 1))


end

type block_hash = string
type operation_hash = string
type account_hash = string
type pubkey_hash = string
type proposal_hash = string
type protocol_hash = string
type network_hash = string
type nonce_hash = string
type timestamp = string
type peer = string

type voting_period_kind =
  | NProposal
  | NTesting_vote
  | NTesting
  | NPromotion_vote

type voting_period_status =
  | VPS_passed
  | VPS_wait
  | VPS_current
  | VPS_ignored (* period never reached  *)

type chain_status =
  | CSNot_running
  | CSForking of {
      protocol: protocol_hash ;
      expiration: timestamp ;
    }
  | CSRunning of {
      chain_id: string ;
      genesis: block_hash ;
      protocol: protocol_hash ;
      expiration: timestamp ;
    }


type block_header = {

  (* SHELL PART: lib_base/block_header.ml *)
  header_level : int ;
  header_proto : int ;
  header_predecessor : block_hash ;
  header_timestamp : Date.t ;
  header_validation_pass : int ;
  header_operations_hash : string ; (* hash (operation list list) *)
  header_fitness : string ;
  header_context : string ;

  (* PROTOCOL PART: lib_protocol/src/block_header_repr.ml *)

  (* CONTENTS *)
  header_priority : int ;
  header_seed_nonce_hash : nonce_hash ;
  header_proof_of_work_nonce : string ;

  header_signature : string ;
}

and block_header_metadata = {
  header_meta_baker : account_hash ;
  header_meta_level : node_level ;
  header_meta_voting_period_kind : voting_period_kind ;
  header_meta_nonce_hash : string option;
  header_meta_consumed_gas : Z.t ;
  header_meta_deactivated : string list ;
  header_meta_balance_updates : balance_updates list option;
}

and block_metadata = {
  meta_protocol : protocol_hash ;
  meta_next_protocol : protocol_hash ;
  meta_test_chain_status : chain_status;
  meta_max_operations_ttl : int ;
  meta_max_operation_data_length : int ;
  meta_max_block_header_length : int ;
  meta_max_operation_list_length : (int * int option) list ;
  meta_header : block_header_metadata ;
}

and node_block = {
  node_protocol : protocol_hash ;
  node_chain_id : string ;
  node_hash : block_hash ;
  node_header : block_header ;
  node_metadata : block_metadata ;
  node_operations : node_operation list list ;
}

and node_level = {
  node_lvl_level : int ;
  node_lvl_level_position : int ;

  node_lvl_cycle : int ;
  node_lvl_cycle_position : int ;

  node_lvl_voting_period : int ;
  node_lvl_voting_period_position : int ;

  node_lvl_expected_commitment : bool ;
}

and level = {
  lvl_level : int ;
  lvl_level_position : int ;

  lvl_cycle : int ;
  lvl_cycle_position : int ;

  lvl_voting_period : int ;
  lvl_voting_period_position : int ;

}

and refused_operation_unparsed =
  Json_repr.any list
  (* (string * (pending_operation_parsed list *
   *            (string * string * string * string) list)) list *)
  (* (string * ((string * string) * (string * string * string * string) list)) list *)

and pending_operation = {
    applied : pending_operation_parsed list ;
    refused : refused_operation_unparsed ;
    branch_refused : refused_operation_unparsed ;
    branch_delayed : refused_operation_unparsed ;
    unprocessed : (* (string * string) list *) refused_operation_unparsed
  }

and pending_operation_parsed = {
  pending_hash : string ;
  pending_branch : string ;
  pending_contents : node_operation_type list ;
  pending_signature : string option ;
}

and pending_operation_parsed_list = pending_operation_parsed list

and node_operation = {
  node_op_protocol : protocol_hash ;
  node_op_chain_id : string ;
  node_op_hash : operation_hash ;
  node_op_branch : string ;
  node_op_contents : node_operation_type list ;
  node_op_signature : string option;
}

and balance_updates =
  (*
  | Debited of int64 * int64
  | Credited of int64 * int64
*)
  | Contract of account_hash * int64
  | Rewards of account_hash * int * int64
  | Fees of account_hash * int * int64
  | Deposits of account_hash * int * int64

and manager_metadata = {
  manager_meta_balance_updates : balance_updates list option ;
  manager_meta_operation_result : op_metadata option ;
  manager_meta_internal_operation_results : node_operation_type list ;
}

and op_metadata = {
  meta_op_status : string option ;
  meta_op_balance_updates : balance_updates list option ;

  meta_op_storage : script_expr_t option ;

  meta_op_consumed_gas : Z.t option ;

  meta_op_originated_contracts : account_hash list option;
  meta_op_storage_size_diff : int64 option ;

  meta_op_delegate : account_hash option ;
  meta_op_slots : int list option ;
  meta_op_paid_storage_size_diff : Z.t option ;
  meta_op_big_map_diff : (string * script_expr_t * script_expr_t option) list option ;
  meta_op_allocated_destination_contract : bool option ;
}

and node_operation_type =
  | NTransaction of node_transaction
  | NOrigination of node_origination
  | NReveal of node_reveal
  | NDelegation of node_delegation
  | NSeed_nonce_revelation of node_seed_nonce_revelation
  | NActivation of node_activation
  | NDouble_endorsement_evidence of node_double_endorsement_evidence
  | NDouble_baking_evidence of node_double_baking_evidence
  | NEndorsement of node_endorsement
  | NProposals of node_proposals
  | NBallot of node_ballot
  | NActivate
  | NActivate_testnet

and location = bool * int * string option

and script_expr_t =
  | Int of location * Z.t
  | String of location * string
  | Bytes of location * bytes
  | Prim of location * string * script_expr_t list * string list
  | Seq of location * script_expr_t list

and canonical = Canonical of script_expr_t

and node_activation = {
  node_act_pkh : account_hash ;
  node_act_secret : string ;
  node_act_metadata : op_metadata option ;
}

and node_transaction = {
  node_tr_src : account_hash ;
  node_tr_fee : int64 ;
  node_tr_counter : Z.t ;
  node_tr_gas_limit : Z.t ;
  node_tr_storage_limit : Z.t ;
  node_tr_dst : account_hash ;
  node_tr_amount : int64 ;
  node_tr_parameters : string option ;
  node_tr_metadata : manager_metadata option ;
}

and node_origination = {
  node_or_src : account_hash ;
  node_or_fee : int64 ;
  node_or_counter : Z.t ;
  node_or_gas_limit : Z.t ;
  node_or_storage_limit : Z.t ;
  node_or_manager : account_hash ;
  node_or_balance : int64 ;
  node_or_spendable : bool ;
  node_or_delegatable : bool ;
  node_or_delegate : account_hash option ;
  node_or_script : script option ;
  node_or_metadata : manager_metadata option ;
}

and node_delegation = {
  node_del_src : account_hash ;
  node_del_fee : int64 ;
  node_del_counter : Z.t ;
  node_del_gas_limit : Z.t ;
  node_del_storage_limit : Z.t ;
  node_del_delegate : account_hash ;
  node_del_metadata : manager_metadata option ;
}

and node_reveal = {
  node_rvl_src : account_hash ;
  node_rvl_fee : int64 ;
  node_rvl_counter : Z.t ;
  node_rvl_gas_limit : Z.t ;
  node_rvl_storage_limit : Z.t ;
  node_rvl_pubkey : string ;           (* public key *)
  node_rvl_metadata : manager_metadata option ;
}

and node_endorsement = {
  node_endorse_block_level : int ;
  node_endorse_metadata : op_metadata option ;
}

and node_proposals = {
  node_prop_src : account_hash ;
  node_prop_voting_period : int32 ;
  node_prop_proposals : proposal_hash list ;
  node_prop_metadata : op_metadata option ;
}

and ballot_type = Yay | Nay | Pass

and node_ballot = {
  node_ballot_src : account_hash ;
  node_ballot_voting_period : int32 ;
  node_ballot_proposal : proposal_hash ;
  node_ballot_vote : ballot_type ;
  node_ballot_metadata : op_metadata option ;
}

and node_seed_nonce_revelation = {
  node_seed_level : int ;
  node_seed_nonce : string ;
  node_seed_metadata : op_metadata option ;
}

and node_double_baking_evidence = {
  node_double_bh1 : block_header ;
  node_double_bh2 : block_header ;
  node_double_bh_metadata : op_metadata option ;
}

and node_double_endorsement_evidence_op = {
  ndee_branch : string ;
  ndee_level : int ;
  ndee_signature : string ;
}

and node_double_endorsement_evidence = {
  node_double_endorsement1 : node_double_endorsement_evidence_op ;
  node_double_endorsement2 : node_double_endorsement_evidence_op ;
  node_double_endorsement_metadata : op_metadata option ;
}

and script = {
  sc_code : string ;
  sc_storage : string ;
}


type delegate_details = {
  delegate_balance : int64 ;
  delegate_frozen_balance : int64 ;
  delegate_staking_balance : int64 ;
  delegate_delegated_contracts : account_hash list ;
  delegate_delegated_balance : int64 ;
  delegate_deactivated : bool ;
  delegate_grace_period : int
}

type roll = {
  roll_owner : account_hash ;
  roll_count : int ;
  roll_change : int64 ;
}

type baking_rights = {
  node_br_level : int ;
  node_br_delegate : account_hash ;
  node_br_priority : int ;
  node_br_timestamp : string option ;
}

type endorsing_rights = {
  node_er_level : int;
  node_er_delegate : account_hash;
  node_er_slots : int list ;
  node_er_estimated_time : string option ;
}

type data_stats = {
  total_sent : int64;
  total_recv : int64;
  current_inflow : int ;
  current_outflow : int ;
}

type state = Accepted | Running | Disconnected

type conn_metadata = {
  disable_mempool : bool ;
  private_node : bool ;
}

type network_stats = {
  peer_id : string ;
  country : string * string ;   (* Country name * country code *)
  score : float ;
  trusted : bool ;
  conn_metadata : conn_metadata option ;
  state : state ;
  id_point : peer option ;
  stat : data_stats ;
  last_failed_connection : (peer * timestamp)  option ;
  last_rejected_connection : (peer * timestamp)  option ;
  last_established_connection : (peer * timestamp)  option ;
  last_disconnection : (peer * timestamp)  option ;
  last_seen : (peer * timestamp)  option ;
  last_miss : (peer * timestamp)  option ;
}

type injection_error = (* in this order *)
  | Inj_other
  | Inj_generic of Json_schema.schema
  | Inj_outdated
  | Inj_illformed

type injection_result =
  | Inj_ok of string
  | Inj_ko of injection_error list

type rolls = roll list


type constants = {
  proof_of_work_nonce_size: int;
  nonce_length: int;
  max_revelations_per_block: int;
  max_operation_data_length: int;
  preserved_cycles: int;
  blocks_per_cycle: int;
  blocks_per_commitment: int;
  blocks_per_roll_snapshot: int;
  blocks_per_voting_period: int;
  time_between_blocks: int list;
  endorsers_per_block: int;
  hard_gas_limit_per_operation: int64;
  hard_gas_limit_per_block: int64;
  proof_of_work_threshold: int64;
  tokens_per_roll: int64;
  michelson_maximum_type_size: int;
  seed_nonce_revelation_tip: int64;
  origination_burn: int64;
  block_security_deposit: int64;
  endorsement_security_deposit: int64;
  block_reward: int64;
  endorsement_reward: int64;
  cost_per_byte: int64;
  hard_storage_limit_per_operation: int64;
}

type network_version = {
  v_name : string ;
  v_major : int ;
  v_minor : int ;
}

type voting_rolls = {
  vroll_pkh: account_hash;
  vroll_count: int
}
