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

type url = { address : string ; port : int }

type account_name = { tz : account_hash; alias : string option }

type config =
  { nodes : url list ; data_path : string option }

type nonces = int * (operation_hash * (int list)) list

type block = {
  hash : block_hash ;
  predecessor_hash : block_hash ;
  fitness : string ;
  baker : account_name ;
  timestamp : Date.t ;
  validation_pass: int ;
               (*  op_hash     * branch * data *)
  operations : (operation_hash * string * string) list ;
  nb_operations : int ;
  protocol : protocol ;
  test_protocol : protocol ;
  network : network_hash ;
  test_network : network_hash ;
  test_network_expiration : timestamp ;
  priority : int ; (* niveau du baker *)
  level : int ;
  commited_nonce_hash : nonce_hash ;
  pow_nonce : string ;
  proto : int ;
  data : string ;
  signature : string ;
  volume : int64 ;
  fees : int64 ;
  distance_level : int ;
  }

and protocol = {
  proto_name : string ;
  proto_hash : protocol_hash ;
}

and operation_type =
  | Anonymous of anonymous_operation list
  | Sourced of sourced_operations

and anonymous_operation =
  | Seed_nonce_revelation of seed_nonce_revelation
  | Activation of activation
  | Double_endorsement_evidence of double_endorsement_evidence
  | Double_baking_evidence of double_baking_evidence

and seed_nonce_revelation = {
  seed_level : int ;
  seed_nonce : string ;
}

and sourced_operations =
  | Consensus of consensus_operation
  (* Amendment (source * amendment_operation) *)
  | Amendment of (account_name * amendment_operation)
  (* Manager (kind * source * manager_operation ) *)
  | Manager of (string * account_name * manager_operation list)
  | Dictator of dictator_operation

and dictator_operation =
  | Activate
  | Activate_testnet

and consensus_operation =
  | Endorsement of endorsement

and amendment_operation =
  | Proposal of proposals
  | Ballot of ballot

and ballot = {
  ballot_voting_period : int32 ;
  ballot_proposal : proposal_hash ;
  ballot_vote : ballot_type ;
}

and manager_operation =
  | Transaction of transaction
  | Origination of origination
  | Reveal of reveal
  | Delegation of delegation

and operation = {
  op_hash : operation_hash ;
  op_block_hash : block_hash ;
  op_network_hash : network_hash ;
  op_type : operation_type ;
}

and transaction = {
  tr_src : account_name ;
  tr_dst : account_name ;
  tr_amount : int64 ;
  tr_counter : int32 ;
  tr_fee : int64 ;
  tr_gas_limit : Z.t ;
  tr_storage_limit : Z.t ;
  tr_parameters : string option ;
  tr_failed : bool ;
  tr_internal : bool ;
  tr_burn : int64 ;
  tr_op_level : int ;
  tr_timestamp : string
}

and origination = {
  or_src : account_name ;
  or_manager : account_name ;
  or_delegate : account_name ;
  or_script : Tezos_types.script option ;
  or_spendable : bool ;
  or_delegatable : bool ;
  or_balance : int64 ;
  or_counter : int32 ;
  or_fee : int64 ;
  or_gas_limit : Z.t ;
  or_storage_limit : Z.t ;
  or_tz1 : account_name ;
  or_failed : bool ;
  or_internal : bool ;
  or_burn : int64 ;
}

and delegation = {
  del_src : account_name ;
  del_delegate : account_name ;
  del_counter : int32 ;
  del_fee : int64 ;
  del_gas_limit : Z.t ;
  del_storage_limit : Z.t ;
  del_failed : bool ;
  del_internal : bool ;
}

and reveal = {
  rvl_src : account_name ;
  rvl_pubkey : string ;           (* public key *)
  rvl_counter : int32 ;
  rvl_fee : int64 ;
  rvl_gas_limit : Z.t ;
  rvl_storage_limit : Z.t ;
  rvl_failed : bool ;
  rvl_internal : bool ;
}

and activation = {
  act_pkh : account_name ;
  act_secret : string ;
}

and endorsement = {
  endorse_src : account_name ;
  endorse_block_hash : block_hash ;
  endorse_block_level : int ;
  endorse_slot : int list ;
  endorse_op_level : int ;
  endorse_priority : int ;
  endorse_timestamp : string
}

and proposals = {
  prop_voting_period : int32 ;
  prop_proposals : proposal_hash list ;
}

and account = {
  account_hash : account_name ;
  account_manager : account_name ;
  account_spendable : bool ;
  account_delegatable : bool ;
}

and double_baking_evidence = {
  double_baking_header1 : Tezos_types.block_header ;
  double_baking_header2 : Tezos_types.block_header ;
  double_baking_main : int ;
  double_baking_accused : account_name ;
  double_baking_denouncer : account_name ;
  double_baking_lost_deposit : int64 ;
  double_baking_lost_rewards : int64 ;
  double_baking_lost_fees : int64 ;
  double_baking_gain_rewards : int64 ;
}

and double_endorsement_evidence = {
  double_endorsement1 : operation_type ;
  double_endorsement2 : operation_type ;
}




type baking = {
  bk_block_hash : block_hash;
  bk_baker_hash : account_name;
  bk_level : int;
  bk_cycle : int;
  bk_priority : int;
  bk_distance_level : int;
  bk_fees : int64;
  bk_bktime : int;
  bk_baked : bool;
}

type baking_endorsement = {
  ebk_block : block_hash option;
  ebk_source : account_name option;
  ebk_level : int;
  ebk_cycle : int option;
  ebk_priority : int option;
  ebk_dist : int option;
  ebk_slots: int list option;
  ebk_lr_nslot: int
}

type bk_count = {
  cnt_all: int64;
  cnt_miss: int64;
  cnt_steal: int64
}

type bk_tez = {
  tez_fee: int64;
  tez_reward: int64;
  tez_deposit: int64
}

type cycle_baking = {
  cbk_cycle : int;
  cbk_depth : int;
  cbk_count : bk_count;
  cbk_tez : bk_tez;
  cbk_priority : float option;
  cbk_bktime : int option;
}

type cycle_endorsement = {
  ced_cycle : int;
  ced_depth : int;
  ced_slots : bk_count;
  ced_tez : bk_tez;
  ced_priority : float;
}

type baker_rights = {
  br_level : int;
  br_cycle : int;
  br_priority : int;
  br_depth : int;
}

type endorser_rights = {
  er_level : int;
  er_cycle : int;
  er_nslot : int;
  er_depth : int;
}

type cycle_rights = {
  cr_cycle : int;
  cr_nblocks : int;
  cr_priority : float; (* used for number of slots for endorsements *)
}

type rights = {
  r_level : int;
  r_bakers : account_name list;
  r_endorsers : account_name list;
  r_bakers_priority : int list;
  r_baked : (account_name * int) option
}

type marketcap = {
  mc_id : string ;
  name : string ;
  symbol : string ;
  rank : string ;
  price_usd : string ;
  price_btc : string ;
  volume_usd_24 : string option ;
  market_cap_usd : string option ;
  available_supply : string option ;
  total_supply : string option ;
  max_supply : string option ;
  percent_change_1 : string option ;
  percent_change_24 : string option ;
  percent_change_7 : string option ;
  last_updated : string ;
}

type country_stats = {
  country_name : string ;
  country_code : string ;
  total : int ;
}

type baker_stats = {
  baker_hash : account_name ;
  nb_blocks : int ;
  volume_total : int64 ;
  fees_total : int64 ;
}

type 'a per_day = {
    pd_days : string array;
    pd_value : 'a array;
  }

type service_stats = {
    service_name : string;
    service_ok_days_nb : int array;
    service_ok_days_dt : float array;
    service_fail_days_nb : int array;
    service_fail_days_dt : float array;
  }

type timing_stats = {
    timing_uptime : string;
    timing_period : string;
    timing_services : service_stats array;
  }

type mini_stats = {
    ms_period : string array;
    ms_nhours : int array;
    ms_nblocks : int array;
    ms_nops : int array;
    ms_volume : int64 array;
    ms_fees : int64 array;
  }

type health_stats = {
  cycle_start_level : int ;
  cycle_end_level : int ;
  cycle_volume : int64 ;
  cycle_fees : int64 ;
  cycle_bakers : int ;
  cycle_endorsers : int ;
  cycle_date_start : (int * int * int) ;
  cycle_date_end : (int * int * int) ;
  endorsements_rate : float ;
  main_endorsements_rate : float ;
  alt_endorsements_rate : float ;
  empty_endorsements_rate : float ;
  double_endorsements : int ;

  main_revelation_rate : float ;

  alternative_heads_number : int ;
  switch_number : int ;
  longest_switch_depth : int ;

  mean_priority : float ;
  score_priority : float ;
  biggest_block_volume : string * int ;
  biggest_block_fees : string * int ;
  top_baker : account_name;
}

type account_bonds_rewards = {
  acc_b_rewards : int64 ;
  acc_b_deposits : int64 ;
  acc_fees : int64 ;
  acc_e_rewards : int64 ;
  acc_e_deposits : int64
}

type account_status = {
  account_status_hash : account_name ;
  account_status_revelation : operation_hash option ;
  account_status_origination : operation_hash option ;
}

type tz_node_block =
  ((string * (string * string * string) list list * string)
   * (string * float * float * string * string * int * string * string list
        * string * string ))

type node_account_details =
  (account_hash * int64 * bool * (bool * account_hash option) *
   (Tezos_types.script_expr_t * Tezos_types.script_expr_t) option *
   Tezos_types.script_expr_t option * Z.t)

type account_details = {
  acc_name : account_name ;
  acc_manager : account_name ;
  acc_balance : int64 ;
  acc_spendable : bool ;
  acc_dlgt : (bool * account_name option);
  acc_script : (Tezos_types.script_expr_t * Tezos_types.script_expr_t) option;
  acc_storage : Tezos_types.script_expr_t option ;
  acc_counter : Z.t;
  acc_node_timestamp : string option;
}

type delegate_service = {
  del_srv_tz1 : account_name option ;
  del_srv_name : string ;
  del_srv_descr : string ;
  del_srv_url : string ;
  del_srv_logo : string ; (* file name, relative to "/images/" *)
  del_srv_logo2 : string option ; (* file name, relative to "/images/" *)
}

type supply_info = {
  dls : int64 ;
  foundation : int64 ;
  early_bakers : int64 ;
  contributors : int64 ;

  unfrozen_rewards : int64 ;

  missing_revelations : int ;
  revelation_rewards : int64 ;
  burned_tez_revelation : int64 ;

  burned_tez_origination : int64 ;
  burned_tez_double_baking : int64 ;

  total_supply_ico : int64 ;
  current_circulating_supply : int64 ;
}

type balance_break_down = {
  h_activated_balance : int64 ;
  h_unfrozen_rewards : int64 ;
  h_revelation_rewards : int64 ;
  h_missing_revelations : int ;
  h_burned_tez_revelation : int64 ;
  h_burned_tez_origination : int64 ;
  h_tez_origination_recv : int64 ;
  h_tez_origination_send : int64 ;
  h_burned_tez_transaction : int64 ;
  h_tez_transaction_recv : int64 ;
  h_tez_transaction_send : int64 ;
  h_burned_tez_double_baking : int64 ;
  h_tez_dbe_rewards : int64 ;
  h_total : int64 ;
}

type service = {
  srv_kind : string; (* "wallet" or "delegate" or "tzscan" *)
  srv_tz1 : string option;
  srv_name : string;
  srv_url : string;
  srv_logo : string; (* file name, related to /images/ *)
  srv_logo2 : string option; (* file name, related to /images/ *)
  srv_descr : string option;
  srv_sponsored : string option;
}

type crawler_activity = {
  crawler_name : string ;
  crawler_timestamp : float ;
  crawler_delay : int ;
}

type rewards_status =
  | Rewards_pending
  | Rewards_delivered
  | Cycle_pending
  | Cycle_in_progress

type rewards_split = {
  rs_delegate_staking_balance : int64 ;
  rs_delegators_nb : int ;
  rs_delegators_balance : (account_name * int64) list ;
  rs_block_rewards : int64 ;
  rs_fees : int64 ;
  rs_endorsement_rewards : int64 ;
  rs_baking_rights_rewards : int64 ;
  rs_endorsing_rights_rewards : int64 ;
  rs_gain_from_denounciation : int64 ;
  rs_lost_deposit : int64 ;
  rs_lost_rewards : int64 ;
  rs_lost_fees : int64 ;
  rs_rv_rewards : int64 ;
  rs_rv_lost_rewards : int64 ;
  rs_rv_lost_fees : int64
}

type all_rewards_split = {
  ars_cycle : int ;
  ars_delegate_staking_balance : int64 ;
  ars_delegators_nb : int ;
  ars_delegate_delegated_balance : int64 ;
  ars_block_rewards : int64 ;
  ars_fees : int64 ;
  ars_endorsement_rewards : int64 ;
  ars_baking_rights_rewards : int64 ;
  ars_endorsing_rights_rewards : int64 ;
  ars_status : rewards_status ;
  ars_gain_from_denounciation : int64 ;
  ars_lost_deposit : int64 ;
  ars_lost_rewards : int64 ;
  ars_lost_fees : int64 ;
  ars_rv_rewards : int64 ;
  ars_rv_lost_rewards : int64 ;
  ars_rv_lost_fees : int64
}

type delegator_reward = {
  dor_cycle: int;
  dor_delegate: account_name;
  dor_staking_balance: int64;
  dor_balance: int64;
  dor_rewards: int64;
  dor_extra_rewards: int64;
  dor_losses: int64;
  dor_status: rewards_status
}

type rewards_stats = {
  rstats_staking_balance : int64 ;
  rstats_delegators_nb : int ;
  rstats_rewards : int64 ;
  rstats_pc_blocks : float ;
  rstats_pc_endorsements : float ;
}

type snapshot = {
  snap_cycle : int ;
  snap_index : int ;
  snap_level : int ;
  snap_rolls : int
}

type proto_details = {
  prt_index: int;
  prt_hash: protocol_hash;
  prt_name: string;
  prt_start: int;
  prt_end: int;
}

type h24_stats = {
  h24_end_rate : float ;
  h24_block_0_rate : float ;
  h24_transactions : int ;
  h24_originations : int ;
  h24_delegations : int ;
  h24_activations : int ;
  h24_baking_rate : float ;
  h24_active_baker : int ;
}

type balance_update_info =
  {bu_account : account_hash;
   bu_diff : int64;
   bu_date : Date.t;
   bu_update_type : string;
   bu_internal : bool;
   bu_level : int32;
   bu_frozen : bool;
   mutable bu_burn : bool
  }

type chart_file = {
  chart_period : string;
  chart_period_kind : string;
  chart_name : string;
  chart_values : (string * float) array;
}

type context_top_kind =
  | TBalances
  | TFrozen_balances
  | TFrozen_deposits
  | TFrozen_rewards
  | TPaid_bytes
  | TStaking_balances
  | TTotal_balances
  | TTotal_delegated
  | TTotal_delegators
  | TTotal_frozen_fees
  | TUsed_bytes

(* top accounts at the beginning of this period *)
type top_accounts = {
  top_period : string;
  top_kind : string;
  top_hash : string;
  top_list : (string * int64) list ;
}

type context_file_with_diff = {
  context_level : level option ;

  context_addresses : int;
  context_addresses_diff : float ;
  context_keys : int ;
  context_keys_diff : float ;
  context_revealed : int ;
  context_revealed_diff : float ;
  context_originated : int ;
  context_originated_diff : float ;
  context_contracts : int ;
  context_contracts_diff : float ;
  context_roll_owners : int ;
  context_roll_owners_diff : float ;
  context_rolls : int ;
  context_rolls_diff : float ;
  context_delegated : int64 ;
  context_delegated_diff : float ;
  context_delegators : int ;
  context_delegators_diff : float ;
  context_deleguees : int ;
  context_deleguees_diff : float ;
  context_self_delegates : int ;
  context_self_delegates_diff : float ;
  context_multi_deleguees : int ;
  context_multi_deleguees_diff : float ;
  context_current_balances : int64 ;
  context_current_balances_diff : float ;
  context_full_balances : int64 ;
  context_full_balances_diff : float ;
  context_staking_balances : int64 ;
  context_staking_balances_diff : float ;
  context_frozen_balances : int64 ;
  context_frozen_balances_diff : float ;
  context_frozen_deposits : int64 ;
  context_frozen_deposits_diff : float ;
  context_frozen_rewards : int64 ;
  context_frozen_rewards_diff : float ;
  context_frozen_fees : int64 ;
  context_frozen_fees_diff : float ;
  context_paid_bytes : int64 ;
  context_paid_bytes_diff : float ;
  context_used_bytes : int64 ;
  context_used_bytes_diff : float ;
}

type ico_constants = {
  ico_company_tokens : int64 ;
  ico_foundation_tokens : int64 ;
  ico_early_tokens : int64 ;
  ico_contributors_tokens : int64 ;
  ico_wallets : int ;
}

type api_server_config = {
  conf_network : string ;
  conf_constants : ( int * constants ) list ; (* cycle x constants *)
  conf_ico : ico_constants ;
  conf_rampup_cycles : int ;
  conf_has_delegation : bool ;
  conf_has_marketcap : bool ;
}

type versions = {
    server_version : string;
    server_build : string;
    server_commit : string;
  }

type api_server_info = {
  mutable api_config : api_server_config ;
  mutable api_date : float ;
  mutable api_versions : versions ;
}

type www_server_info = {
  mutable www_currency_name : string ;
  mutable www_currency_short : string ;
  mutable www_currency_symbol : string ;
  mutable www_languages : (string * string) list ;
  mutable www_apis : string array ;
  mutable www_auth : string option ;
  mutable www_logo : string ;
  mutable www_footer : string ;
  mutable www_networks : ( string * string ) list ; (* name x www_url *)
}
