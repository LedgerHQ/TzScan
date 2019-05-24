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
open EzAPI

let tup1_int = EzEncoding.tup1_int
let tup1_int64 = EzEncoding.tup1_int64
let tup1_string = EzEncoding.tup1_string

let int_enc = Api_encoding.int
let int64_enc = Api_encoding.int64
let tez_enc = Api_encoding.tez

let param_number =
  Param.int
    ~name:"page_size"
    ~descr:"Number of replies"
    "number"

let param_operations =
  Param.bool
    ~name:"operations"
    ~descr:"Include operations"
    "operations"

let param_page =
  Param.int
    ~name:"page"
    ~descr:"Offset in number of pages"
    "p"

let param_contract =
  Param.bool
    ~name:"contract"
    ~descr:"Contracts if true, accounts otherwise"
    "contract"

let param_type =
  Param.string
    ~required:true
    ~name:"type of operation"
    ~descr:"Limit the request to a particular kind of operations (`Transaction`, etc.)"
    "type"

let param_status =
  Param.string
    ~name:"status of operation"
    ~descr:"Status of an operation: Pending or Processed"
    "status"

let param_peers =
  Param.string
    ~name:"state of peers"
    ~descr:"'running' or 'disconnected' or any of them if let empty"
    "state"

let param_level =
  Param.int
    ~name:"level"
    ~descr:"Filtering by level"
    "level"

let param_cycle =
  Param.int
    ~name:"cycle"
    ~descr:"Filtering by cycle"
    "cycle"

let param_delegate =
  Param.bool
    ~name:"delegate"
    ~descr:"Originations as delegate if true, manager or account otherwise"
    "delegate"

let param_search_filter =
  Param.string
    ~name:"filter"
    ~descr:"Filter search results"
    "filter"

let param_future =
  Param.bool
    ~name:"future"
    ~descr:"Get future rights"
    "future"

let param_kind =
  Param.string
    ~name:"kind of tops"
    ~descr:"Limit the request to a particular kind of top accounts (`Frozen_deposits`, etc.)"
    "kind"

let param_spendable =
  Param.bool
    ~name:"spendable"
    ~descr:"Specifies the type of balance"
    "spendable"

let param_prio =
  Param.int
    ~name:"priority"
    ~descr:"Filter by priority"
    "priority"

let param_block_hash =
  Param.string
    ~name:"block hash"
    ~descr:"Optional block hash containing the operation"
    "block_hash"

let param_token =
  Param.string
    ~name:"token"
    ~descr:"Token of recaptcha"
    "token"

let param_period =
  Param.int
    ~name:"period"
    ~descr:"Optional period for voting or other"
    "period"

let param_period_kind =
  Param.string
    ~name:"period kind"
    ~descr:"Optional period kind for voting or other"
    "period_kind"

let param_ballot =
  Param.string
    ~name:"ballot"
    ~descr:"Optional ballot"
    "ballot"

let pagination_params = [ param_page; param_number ]
let number_params = [ param_number ]
let operations_params = [ param_operations ]
let contract_params = [ param_contract ]
let filters_params = [ param_type; param_status ]
let peers_params = [ param_peers ]
let cycle_params = [ param_cycle ]
let delegate_params = [ param_delegate ]
let search_filter_params = [ param_search_filter ]
let future_params = [ param_future ]
let spendable_params = [ param_spendable ]
let prio_params = [ param_prio ]
let block_hash_params = [ param_block_hash ]
let token_params = [ param_token ]

let arg_block_hash =
  arg_string "block_hash" "BKyKuFqvCG7smSmXTewhigcVvHUTvecS8SAjsx7cjDzKe7js3HL"
let arg_account_hash =
  arg_string "account_hash" "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9"
let arg_contract_hash =
  arg_string "contract_hash" "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG"
let arg_op_hash =
  arg_string "op_hash" "oo5fwMjaLq8jzmKH1HJi9Qpg2VAfT3yMsMGtjnbHuCUAWAjiehV"
let arg_proposal_hash =
  arg_string "proposal_hash" "Psd1ynUBhMZAeajwcZJAeq5NrxorM6UCU4GJqxZ7Bx2e9vUWB6z"
let arg_hash =
  arg_string "hash" "BKyKuFqvCG7smSmXTewhigcVvHUTvecS8SAjsx7cjDzKe7js3HL"
let arg_period = arg_string "period" "minutes"
let arg_level = arg_int "level" 188418
let arg_cycle = arg_int "cycle" 42
let arg_node = arg_string "node" "balance"
let arg_alias = arg_string "alias" "Foundation Baker 1"
let arg_day = arg_string "day" "2018-10-01"
let arg_voting_period = arg_int "period" 8

module type VERSION = sig val version_str : string end

module V (V : VERSION) = struct
  let version_v = V.version_str

  let section_blocks = EzAPI.section "Blocks Requests"
  let section_block = EzAPI.section "Block Requests"
  let section_accounts = EzAPI.section "Accounts Requests"
  let section_rewards = EzAPI.section "Rewards Requests"
  let section_bakings = EzAPI.section "Bakings Requests"
  let section_operations = EzAPI.section "Operations Requests"
  let section_level = EzAPI.section "Level Requests"
  let section_protocol = EzAPI.section "Protocol Requests"
  let section_balance_updates = EzAPI.section "Balance Updates Requests"
  let section_rights = EzAPI.section "Rights Requests"

  let section_server = EzAPI.section "Server Requests" (* Private *)
  let section_stats = EzAPI.section "Stats Requests" (* Private *)
  let section_search = EzAPI.section "Search Requests" (* Private *)
  let section_node = EzAPI.section "Node Requests" (* Private *)
  let section_website = EzAPI.section "Website Requests" (* Private *)

  let sections = [ (* order is important *)
    section_blocks;
    section_level;
    section_block;
    section_operations;
    section_accounts;
    section_rights;
    section_bakings;
    section_rewards;
    section_balance_updates;
    section_protocol
  ]

  let other_sections = [
    section_server;
    section_stats;
    section_node;
    section_search;
    section_website
  ]

  (*   SECTION BLOCKS *)

  (* A single block identified by its hash *)
  let block : (string, Data_types.block) service1  =
    service
      ~section:section_blocks
      ~name:"block"
      ~params:operations_params
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version_v // "block" /: arg_block_hash)

  (* Shortcut to last block *)
  let head : Data_types.block service0 =
    service
      ~name:"head"
      ~section:section_blocks
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version_v // "head")

  (* Shortcut to first block *)
  let genesis : Data_types.block service0 =
    service
      ~name:"genesis"
      ~section:section_blocks
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version_v // "genesis")

  (* A list of blocks *)
  let blocks : Data_types.block list service0 =
    service
      ~section:section_blocks
      ~name:"blocks"
      ~params: (operations_params@pagination_params)
      ~output:(Json_encoding.list Api_encoding.V1.Block.encoding)
      Path.(root // version_v // "blocks")

  let blocks_with_pred_fitness : (Data_types.block * string) list service0 =
    service
      ~section:section_website
      ~name:"blocks_with_pred_fitness"
      ~params:pagination_params
      ~output:Json_encoding.(
          list (obj2
                  (req "block" Api_encoding.V1.Block.encoding)
                  (req "predecessor_fitness" string)))
      Path.(root // version_v // "blocks_with_pred_fitness")

  (* Alternative heads *)
  let heads : Data_types.block list service0 =
    service
      ~section:section_blocks
      ~name:"heads"
      ~params:(param_level :: pagination_params)
      ~output:(Json_encoding.list Api_encoding.V1.Block.encoding)
      Path.(root // version_v // "heads")

  let nb_heads : int service0 =
    service
      ~section:section_blocks
      ~name:"nb_heads"
      ~output:tup1_int
      Path.(root // version_v // "nb_heads")

  let heads_with_pred_fitness : (Data_types.block * string) list service0 =
    service
      ~section:section_website
      ~name:"heads_with_pred_fitness"
      ~params:(param_level :: pagination_params)
      ~output:Json_encoding.(
          list (obj2
                  (req "block" Api_encoding.V1.Block.encoding)
                  (req "predecessor_fitness" string)))
      Path.(root // version_v // "heads_with_pred_fitness")

    (* Count Alternative heads *)
  let nb_uncles : (int, int) service1 =
    service
      ~section:section_blocks
      ~name:"nb_uncles"
      ~output:tup1_int
      Path.(root // version_v // "nb_uncles" /: arg_level)

  let snapshot_levels : int list service0 =
    service
      ~section:section_blocks
      ~name:"snapshot_levels"
      ~output:(Json_encoding.list int_enc)
      Path.(root // version_v // "snapshot_levels")

  let snapshot_blocks : Data_types.snapshot list service0 =
    service
      ~section:section_blocks
      ~name:"snapshot_blocks"
      ~params:pagination_params
      ~output:Api_encoding.V1.Snapshot.encoding
      Path.(root // version_v // "snapshot_blocks")

  let nb_snapshot_blocks : int service0 =
    service
      ~section:section_blocks
      ~name:"nb_snapshot_blocks"
      ~output:tup1_int
      Path.(root // version_v // "nb_snapshot_blocks")

  let nb_cycle: int service0 =
    service
      ~section:section_website
      ~name:"nb_cycle"
      ~output:tup1_int
      Path.(root // version_v // "nb_cycle")

  (*    SECTION ACCOUNTS *)

  (* A list of accounts *)
  let accounts : Data_types.account list service0 =
    service
      ~name:"accounts"
      ~params:(pagination_params @ contract_params)
      ~section:section_accounts
      ~output:(Json_encoding.list Api_encoding.V1.Account.encoding)
      Path.(root // version_v // "accounts")

  (* The number of accounts *)
  let nb_accounts : int service0 =
    service
      ~section:section_accounts
      ~name:"nb_accounts"
      ~params: contract_params
      ~output:tup1_int
      Path.(root // version_v // "number_accounts")

  let account_bonds_rewards : (string, Data_types.account_bonds_rewards) service1 =
    service
      ~section:section_accounts
      ~name:"bonds_rewards"
      ~output:Api_encoding.V1.Bonds_rewards.encoding
      Path.(root // version_v // "bonds_rewards" /: arg_account_hash)

  let extra_bonds_rewards : (string, Data_types.account_extra_rewards) service1 =
    service
      ~section:section_accounts
      ~name:"extra_bonds_rewards"
      ~output:Api_encoding.V1.Bonds_rewards.extra
      Path.(root // version_v // "extra_bonds_rewards" /: arg_account_hash)

  let max_roll_cycle : int service0 =
    service
      ~section:section_stats
      ~name:"max_roll_cycle"
      ~output:tup1_int
      Path.(root // version_v // "max_roll_cycle")

  let rolls_distribution : (int, (account_name * int) list) service1 =
    service
      ~section:section_stats
      ~name:"roll_distribution"
      ~output:Api_encoding.V1.Rolls_distribution.encoding
      Path.(root // version_v // "rolls_distribution" /: arg_cycle)

  (* tz1 -> [ cycle, roll_count, total_roll_count ] *)
  let rolls_history : (account_hash, (int64 * int32 * int32) list) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"rolls_history"
      ~output:Json_encoding.(list (tup3 Api_encoding.int64 int32 int32))
      Path.(root // version_v // "rolls_history" /: arg_account_hash)

  (* tz1 -> roll_count *)
  let roll_number : (account_hash, int) service1 =
    service
      ~section:section_accounts
      ~name:"roll_number"
      ~output:tup1_int
      Path.(root // version_v // "roll_number" /: arg_account_hash)

  (* tz1 -> [ hash, reveal option, origination option ] *)
  let account_status : (account_hash, Data_types.account_status) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"account_status"
      ~output:Api_encoding.V1.Account_status.encoding
      Path.(root // version_v // "account_status" /: arg_account_hash)

  let account_from_alias : (string, account_hash option) service1 =
     service
      ~section:section_accounts
      ~name:"account_from_alias"
      ~output:Json_encoding.(tup1 (option string))
      Path.(root // version_v // "account_from_alias" /: arg_alias)

  let votes_account: (string, proposal list) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"votes_account"
      ~output:Api_encoding.V1.Proposal.encodings
      Path.(root // version_v // "votes_account" /: arg_account_hash)

  let vote_graphs_account: (string, (int * int * int) list *
                                    (int * int * int) list) service1 =
    service
      ~section:section_website
      ~name:"vote_graphs_account"
      ~output:Api_encoding.V1.Proposal.vote_graphs_encoding
      Path.(root // version_v // "vote_graphs_account" /: arg_account_hash)

  (* SECTION REWARDS *)

  let rewards_split_cycles : (account_hash, all_rewards_split list) service1 =
    service
      ~params:pagination_params
      ~name:"rewards_split_cycles"
      ~section:section_rewards
      ~output: (Json_encoding.list Api_encoding.V1.Rewards_split.all_encoding)
      Path.(root // version_v // "rewards_split_cycles" /: arg_account_hash)

  let nb_cycle_rewards : (account_hash, int) service1 =
    service
      ~name:"nb_cycle_rewards"
      ~section:section_rewards
      ~output:tup1_int
      Path.(root // version_v // "nb_cycle_rewards" /: arg_account_hash)

  let rewards_split : (account_hash, rewards_split) service1 =
    service
      ~params:(pagination_params @ cycle_params)
      ~name:"rewards_split"
      ~section:section_rewards
      ~output:Api_encoding.V1.Rewards_split.encoding
      Path.(root // version_v // "rewards_split" /: arg_account_hash)

  let rewards_split_fast : (account_hash, (account_name * int64) list) service1 =
    service
      ~params: (pagination_params @ cycle_params)
      ~name:"rewards_split_fast"
      ~section:section_website
      ~output:Json_encoding.(
          list (tup2 Api_encoding.account_name_encoding tez_enc))
      Path.(root // version_v // "rewards_split_fast" /: arg_account_hash)

  let nb_delegators : (account_hash, int) service1 =
    service
      ~params:cycle_params
      ~name:"nb_delegators"
      ~section:section_rewards
      ~output:tup1_int
      Path.(root // version_v // "nb_delegators" /: arg_account_hash)

  let delegator_rewards : (account_hash, delegator_reward list) service1 =
    service
      ~params:pagination_params
      ~name:"delegator_rewards"
      ~section:section_rewards
      ~output:Api_encoding.V1.Rewards_split.delegator_encodings
      Path.(root // version_v // "delegator_rewards" /: arg_contract_hash)

  let delegator_rewards_with_details :
    (account_hash, (delegator_reward * delegator_reward_details) list) service1 =
    service
      ~params:pagination_params
      ~name:"delegator_rewards_with_details"
      ~section:section_rewards
      ~output:(Json_encoding.list Api_encoding.V1.Rewards_split.delegator_rewards_all)
      Path.(root // version_v // "delegator_rewards_with_details" /: arg_contract_hash)

  let nb_cycle_delegator_rewards : (account_hash, int) service1 =
    service
      ~name:"nb_cycle_delegator_rewards"
      ~section:section_rewards
      ~output:tup1_int
      Path.(root // version_v // "nb_cycle_delegator_rewards" /: arg_contract_hash)


  (*   SECTION OPERATIONS *)

  (* A single operation *)
  let operation : (string, Data_types.operation) service1 =
    service
      ~params:block_hash_params
      ~name:"operation"
      ~section:section_operations
      ~output:Api_encoding.V1.Operation.operation
      Path.(root // version_v // "operation" /: arg_op_hash)

  (* A list of operations *)
  let operations_bh : (string, Data_types.operation list) service1 =
    service
      ~params:(filters_params @ pagination_params @ delegate_params)
      ~section:section_operations
      ~name:"operations_bh"
      ~output:(Json_encoding.list Api_encoding.V1.Operation.operation)
      Path.(root // version_v // "operations" /: arg_block_hash)

  (* The number of operations for a block or an account *)
  let nb_operations_hash : (string, int) service1 =
    service
      ~params:(filters_params @ delegate_params)
      ~name:"nb_operations_hash"
      ~section:section_operations
      ~output:tup1_int
      Path.(root // version_v // "number_operations" /: arg_hash)

  let operations : Data_types.operation list service0 =
    service
      ~params:(filters_params @ pagination_params)
      ~section:section_operations
      ~name:"operations"
      ~output:(Json_encoding.list Api_encoding.V1.Operation.operation)
      Path.(root // version_v // "operations")

  (* estimate gas *)
  let estimate_gas : int service0 =
    service
      ~params:token_params
      ~name:"estimate_gas"
      ~section:section_operations
      ~output:tup1_int
      Path.(root // version_v // "estimate_gas")

  (* estimate storage *)
  let estimate_storage : int service0 =
    service
      ~name:"estimate_storage"
      ~section:section_operations
      ~output:tup1_int
      Path.(root // version_v // "estimate_storage")

  (* broadcast transaction *)
  let broadcast_transaction : (string, string) post_service0 =
    post_service
      ~name:"broadcast_transaction"
      ~input:tup1_string
      ~params:token_params
      ~section:section_operations
      ~output:tup1_string
      Path.(root // version_v // "broadcast_transaction")
  
  (* counter *)
  let counter : int service0 =
    service
      ~params:token_params
      ~name:"counter"
      ~section:section_operations
      ~output:tup1_int
      Path.(root // version_v // "counter")

  (* The number of operations *)
  let nb_operations : int service0 =
    service
      ~params:filters_params
      ~name:"nb_operations"
      ~section:section_operations
      ~output:tup1_int
      Path.(root // version_v // "number_operations")

  let endorsements_level : (int, Data_types.operation list) service1 =
    service
      ~section:section_operations
      ~name:"endorsements_level"
      ~output:(Json_encoding.list Api_encoding.V1.Operation.operation)
      Path.(root // version_v // "endorsements" /: arg_level)

  let nonces : (Data_types.nonces list) service0 =
    service
      ~params:pagination_params
      ~section:section_operations
      ~name:"nonces"
      ~output:(Json_encoding.list Api_encoding.V1.Nonce_hash.encoding)
      Path.(root // version_v // "nonces")

  let transaction_account_csv : (string, string) service1 =
    service
      ~params:token_params
      ~section:section_website
      ~name:"transaction_csv"
      ~output:tup1_string
      Path.(root // version_v // "transaction_account_csv" /: arg_account_hash)

      (* <--- not used ?
  let pending_operations : Data_types.operation list service0 =
    service
      ~output:(Json_encoding.list Api_encoding.Operation.API.api_operation)
      Path.(root // version_v // "pending_operations")

  let pending_operations_hash : (string, Data_types.operation list) service1 =
    service
      ~output:(Json_encoding.list Api_encoding.Operation.API.api_operation)
      Path.(root // version_v // "pending_operations" /: arg_string "hash")

  let pending_endorsements : (string, Data_types.operation list) service1 =
    service
      ~output:(Json_encoding.list Api_encoding.Operation.API.api_operation)
      Path.(root // version_v // "pending_endorsements" /: arg_string "hash")
       *)

  (* SECTION BAKINGS *)

  (* List of baked blocks for a given account *)
  let bakings : (string, Data_types.baking list) service1 =
    service
      ~params:(cycle_params @ pagination_params)
      ~name:"bakings"
      ~section:section_bakings
      ~output:Api_encoding.V1.BakeOp.bakings
      Path.(root // version_v // "bakings" /: arg_account_hash)

  (* The number of baked blocks for an account *)
  let nb_bakings : (string, int) service1 =
    service
      ~params:cycle_params
      ~name:"nb_bakings"
      ~section:section_bakings
      ~output:tup1_int
      Path.(root // version_v // "number_bakings" /: arg_account_hash)

  let bakings_endorsement : (string, Data_types.baking_endorsement list) service1 =
    service
      ~params:(cycle_params @ pagination_params)
      ~name:"bakings_endorsement"
      ~section:section_bakings
      ~output:Api_encoding.V1.BakeEndorsementOp.bakings
      Path.(root // version_v // "bakings_endorsement" /: arg_account_hash)

  let nb_bakings_endorsement : (string, int) service1 =
    service
      ~params:cycle_params
      ~name:"nb_bakings_endorsement"
      ~section:section_bakings
      ~output:tup1_int
      Path.(root // version_v // "number_bakings_endorsement" /: arg_account_hash)

  let cycle_bakings : (string, Data_types.cycle_baking list) service1 =
    service
      ~params:pagination_params
      ~name:"cycle_bakings"
      ~section:section_bakings
      ~output:Api_encoding.V1.CycleBakeOp.bakings
      Path.(root // version_v // "cycle_bakings" /: arg_account_hash)

  let nb_cycle_bakings : (string, int) service1 =
    service
      ~name:"nb_cycle_bakings"
      ~section:section_bakings
      ~output:tup1_int
      Path.(root // version_v // "number_cycle_bakings" /: arg_account_hash)

  let cycle_endorsements : (string, Data_types.cycle_endorsement list) service1 =
    service
      ~params:pagination_params
      ~name:"cycle_endorsements"
      ~section:section_bakings
      ~output:Api_encoding.V1.CycleEndorsementOp.bakings
      Path.(root // version_v // "cycle_endorsements" /: arg_account_hash)

  let nb_cycle_endorsements : (string, int) service1 =
    service
      ~name:"nb_cycle_endorsements"
      ~section:section_bakings
      ~output:tup1_int
      Path.(root // version_v // "number_cycle_endorsements" /: arg_account_hash)

  let bakings_history :
    (string, Data_types.cycle_baking list * Data_types.cycle_rights list *
             Data_types.cycle_baking list) service1 =
    service
      ~params:pagination_params
      ~name:"bakings_history"
      ~section:section_website
      ~output:(Json_encoding.tup3
                 Api_encoding.V1.CycleBakeOp.bakings
                 Api_encoding.V1.CycleRights.rights
                 Api_encoding.V1.CycleBakeOp.bakings)
      Path.(root // version_v // "bakings_history" /: arg_account_hash)

  let nb_bakings_history : (string, int) service1 =
    service
      ~name:"nb_bakings_history"
      ~section:section_website
      ~output:tup1_int
      Path.(root // version_v // "number_bakings_history" /: arg_account_hash)

  let total_bakings : (string, Data_types.cycle_baking list) service1 =
    service
      ~name:"total_bakings"
      ~section:section_bakings
      ~output:Api_encoding.V1.CycleBakeOp.bakings
      Path.(root // version_v // "total_bakings" /: arg_account_hash)

  let endorsements_history :
    (string, Data_types.cycle_endorsement list * Data_types.cycle_rights list *
             Data_types.cycle_endorsement list) service1 =
    service
      ~params:pagination_params
      ~name:"endorsements_history"
      ~section:section_website
      ~output:(Json_encoding.tup3
                 Api_encoding.V1.CycleEndorsementOp.bakings
                 Api_encoding.V1.CycleRights.rights
                 Api_encoding.V1.CycleEndorsementOp.bakings)
      Path.(root // version_v // "endorsements_history" /: arg_account_hash)

  let nb_endorsements_history : (string, int) service1 =
    service
      ~name:"nb_endorsements_history"
      ~section:section_website
      ~output:tup1_int
      Path.(root // version_v // "number_endorsements_history" /: arg_account_hash)

  let total_endorsements : (string, Data_types.cycle_endorsement list) service1 =
    service
      ~name:"total_endorsements"
      ~section:section_bakings
      ~output:Api_encoding.V1.CycleEndorsementOp.bakings
      Path.(root // version_v // "total_endorsements" /: arg_account_hash)

  let last_baking_and_endorsement :
    (string, baking list * baking_endorsement list * int * int) service1 =
    service
      ~section:section_bakings
      ~name:"last_baking_and_endorsement"
      ~output:(Json_encoding.tup4
                 Api_encoding.V1.BakeOp.bakings
                 Api_encoding.V1.BakeEndorsementOp.bakings
                 int_enc int_enc)
      Path.(root // version_v // "last_baking_and_endorsement" /: arg_account_hash)

  let next_baking_and_endorsement : (string, int * int * int * int * string) service1 =
    service
      ~section:section_bakings
      ~name:"next_baking_and_endorsement"
      ~output:Json_encoding.(tup5 int_enc int_enc int_enc int_enc string)
      Path.(root // version_v // "next_baking_and_endorsement" /: arg_account_hash)

  (* SECTION RIGHTS *)

  let cycle_rights : Data_types.rights list service0 =
    service
      ~params:(pagination_params @ search_filter_params @ future_params)
      ~name:"cycle_rights"
      ~section:section_rights
      ~output:(Json_encoding.list Api_encoding.V1.Rights.encoding)
      Path.(root // version_v // "baking_rights")

  let nb_cycle_rights : int service0 =
    service
      ~params:(search_filter_params @ future_params)
      ~name:"nb_cycle_rights"
      ~section:section_rights
      ~output:tup1_int
      Path.(root // version_v // "number_baking_rights")

  let baker_rights : (string, Data_types.baker_rights list) service1 =
    service
      ~params:(cycle_params @ pagination_params)
      ~name:"baker_rights"
      ~section:section_rights
      ~output:Api_encoding.V1.BakerRights.rights
      Path.(root // version_v // "baker_rights" /: arg_account_hash)

  let nb_baker_rights : (string, int) service1 =
    service
      ~params:cycle_params
      ~name:"nb_baker_rights"
      ~section:section_rights
      ~output:tup1_int
      Path.(root // version_v // "number_baker_rights" /: arg_account_hash)

  let cycle_all_rights : (string, int * int) service1 =
    service
      ~params:(cycle_params@prio_params)
      ~name:"cycle_all_rights"
      ~section:section_website
      ~output:(Json_encoding.tup2 int_enc int_enc)
      Path.(root // version_v // "cycle_all_rights" /: arg_account_hash)

  let cycle_baker_rights : (string, Data_types.cycle_rights list) service1 =
    service
      ~name:"cycle_baker_rights"
      ~section:section_rights
      ~output:Api_encoding.V1.CycleRights.rights
      Path.(root // version_v // "cycle_baker_rights" /: arg_account_hash)

  let nb_endorser_rights : (string, int) service1 =
    service
      ~params:cycle_params
      ~name:"nb_endorser_rights"
      ~section:section_rights
      ~output:tup1_int
      Path.(root // version_v // "number_endorser_rights" /: arg_account_hash)

  let endorser_rights : (string, Data_types.endorser_rights list) service1 =
    service
      ~params:(cycle_params @ pagination_params)
      ~name:"endorser_rights"
      ~section:section_rights
      ~output:(Json_encoding.list Api_encoding.V1.EndorserRights.encoding)
      Path.(root // version_v // "endorser_rights" /: arg_account_hash)

  let cycle_endorser_rights : (string, Data_types.cycle_rights list) service1 =
    service
      ~name:"cycle_endorser_rights"
      ~section:section_rights
      ~output:Api_encoding.V1.CycleRights.rights
      Path.(root // version_v // "cycle_endorser_rights" /: arg_account_hash)

  let required_balance : (string, (int * int64 * int64 * int64 * int * int) list) service1 =
    service
      ~name:"required_balance"
      ~section:section_rights
      ~output:(Json_encoding.list
                 (Json_encoding.tup6
                    int_enc tez_enc tez_enc tez_enc int_enc int_enc))
      Path.(root // version_v // "required_balance" /: arg_account_hash)

  (*   SECTION BLOCK *)

  let block_succ : (string, block_hash) service1 =
    service
      ~section:section_block
      ~name:"block_next"
      ~output:tup1_string
      Path.(root // version_v // "block_next" /: arg_block_hash)

  let block_pred : (string, block_hash) service1 =
    service
      ~section:section_block
      ~name:"block_prev"
      ~output:tup1_string
      Path.(root // version_v // "block_prev" /: arg_block_hash)

  let timestamp : (string, timestamp) service1 =
    service
      ~section:section_block
      ~name:"timestamp"
      ~output:tup1_string
      Path.(root // version_v // "timestamp" /: arg_block_hash)

  let level : (string, Tezos_types.level) service1 =
    service
      ~section:section_block
      ~name:"level"
      ~output:Api_encoding.V1.Level.encoding
      Path.(root // version_v // "level" /: arg_block_hash)

  let network : (string, network_hash) service1 =
    service
      ~section:section_block
      ~name:"network"
      ~output:tup1_string
      Path.(root // version_v // "network" /: arg_block_hash)

  let priority : (string, int) service1 =
    service
      ~section:section_block
      ~name:"priority"
      ~output:tup1_int
      Path.(root // version_v // "priority" /: arg_block_hash)

  let volume : (string, int64) service1 =
    service
      ~section:section_block
      ~name:"volume"
      ~output:tup1_int64
      Path.(root // version_v // "volume" /: arg_block_hash)


  (*    SECTION LEVEL *)

  let block_level : (int, Data_types.block) service1 =
    service
      ~params:operations_params
      ~section:section_level
      ~name:"block_level"
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version_v // "block_level" /: arg_level)

  let block_hash_level : (int, block_hash) service1 =
    service
      ~section:section_level
      ~name:"block_hash_level"
      ~output:tup1_string
      Path.(root // version_v // "block_hash_level" /: arg_level)

  (* SECTION STATS *)

  let marketcap : Data_types.marketcap service0 =
    service
      ~section:section_stats
      ~name:"marketcap"
      ~output:Api_encoding.V1.MarketCap.encoding
      Path.(root // version_v // "marketcap" )

  (* The number of peers *)
  let nb_network_peers : int service0 =
    service
      ~params:peers_params
      ~section:section_stats
      ~name:"nb_network_peers"
      ~output:tup1_int
      Path.(root // version_v // "number_network_peers")

  let network_stats : Tezos_types.network_stats list service0 =
    service
      ~params:(pagination_params @ peers_params)
      ~section:section_stats
      ~name:"network_stats"
      ~output:Api_encoding.V1.Network.encoding
      Path.(root // version_v // "network" )

  let country_stats : Data_types.country_stats list service0 =
    service
      ~params:peers_params
      ~section:section_stats
      ~name:"country_stats"
      ~output:Api_encoding.V1.Network.country_stats_encoding
      Path.(root // version_v // "country" )

  let baker : (string, Data_types.baker_stats) service1 =
    service
      ~section:section_stats
      ~name:"baker"
      ~output:Api_encoding.V1.Baker.encoding
      Path.(root // version_v // "baker_stats" /: arg_account_hash )

  let bakers : Data_types.baker_stats list service0 =
    service
      ~params:cycle_params
      ~section:section_stats
      ~name:"bakers"
      ~output:Api_encoding.V1.Baker.bakers_encoding
      Path.(root // version_v // "baker_stats" )

  let blocks_per_day : int Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"blocks_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version_v // "blocks_per_day" )

  let bakers_per_day : int Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"bakers_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version_v // "bakers_per_day" )

  let priorities_per_day : float Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"priorities_per_day"
      ~output:Api_encoding.V1.Charts.float_per_day_encoding
      Path.(root // version_v // "priorities_per_day" )

  let operations_per_day : int Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"operations_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version_v // "operations_per_day" )

  let operations_per_block_per_day : int Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"operations_per_block_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version_v // "operations_per_block_per_day" )

  let fees_per_day : int64 Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"fees_per_day"
      ~output:Api_encoding.V1.Charts.int64_per_day_encoding
      Path.(root // version_v // "fees_per_day" )

  let volume_per_day : int64 Data_types.per_day service0 =
    service
      ~section:section_stats
      ~name:"volume_per_day"
      ~output:Api_encoding.V1.Charts.int64_per_day_encoding
      Path.(root // version_v // "volume_per_day" )

  let mini_stats : mini_stats service0 =
    service
      ~name:"mini_stats"
      ~section:section_stats
      ~output:Api_encoding.V1.Charts.mini_stats
      Path.(root // version_v // "mini_stats" )

  let health_stats : (int, Data_types.health_stats) service1 =
    service
      ~section:section_stats
      ~name:"health"
      ~output:Api_encoding.V1.Health.encoding
      Path.(root // version_v // "health_stats" /: arg_cycle)

  let context_days : (string list) service0 =
    service
      ~section:section_stats
      ~name:"context_days"
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "context_days")

  let context_stats : (string, Data_types.context_file_with_diff) service1 =
    service
      ~section:section_stats
      ~name:"context_stats"
      ~output:Api_encoding.Context_stats.context_with_diff_encoding
      Path.(root // version_v // "context_stats" /: arg_day)

  let nb_tops : int service0 =
    service
      ~params:[ param_kind ]
      ~section:section_stats
      ~name:"nb_tops"
      ~output:tup1_int
      Path.(root // version_v // "nb_tops")

  let tops : top_accounts service0 =
    service
      ~params: (param_kind :: pagination_params)
      ~section:section_stats
      ~name:"tops"
      ~output:Api_encoding.Tops.top_accounts_encoding
      Path.(root // version_v // "tops")

  let h24_stats : Data_types.h24_stats service0 =
    service
      ~section:section_stats
      ~name:"24h"
      ~output:Api_encoding.V1.H24_stats.encoding
      Path.(root // version_v // "24h_stats")

  let supply : supply_info service0 =
    service
      ~name:"supply"
      ~section: section_stats
      ~output: Api_encoding.V1.Supply.encoding
      Path.(root // version_v // "supply")

  let balance_break_down : (string, balance_break_down) service1 =
    service
      ~name:"balance_break_down"
      ~section: section_stats
      ~output: Api_encoding.V1.Supply.h_encoding
      Path.(root // version_v // "balance_break_down" /: arg_account_hash)

  let activated_balances : int64 service0 =
    service
      ~name:"activated_balances"
      ~section: section_stats
      ~output:tup1_int64
      Path.(root // version_v // "activated_balances")


  let market_prices : (string * ( string * float ) array) array service0 =
    service
      ~section:section_stats
      ~name:"market_prices"
      ~output:Json_encoding.(array (tup2 string (array (tup2 string float))))
      Path.(root // version_v // "market_prices")

  (*   SECTION SEARCH   *)

  let search_block : (string, string list) service1 =
    service
      ~name:"search_block"
      ~section: section_search
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "search_block" /: arg_block_hash)

  let search_operation : (string, string list) service1 =
    service
      ~name:"search_operation"
      ~section: section_search
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "search_operation" /: arg_op_hash)

  let search_account : (string, (account_name * string) list) service1 =
    service
      ~name:"search_account"
      ~section: section_search
      ~output:Json_encoding.(list (tup2 Api_encoding.account_name_encoding string))
      Path.(root // version_v // "search_account" /: arg_account_hash)

  let nb_search_block : (string, int) service1 =
    service
      ~name:"nb_search_block"
      ~section: section_search
      ~output:tup1_int
      Path.(root // version_v // "nb_search_block" /: arg_block_hash)

  let nb_search_operation : (string, int) service1 =
    service
      ~name:"nb_search_operation"
      ~section: section_search
      ~output:tup1_int
      Path.(root // version_v // "nb_search_operation" /: arg_op_hash)

  let nb_search_account : (string, int) service1 =
    service
      ~name:"nb_search_account"
      ~section: section_search
      ~output:tup1_int
      Path.(root // version_v // "nb_search_account" /: arg_account_hash)

  let alias : (account_hash, string option) service1 =
    service
      ~section:section_search
      ~name:"alias"
      ~output:Json_encoding.(tup1 (option string))
      Path.(root // version_v // "alias" /: arg_account_hash)

  (* SECTION NODE *)

  let node_timestamps: (string * string option) list service0 =
    service
      ~section:section_node
      ~name:"node_timestamps"
      ~output: Json_encoding.(list (tup2  string (option string)))
      Path.(root // version_v // "node_timestamps")

  let node_account: (string, account_details) service1 =
    service
      ~section:section_node
      ~name:"node_account"
      ~output:Api_encoding.V1.Account_details.encoding
      Path.(root // version_v // "node_account" /: arg_account_hash)

  let node_deactivated_grace_period: (string, bool * int) service1 =
    service
      ~section:section_node
      ~name:"node_is_deactivated"
      ~output:(Json_encoding.tup2 Json_encoding.bool int_enc)
      Path.(root // version_v // "node_deactivated_grace_period" /: arg_account_hash)

  let node_nb_delegated_contracts: (string, int) service1 =
    service
      ~section:section_node
      ~name:"node_nb_delegated_contracts"
      ~output:tup1_int
      Path.(root // version_v // "nb_delegated_contracts" /: arg_account_hash)

  let node_delegated_contracts: (string, string list) service1 =
    service
      ~params:pagination_params
      ~section:section_node
      ~name:"node_delegated_contracts"
      ~output: Json_encoding.(list string)
      Path.(root // version_v // "delegated_contracts" /: arg_account_hash)

  let node_staking_balance: (string, int64) service1 =
    service
      ~section:section_node
      ~name:"node_staking_balance"
      ~output:tup1_int64
      Path.(root // version_v // "staking_balance" /: arg_account_hash)

  (* SECTION SERVER *)

  let version : versions service0 =
    service
      ~section:section_server
      ~name:"version"
      ~output:Api_encoding.V1.Server.versions
      Path.(root // version_v // "version" )

  let api_server_info: api_server_info service0 =
    service
      ~section:section_server
      ~name:"info"
      ~output: Api_encoding.V1.Server.api_server_info
      Path.(root // version_v // "info")

  let date: (float) service0 =
    service
      ~section: section_server
      ~name:"server"
      ~output: (Json_encoding.tup1 Json_encoding.float)
      Path.(root // version_v // "date")

  (* SECTION PROTOCOL *)

  let protocols : proto_details list service0 =
    service
      ~section:section_protocol
      ~name:"protocols"
      ~params:pagination_params
      ~output:Api_encoding.V1.Proto_details.encoding
      Path.(root // version_v // "protocols")

  let nb_protocol : int service0 =
    service
      ~section:section_protocol
      ~name:"nb_protocol"
      ~output:tup1_int
      Path.(root // version_v // "nb_protocol")

  let voting_period_info : (int * voting_period_kind * int *
                            int * bool * voting_period_status list * int) service0 =
    service
      ~section:section_protocol
      ~name:"voting_period_info"
      ~params:[param_period]
      ~output:Api_encoding.V1.Proposal.voting_info
      Path.(root // version_v // "voting_period_info")

  let total_voters : (int, int * int) service1 =
    service
      ~section:section_protocol
      ~name:"total_voters"
      ~output:Json_encoding.(obj2 (req "count" int_enc) (req "votes" int_enc))
      Path.(root // version_v // "total_voters" /: arg_voting_period)

  let proposals : proposal list service0 =
    service
      ~section:section_protocol
      ~name:"proposals"
      ~params:(param_period :: pagination_params)
      ~output:Api_encoding.V1.Proposal.encodings
      Path.(root // version_v // "proposals")

  let nb_proposals : int service0 =
    service
      ~section:section_protocol
      ~name:"nb_proposals"
      ~params:[param_period]
      ~output:tup1_int
      Path.(root // version_v // "nb_proposals")

  let proposal_votes : (proposal_hash, proposal list) service1 =
    service
      ~section:section_protocol
      ~name:"proposal_votes"
      ~params:(param_period :: pagination_params)
      ~output:Api_encoding.V1.Proposal.encodings
      Path.(root // version_v // "proposal_votes" /: arg_proposal_hash)

  let nb_proposal_votes : (proposal_hash, int * int) service1 =
    service
      ~section:section_protocol
      ~name:"nb_proposal_votes"
      ~params:[param_period]
      ~output:Json_encoding.(obj2 (req "count" int_enc) (req "votes" int_enc))
      Path.(root // version_v // "nb_proposal_votes" /: arg_proposal_hash)

  let total_proposal_votes : (int, int * int * int * int * int * int * int) service1 =
    service
      ~section:section_protocol
      ~name:"total_proposal_votes"
      ~output:Json_encoding.(
          obj7 (req "proposal_count" int_enc)
            (req "total_count" int_enc) (req "total_votes" int_enc)
            (req "used_count" int_enc) (req "used_votes" int_enc)
            (req "unused_count" int_enc) (req "unused_votes" int_enc))
      Path.(root // version_v // "total_proposal_votes" /: arg_voting_period)

  let testing_proposal : (int, proposal_hash) service1 =
    service
      ~section:section_protocol
      ~name:"testing_proposal"
      ~params:[param_period_kind]
      ~output:tup1_string
      Path.(root // version_v // "testing_protocol" /: arg_voting_period)

  let ballots : (int, (proposal_hash * int * int * int * int * int * int)) service1 =
    service
      ~section:section_protocol
      ~name:"ballots"
      ~params:[param_period_kind]
      ~output:Api_encoding.V1.Proposal.ballot_encoding
      Path.(root // version_v // "ballots" /: arg_voting_period)


  let ballot_votes : (proposal_hash, proposal list) service1 =
    service
      ~section:section_protocol
      ~name:"ballot_votes"
      ~params:(param_period :: param_ballot :: pagination_params)
      ~output:Api_encoding.V1.Proposal.encodings
      Path.(root // version_v // "ballot_votes" /: arg_proposal_hash)

  let nb_ballot_votes : (proposal_hash, (int * int)) service1 =
    service
      ~section:section_protocol
      ~name:"nb_ballot_votes"
      ~params:[param_period; param_ballot]
      ~output:Json_encoding.(obj2 (req "count" int_enc) (req "votes" int_enc))
      Path.(root // version_v // "nb_ballot_votes" /: arg_proposal_hash)

  (* SECTION BALANCE UPDATES *)

  let balance_updates : (string, balance_update_info list) service1 =
    service
      ~section:section_balance_updates
      ~name:"balance_updates"
      ~params:(pagination_params@cycle_params)
      ~output:Api_encoding.V1.Balance_update_info.encoding
      Path.(root // version_v // "balance_updates" /: arg_account_hash)

  let balance_updates_number : (string, int) service1 =
    service
      ~section:section_balance_updates
      ~name:"nb_balance_updates"
      ~params:cycle_params
      ~output:tup1_int
      Path.(root // version_v // "balance_updates_number" /: arg_account_hash)

  let active_balance_updates : (string, balance_update_info list) service1 =
    service
      ~section:section_balance_updates
      ~name:"active_balance_updates"
      ~params:cycle_params
      ~output:Api_encoding.V1.Balance_update_info.encoding
      Path.(root // version_v // "active_balance_updates" /: arg_account_hash)

  let balance : (string, Int64.t) service1 =
    service
      ~section:section_balance_updates
      ~name:"balance"
      ~output:tup1_int64
      Path.(root // version_v // "balance" /: arg_account_hash)

  let balance_from_balance_updates :
        (string, balance)
          service1 =
    service
      ~section:section_balance_updates
      ~name:"balance_from_balance_updates"
      ~output:(Api_encoding.V1.Balance.encoding)
      Path.(root // version_v // "balance_from_balance_updates" /: arg_account_hash)

  let balance_history : (string, (Int32.t * balance) list) service1 =
    service
      ~section:section_balance_updates
      ~name:"balance_history"
      ~output:(Json_encoding.list @@
                 Json_encoding.tup2
                   Json_encoding.int32
                   Api_encoding.V1.Balance.encoding)
      Path.(root // version_v // "balance_history" /: arg_account_hash)

  let cycle_frozen : (string, balance) service1 =
    service
      ~section:section_balance_updates
      ~name:"cycle_frozen"
      ~params:[param_cycle]
      ~output:(Api_encoding.V1.Balance.encoding)
      Path.(root // version_v // "cycle_frozen" /: arg_account_hash)

  let balance_ranking : ((int * account_name * Int64.t) list) service0 =
    service
      ~section:section_balance_updates
      ~name:"balance_ranking"
      ~params:(pagination_params @ cycle_params @ spendable_params)
      ~output:(Json_encoding.list @@
               Json_encoding.tup3
                 int_enc
                 Api_encoding.account_name_encoding
                 tez_enc)
      Path.(root // version_v // "balance_ranking")

  let balance_number : int service0 =
    service
      ~section:section_balance_updates
      ~name:"nb_balance"
      ~params:cycle_params
      ~output:tup1_int
      Path.(root // version_v // "balance_number")

end

let init () = ()

module V1 = V (struct let version_str = "v1" end)

module V2 = V (struct let version_str = "v2" end)

module V3 = struct
  include V (struct let version_str = "v3" end)

  let rolls_history : (account_hash, (int64 * int32 * int32) list) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"rolls_history"
      ~output:Api_encoding.V3.rolls_history
      Path.(root // version_v // "rolls_history" /: arg_account_hash)

  let required_balance :
    (string, (int * int64 * int64 * int64 * int * int) list) service1 =
    service
      ~name:"required_balance"
      ~section:section_rights
      ~output:Api_encoding.V3.required_balance
      Path.(root // version_v // "required_balance" /: arg_account_hash)

  let bakings_history :
    (string, (cycle_baking list * cycle_rights list * cycle_baking list)) service1 =
    service
      ~params:pagination_params
      ~name:"bakings_history"
      ~section:section_website
      ~output:Api_encoding.V3.bakings_history
      Path.(root // version_v // "bakings_history" /: arg_account_hash)

  let endorsements_history :
    (string, (cycle_endorsement list * cycle_rights list * cycle_endorsement list)) service1 =
    service
      ~params:pagination_params
      ~name:"endorsements_history"
      ~section:section_website
      ~output:Api_encoding.V3.endorsements_history
      Path.(root // version_v // "endorsements_history" /: arg_account_hash)

  let last_baking_and_endorsement :
    (string, baking list * baking_endorsement list * int * int) service1 =
    service
      ~section:section_bakings
      ~name:"last_baking_and_endorsement"
      ~output:Api_encoding.V3.last_baking
      Path.(root // version_v // "last_baking_and_endorsement" /: arg_account_hash)

  let next_baking_and_endorsement : (string, int * int * int * int * string) service1 =
    service
      ~section:section_bakings
      ~name:"next_baking_and_endorsement"
      ~output:Api_encoding.V3.next_baking
      Path.(root // version_v // "next_baking_and_endorsement" /: arg_account_hash)

  let search_account : (string, (account_name * string) list) service1 =
    service
      ~name:"search_account"
      ~section:section_website
      ~output:Api_encoding.V3.account_search
      Path.(root // version_v // "search_account" /: arg_account_hash)

  let balance_history : (string, (int32 * balance) list) service1 =
    service
      ~section:section_balance_updates
      ~name:"balance_history"
      ~output:Api_encoding.V3.balance_history
      Path.(root // version_v // "balance_history" /: arg_account_hash)

  let balance_ranking : ((int * account_name * int64) list) service0 =
    service
      ~section:section_balance_updates
      ~name:"balance_ranking"
      ~params:(pagination_params @ cycle_params @ spendable_params)
      ~output:Api_encoding.V3.balance_ranking
      Path.(root // version_v // "balance_ranking")

  let cycle_all_rights : (string, int * int) service1 =
    service
      ~params:(cycle_params@prio_params)
      ~name:"cycle_all_rights"
      ~section:section_website
      ~output:Api_encoding.V3.nb_all_rights
      Path.(root // version_v // "cycle_all_rights" /: arg_account_hash)

  let cycle_rights : Data_types.rights list service0 =
    service
      ~params:(pagination_params @ search_filter_params @ future_params)
      ~name:"cycle_rights"
      ~section:section_rights
      ~output:Api_encoding.V3.rights
      Path.(root // version_v // "baking_rights")

  let rewards_split : (account_hash, rewards_split) service1 =
    service
      ~params:(pagination_params @ cycle_params)
      ~name:"rewards_split"
      ~section:section_rewards
      ~output:Api_encoding.V3.rewards_split
      Path.(root // version_v // "rewards_split" /: arg_account_hash)

  let account_from_alias : (string, account_hash option) service1 =
    service
      ~section:section_accounts
      ~name:"account_from_alias"
      ~output:Json_encoding.(obj1 (opt "address" string))
      Path.(root // version_v // "account_from_alias" /: arg_alias)

end




module Node = struct

  let head = EzAPI.post_service
      ~input:Json_encoding.empty
      ~output:Tezos_encoding.Encoding.Block.encoding
      EzAPI.Path.(root // "chains" // "main" // "blocks" // "head")

  let peers = EzAPI.post_service
      ~input:Json_encoding.empty
      ~output:Tezos_encoding.Encoding.Network.encoding
      EzAPI.Path.(root // "network" // "peers" )

end
