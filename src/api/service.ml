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

let param_number =
  Param.int ~name:"page_size" ~descr:"Max number of replies" "number"
let param_operations =
  Param.bool ~name:"operations" ~descr:"Whether the field `operations` should be filled (`false` by default)." "operations"
let param_page =
  Param.int ~name:"page" ~descr:"Offset in number of pages" "p"
let param_contract =
  Param.bool ~name:"contract" ~descr:"Limit the request to contracts, i.e. accounts with code" "contract"
let param_type =
  Param.string ~name:"type of operation" ~descr:"Limit the request to a particular kind of operations (`Transaction`, etc.)" "type"
let param_status =
  Param.string ~name:"status of operation" ~descr:"TODO" "status"
let param_peers =
  Param.string ~name:"state of peers" ~descr:"TODO" "state"
let param_level =
  Param.string ~name:"level" ~descr:"Filtering by level" "level"
let param_cycle =
  Param.string ~name:"cycle" ~descr:"Filtering by cycle" "cycle"
let param_delegate =
  Param.bool ~name:"delegate" ~descr:"Originations as delegate" "delegate"
let param_search_filter =
  Param.string ~name:"filter" ~descr:"Filter search results" "filter"
let param_future =
  Param.bool ~name:"future" ~descr:"Get future rights" "future"
let param_kind =
  Param.string
    ~name:"kind of tops"
    ~descr:"Limit the request to a particular kind of top accounts (`Frozen_deposits`, etc.)"
    "kind"

let pagination_params = [ param_page; param_number ]
let number_params = [ param_number ]
let operations_params = [ param_operations ]
let contract_params = [ param_contract ]
let filters_params = [ param_type; param_status ]
let peers_params = [ param_peers ]
let bakings_params = [ param_status ]
let cycle_params = [ param_cycle ]
let delegate_params = [ param_delegate ]
let search_filter_params = [ param_search_filter ]
let future_params = [ param_future ]

let arg_block_hash =
  arg_string "block_hash",
  "BMVCUZCPwFMRD2SJkVMyVsVKW7bmx5PoVoEoN9TS1m9Hu2gdGpW"

let arg_account_hash =
  arg_string "account_hash",
  "tz1ey28xfyVvtPRPN9d43Wbf1vkPs868CGXM"

let arg_contract_hash =
  arg_string "contract_hash",
  "KT1ey28xfyVvtPRPN9d43Wbf1vkPs868CGXM"

let arg_op_hash =
  arg_string "op_hash",
  "oo9wLVpWkGAsmFTsFPv8DT1H9ArgUk98vDtc3YqYFncenYW9wEn"

let arg_hash =
  arg_string "hash",
  "BMVCUZCPwFMRD2SJkVMyVsVKW7bmx5PoVoEoN9TS1m9Hu2gdGpW"

let arg_period =
  arg_string "period",
  "minutes"

let arg_level =
  arg_int "level",
  12333

let arg_cycle =
  arg_int "cycle",
  123

let arg_node =
  arg_string "node",
  "balance"

let arg_alias =
  arg_string "alias",
  "alias"

let arg_day =
  arg_string "day",
  "2018-10-01"

module type VERSION = sig val version_str : string end

module V (V : VERSION) = struct
  let version = V.version_str
  let version_v = V.version_str



let section_server = EzAPI.section "Server Requests"
let section_stats = EzAPI.section "Stats Requests" (* Private *)
let section_blocks = EzAPI.section "Blocks Requests"
let section_block = EzAPI.section "Block Requests"
let section_accounts = EzAPI.section "Accounts Requests"
let section_operations = EzAPI.section "Operations Requests"
let section_level = EzAPI.section "Level Requests"
let section_search = EzAPI.section "Search Requests"
let section_node = EzAPI.section "Node Requests"
let section_protocol = EzAPI.section "Protocol Requests"

let sections = [
  section_server;
  section_blocks;
  section_block;
  section_operations;
  section_accounts;
  section_level;
  section_search;
  section_node;
  section_protocol;
  ]

let other_sections = [ section_stats ]

  (*   SECTION BLOCKS *)


  (* A single block identified by its hash *)
  let block : (string, Data_types.block) service1  =
    service
      ~section:section_blocks
      ~name:"block"
      ~params:operations_params
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version // "block" /: arg_block_hash)

  (* Shortcut to last block *)
  let head : Data_types.block service0 =
    service
      ~params:[]
      ~name:"head"
      ~section:section_blocks
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version // "head")

  (* Shortcut to fist block *)
  let genesis : Data_types.block service0 =
    service
      ~params:[]
      ~name:"genesis"
      ~section:section_blocks
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version // "genesis")

  (* A list of blocks *)
  let blocks : Data_types.block list service0 =
    service
      ~section:section_blocks
      ~name:"blocks"
      ~params: (operations_params@pagination_params)
      ~output:(Json_encoding.list Api_encoding.V1.Block.encoding)
      Path.(root // version // "blocks")

  let nb_snapshot_blocks : int service0 =
    service
      ~section:section_blocks
      ~name:"nb_snapshot_blocks"
      ~params:[]
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "nb_snapshot_blocks")

  let snapshot_blocks : Data_types.snapshot list service0 =
    service
      ~section:section_blocks
      ~name:"snapshot_blocks"
      ~params:pagination_params
      ~output:Api_encoding.V1.Snapshot.encoding
      Path.(root // version // "snapshot_blocks")

  let snapshot_levels : int list service0 =
    service
      ~section:section_blocks
      ~name:"snapshot_levels"
      ~params:[]
      ~output:Json_encoding.(list int)
      Path.(root // version // "snapshot_levels")

  (* Alternative heads *)
  let heads : Data_types.block list service0 =
    service
      ~section:section_blocks
      ~name:"heads"
      ~params:pagination_params
      ~output:(Json_encoding.list Api_encoding.V1.Block.encoding)
      Path.(root // version // "heads")

  let nb_heads : int service0 =
    service
      ~section:section_blocks
      ~name:"nb_heads"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "nb_heads")

  (* Count Alternative heads *)
  let nb_uncles : (int, int) service1 =
    service
      ~section:section_blocks
      ~name:"nb_uncles"
      ~params:pagination_params
      ~output: Json_encoding.(tup1 int)
      Path.(root // version // "nb_uncles" /: arg_level)


  (*    SECTION ACCOUNTS *)

  (* A list of accounts *)
  let accounts : Data_types.account list service0 =
    service
      ~name:"accounts"
      ~params:(pagination_params @ contract_params)
      ~section:section_accounts
      ~output:(Json_encoding.list Api_encoding.V1.Account.encoding)
      Path.(root // version // "accounts")

  (* The number of accounts *)
  let nb_accounts : int service0 =
    service
      ~section:section_accounts
      ~name:"nb_accounts"
      ~params: contract_params
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_accounts")

  let account_bonds_rewards : (string, Data_types.account_bonds_rewards) service1 =
    service
      ~params:[]
      ~section:section_accounts
      ~name:"bonds_rewards"
      ~output:Api_encoding.V1.Bonds_rewards.encoding
      Path.(root // version_v // "bonds_rewards" /: arg_account_hash)

  let max_roll_cycle : int service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"max_roll_cycle"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "max_roll_cycle")

  let rolls_distribution : (int, (account_name * int) list) service1 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"roll_distribution"
      ~output:Api_encoding.V1.Rolls_distribution.encoding
      Path.(root // version_v // "rolls_distribution" /: arg_cycle)

  (* tz1 -> roll_count *)
  let roll_number : (account_hash, int) service1 =
    service
      ~params:[]
      ~section:section_accounts
      ~name:"roll_number"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "roll_number" /: arg_account_hash)

  (* tz1 -> [ cycle, roll_count, total_roll_count ] *)
  let rolls_history : (account_hash, (int64 * int32 * int32) list) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"rolls_history"
      ~output:Json_encoding.(list (tup3 Api_encoding.int64 int32 int32))
      Path.(root // version_v // "rolls_history" /: arg_account_hash)

  (* tz1 -> [ hash, reveal option, origination option ] *)
  let account_status : (account_hash, Data_types.account_status) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"account_status"
      ~output:Api_encoding.V1.Account_status.encoding
      Path.(root // version_v // "account_status" /: arg_account_hash)

  (* delegate -> nbr_deleguees *)
  let deleguees_count : (account_hash, int) service1 =
    service
      ~params:[]
      ~section:section_accounts
      ~name:"deleguees_count"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "deleguees_count" /: arg_account_hash)

  (* delegate -> [ deleguee ] *)
  let deleguees : (account_hash, account_hash list) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"deleguees"
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "deleguees" /: arg_account_hash)

  (* delegate -> nbr_cycle *)
  let deleguees_count_by_cycle_count : (account_hash, int) service1 =
    service
      ~params:[]
      ~section:section_accounts
      ~name:"deleguees_count_by_cycle_count"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "deleguees_count_by_cycle_count" /: arg_account_hash)

  (* delegate -> [ cycle, nbr_deleguees ] *)
  let deleguees_count_by_cycle : (account_hash, (int64 * int64) list) service1 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"deleguees_count_by_cycle"
      ~output:Json_encoding.(list (tup2 Api_encoding.int64 Api_encoding.int64))
      Path.(root // version_v // "deleguees_count_by_cycle" /: arg_account_hash)

  (* unit -> nbr_cycle *)
  let all_deleguees_count_by_cycle_count : int service0 =
    service
      ~params:[]
      ~section:section_accounts
      ~name:"all_deleguees_count_by_cycle_count"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "all_deleguees_count_by_cycle_count")

  (* unit -> [ cycle, nbr_all_deleguees ] *)
  let all_deleguees_count_by_cycle : (int64 * int64) list service0 =
    service
      ~params:pagination_params
      ~section:section_accounts
      ~name:"all_deleguees_count_by_cycle"
      ~output:Json_encoding.(list (tup2 Api_encoding.int64 Api_encoding.int64))
      Path.(root // version_v // "all_deleguees_count_by_cycle")

  (* Number of cycles where the account was (or will be) involved in the baking
     process (meaning. at least one endorsement / block baked or rights) *)
  let nb_cycle_rewards : (account_hash, int) service1 =
    service
      ~params: []
      ~name:"nb_cycle_rewards"
      ~section: section_accounts
      ~output: Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_cycle_rewards" /: arg_hash)

  let nb_delegators : (account_hash, int) service1 =
    service
      ~params: cycle_params
      ~name:"nb_delegators"
      ~section: section_accounts
      ~output: Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_delegators" /: arg_hash)

  let rewards_split_cycles : (account_hash, all_rewards_split list) service1 =
    service
      ~params:pagination_params
      ~name:"rewards_split_cycles"
      ~section: section_accounts
      ~output: (Json_encoding.list Api_encoding.V1.Rewards_split.all_encoding)
      Path.(root // version_v // "rewards_split_cycles" /: arg_hash)

  let rewards_split : (account_hash, rewards_split) service1 =
    service
      ~params: (pagination_params @ cycle_params)
      ~name:"rewards_split"
      ~section: section_accounts
      ~output:Api_encoding.V1.Rewards_split.encoding
      Path.(root // version_v // "rewards_split" /: arg_hash)

  let rewards_split_fast : (account_hash, (account_name * int64) list) service1 =
    service
      ~params: (pagination_params @ cycle_params)
      ~name:"rewards_split_fast"
      ~section: section_accounts
      ~output:Json_encoding.(
          list (tup2 Api_encoding.account_name_encoding Api_encoding.tez))
      Path.(root // version_v // "rewards_split_fast" /: arg_hash)

  let nb_cycle_delegator_rewards : (account_hash, int) service1 =
    service
      ~params: []
      ~name:"nb_cycle_delegator_rewards"
      ~section: section_accounts
      ~output: Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_cycle_delegator_rewards" /: arg_hash)

  let delegator_rewards : (account_hash, delegator_reward list) service1 =
    service
      ~params:pagination_params
      ~name:"delegator_rewards"
      ~section:section_accounts
      ~output:Api_encoding.V1.Rewards_split.delegator_encodings
      Path.(root // version_v // "delegator_rewards" /: arg_hash)

  let rewards_stats : (account_hash, rewards_stats) service1 =
    service
      ~params:cycle_params
      ~name:"rewards_stats"
      ~section: section_accounts
      ~output: Api_encoding.V1.Rewards_stats.encoding
      Path.(root // version_v // "rewards_stats" /: arg_hash)

  (*   SECTION OPERATIONS *)

  let alias : (account_hash, string option) service1 =
    service
      ~params:[]
      ~section:section_accounts
      ~name:"alias"
      ~output:Json_encoding.(tup1 (option string))
      Path.(root // version_v // "alias" /: arg_account_hash)

  let account_from_alias : (string, account_hash option) service1 =
     service
      ~params:[]
      ~section:section_accounts
      ~name:"accoutn_from_alias"
      ~output:Json_encoding.(tup1 (option string))
      Path.(root // version_v // "account_from_alias" /: arg_alias)


  (*   SECTION OPERATIONS *)


  (* A single operation *)
  let operation : (string, Data_types.operation) service1 =
    service
      ~params: []
      ~name:"operation"
      ~section: section_operations
      ~output:Api_encoding.V1.Operation.operation
      Path.(root // version // "operation" /: arg_op_hash)

  (* A list of operations *)
  let operations_bh : (string, Data_types.operation list) service1 =
    service
      ~params:(filters_params @ pagination_params @ delegate_params)
      ~section: section_operations
      ~name:"operations_bh"
      ~output:(Json_encoding.list Api_encoding.V1.Operation.operation)
      Path.(root // version // "operations" /: arg_block_hash)

  (* List of baked blocks for a given account *)
  let bakings : (string, Data_types.baking list) service1 =
    service
      ~params:(bakings_params @ cycle_params @ pagination_params)
      ~name:"bakings"
      ~section: section_operations
      ~output:Api_encoding.V1.BakeOp.bakings
      Path.(root // version // "bakings" /: arg_account_hash)

  (* The number of baked blocks for an account *)
  let nb_bakings : (string, int) service1 =
    service
      ~params:(bakings_params @ cycle_params)
      ~name:"nb_bakings"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_bakings" /: arg_account_hash)

  let nb_bakings_endorsement : (string, int) service1 =
    service
      ~params:(bakings_params @ cycle_params)
      ~name:"nb_bakings_endorsement"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_bakings_endorsement" /: arg_account_hash)

  let bakings_endorsement : (string, Data_types.baking_endorsement list) service1 =
    service
      ~params:(bakings_params @ cycle_params @ pagination_params)
      ~name:"bakings_endorsement"
      ~section: section_operations
      ~output:Api_encoding.V1.BakeEndorsementOp.bakings
      Path.(root // version // "bakings_endorsement" /: arg_account_hash)

  let nb_bakings_history : (string, int) service1 =
    service
      ~params:bakings_params
      ~name:"nb_bakings_history"
      ~section:section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_bakings_history" /: arg_account_hash)

  let bakings_history :
    (string, Data_types.cycle_baking list * Data_types.cycle_rights list *
             Data_types.cycle_baking list) service1 =
    service
      ~params:(bakings_params @ pagination_params)
      ~name:"bakings_history"
      ~section:section_operations
      ~output:(Json_encoding.tup3
                 Api_encoding.V1.CycleBakeOp.bakings
                 Api_encoding.V1.CycleRights.rights
                 Api_encoding.V1.CycleBakeOp.bakings)
      Path.(root // version // "bakings_history" /: arg_account_hash)

  let total_bakings : (string, Data_types.cycle_baking list) service1 =
    service
      ~params:bakings_params
      ~name:"total_bakings"
      ~section:section_operations
      ~output:Api_encoding.V1.CycleBakeOp.bakings
      Path.(root // version // "total_bakings" /: arg_account_hash)

  let nb_endorsements_history : (string, int) service1 =
    service
      ~params:bakings_params
      ~name:"nb_endorsements_history"
      ~section:section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_endorsements_history" /: arg_account_hash)

  let endorsements_history :
    (string, Data_types.cycle_endorsement list * Data_types.cycle_rights list *
             Data_types.cycle_endorsement list) service1 =
    service
      ~params:(bakings_params @ pagination_params)
      ~name:"endorsements_history"
      ~section:section_operations
      ~output:(Json_encoding.tup3
                 Api_encoding.V1.CycleEndorsementOp.bakings
                 Api_encoding.V1.CycleRights.rights
                 Api_encoding.V1.CycleEndorsementOp.bakings)
      Path.(root // version // "endorsements_history" /: arg_account_hash)

  let total_endorsements : (string, Data_types.cycle_endorsement list) service1 =
    service
      ~params:bakings_params
      ~name:"total_endorsements"
      ~section:section_operations
      ~output:Api_encoding.V1.CycleEndorsementOp.bakings
      Path.(root // version // "total_endorsements" /: arg_account_hash)

  let nb_cycle_rights : int service0 =
    service
      ~params:(search_filter_params @ future_params)
      ~name:"nb_baking_priorities"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_baking_rights")

  let cycle_rights : Data_types.rights list service0 =
    service
      ~params:(bakings_params @ pagination_params @ search_filter_params @ future_params)
      ~name:"baking_priorities"
      ~section: section_operations
      ~output:(Json_encoding.list Api_encoding.V1.Rights.encoding)
      Path.(root // version // "baking_rights")

  let nb_baker_rights : (string, int) service1 =
    service
      ~params:(bakings_params @ cycle_params)
      ~name:"nb_pending_priorities_hash"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_baker_rights" /: arg_account_hash)

  let baker_rights : (string, Data_types.baker_rights list) service1 =
    service
      ~params:(bakings_params @ cycle_params @ pagination_params)
      ~name:"pending_priorities"
      ~section: section_operations
      ~output:Api_encoding.V1.BakerRights.rights
      Path.(root // version // "baker_rights" /: arg_account_hash)

   let cycle_baker_rights : (string, Data_types.cycle_rights list) service1 =
    service
      ~params:[]
      ~name:"cycles_pending_priorities"
      ~section: section_operations
      ~output:Api_encoding.V1.CycleRights.rights
      Path.(root // version // "cycle_baker_rights" /: arg_account_hash)

  let nb_endorser_rights : (string, int) service1 =
    service
      ~params:(bakings_params @ cycle_params)
      ~name:"nb_pending_priorities_hash"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_endorser_rights" /: arg_account_hash)

  let endorser_rights : (string, Data_types.endorser_rights list) service1 =
    service
      ~params:(bakings_params @ cycle_params @ pagination_params)
      ~name:"pending_priorities"
      ~section: section_operations
      ~output:(Json_encoding.list Api_encoding.V1.EndorserRights.encoding)
      Path.(root // version // "endorser_rights" /: arg_account_hash)

  let cycle_endorser_rights : (string, Data_types.cycle_rights list) service1 =
    service
      ~params:[]
      ~name:"cycle_pending_priorities"
      ~section: section_operations
      ~output:Api_encoding.V1.CycleRights.rights
      Path.(root // version // "cycle_endorser_rights" /: arg_account_hash)

  let required_balance : (string, (int * int64 * int64 * int64 * int * int) list) service1 =
    service
      ~params: []
      ~name:"required_balance"
      ~section: section_accounts
      ~output:(Json_encoding.list
                 (Json_encoding.tup6 Json_encoding.int
                    Api_encoding.tez Api_encoding.tez Api_encoding.tez
                    Json_encoding.int Json_encoding.int))
      Path.(root // version // "required_balance" /: arg_hash)

  (* The number of operations for a block or an account *)
  let nb_operations_hash : (string, int) service1 =
    service
      ~params: (filters_params @ delegate_params)
      ~name:"nb_operations_hash"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_operations" /: arg_hash)

  (* The number of operations *)
  let nb_operations : int service0 =
    service
      ~params: filters_params
      ~name:"nb_operations"
      ~section: section_operations
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_operations")

  let endorsements_level : (int, Data_types.operation list) service1 =
    service
      ~params:[]
      ~section: section_operations
      ~name:"endorsements_level"
      ~output:(Json_encoding.list Api_encoding.V1.Operation.operation)
      Path.(root // version // "endorsements" /: arg_level)

  let operations : Data_types.operation list service0 =
    service
      ~params: (filters_params @ pagination_params)
      ~section: section_operations
      ~name:"operations"
      ~output:(Json_encoding.list Api_encoding.V1.Operation.operation)
      Path.(root // version // "operations")

      (* <--- not used ?
  let pending_operations : Data_types.operation list service0 =
    service
      ~output:(Json_encoding.list Api_encoding.Operation.API.api_operation)
      Path.(root // version // "pending_operations")

  let pending_operations_hash : (string, Data_types.operation list) service1 =
    service
      ~output:(Json_encoding.list Api_encoding.Operation.API.api_operation)
      Path.(root // version // "pending_operations" /: arg_string "hash")

  let pending_endorsements : (string, Data_types.operation list) service1 =
    service
      ~output:(Json_encoding.list Api_encoding.Operation.API.api_operation)
      Path.(root // version // "pending_endorsements" /: arg_string "hash")
       *)

  (*   SECTION BLOCK *)

  let block_succ : (string, block_hash) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"block_next"
      ~output:Json_encoding.(tup1 string)
      Path.(root // version // "block_next" /: arg_block_hash)

  let block_pred : (string, block_hash) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"block_prev"
      ~output:Json_encoding.(tup1 string)
      Path.(root // version // "block_prev" /: arg_block_hash)

  let timestamp : (string, timestamp) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"timestamp"
      ~output:Json_encoding.(tup1 string)
      Path.(root // version // "timestamp" /: arg_block_hash)

  let level : (string, Tezos_types.level) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"level"
      ~output:Api_encoding.V1.Level.encoding
      Path.(root // version // "level" /: arg_block_hash)

  let network : (string, network_hash) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"network"
      ~output:Json_encoding.(tup1 string)
      Path.(root // version // "network" /: arg_block_hash)

  let priority : (string, int) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"priority"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "priority" /: arg_block_hash)

  let volume : (string, int64) service1 =
    service
      ~params:[]
      ~section:section_block
      ~name:"volume"
      ~output:(Json_encoding.tup1 Api_encoding.tez)
      Path.(root // version // "volume" /: arg_block_hash)





  (*    SECTION LEVEL *)


  let block_level : (int, Data_types.block) service1 =
    service
      ~params:operations_params
      ~section:section_level
      ~name:"block_level"
      ~output:Api_encoding.V1.Block.encoding
      Path.(root // version // "block_level" /: arg_level)

  let block_hash_level : (int, block_hash) service1 =
    service
      ~params:[]
      ~section:section_level
      ~name:"block_hash_level"
      ~output:Json_encoding.(tup1 string)
      Path.(root // version // "block_hash_level" /: arg_level)

   let nonces : (Data_types.nonces list) service0 =
    service
      ~params:pagination_params
      ~section:section_level
      ~name:"nonces"
      ~output:(Json_encoding.list Api_encoding.V1.Nonce_hash.encoding)
      Path.(root // version // "nonces")


  (* SECTION STATS *)

  let marketcap : Data_types.marketcap service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"marketcap"
      ~output:Api_encoding.V1.MarketCap.encoding
      Path.(root // version // "marketcap" )

  (* The number of peers *)
  let nb_network_peers : int service0 =
    service
      ~params: peers_params
      ~section: section_stats
      ~name:"nb_network_peers"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version // "number_network_peers")

  let network_stats : Tezos_types.network_stats list service0 =
    service
      ~params: (pagination_params @ peers_params)
      ~section:section_stats
      ~name:"network_stats"
      ~output:Api_encoding.V1.Network.encoding
      Path.(root // version // "network" )

  let country_stats : Data_types.country_stats list service0 =
    service
      ~params: peers_params
      ~section:section_stats
      ~name:"country_stats"
      ~output:Api_encoding.V1.Network.country_stats_encoding
      Path.(root // version // "country" )

  let baker : (string, Data_types.baker_stats) service1 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"baker"
      ~output:Api_encoding.V1.Baker.encoding
      Path.(root // version // "baker_stats" /: arg_account_hash )

  let bakers : Data_types.baker_stats list service0 =
    service
      ~params:cycle_params
      ~section:section_stats
      ~name:"bakers"
      ~output:Api_encoding.V1.Baker.bakers_encoding
      Path.(root // version // "baker_stats" )

  let blocks_per_day : int Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"blocks_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version // "blocks_per_day" )

  let bakers_per_day : int Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"bakers_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version // "bakers_per_day" )

  let priorities_per_day : float Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"priorities_per_day"
      ~output:Api_encoding.V1.Charts.float_per_day_encoding
      Path.(root // version // "priorities_per_day" )

  let operations_per_day : int Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"operations_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version // "operations_per_day" )

  let operations_per_block_per_day : int Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"operations_per_block_per_day"
      ~output:Api_encoding.V1.Charts.int_per_day_encoding
      Path.(root // version // "operations_per_block_per_day" )

  let fees_per_day : int64 Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"fees_per_day"
      ~output:Api_encoding.V1.Charts.int64_per_day_encoding
      Path.(root // version // "fees_per_day" )

  let volume_per_day : int64 Data_types.per_day service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"volume_per_day"
      ~output:Api_encoding.V1.Charts.int64_per_day_encoding
      Path.(root // version // "volume_per_day" )

  let mini_stats : mini_stats service0 =
    service
      ~params:[]
      ~name:"mini_stats"
      ~section:section_server
      ~output:Api_encoding.V1.Charts.mini_stats
      Path.(root // version_v // "mini_stats" )

  let version : versions service0 =
    service
      ~params:[]
      ~section:section_server
      ~name:"version"
      ~output:Api_encoding.V1.Server.versions
      Path.(root // version_v // "version" )

  let health_stats : (int, Data_types.health_stats) service1 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"health"
      ~output:Api_encoding.V1.Health.encoding
      Path.(root // version_v // "health_stats" /: arg_cycle)

  let context_days : (string list) service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"context_days"
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "context_days")

  let context_stats : (string, Data_types.context_file_with_diff) service1 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"context_stats"
      ~output:Api_encoding.Context_stats.context_with_diff_encoding
      Path.(root // version_v // "context_stats" /: arg_day)

  let nb_tops : int service0 =
    service
      ~params:[ param_kind ]
      ~section:section_stats
      ~name:"nb_tops"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_tops")

  let tops : (string * int64) list service0 =
    service
      ~params: (param_kind :: pagination_params)
      ~section:section_stats
      ~name:"tops"
      ~output:Json_encoding.(list (tup2 string Api_encoding.int64))
      Path.(root // version_v // "tops")

  let h24_stats : Data_types.h24_stats service0 =
    service
      ~params:[]
      ~section:section_stats
      ~name:"24h"
      ~output:Api_encoding.V1.H24_stats.encoding
      Path.(root // version_v // "24h_stats")

  (*   SECTION SEARCH   *)

  let search_block : (string, string list) service1 =
    service
      ~params: []
      ~name:"search_block"
      ~section: section_search
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "search_block" /: arg_block_hash)

  let search_operation : (string, string list) service1 =
    service
      ~params: []
      ~name:"search_operation"
      ~section: section_search
      ~output:Json_encoding.(list string)
      Path.(root // version_v // "search_operation" /: arg_op_hash)

  let search_account : (string, (account_name * string) list) service1 =
    service
      ~params: []
      ~name:"search_account"
      ~section: section_search
      ~output:Json_encoding.(list (tup2 Api_encoding.account_name_encoding string))
      Path.(root // version_v // "search_account" /: arg_account_hash)

  let nb_search_block : (string, int) service1 =
    service
      ~params: []
      ~name:"search"
      ~section: section_search
      ~output: Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_search_block" /: arg_block_hash)

  let nb_search_operation : (string, int) service1 =
    service
      ~params: []
      ~name:"search"
      ~section: section_search
      ~output: Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_search_operation" /: arg_op_hash)

  let nb_search_account : (string, int) service1 =
    service
      ~params: []
      ~name:"search"
      ~section: section_search
      ~output: Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_search_account" /: arg_account_hash)

  let supply : supply_info service0 =
    service
      ~params: []
      ~name:"supply"
      ~section: section_stats
      ~output: Api_encoding.V1.Supply.encoding
      Path.(root // version_v // "supply")

  let balance_break_down : (string, balance_break_down) service1 =
    service
      ~params: []
      ~name:"balance_break_down"
      ~section: section_stats
      ~output: Api_encoding.V1.Supply.h_encoding
      Path.(root // version_v // "balance_break_down" /: arg_account_hash)

  let activated_balances : int64 service0 =
    service
      ~params: []
      ~name:"activated_balances"
      ~section: section_stats
      ~output: (Json_encoding.tup1 Api_encoding.int64)
      Path.(root // version_v // "activated_balances")

  (* SECTION NODE *)
  let node_timestamps: (string * string option) list service0 =
    service
      ~params:[]
      ~section: section_node
      ~name:"node_timestamps"
      ~output: Json_encoding.(list (tup2  string (option string)))
      Path.(root // version_v // "node_timestamps")

  let node_account: (string, account_details) service1 =
    service
      ~params:[]
      ~section: section_node
      ~name:"node_account"
      ~output:Api_encoding.V1.Account_details.encoding
      Path.(root // version_v // "node_account" /: arg_account_hash)

  let node_deactivated_grace_period: (string, bool * int) service1 =
    service
      ~params:[]
      ~section: section_node
      ~name:"node_is_deactivated"
      ~output: Json_encoding.(tup2 bool int)
      Path.(root // version_v // "node_deactivated_grace_period" /: arg_account_hash)

  let node_nb_delegated_contracts: (string, int) service1 =
    service
      ~params: []
      ~section: section_node
      ~name:"node_nb_delegated_contracts"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_delegated_contracts" /: arg_account_hash)

  let node_delegated_contracts: (string, string list) service1 =
    service
      ~params: pagination_params
      ~section: section_node
      ~name:"node_delegated_contracts"
      ~output: Json_encoding.(list string)
      Path.(root // version_v // "delegated_contracts" /: arg_account_hash)

  let node_staking_balance: (string, int64) service1 =
    service
      ~params:[]
      ~section: section_node
      ~name:"node_staking_balance"
      ~output: (Json_encoding.tup1 Api_encoding.tez)
      Path.(root // version_v // "staking_balance" /: arg_account_hash)

  let api_server_info: api_server_info service0 =
    service
      ~params:[]
      ~section: section_node
      ~name:"info"
      ~output: Api_encoding.V1.Server.api_server_info
      Path.(root // version_v // "info")

  let date: (float) service0 =
    service
      ~params:[]
      ~section: section_server
      ~name:"server"
      ~output: (Json_encoding.tup1 Json_encoding.float)
      Path.(root // version_v // "date")

  let nb_cycle: int service0 =
    service
      ~params:[]
      ~section:section_block
      ~name:"nb_cycle"
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_cycle")

  let nb_protocol : int service0 =
    service
      ~section:section_protocol
      ~name:"nb_protocol"
      ~params:[]
      ~output:Json_encoding.(tup1 int)
      Path.(root // version_v // "nb_protocol")

  let protocols : proto_details list service0 =
    service
      ~section:section_protocol
      ~name:"protocols"
      ~params:pagination_params
      ~output:Api_encoding.V1.Proto_details.encoding
      Path.(root // version_v // "protocols")

  let transaction_account_csv : (string, string) service1 =
    service
      ~output:Json_encoding.(tup1 string)
      Path.(root // version_v // "transaction_account_csv" /: arg_account_hash)

  let market_prices : (string * ( string * float ) array) array service0 =
    service
      ~section:section_stats
      ~name:"market_prices"
      ~output:Json_encoding.(array (tup2 string (array (tup2 string float))))
      Path.(root // version_v // "market_prices")
end

let init () = ()

module V1 = V (struct let version_str = "v1" end)

module V2 = V (struct let version_str = "v2" end)




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
