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

open EzAPIServer

module MakeRegisterer(S: module type of Service.V1)(H:module type of Handler.V1) =
struct

  let register dir =
    dir
  (* Block services *)
  |> EzAPIServer.register S.block H.block
  |> register S.blocks H.blocks
  |> register S.heads H.heads
  |> register S.nb_heads H.nb_heads
  |> register S.nb_uncles H.nb_uncles
  |> register S.head H.head
  |> register S.nb_cycle H.nb_cycle
  |> register S.nb_snapshot_blocks H.nb_snapshot_blocks
  |> register S.snapshot_blocks H.snapshot_blocks
  |> register S.snapshot_levels H.snapshot_levels
  (* Account services *)
  |> register S.accounts H.accounts
  |> register S.nb_accounts H.nb_accounts
  |> register S.account_bonds_rewards H.bonds_rewards
  |> register S.extra_bonds_rewards H.extra_bonds_rewards
  |> register S.rolls_history H.rolls_history
  |> register S.account_status H.account_status
  (* Operation services *)
  |> register S.operation H.operation
  |> register S.operations H.operations
  |> register S.operations_bh H.operations_bh
  |> register S.nb_bakings H.nb_bakings
  |> register S.bakings H.bakings
  |> register S.nb_bakings_endorsement H.nb_bakings_endorsement
  |> register S.bakings_endorsement H.bakings_endorsement
  |> register S.nb_cycle_bakings H.nb_cycle_bakings
  |> register S.cycle_bakings H.cycle_bakings
  |> register S.nb_cycle_endorsements H.nb_cycle_endorsements
  |> register S.cycle_endorsements H.cycle_endorsements
  |> register S.nb_bakings_history H.nb_bakings_history
  |> register S.bakings_history H.bakings_history
  |> register S.nb_endorsements_history H.nb_endorsements_history
  |> register S.endorsements_history H.endorsements_history
  |> register S.nb_cycle_rights H.nb_cycle_rights
  |> register S.cycle_rights H.cycle_rights
  |> register S.nb_baker_rights H.nb_baker_rights
  |> register S.baker_rights H.baker_rights
  |> register S.cycle_all_rights H.cycle_all_rights
  |> register S.nb_endorser_rights H.nb_endorser_rights
  |> register S.endorser_rights H.endorser_rights
  |> register S.required_balance H.required_balance
  |> register S.nb_operations_hash H.nb_operations_hash
  |> register S.nb_operations H.nb_operations
  |> register S.endorsements_level H.endorsements_level
  (* Misc services *)
  |> register S.block_succ H.block_succ
  |> register S.block_hash_level H.block_hash_level
  |> register S.timestamp H.timestamp
  |> register S.level H.level
  |> register S.nb_network_peers H.nb_network_peers
  |> register S.volume H.volume
  |> register S.nonces H.nonces
  |> register S.marketcap H.marketcap
  |> register S.network_stats H.network_stats
  |> register S.country_stats H.country_stats
  |> register S.bakers H.bakers_stats
  |> register S.blocks_per_day H.blocks_per_day
  |> register S.bakers_per_day H.bakers_per_day
  |> register S.priorities_per_day H.priorities_per_day
  |> register S.operations_per_day H.operations_per_day
  |> register S.operations_per_block_per_day H.operations_per_block_per_day
  |> register S.fees_per_day H.fees_per_day
  |> register S.volume_per_day H.volume_per_day
  |> register S.version H.version
  |> register S.mini_stats H.mini_stats
  |> register S.health_stats H.health_stats
  |> register S.context_days H.context_days
  |> register S.context_stats H.context_stats
  |> register S.nb_tops H.nb_tops
  |> register S.tops H.tops
  |> register S.max_roll_cycle H.max_roll_cycle
  |> register S.rolls_distribution H.rolls_distribution
  |> register S.search_block H.search_block
  |> register S.search_operation H.search_operation
  |> register S.search_account H.search_account
  |> register S.nb_search_block H.nb_search_block
  |> register S.nb_search_operation H.nb_search_operation
  |> register S.nb_search_account H.nb_search_account
  |> register S.activated_balances H.activated_balances
  |> register S.supply H.supply
  |> register S.node_timestamps H.node_timestamps
  |> register S.node_account H.node_account
  |> register S.node_deactivated_grace_period H.node_deactivated_grace_period
  |> register S.node_delegated_contracts H.node_delegated_contracts
  |> register S.node_nb_delegated_contracts H.node_nb_delegated_contracts
  |> register S.node_staking_balance H.node_staking_balance
  |> register S.date H.date
  |> register S.nb_delegators H.nb_delegators
  |> register S.nb_cycle_rewards H.nb_cycle_rewards
  |> register S.rewards_split_cycles H.rewards_split_cycles
  |> register S.rewards_split H.rewards_split
  |> register S.rewards_split_fast H.rewards_split_fast
  |> register S.nb_cycle_delegator_rewards H.nb_cycle_delegator_rewards
  |> register S.delegator_rewards H.delegator_rewards
  |> register S.delegator_rewards_with_details H.delegator_rewards_with_details
  |> register S.h24_stats H.h24_stats
  |> register S.nb_protocol H.nb_protocol
  |> register S.protocols H.protocols
  |> register S.voting_period_info H.voting_period_info
  |> register S.nb_proposals H.nb_proposals
  |> register S.proposals H.proposals
  |> register S.testing_proposal H.testing_proposal
  |> register S.ballots H.ballots
  |> register S.votes_account H.votes_account
  |> register S.vote_graphs_account H.vote_graphs_account
  |> register S.nb_proposal_votes H.nb_proposal_votes
  |> register S.proposal_votes H.proposal_votes
  |> register S.total_proposal_votes H.total_proposal_votes
  |> register S.nb_ballot_votes H.nb_ballot_votes
  |> register S.ballot_votes H.ballot_votes
  |> register S.total_voters H.total_voters
  |> register S.transaction_account_csv H.transaction_account_csv
  |> register S.market_prices H.market_prices
  |> register S.api_server_info H.api_server_info
  |> register S.balance_updates_number H.balance_updates_number
  |> register S.balance_updates H.balance_updates
  |> register S.active_balance_updates H.active_balance_updates
  |> register S.cycle_frozen H.cycle_frozen
  |> register S.balance H.balance
  |> register S.balance_from_balance_updates H.balance_from_balance_updates
  |> register S.balance_history H.balance_history
  |> register S.balance_number H.balance_number
  |> register S.balance_ranking H.balance_ranking
  |> register S.last_baking_and_endorsement H.last_baking_and_endorsement
  |> register S.next_baking_and_endorsement H.next_baking_and_endorsement
  |> register S.blocks_with_pred_fitness H.blocks_with_pred_fitness
  |> register S.heads_with_pred_fitness H.heads_with_pred_fitness

  (* not used in website *)
  |> register S.roll_number H.roll_number
  |> register S.total_bakings H.total_bakings
  |> register S.total_endorsements H.total_endorsements
  |> register S.cycle_baker_rights H.cycle_baker_rights
  |> register S.cycle_endorser_rights H.cycle_endorser_rights
  |> register S.block_pred H.block_pred
  |> register S.priority H.priority
  |> register S.network H.network
  |> register S.block_level H.block_level
  |> register S.baker H.baker_stats
  |> register S.balance_break_down H.balance_break_down
  |> register S.alias H.alias
  |> register S.account_from_alias H.account_from_alias
end

module V1 = MakeRegisterer(Service.V1)(Handler.V1)
module V2 = MakeRegisterer(Service.V2)(Handler.V1)
module V3 = MakeRegisterer(Service.V3)(Handler.V1)

let services =
  EzAPIServer.empty |> V1.register |> V2.register |> V3.register
