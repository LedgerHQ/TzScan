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

(* Format: (name, summary, desciption) *)
let doc = [

  (* blocks section *)
  "block", "Block", {|
  Returns information about the block with hash `block_hash`. If the `operations`
  parameter is `true`, the field `operations` will be filled with a list of
  operations.
  |};
  "head", "Head", {|
  Returns information about the block at the current head of the blockchain.
  |};
  "nb_heads", "Number of alternative blocks", {|
  Returns the overall number of alternative heads seen by the nodes.
  |};
  "heads", "Alternative blocks", {|
  Returns information about all the blocks at the head of alternative chains.
  The current head is supposed to be the one with the highest fitness.
  |};
  "genesis", "Genesis block", {|
  Returns the genesis block, i.e. the block at level 0.
  |};
  "blocks", "Blocks", {|
  Returns all the blocks in the current chain. Parameters `p` and `number` are
  used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_snapshot_blocks", "Number of snapshotted blocks", {|
  Returns the overall number of snanpshotted blocks (normally the number of cycle)"
  |};
  "snapshot_blocks", "Snapshotted blocks", {|
  Returns the snapshotted blocks in the current chain. Parameters `p` and `number` are
  used for pagination (`p` is the number of the page, of size `number`).
  |};
  "snapshot_levels", "Snapshotted levels", {|
  Returns the list of snapshotted levels.
  |};
  "nb_uncles", "Number of uncles", {|
  Returns the number of alternative blocks at some level.
  |};

  (* account section *)
  "nb_accounts", "Number of accounts", {|
  Returns the overall number of accounts or contracts depending on the boolean parameter `contract`.
  |};
  "accounts", "Accounts", {|
  Returns all the accounts or contracts depending on the boolean parameter `contract`.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`)
  |};
  "bonds_rewards", "Frozen rewards", {|
  Returns the frozen rewards, fees and deposits of an account.
  |};
  "extra_bonds_rewards", "Extra rewards/losses", {|
  Returns the frozen rewards/deposits/losses from denounciation and revelation.
  |};
  "roll_number", "Rolls", {|
  Returns the number of rolls for an account for the last cycle (current cycle + 5).
  |};
  "rolls_history", "Rolls by cycle", {|
  Returns the number of rolls for an account and the total number of rolls by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "account_from_alias", "Alias address", {|
  Returns the account address associated with an alias.
  |};
  "account_status", "Account status", {|
  Returns if an account has been revealed or originated.
  |};
  "votes_account", "Account votes", {|
  Returns the votes of an account (Proposals and Ballots)
  |};



  (* rewards section *)
  "nb_cycle_rewards", "Number of cycles with rolls", {|
  Returns the number of cycles where an account has some rolls
  |};
  "nb_delegators", "Number of delegators for a cycle", {|
  Returns the number of delegators of an account accounted for the cycle given
  with the parameter `cycle`.
  |};
  "rewards_split_cycles", "Delegate rewards", {|
  Returns a list of infomation for a delegate by cycle: staking balance,
  number of delegators, rewards, fees, etc.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "rewards_split", "Delegate info for a cycle", {|
  Returns a list of infomation for each delegator for the delegate for a cycle
  given by the parameter `cycle`.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "rewards_split_fast", "Delegators with balance", {|
  Returns a list of (delegator, delegated balance) for the cycle given
  by the parameter `cycle`.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_cycle_delegator_rewards", "Number of cycles as delegator", {|
  Returns the number of cycles where a contract is delegated"
  |};
  "delegator_rewards", "Delegator rewards", {|
  Returns a list of information for a delegator by cycle: delegate,
  delegate staking balance, delegated balance, rewards, etc.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "delegator_rewards_with_details", "Delegator rewards (detailed)", {|
  Returns a list of exhaustive information for a delegator by cycle: delegate,
  delegate staking balance, delegated balance, rewards, etc.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};

  (* operations section *)
  "operation", "Operation", {|
  Returns information about the operation. The `block_hash` parameter can be used
  since an operation can be included in different blocks in different chains
  |};
  "operations_bh", "Block/Account operations", {|
  Returns the list of operations (with their information) associated
  with a block or an account.
  The required parameter `type` must be used to differentiate the type of operations
  (Transaction, Origination, Delegation, Activation, etc).
  The `delegate` boolean parameter can be used to differentiate originations as a
  delegate, manager or account.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_operations_hash", "Number of operations for a block/account", {|
  Returns the number of operations for an account or a block.
  The required parameter `type` allows to differentiate by type of operation
  (Transaction, Origination, Delegation, Activation...) and the `delegate` boolean
  parameter allows to differentiate origination as a delegate or manager or account.
  |};
  "nb_operations", "Number of operations", {|
  Returns the overall number of operations.
  The required parameter `type` allows to differentiate by type of operation
  (Transaction, Origination, Delegation, Activation...) and the `delegate` boolean
  parameter allows to differentiate origination as a delegate or manager or account.
  |};
  "endorsements_level", "Level endorsements", {|
  Returns the list of endorsements endorsing some level.
  |};
  "operations", "Operations", {|
  Returns a list of operations.
  The required parameter `type` allows to differentiate by type of operation
  (Transaction, Origination, Delegation, Activation...) and the `delegate` boolean
  parameter allows to differentiate origination as a delegate or manager or account.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nonces", "Nonce revelations", {|
  Returns the list by cycle of a list of all nonce revelations in this cycle.
  |};

  (* bakings section *)
  "nb_bakings", "Number of bakings for a cycle", {|
  Returns the number of baked + missed blocks of an account for a cycle given by
  the parameter `cycle`.
  |};
  "bakings", "Bakings for a cycle", {|
  Returns the list of baked + missed blocks with their information (priority, fees,
  time, timestamp, etc).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_bakings_endorsement", "Number of endorsements for a cycle", {|
  Returns the number of endorsed + missed blocks of an account for a cycle given by
  the parameter `cycle`.
  |};
  "bakings_endorsement", "Endorsements for a cycle", {|
  Returns the list of endorsed + missed blocks with their information (priority,
  slots, timestamp, etc).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_cycle_bakings", "Number of cycles with bakings", {|
  Returns the number of cycles involving baked or missed blocks of an account.
  |};
  "cycle_bakings", "Cycles Bakings", {|
  Returns a summary of bakings by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_cycle_endorsements", "Number of cycles with endorsements", {|
  Returns the number of cycles involving endorsed or missed blocks of an account.
  |};
  "cycle_endorsements", "Cycles Endorsements", {|
  Returns a summary of endorsements by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "total_bakings", "Total bakings", {|
  Returns the cumulated information about bakings for an account.
  |};
  "total_endorsements", "Total endorsements", {|
  Returns the cumulated information about endorsement for an account.
  |};
  "last_baking_and_endorsement", "Last baking/endorsement", {|
  Returns information about the last block baker and the last block endorsed
  by an account, with the last endorsing and baking rights
  |};
  "next_baking_and_endorsement", "Next baking/endorsement", {|
  Returns information about the last block baker and the last block endorsed
  by an account, with the last endorsing and baking rights
  |};

  (* rights section *)
  "nb_cycle_rights", "Number of rights", {|
  Returns the number of levels where the account has some baking rights (priority < 4).
  The boolean parameter `future` can be used to look at future or passed rights.
  |};
  "cycle_rights", "Rights", {|
  Returns information about baking and endorsing rights at some level: endorsers, bakers,
  priorities. The boolean parameter `future` can be used to look at future or passed rights.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_baker_rights", "Number of baking rights", {|
  Returns the number of future levels where the account has some baking rights.
  The parameter `cycle` can be used to select a cycle.
  |};
  "baker_rights", "Baking rights", {|
  Returns information about baking rights for future levels (priorities).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "cycle_baker_rights", "Baking rights by cycle", {|
  Returns a list of information about future baking rights by cycle: count, average
  priority, etc.
  |};
  "nb_endorser_rights", "Number of endorsing rights", {|
  Returns the number of future levels where the account has some endorsing rights.
  The parameter `cycle` can be used to select a cycle.
  |};
  "endorser_rights", "Endorsing rights", {|
  Returns information about future endorsing rights by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "cycle_endorser_rights", "Endorsing rights by cycle", {|
  Returns a list of information about future endorsing rights by cycle: count,
  number of slots, etc.
  |};
  "required_balance", "Required deposits by cycle", {|
  Returns a list for the future cycles (5 or 4 cycles) for an account of
  the required deposit amount for each cycle,
  the required cumulated amount with unfrozen rewards, deposits and fees deducted,
  the number of rolls of the account and the total number of rolls.
  |};

  (* block section *)
  "block_next", "Following block", {|
  Returns the list of hashes of the blocks after the block with hash `block_hash`.
  |};
  "block_prev", "Previous block", {|
  Returns information about the block before the block with hash `block_hash`.
  The request will fail on the genesis block.
  |};
  "timestamp", "Block timestamp", {|
  Returns the timestamp of a block.
  |};
  "level", "Block level", {|
  Returns the level of a block.
  |};
  "network", "Block network", {|
  Returns the chain id of a block.
  |};
  "priority", "Block priority", {|
  Returns the priority of a block that has been baked.
  |};
  "volume", "Block volume", {|
  Returns the total volume of operations of a block.
  |};

  (* level section *)
  "block_level", "Main chain block", {|
  "Returns the block in the main chain at a given level"
  |};
  "block_hash_level", "Main chain block hash", {|
  Returns the block hash of the block in the main chain at a given level."
  |};

  (* search section *)
  "search_block", "Block search", {|
  Returns a list of block hashes starting by the argument.
  |};
  "search_operation", "Operation search", {|
  Returns a list of operation hashes starting by the argument.
  |};
  "search_account", "Account search", {|
  Returns a list of account hashes starting by the argument.
  |};
  "nb_search_block", "Number of blocks in a search", {|
  Returns the number of block hashes starting by the argument limited by 20.
  |};
  "nb_search_operation", "Number of operations in a search", {|
  Returns the number of operation hashes starting by the argument limited by 20.
  |};
  "nb_search_account", "Number of accounts in a search", {|
  Returns the number of account hashes starting by the argument limited by 20.
  |};
  "nb_cycle", "Number of Cycles", {|
  Returns the number of cycles in the chain (current cycle + 1).
  |};

  (* protocol section *)
  "nb_protocol", "Number of protocols", {|
  Returns the number of different protocols in the chain.
  |};
  "protocols", "Protocols", {|
  Returns a list of protocols with their information.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "proposals", "Proposals", {|
  Returns the list of for a voting period (`period` parameter) or all.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_proposals", "Number of proposals", {|
  Returns the number of proposals for a voting period (`period`) or all.
  |};
  "voting_period_info", "Period info", {|
  Returns information about the status in the voting period
  |};
  "testing_proposal", "Testing proposal", {|
  Returns the proposal being tested in the voting period
  |};
  "ballots", "Ballots", {|
  Returns the counts and the votes during a Ballot session.
  |};
  "proposal_votes", "Proposal votes", {|
  Returns the proposal votes for a voting period (parameter `period`).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_proposal_votes", "Number of proposal votes", {|
  Returns the number of proposal votes for a period (parameter `period`) or all.
  |};
  "total_proposal_votes", "Votes totals", {|
  Returns the number of totals and used counts/votes for a voting period
  |};
  "total_voters", "Voters totals", {|
  Returns the number of total voters and the total of rolls
  |};
  "ballot_votes", "Ballot votes", {|
  Returns the ballot votes for a proposal.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_ballot_votes", "Number of ballot votes", {|
  Returns the number of ballot votes for a proposal.
  |};

  (* stats section *)
  "mini_stats", "24h statistics", {|
  Returns statistics on the last hours and days of the Network: number of
  blocks, operations, volume (kXTZ), fees (XTZ cents)
  |};

  (* balances section *)
  "nb_balance_updates", "Number of balance updates", {|
  Returns the number of balance updates of an account/contract from the cycle in argument.
  Without cycle, returns the number of balance updates.
  Only the 5 previous cycle balance updates are kept in memory.
  |};
  "balance_updates", "Balance updates", {|
  Returns the balance updates of an account/contract from the cycle in argument.
  Without cycle, returns all the registered balance updates.
  Only the 5 previous cycle balance updates are kept in memory.
  |};
  "active_balance_updates", "Active balance updates", {|
  Returns the balance updates having an impact on the non frozen balance or that
  deletes frozen assets in the case of double baking/endorsements.
  |};
  "balance", "Balance from context", {|
  Returns the balance of an account.
  |};
  "balance_from_balance_updates", "Balance from balance updates", {|
  Returns `(spend,frozen,rew,fees,deps)`, where `spend` is the current balance of the account,
  `frozen` is the frozen balance of an account, `rew` is the amount of frozen rewards, `fees` the amount
  of frozen fees and `deps` the number of deposits.
  |};
  "balance_history", "History of balances", {|
  Returns the history of balance from balance updates (see the documentation of `balance_from_balance_updates`)
  |};
  "cycle_frozen", "Frozen assets for a cycle", {|
  Return the total of assets that has been frozen during a cycle.
  |};
  "nb_balance", "Number of balances", {|
  Returns the number of registered balances
  |};
  "balance_ranking", "Balance ranking", {|
  Returns the ranking of assets owners. The `spendable` parameter
  specifies whether the ranking is made on the spendable balance (`spendable=true`) or on the frozen
  balance (`spendable=false`).
  |};
]
