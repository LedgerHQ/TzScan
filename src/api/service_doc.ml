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

let doc = [
  "block", {|
  Returns information about the block with hash `block_hash`. If the `operations`
  parameter is `true`, the field `operations` will be filled with a list of
  operations.
  |};
  "version", {|
  Returns the current version of the API server, when it was built and on
  which commit. Useful to add in bug reports.
  |};
  "head", {|
  Returns information about the block at the current head of the blockchain.
  |};
  "nb_heads", {|
  Returns the overall number of alternative heads seen by the nodes.
  |};
  "heads", {|
  Returns information about all the blocks at the head of alternative chains.
  The current head is supposed to be the one with the highest fitness.
  |};
  "genesis", {|
  Returns the genesis block, i.e. the block at level 0.
  |};
  "blocks", {|
  Returns all the blocks in the current chain. Parameters `p` and `number` are
  used for pagination (`p` is the number of the page, of size `number`).
  |};
  "block_next", {|
  Returns the list of hashes of the blocks after the block with hash `block_hash`.
  |};
  "block_prev", {|
  Returns information about the block before the block with hash `block_hash`.
  The request will fail on the genesis block.
  |};
  "nb_snapshot_blocks", {|
  Returns the overall number of snanpshotted blocks (normally the number of cycle)"
  |};
  "snapshot_blocks", {|
  Returns the snapshotted blocks in the current chain. Parameters `p` and `number` are
  used for pagination (`p` is the number of the page, of size `number`).
  |};
  "snapshot_levels", {|
  Returns the list of snapshotted levels.
  |};
  "nb_uncles", {|
  Returns the number of alternative blocks at some level.
  |};
  "nb_accounts", {|
  Returns the overall number of accounts or contracts depending on the boolean parameter `contract`.
  |};
  "accounts", {|
  Returns all the accounts or contracts depending on the boolean parameter `contract`.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`)
  |};
  "bonds_rewards", {|
  Returns the frozen rewards, fees and deposits of an account.
  |};
  "roll_number", {|
  Returns the number of rolls for an account for the last cycle (current cycle + 5).
  |};
  "rolls_history", {|
  Returns the number of rolls for an account and the total number of rolls by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "deleguees_count", {|
  Returns the number of snapshotted delegators of an account for the last cycle
  (current cycle + 5).
  |};
  "deleguees", {|
  Returns the snapshotted delegators of an account for the last cycle (current cycle + 5).
  |};
  "deleguees_count_by_cycle_count", {|
  Returns the number of cycles where the account has some snapshotted delegators".
  |};
  "deleguees_count_by_cycle", {|
  Returns the the number of delegators of an account by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "all_deleguees_count_by_cycle_count", {|
  Returns the number of cycles where any account has some snapshotted delegators."
  |};
  "all_deleguees_count_by_cycle", {|
  Returns the overall number of delegators by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_cycle_rewards", {|
  Returns the number of cycles where an account has some rolls
  |};
  "nb_delegators", {|
  Returns the number of delegators of an account accounted for the cycle given
  with the parameter `cycle`.
  |};
  "rewards_split_cycles", {|
  Returns a list of infomation for a delegate by cycle: staking balance,
  number of delegators, rewards, fees, etc.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "rewards_split", {|
  Returns a list of infomation for each delegator for the delegate for a cycle
  given by the parameter `cycle`.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "rewards_split_fast", {|
  Returns a list of (delegator, delegated balance) for the cycle given
  by the parameter `cycle`.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_cycle_delegator_rewards", {|
  Returns the number of cycles where a contract is delegated"
  |};
  "delegator_rewards", {|
  Returns a list of information for a delegator by cycle: delegate,
  delegate staking balance, delegated balance, rewards, etc.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "alias", {|
  Returns the alias of an account.
  |};
  "account_from_alias", {|
  Returns the account address associated with an alias.
  |};
  "operation", {|
  Returns information about the operation. The `block_hash` parameter can be used
  since an operation can be included in different blocks in different chains
  |};
  "operations_bh", {|
  Returns the list of operations (with their information) associated with a block.
  The required parameter `type` must be used to differentiate the type of operations
  (Transaction, Origination, Delegation, Activation, etc).
  The `delegate` boolean parameter can be used to differentiate originations as a
  delegate, manager or account.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_bakings", {|
  Returns the number of baked + missed blocks of an account for a cycle given by
  the parameter `cycle`.
  |};
  "bakings", {|
  Returns the list of baked + missed blocks with their information (priority, fees,
  time, timestamp, etc).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_bakings_endorsement", {|
  Returns the number of endorsed + missed blocks of an account for a cycle given by
  the parameter `cycle`.
  |};
  "bakings_endorsement", {|
  Returns the list of endorsed + missed blocks with their information (priority,
  slots, timestamp, etc).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_bakings_history", {|
  Returns the number of passed and future cycle (+1 for total row) where the account
  has some baking activity.
  |};
  "bakings_history", {|
  Returns a triplet of list of information about baking per cycle for an account:
  counts, rewards, deposits, fees, average priority, average bake time, etc... for
  future, total and passed cycles.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "total_bakings", {|
  Returns the cumulated information about baking for an account.
  |};
  "nb_endorsements_history", {|
  Returns the number of passed and future cycle (+1 for total row) where the account
  has some endorsement activity.
  |};
  "endorsements_history", {|
  Returns a triplet of list of information about endorsement per cycle for an account:
  counts, rewards, deposits, average priority, etc... for
  future, total and passed cycles.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "total_endorsements", {|
  Returns the cumulated information about endorsement for an account.
  |};
  "nb_cycle_rights", {|
  Returns the number of levels where the account has some baking rights (priority < 4).
  The boolean parameter `future` can be used to look at future or passed rights.
  |};
  "cycle_rights", {|
  Returns information about baking and endorsing rights at some level: endorsers, bakers,
  priorities. The boolean parameter `future` can be used to look at future or passed rights.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "nb_baker_rights", {|
  Returns the number of future levels where the account has some naking rights.
  The parameter `cycle` can be used to select a cycle.
  |};
  "baker_rights", {|
  Returns information about baking rights for future levels (priorities).
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "cycle_baker_rights", {|
  Returns a list of information about future baking rights by cycle: count, average
  priority, etc.
  |};
  "nb_endorser_rights", {|
  Returns the number of future levels where the account has some endorsing rights.
  The parameter `cycle` can be used to select a cycle.
  |};
  "endorser_rights", {|
  Returns information about future endorsing rights by cycle.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "cycle_endorser_rights", {|
  Returns a list of information about future endorsing rights by cycle: count,
  number of slots, etc.
  |};
  "required_balance", {|
  Returns a list for the future cycles (5 or 4 cycles) for an account of
  the required deposit amount for each cycle,
  the required cumulated amount with unfrozen rewards, deposits and fees deducted,
  the number of rolls of the account and the total number of rolls.
  |};
  "nb_operations_hash", {|
  Returns the number of operations for an account or a block.
  The required parameter `type` allows to differentiate by type of operation
  (Transaction, Origination, Delegation, Activation...) and the `delegate` boolean
  parameter allows to differentiate origination as a delegate or manager or account.
  |};
  "nb_operations", {|
  Returns the overall number of operations.
  The required parameter `type` allows to differentiate by type of operation
  (Transaction, Origination, Delegation, Activation...) and the `delegate` boolean
  parameter allows to differentiate origination as a delegate or manager or account.
  |};
  "endorsements_level", {|
  Returns the list of endorsements endorsing some level.
  |};
  "operations", {|
  Returns a list of operations.
  The required parameter `type` allows to differentiate by type of operation
  (Transaction, Origination, Delegation, Activation...) and the `delegate` boolean
  parameter allows to differentiate origination as a delegate or manager or account.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "timestamp", {|
  Returns the timestamp of a block.
  |};
  "level", {|
  Returns the level of a block.
  |};
  "network", {|
  Returns the chain id of a block.
  |};
  "priority", {|
  Returns the priority of a block that has been baked.
  |};
  "volume", {|
  Returns the total volume of operations of a block.
  |};
  "block_level", {|
  "Returns the block in the main chain at a given level"
  |};
  "block_hash_level", {|
  Returns the block hash of the block in the main chain at a given level."
  |};
  "nonces", {|
  Returns the list by cycle of a list of all nonce revelations in this cycle.
  |};
  "search_block", {|
  Returns a list of block hashes starting by the argument.
  |};
  "search_operation", {|
  Returns a list of operation hashes starting by the argument.
  |};
  "search_account", {|
  Returns a list of account hashes starting by the argument.
  |};
  "nb_search_block", {|
  Returns the number of block hashes starting by the argument limited by 20.
  |};
  "nb_search_operation", {|
  Returns the number of operation hashes starting by the argument limited by 20.
  |};
  "nb_search_account", {|
  Returns the number of account hashes starting by the argument limited by 20.
  |};
  "nb_cycle", {|
  Returns the number of cycles in the chain (current cycle + 1).
  |};
  "nb_protocol", {|
  Returns the number of different protocols in the chain.
  |};
  "protocols", {|
  Returns a list of protocols with their information.
  Parameters `p` and `number` are used for pagination (`p` is the number of the page, of size `number`).
  |};
  "mini_stats", {|
  Returns statistics on the last hours and days of the Network: number of
  blocks, operations, volume (kXTZ), fees (XTZ cents)
  |};
  "nb_balance_updates", {|
  Returns the number of balance updates of an account/contract from the cycle in argument.
  Without cycle, returns the number of balance updates.
  Only the 5 previous cycle balance updates are kept in memory.
  |};
  "balance_updates", {|
  Returns the balance updates of an account/contract from the cycle in argument.
  Without cycle, returns all the registered balance updates.
  Only the 5 previous cycle balance updates are kept in memory.
  |};
  "active_balance_updates", {|
  Returns the balance updates having an impact on the non frozen balance or that
  deletes frozen assets in the case of double baking/endorsements.
  |};
  "balance", {|
  Returns the balance of an account.
  |};
  "balance_from_balance_updates", {|
  Returns `(spend,frozen,rew,fees,deps)`, where `spend` is the current balance of the account,
  `frozen` is the frozen balance of an account, `rew` is the amount of frozen rewards, `fees` the amount
  of frozen fees and `deps` the number of deposits.
  |};
  "balance_history", {|
  Returns the history of balance from balance updates (see the documentation of `balance_from_balance_updates`)
  |};
  "cycle_frozen", {|
  Return the total of assets that has been frozen during a cycle.
  |};
  "nb_balance", {|
  Returns the number of registered balances
  |};
  "balance_ranking", {|
  Returns the ranking of assets owners. The `spendable` parameter
  specifies whether the ranking is made on the spendable balance (`spendable=true`) or on the frozen
  balance (`spendable=false`).
  |};
  "cycle_all_rights", {|
  Returns `(bakes,endors)` where `bakes` (`endors`) is the number of future bakes (future
  endorsments of a given cycle at a given priority.
  |};
  "last_baking_and_endorsement", {|
  Returns information about the last block baker and the last block endorsed
  by an account, with the last endorsing and baking rights
  |};
  "account_status", {|
  Returns if an account has been revealed or originated.
  |}
]
