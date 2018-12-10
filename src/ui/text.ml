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

open Lang

include Format_date.TEXT

let s_transactions = ss_ "Transactions"
let s_accounts = ss_ "Accounts"
let s_top_accounts = ss_ "Top Accounts"
let s_contracts = ss_ "Contracts"


let s_delegations = ss_ "Delegations"
let s_originations = ss_ "Originations"
let s_endorsements = ss_ "Endorsements"
let s_revelations = ss_ "Revelations"
let s_activations = ss_ "Activations"
let s_reveals = ss_ "Reveals"

let s_alternative_heads = ss_ "Alternative Heads"
let s_snapshot_blocks = ss_ "Snapshot Blocks"
let s_protocols = ss_ "Protocols"
let s_nbslots_miss = ss_ "#Slots (Miss)"

let s_bakers = ss_ "Bakers"
let s_baking_rights = ss_ "Baking Rights"
let s_endorsement_rights = ss_  "Endorsement Rights"
let s_passed_baking_rights = ss_ "Passed Baking Rights"
let s_nonces = ss_ "Nonces"
let s_bakings = ss_ "Bakings"
let s_rewards = ss_ "Rewards"
let s_code = ss_ "Code"
let s_michelson = ss_ "Michelson"
let s_liquidity = ss_ "Liquidity"
let s_storage = ss_ "Storage"
let s_baking_history = ss_ "Baking History"

let s_view_dapp = ss_ "View DApp"
let s_delegators = ss_ "Delegators"

let s_crawlers = ss_ "Crawlers"
let s_api_servers = ss_ "API Servers"
let s_node_servers = ss_ "Node Servers"
let s_tzscan_users = ss_ "TzScan Users"

let s_bookmarks = ss_ "Bookmarks"


let s_tzscan_tezos_nodes = ss_ "State of TzScan Tezos-nodes"
let s_tzscan_tezos_nodes_xs = ss_ "State of TzScan"
let s_tzscan_node_chain = ss_ "Node 1 (blocks)"
let s_tzscan_node_balances = ss_ "Node 2 (balances)"

let s_txns = ss_ "Txns"
let s_block = ss_ "Block"
let s_ico_ratio = ss_ "ICO ratio"
let s_total_supply = ss_ "Total Supply"

let s_price_usd = ss_"Price $"
let s_price_btc = ss_ "Price BTC"
let s_volume_24h = ss_ "Volume 24h"
let s_change_1h = ss_ "Change 1h"
let s_change_24h = ss_  "Change 24h"
let s_change_7d = ss_ "Change 7d"

let s_chain_stats = ss_ "Chain Stats"
let s_age = ss_ "Age"
let s_level = ss_ "Level"
let s_volume = ss_ "Volume"
let s_baker = ss_ "Baker"

let s_total_ico_supply = ss_ "Total ICO supply"

let s_blocks = ss_ "Blocks"
let s_ops = ss_ "Ops"
let s_fees = ss_ "Fees"

let s_last_24h = ss_ "Last 24h"
let s_endorsement_rate = ss_ "Endorsement Rate"
let s_block_prio_0_baked = ss_ "Block Prio. 0 Baked"
let s_last_snapshot = ss_ "Last Snapshot"
let s_staking_ratio = ss_ "Staking Ratio"
let s_roll_owners = ss_ "Roll Owners"

let s_tzscan_title = ss_ "TzScan: the Tezos Block Explorer by OCamlPro"
let s_go = ss_ "Go"
let s_search_by_address =
  ss_ "Search by address : Transaction / Block / Account / Level"

let s_nbops = ss_ "#Ops"

let s_first_priority = ss_ "First Priority"
let s_second_priority = ss_ "Second Priority"
let s_third_priority = ss_ "Third Priority"
let s_fourth_priority = ss_ "Fourth Priority"

let s_fitness = ss_ "Fitness"
let s_uncles = ss_ "Uncles"
let s_protocol = ss_ "Protocol"
let s_hash = ss_ "Hash"
let s_priority = ss_ "Priority"
let s_loading = ss_ "Loading"
let s_pending = ss_ "Pending"
let s_no_transaction = ss_ "No transaction"
let s_no = ss_ "No"
let s_failed = ss_ "Failed"
let s_yes = ss_ "Yes"
let s_bullshit = ss_ "--"
let s_rows = ss_ "Rows"
let s_account_hash_alias = ss_ "Account hash / alias"
let s_filter = ss_ "Filter"
let s_total_activated_balances = ss_ "Total activated balances"
let s_of_total_allocations = ss_ "of total allocations"
let s_cant_recover = ss_ "Can't recover"
let s_nonces_revelation_for_cycle = ss_ "Nonces Revelation for Cycle"
let s_manager = ss_ "Manager"
let s_spendable = ss_ "Spendable"
let s_delegate = ss_ "Delegate"
let s_balance = ss_ "Balance"

let s_number = ss_ "Number"
let s_start = ss_ "Start"
let s_end = ss_ "End"
let s_no_block_baked_for_this_cycle = ss_ "No block baked for this cycle"
let s_no_delegations = ss_ "No Delegation"
let s_no_delegators = ss_ "No Delegator"
let s_no_origination = ss_ "No Origination"
let s_no_endorsement = ss_ "No Endorsement"
let s_deposits = ss_ "Deposits"
let s_bake_time = ss_ "Bake Time"
let s_status = ss_ "Status"
let s_no_baking_rights_for_this_cycle = ss_ "No baking rights for this cycle"
let s_no_block_baked_and_no_baking_rights =
  ss_ "No block baked and no baking rights"
let s_total = ss_ "Total"
let s_delivered = ss_ "Delivered"
let s_cycle_detail = ss_ "Cycle Detail"
let s_at_snapshot_time = ss_ "At snapshot time"
let s_doesnot_include_services_fee = ss_ "Doesn't include services fee"
let s_doesnot_include_extras_nor_losses_nor_services_fee =
  ss_ "Doesn't include extras nor losses nor services fee"
let s_rewards_share = ss_ "Rewards Share"
let s_delegator = ss_ "Delegator"
let s_delegator_balance = ss_ "Delegator Balance"
let s_share = ss_ "Share"
let s_no_delegators_for_this_cycle = ss_ "No delegators for this cycle"
let s_delegate_rewards = ss_ "Delegate Rewards"
let s_staking_balance = ss_ "Staking Balance"
let s_blocks_rewards = ss_ "Blocks Rewards"
let s_endorsement_rewards = ss_ "End. Rewards"
let s_denounciation_rewards = ss_ "Denoun. Rewards"
let s_losses = ss_ "Losses"
let s_nbdel = ss_ "#Del."
let s_cycle = ss_ "Cycle"
let s_no_data = ss_ "No Data"
let s_in_progress = ss_ "In Progress"
let s_rew_pending = ss_ "Rew. pending"
let s_rew_delivered = ss_ "Rew. delivered"
let s_delegator_rewards = ss_ "Delegator Rewards"
let s_no_code = ss_ "No Code"
let s_false = ss_ "False"
let s_evaluated_balance = ss_ "Evaluated Balance"
let s_baking = ss_ "Baking"
let s_endorsement = ss_ "Endorsement"
let s_pending_rewards = ss_ "Pending Rewards"
let s_current_cycle = ss_ "Current Cycle"
let s_tokens_required = ss_ "XTZ Required"
let s_grace_period = ss_ "Grace Period"
let s_current_deposits = ss_ "Current Deposits"
let s_no_block_endorsed_for_this_cycle = ss_ "No block endorsed for this cycle"
let s_txn_hash = ss_ "Txn Hash"
let s_date = ss_ "Date"
let s_to = ss_ "To"
let s_from = ss_ "From"
let s_amount = ss_ "Amount"
let s_fee = ss_ "Fee"
let s_param = ss_ "Param"
let s_del_hash = ss_ "Del. Hash"
let s_account = ss_ "Account"
let s_operation_hash = ss_  "Operation Hash"
let s_end_hash = ss_ "End. Hash"
let s_new_balance = ss_ "New Balance"
let s_originator = ss_ "Originator"
let s_burn = ss_ "Burn"
let s_endorser = ss_ "Endorser"
let s_slots = ss_ "Slots"
let s_eta = ss_ "ETA"
let s_missed = ss_ "Missed"
let s_good = ss_ "Good"
let s_alternative_branch = ss_ "Alt."
let s_nbslots = ss_ "#Slots"
let s_av_priority = ss_ "Av. Priority"
let s_tot_rewards = ss_ "Tot. Rewards"
let s_tot_deposits = ss_ "Tot. Deposits"
let s_rew_status = ss_ "Rew. Status"
let s_endorsement_history = ss_ "Endorsement History"
let s_doesnot_include_service_fee = ss_ "Doesn't include services fee"
let s_fetching_data = ss_ "Fetching Data"
let s_included_in_block = ss_  "Included in Block"
let s_success = ss_ "Success"
let s_timestamp = ss_ "Timestamp"
let s_blocks_confirmation = ss_ "Blocks Confirmation"
let s_market_cap = ss_ "Market Cap"
let s_seed_nonce_revelation = ss_ "Seed Nonce Revelation"
let s_nonce = ss_ "Nonce"
let s_tz1 = ss_ "TZ1"
let s_secret = ss_ "Secret"
let s_activation = ss_ "Activation"
let s_view_all = ss_ "View All"
let s_endorsed_block = ss_ "Endorsed Block"
let s_fail = ss_ "Fail"
let s_endorsed_level = ss_ "Endorsed Level"
let s_empty = ss_ ""
let s_source = ss_ "Source"
let s_destination = ss_ "Destination"
let s_voting_period = ss_ "Voting Period"
let s_roll_owner = ss_ "Roll Owner"
let s_percent = ss_ "Percent"
let s_rolls = ss_ "Rolls"
let s_block_information = ss_ "Block Information"
let s_protocol_hash = ss_ "Protocol Hash"
let s_vote = ss_ "Vote"
let s_peer = ss_ "Peer"
let s_point = ss_ "Point"
let s_country = ss_ "Country"
let s_trusted = ss_ "trusted"
let s_score = ss_ "Score"
let s_state = ss_ "State"
let s_total_sent = ss_ "Total Sent"
let s_total_recv = ss_ "Total Recv"
let s_inflow = ss_ "Inflow"
let s_outflow = ss_ "Outflow"
let s_accepted = ss_ "Accepted"
let s_running = ss_ "Running"
let s_disconnected = ss_ "Disconnected"
let s_legend_runs_nodes = ss_ "[[title]] runs [[customData]] nodes"
let s_all = ss_ "All"
let s_double_baking_at_level = ss_ "Double Baking at level"
let s_baker_rewards = ss_ "Baker Rewards"
let s_network_stats = ss_ "Network Stats"
let s_peers = ss_ "Peers"
let s_connected = ss_ "Connected"
let s_others = ss_ "Others"
let s_rolls_distribution = ss_  "Rolls Distribution"
let s_counter = ss_ "Counter"
let s_gas_limit = ss_ "Gas Limit"
let s_storage_limit = ss_ "Storage Limit"
let s_parameter = ss_ "Parameter"
let s_internal = ss_ "Internal"
let s_new_account = ss_ "New Account"
let s_originator_manager = ss_  "Originator's Manager"
let s_account_to_delegate = ss_ "Account To Delegate"
let s_new_delegate = ss_ "New Delegate"
let s_public_key = ss_ "Public Key"
let s_nbblocks = ss_ "#Blocks"
let s_miss_steal = ss_ "Miss/Steal"
let s_offender = ss_ "Offender"
let s_lost_deposit = ss_ "Lost Deposit"
let s_lost_rewards = ss_ "Lost Rewards"
let s_lost_fees = ss_ "Lost Fees"
let s_double_endorsement_evidence = ss_ "Double Endorsement Evidence"
let s_predecessor = ss_ "Predecessor"
let s_validation_pass = ss_ "Validation Pass"
let s_context = ss_ "Context"
let s_nonce_hash = ss_ "Nonce Hash"
let s_pow_nonce = ss_ "POW Nonce"
let s_double_baking_evidence = ss_ "Double Baking Evidence"
let s_signature = ss_ "Signature"
let s_block_hash = ss_ "Block Hash"
let s_no_delegate = ss_ "No Delegate"
let s_anonymous_operations = ss_ "Anonymous Operations"
let s_proposals = ss_ "Proposals"
let s_ballot = ss_ "Ballot"
let s_manager_operations = ss_ "Manager Operations"
let s_activate = ss_ "Activate"
let s_activate_testnet = ss_ "Activate Testnet"
let s_cycle_details = ss_ "Cycle Details"
let s_alt = ss_ "Alt."
let s_no_endorsement_rights_for_this_cycle =
  ss_ "No endorsement rights for this cycle"
let s_no_block_endorsed_and_no_endorsement_rights =
  ss_ "No block endorsed and no endorsement rights"
let s_sum_over_cycles_of = ss_
    "sum over cycles of (deposits - rewards - fees)"
let s_no_information_for_this_account = ss_ "No information for this account."
let s_open_in_try_liquidity = ss_ "Open in Try-Liquidity"
let s_michelson_storage = ss_ "Michelson storage"
let s_liquidity_storage = ss_ "Liquidity Storage"
let s_liquidity_storage_coming_soon = ss_ "<Liquidity Storage> Soon..."
let s_liquidity_code = ss_ "< Liquidity code > Soon..."
let s_txs_number = ss_ "Txs Number"
let s_originated = ss_ "Originated"
let s_delegatable = ss_ "Delegatable"
let s_delegate_status = ss_ "Delegate status"
let s_inactive = ss_ "Inactive"
let s_active = ss_ "Active"
let s_website = ss_ "Website"
let s_not_revealed = ss_ "Not Revealed"
let s_revealed = ss_ "Revealed"
let s_xtz_required = ss_ "XTZ Required"
let s_xtz = ss_ "XTZ"
let s_cumul = ss_ "cumul."
let s_account_details = ss_ "Account Details"
let s_nb_reqs = ss_ "Nb Reqs"
let s_last_req = ss_ "Last Req"
let s_name = ss_ "Name"
let s_service = ss_ "Service"
let s_nb_ok_requests = ss_ "Nb OK Requests"
let s_nb_ko_requests = ss_ "Nb KO Requests"
let s_average_duration_ms = ss_ "Average Duration (ms)"
let s_last_activty = ss_ "Last Activity"
let s_timings_of_api_server = ss_ "Timings of API Server"
let s_commit = ss_ "Commit"
let s_build = ss_ "Build"
let s_head = ss_ "Head"
let s_alias = ss_ "Alias"
let s_include_internal_operations = ss_ "Include Internal Operations"
let s_end_rewards = ss_ "End. Rewards"
let s_extra_rewards = ss_ "Extra Rewards"
let s_extras = ss_ "Extras"
let s_flags = ss_ "Flags"
let s_options = ss_ "Options"

let s_balance_ranking = ss_ "Balance ranking"
let s_icon_bake = ss_ "#x1f35e"
let s_balance_updates_last = ss_ "Balance updates (last 5 cycles)"
let s_diff = ss_ "Diff"
let s_type_of_update = ss_ "Type of update"
let s_frozen = ss_ "Frozen"
let s_frozen_balance_updates = ss_ "Frozen balance updates"
let s_bakes = ss_ "Bakes"
let s_reward_estimation = ss_ "Rewards estimation"
let s_fees_estimation = ss_ "Fees estimation"
let s_deposit_estimation = ss_ "Deposits estimation"
let s_frozen_balance_estimation = ss_ "Frozen balance estimation"
let s_balance_estimation = ss_ "Balance estimation"
let s_rewards_frozen = ss_ "Rewards (frozen)"
let s_fees_frozen = ss_ "Fees (frozen)"
let s_deposits_frozen = ss_ "Deposits (frozen)"
let s_frozen_balance = ss_ "Frozen balance"
let s_now = ss_ "Now"
let s_ranking = ss_ "Ranking"
let s_merged_charts = ss_ "Merged charts"
let s_splitted_charts = ss_ "Splitted charts"
let s_currency = ss_ "Currency"
let s_balance_evolution = ss_ "Balance evolution (in Tz)"
let s_estimated_at_the_end_of_cycle = ss_ "Estimated at the end of cycle"

let s_version = ss_ "Version"
let s_baker_version = ss_ "Baker Version"

let t_subst t f =
  let b = Buffer.create 100 in
  Buffer.add_substitute b f (t_ t);
  Buffer.contents b

let s_subst_block_cycle = ss_ "Block $level (Cycle $cycle)"
let s_subst_evaluated_balance = ss_
 "The evaluated balance is snapshotted during \
  the current cycle and will be used in $cycles cycles. \
  This is equal to $rolls rolls."

let s_balances = ss_ "Balances"
let s_frozen_balances = ss_ "Frozen balances"
let s_frozen_deposits = ss_ "Frozen deposits"
let s_frozen_rewards = ss_ "Frozen rewards"
let s_paid_bytes = ss_ "Paid bytes"
let s_staking_balances = ss_ "Staking balances"
let s_total_balances = ss_ "Total balances"
let s_total_delegated = ss_ "Total delegated"
let s_total_delegators = ss_ "Total delegators"
let s_total_frozen_fees = ss_ "Total frozen fees"
let s_used_bytes = ss_ "Used bytes"

let s_top_balances = ss_ "Top Balances"
let s_top_frozen_balances = ss_ "Top Frozen balances"
let s_top_frozen_deposits = ss_ "Top Frozen deposits"
let s_top_frozen_rewards = ss_ "Top Frozen rewards"
let s_top_paid_bytes = ss_ "Top Paid bytes"
let s_top_staking_balances = ss_ "Top Staking balances"
let s_top_total_balances = ss_ "Top Total balances"
let s_top_total_delegated = ss_ "Top Total delegated"
let s_top_total_delegators = ss_ "Top Total delegators"
let s_top_total_frozen_fees = ss_ "Top Total frozen fees"
let s_top_used_bytes = ss_ "Top Used bytes"

let s_exchanges = ss_ "Exchanges"
