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

open Data_types
open Tezos_types

type block_selector =
  Hash of block_hash
| Level of int
type hash_selector =
  Block of block_hash
| Account of account_hash
| Pending
| Empty

module type MONAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  type in_channel
  type out_channel
  val open_connection : Unix.sockaddr -> (in_channel * out_channel) t
  val output_char : out_channel -> char -> unit t
  val output_binary_int : out_channel -> int -> unit t
  val output_string : out_channel -> string -> unit t
  val flush : out_channel -> unit t
  val input_char : in_channel -> char t
  val input_binary_int : in_channel -> int t
  val really_input : in_channel -> Bytes.t -> int -> int -> unit t
  val close_in : in_channel -> unit t

  type 'a pool
  val pool_create : int ->
    ?validate:('a -> bool t) ->
    ?check:('a -> (bool -> unit) -> unit) ->
    ?dispose:('a -> unit t) ->
    (unit -> 'a t) -> 'a pool
  val pool_use : 'a pool -> ('a -> 'b t) -> 'b t

end

module Default_monad : MONAD with type 'a t = 'a

(* [filters] is a list of strings, among which "Transaction",
   "Faucet", "Activate",  "Activate_testnet", "Ballot",
   "Proposals", "Origination", "Nonce", "Delegation", "Endorsement" *)

module type READER = sig

  module Monad : MONAD

  (* [block_successor bh] returns the hash of the next block after [bh]
       in the blockchain. *)
  val block_successor : block_hash -> block_hash option Monad.t

  (* [level bh] returns the level of the given block *)
  val level : block_hash -> level option Monad.t

  val nonces : ?page:int -> ?page_size:int -> unit -> nonces list Monad.t

  (* [head ()] returns the block at the top of the current head *)
  val head : unit -> block option Monad.t

  (* [heads ?page ?page_size ()] returns the list of alternative heads *)
  val heads :
    ?page:int ->
    ?page_size:int ->
    ?level:int ->
    unit -> block list Monad.t
  val heads_with_pred_fitness:  ?page:int -> ?page_size:int -> ?level:int ->
    unit -> (block * string) list Monad.t

  val nb_heads : unit -> int Monad.t

  val nb_uncles :
    level:int ->
    unit -> int Monad.t

  (* [block ?operations selector] returns the block corresponding to
     the given [selector]. If [operations] is false, the field +
     operations is empty.*)
  val block: ?operations:bool -> block_selector -> block option Monad.t
  (* [blocks ?page ?page_size ?operations ()] returns the list of
     blocks of the corresponding page. If [page] is not present, it
     defaults to [0]. If [page_size] is not present, it defaults to [20].
     Blocks should be sorted from the most recent one to the least recent one.
  *)

  val blocks :
    ?page:int ->
    ?page_size:int ->
    ?operations:bool ->
    unit ->
    block list Monad.t
  val blocks_with_pred_fitness:  ?page:int -> ?page_size:int -> unit ->
    (block * string) list Monad.t

  val nb_snapshot_blocks: unit -> int Monad.t
  val snapshot_blocks: ?page:int -> ?page_size:int -> unit -> snapshot list Monad.t
  val snapshot_levels: unit -> int list Monad.t

  (* [nb_operations ?filters is_pending selector] returns the number of
     operations corresponding to the filters. *)
  val nb_operations :
    ?delegate:bool ->
    ?filters:string list ->
    hash_selector ->
    int Monad.t

  (* [operation hash] returns the operation corresponding to [hash] *)
  val operation : ?block_hash:block_hash ->
    operation_hash -> operation option Monad.t
  (* [operations ?filters ?page ?page_size selector].

       If [page_size] is not present, it defaults to [max_int].
  *)

  val operations :
    ?delegate:bool ->
    ?filters:string list ->
    ?page:int ->
    ?page_size:int ->
    hash_selector ->
    operation list Monad.t

  val estimate_gas : string -> int Monad.t
  val estimate_storage : string -> int Monad.t

  val nb_bakings : ?rights:bool -> ?cycle:int -> account_hash -> int Monad.t
  val bakings : ?page:int -> ?page_size:int -> ?cycle:int ->
    account_hash -> baking list Monad.t
  val nb_bakings_endorsement : ?cycle:int
    -> account_hash -> int Monad.t
  val bakings_endorsement : ?page:int -> ?page_size:int -> ?cycle:int ->
    account_hash -> baking_endorsement list Monad.t
  val nb_cycle_bakings : account_hash -> int Monad.t
  val cycle_bakings_sv : ?page:int -> ?page_size:int -> account_hash
    -> cycle_baking list Monad.t
  val nb_cycle_endorsements : account_hash -> int Monad.t
  val cycle_endorsements_sv : ?page:int -> ?page_size:int -> account_hash
    -> cycle_endorsement list Monad.t
  val nb_bakings_history : account_hash -> int Monad.t
  val bakings_history : ?page:int -> ?page_size:int -> account_hash ->
    (cycle_baking list * cycle_rights list * cycle_baking list) Monad.t
  val total_bakings : account_hash -> cycle_baking list Monad.t
  val nb_endorsements_history : account_hash -> int Monad.t
  val endorsements_history : ?page:int -> ?page_size:int -> account_hash ->
    (cycle_endorsement list * cycle_rights list * cycle_endorsement list) Monad.t
  val total_endorsements : account_hash -> cycle_endorsement list Monad.t

  val nb_baker_rights : ?cycle:int -> account_hash -> int Monad.t
  val baker_rights : ?cycle:int -> ?page: int -> ?page_size: int ->
    account_hash -> baker_rights list Monad.t
  val cycle_baker_rights : account_hash -> (cycle_rights list) Monad.t
  val cycle_all_rights : ?cycle:int -> ?prio:int -> account_hash -> (int * int) Monad.t

  val nb_endorser_rights : ?cycle:int -> account_hash -> int Monad.t
  val endorser_rights : ?cycle:int -> ?page: int -> ?page_size: int ->
    account_hash -> endorser_rights list Monad.t
  val cycle_endorser_rights : account_hash -> (cycle_rights list) Monad.t

  val nb_cycle_rights : ?future:bool -> ?filter:account_hash -> unit -> int Monad.t
  val cycle_rights : ?future:bool -> ?filter:account_hash ->
    ?page: int -> ?page_size: int -> unit -> rights list Monad.t

  val last_baking_and_endorsement : account_hash ->
    ( baking list * baking_endorsement list * int * int) Monad.t
  val next_baking_and_endorsement : account_hash -> (int * int * int * int * string) Monad.t

  val required_balance : account_hash ->
    (int *int64 * int64 * int64 * int * int) list Monad.t

  val endorsements : block_selector -> operation list Monad.t

  val account : hash_selector -> account option Monad.t
  val accounts : ?page:int -> ?page_size:int -> ?contract:bool -> unit -> account list Monad.t

  val nb_accounts : ?contract:bool -> unit -> int Monad.t
  val protocol : protocol_hash -> protocol option Monad.t

  val marketcap : unit -> marketcap Monad.t

  val nb_network_peers : ?state:string -> unit -> int Monad.t

  val network_stats :
    ?state:string ->
    ?page:int ->
    ?page_size:int ->
    unit ->
    network_stats list Monad.t
  val country_stats : ?state:string -> unit -> country_stats list Monad.t
  val baker_stats : account_hash -> baker_stats Monad.t
  val bakers_stats : ?cycle:int -> unit -> baker_stats list Monad.t
  val health_stats : int -> health_stats Monad.t
  val context_days : unit -> string list Monad.t
  val context_stats : CalendarLib.Calendar.t -> context_file_with_diff option Monad.t
  val h24_stats : unit -> h24_stats Monad.t
  val nb_tops : ?kind:string -> unit -> int Monad.t
  val tops : ?page:int -> ?page_size:int -> ?kind:string -> unit -> top_accounts Monad.t

  val account_bonds_rewards : account_hash -> account_bonds_rewards Monad.t
  val extra_bonds_rewards : account_hash -> account_extra_rewards Monad.t

  val max_roll_cycle : unit -> int Monad.t
  val rolls_distribution : int -> (account_name * int) list Monad.t

  val roll_number : account_hash -> int Monad.t
  val rolls_history :
    ?page:int ->
    ?page_size:int ->
    string ->
    (int64 * int32 * int32) list Monad.t

  val account_status : account_hash -> account_status Monad.t

  val deleguees_count : account_hash -> int Monad.t
  val deleguees :
    ?page:int ->
    ?page_size:int ->
    account_hash ->
    account_hash list Monad.t
  val deleguees_count_by_cycle_count : account_hash -> int Monad.t
  val deleguees_count_by_cycle :
    ?page:int ->
    ?page_size:int ->
    account_hash ->
    (int64 * int64) list Monad.t
  val all_deleguees_count_by_cycle_count :
    unit -> int Monad.t
  val all_deleguees_count_by_cycle :
    ?page:int ->
    ?page_size:int ->
    unit ->
    (int64 * int64) list Monad.t

  val nb_delegators : ?cycle:int -> account_hash -> int Monad.t
  val nb_cycle_rewards : ?only_future:bool -> account_hash -> int Monad.t

  val delegate_rewards_split_cycles :
    ?page:int ->
    ?page_size:int ->
    account_hash ->
    all_rewards_split list Monad.t
  val delegate_rewards_split :
    ?page:int ->
    ?page_size:int ->
    ?cycle:int ->
    account_hash ->
    rewards_split Monad.t
  val delegate_rewards_split_fast :
    ?page:int ->
    ?page_size:int ->
    ?cycle:int ->
    account_hash ->
    (account_name * int64) list Monad.t

  val nb_cycle_delegator_rewards : account_hash -> int Monad.t
  val delegator_rewards : ?page:int -> ?page_size:int ->
    account_hash -> delegator_reward list Monad.t
  val delegator_rewards_with_details : ?page:int -> ?page_size:int ->
    account_hash -> (delegator_reward * delegator_reward_details) list Monad.t

  val search_block : ?limit:int -> string -> string list Monad.t
  val search_operation : ?limit:int -> string -> string list Monad.t
  val search_account : ?limit:int -> string ->
    (account_name * string) list Monad.t
  val nb_search_block : string -> int Monad.t
  val nb_search_operation : string -> int Monad.t
  val nb_search_account : string -> int Monad.t


  val activated_balances : unit -> int64 Monad.t
  val supply : unit -> supply_info Monad.t
  val balance_break_down : account_hash -> balance_break_down Monad.t

  val crawler_activity : unit -> crawler_activity list Monad.t

  val volume_per_day : unit -> (int * int64) list Monad.t

  val alias : string -> string option Monad.t
  val account_from_alias : string -> string option Monad.t
  val all_aliases: unit -> (string * string) list Monad.t

  val nb_cycle : unit -> int Monad.t

  (*
  val get_bookmarks : emailhash:string -> bookmark list Monad.t
  val get_user : emailhash:string -> (string * string) list Monad.t
  val get_session_from_db : cookie:string -> (string * string * float) list Monad.t
  val check_email_pwd : emailhash:string -> pwhash:string -> int32 list Monad.t
*)

  val nb_protocol : unit -> int Monad.t
  val protocols : ?page:int -> ?page_size:int -> unit -> proto_details list Monad.t

  val market_prices: unit -> (string * (string * float) array) list Monad.t

  val nb_balance_updates :
    ?from:int -> ?up_to:int -> account_hash -> int Monad.t
  val balance_updates :
     ?page:int -> ?page_size:int -> ?from:int -> ?up_to:int -> account_hash -> balance_update_info list Monad.t
  val cycle_frozen : int -> account_hash -> balance Monad.t
  val active_balance_updates : int -> account_hash -> balance_update_info list Monad.t

  val balance : account_hash -> Int64.t Monad.t

  val balance_from_balance_updates :
    account_hash ->
    balance Monad.t

  val balance_history :
    account_hash ->
    (Int32.t * balance)
      list Monad.t

  val nb_balance : int32 -> int Monad.t

  val balance_ranking :
    ?page:int -> ?page_size:int -> Int32.t -> bool -> (int * account_name * Int64.t) list Monad.t

  val nb_exchange: unit -> int Monad.t
  val exchange_info: ?page:int -> ?page_size:int -> unit -> exchange_info list Monad.t

  val voting_period_info: ?period:int -> unit ->
    (int * voting_period_kind * int * int * bool * voting_period_status list * int) Monad.t
  val nb_proposals: ?period:int -> unit -> int Monad.t
  val proposals: ?period:int -> ?page:int -> ?page_size:int -> unit ->
    proposal list Monad.t
  val testing_proposal: int -> voting_period_kind -> proposal_hash Monad.t
  val ballots: int -> voting_period_kind
    -> (string * int * int * int * int * int * int) Monad.t
  val votes_account: ?page:int -> ?page_size:int -> account_hash -> proposal list Monad.t
  val vote_graphs_account: account_hash ->
    ((int * int * int) list * (int * int * int) list) Monad.t
  val nb_proposal_votes: ?period:int -> proposal_hash -> (int * int) Monad.t
  val proposal_votes: ?period:int -> ?page:int -> ?page_size:int ->
    proposal_hash -> proposal list Monad.t
  val total_proposal_votes: int -> (int * int * int * int * int * int * int) Monad.t
  val nb_ballot_votes: ?period:int -> ?ballot:string -> proposal_hash
    -> (int * int) Monad.t
  val ballot_votes: ?page:int -> ?page_size:int -> ?period:int -> ?ballot:string
    -> proposal_hash -> proposal list Monad.t
  val total_voters: int -> (int * int) Monad.t
  val quorum : int -> int Monad.t
end

module type READER_GENERIC = functor (M : MONAD) -> READER
  with module Monad = M
   and type 'a Monad.t = 'a M.t

module type WRITER = sig
  val head : unit -> block option

  val is_block_registered : block_hash -> bool

  val block_hash : int -> block_hash

  val register_tezos_user : account_hash -> unit

  val register_protocol : string -> unit

  val register_genesis : node_block -> unit

  val register_pending :
    CalendarLib.Calendar.t -> pending_operation_parsed list -> unit

  val register_init_balance :
    string -> int64 -> Date.t -> int -> unit

  val register_operations :
    node_block -> node_operation list -> unit

  val register_main_chain : bool -> node_block -> unit

  val register_all : node_block -> node_level -> node_operation list -> unit

  val register_network_stats : network_stats list -> unit

  val register_crawler_activity : string -> int -> unit

  val counts_downup : int -> int -> unit
end

module MakeDBReader : functor (DB : READER_GENERIC) -> READER_GENERIC
module MakeDBWriter : functor (DB : WRITER) -> WRITER
