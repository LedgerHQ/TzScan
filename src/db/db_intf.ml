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

type block_selector = Hash of block_hash | Level of int
type hash_selector = Block of block_hash | Account of account_hash | Pending | Empty

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

module Default_monad = struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f =  f v
  let fail = raise
  let catch f fexn = try f () with e -> fexn e

  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel
  let open_connection = Unix.open_connection
  let output_char = output_char
  let output_binary_int = output_binary_int
  let output_string = output_string
  let flush = flush
  let input_char = input_char
  let input_binary_int = input_binary_int
  let really_input = really_input
  let close_in = close_in

  type 'a pool = 'a
  let pool_create _ ?validate ?check ?dispose f =
    let _,_,_ = check,validate,dispose in
    f ()
  let pool_use x f = f x
end

module type READER = sig

  module Monad : MONAD

  val block_successor : block_hash -> block_hash option Monad.t

  val level : block_hash -> level option Monad.t

  val nonces : ?page:int -> ?page_size:int -> unit -> nonces list Monad.t

  val head : unit -> block option Monad.t

  val heads :
    ?page:int ->
    ?page_size:int ->
    ?level:int ->
     unit -> block list Monad.t

  val nb_heads : unit -> int Monad.t

  val nb_uncles :
    level:int ->
    unit -> int Monad.t

  val block: ?operations:bool -> block_selector -> block option Monad.t
  val blocks :
    ?page:int ->
    ?page_size:int ->
    ?operations:bool ->
    unit ->
    block list Monad.t

  val nb_snapshot_blocks: unit -> int Monad.t
  val snapshot_blocks: ?page:int -> ?page_size:int -> unit -> snapshot list Monad.t
  val snapshot_levels: unit -> int list Monad.t

  val nb_operations :
    ?delegate:bool ->
    ?filters:string list ->
    hash_selector ->
    int Monad.t

  val operation : operation_hash -> operation option Monad.t
  val operations :
    ?delegate:bool ->
    ?filters:string list ->
    ?page:int ->
    ?page_size:int ->
    hash_selector ->
    operation list Monad.t

  val nb_bakings : ?rights:bool -> ?cycle:int -> account_hash -> int Monad.t
  val bakings : ?page:int -> ?page_size:int -> ?cycle:int ->
    account_hash -> baking list Monad.t
  val nb_bakings_endorsement : ?cycle:int
    -> account_hash -> int Monad.t
  val bakings_endorsement : ?page:int -> ?page_size:int -> ?cycle:int ->
    account_hash -> baking_endorsement list Monad.t
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

  val nb_endorser_rights : ?cycle:int -> account_hash -> int Monad.t
  val endorser_rights : ?cycle:int -> ?page: int -> ?page_size: int ->
    account_hash -> endorser_rights list Monad.t
  val cycle_endorser_rights : account_hash -> (cycle_rights list) Monad.t

  val nb_cycle_rights : ?future:bool -> ?filter:account_hash -> unit -> int Monad.t
  val cycle_rights : ?future:bool -> ?filter:account_hash ->
    ?page: int -> ?page_size: int -> unit -> rights list Monad.t

  val required_balance : account_hash ->
    (int * int64 * int64 * int64 * int * int) list Monad.t

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
  val tops : ?page:int -> ?page_size:int -> ?kind:string -> unit -> (string * int64) list Monad.t

  val account_bonds_rewards : account_hash -> account_bonds_rewards Monad.t

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

  val nb_delegators : account_hash -> int -> int Monad.t
  val nb_cycle_rewards : account_hash -> int Monad.t

  val delegate_rewards_split_cycles :
    ?page:int ->
    ?page_size:int ->
    account_hash ->
    all_rewards_split list Monad.t
  val delegate_rewards_split :
    ?page:int ->
    ?page_size:int ->
    account_hash ->
    int -> rewards_split Monad.t
  val delegate_rewards_split_fast :
    ?page:int ->
    ?page_size:int ->
    account_hash ->
    int -> (account_name * int64) list Monad.t

  val nb_cycle_delegator_rewards : account_hash -> int Monad.t
  val delegator_rewards : ?page:int -> ?page_size:int ->
    account_hash -> delegator_reward list Monad.t

  val rewards_stats :
    ?cycle:int ->
    account_hash ->
    rewards_stats Monad.t

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

  val register_operations :
    node_block -> node_operation list -> unit

  val register_main_chain : bool -> node_block -> unit

  val register_all : node_block -> node_level -> node_operation list -> unit

  val register_network_stats : network_stats list -> unit

  val register_crawler_activity : string -> int -> unit

  val counts_downup : int -> int -> unit

end

module MakeDBReader(DBG : READER_GENERIC) = functor (M : MONAD) -> struct
  module DB = DBG (M)
  include DB
end

module MakeDBWriter(DB : WRITER) = DB
