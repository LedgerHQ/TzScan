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

(* At some point, those values should be register in DB for each PROTOCOL
   and gather from there *)


type net =
  | Alphanet
  | Zeronet
  | Betanet

let is_next =  TzscanConfig.database = "next"

let net = match TzscanConfig.database with
  | "mainnet" -> Betanet
  | "betanet" -> Betanet
  | "zeronet" -> Zeronet
  | "alphanet" -> Alphanet
  | "beta" -> Betanet
  | "next" -> Betanet
  | "local" -> Betanet
  | _ -> assert false

let ramp_up_cycles = 64 (* IN BLOCK GENESIS *)

let cycle_deposits default_deposits cycle =
  if cycle >= ramp_up_cycles then default_deposits
  else
    Int64.(div (mul default_deposits (of_int cycle)) (of_int ramp_up_cycles))



open Tezos_types
let constants = {
  proof_of_work_nonce_size = 8;
  nonce_length = 32;
  max_revelations_per_block = 32;
  max_operation_data_length = 16384;
  preserved_cycles = 5;
  blocks_per_cycle = 4096;
  blocks_per_commitment = 32;
  blocks_per_roll_snapshot = 512;
  blocks_per_voting_period = 32768;
  time_between_blocks = [ 1; 1 ];
  endorsers_per_block = 32;
  hard_gas_limit_per_operation = 400000L;
  hard_gas_limit_per_block = 4000000L;
  proof_of_work_threshold = -1L;
  tokens_per_roll = 10000000000L;
  michelson_maximum_type_size = 1000;
  seed_nonce_revelation_tip = 125000L;
  origination_burn = 257000L;
  block_security_deposit = 512000000L;
  endorsement_security_deposit = 64000000L;
  block_reward = 16000000L;
  endorsement_reward = 2000000L;
  cost_per_byte = 1000L;
  hard_storage_limit_per_operation = 60000L
}

module type Constants = sig
  val block_per_cycle : int
  val tokens_per_roll : int64

  val allowed_fork : int
  val block_security_deposit : int64
  val endorsement_security_deposit : int64

  val blocks_per_roll_snapshot : int

  val block_reward : int64

  val endorsers_per_block : int

  val endorsement_reward_coeff : int64

  val tez_units : int64
  val cost_per_byte : int

  val origination_burn : int64

  val time_between_blocks : int
  val time_delay_for_priority : int -> int

  val blocks_between_revelations : int

  val revelation_reward : int64
end

module Alphanet = struct

  let tez_units = 1_000_000L (* mutez *)
  let cost_per_byte = 1_000 (* mutez *)
  let block_per_cycle = 2048     (* 2048 *)
  let tokens_per_roll = 10_000_000_000L

  let allowed_fork = 2          (* 6h = ~3 cycles *)
  let block_security_deposit = Int64.mul 512L  tez_units (* mutez *)
  let endorsement_security_deposit = Int64.mul 64L  tez_units (* mutez *)

  let blocks_per_roll_snapshot = 256

  let block_reward = Int64.mul 16L tez_units (* mutez *)

  let endorsers_per_block = 32

  let endorsement_reward_coeff = Int64.mul 2L tez_units (* mutez *)


  let origination_burn = 257_000L

  let blocks_between_revelations = 32

  let time_between_blocks = 30
  let time_delay_for_priority n =
    time_between_blocks + (n *  (time_between_blocks - 10))

  let revelation_reward = 125_000L
end

module Betanet = struct
  let tez_units = 1_000_000L (* mutez *)
  let cost_per_byte = 1_000 (* mutez *)

  let block_per_cycle = 4096     (* 2048 *)

  let tokens_per_roll = 10_000_000_000L

  let allowed_fork = 5          (* 6h = ~3 cycles *)
  let block_security_deposit = Int64.mul 512L tez_units (* mutez *)
  let endorsement_security_deposit = Int64.mul 64L tez_units (* mutez *)

  let blocks_per_roll_snapshot = 256

  let block_reward = Int64.mul 16L tez_units (* mutez *)

  let endorsers_per_block = 32

  let endorsement_reward_coeff = Int64.mul 2L tez_units (* mutez *)

  let origination_burn = 257_000L (* mutez *)


  let blocks_between_revelations = 32


  (* If this constant changes, you have to fix function in
     db/reader/cycle_bakings *)
  let time_between_blocks = 60
  let time_delay_for_priority n =
    time_between_blocks + (n * (time_between_blocks + 15))

  let revelation_reward = 125_000L
end

module Zeronet = struct
  let tez_units = 1_000_000L (* mutez *)
  let cost_per_byte = 1_000 (* mutez *)

  let block_per_cycle = 128     (* 2048 *)

  let tokens_per_roll = 10_000_000_000L

  let allowed_fork = 5          (* 6h = ~3 cycles *)
  let block_security_deposit = Int64.mul 512L tez_units (* mutez *)
  let endorsement_security_deposit = Int64.mul 64L tez_units (* mutez *)

  let blocks_per_roll_snapshot = 8

  let block_reward = Int64.mul 16L tez_units (* mutez *)

  let endorsers_per_block = 32

  let endorsement_reward_coeff = Int64.mul 2L tez_units (* mutez *)

  let origination_burn = 257_000L

  let blocks_between_revelations = 32

  let time_between_blocks = 20
  let time_delay_for_priority n = time_between_blocks * n

  let revelation_reward = 125_000L
end

module Constants = (val (match net with
                         | Betanet -> (module Betanet : Constants)
                         | Zeronet -> (module Zeronet : Constants)
                         | Alphanet -> (module Alphanet : Constants)))


let nb_revelations_per_cycle () =
  Constants.block_per_cycle / Constants.blocks_between_revelations

let cycle_from_level level =
  (level - 1) / Constants.block_per_cycle
