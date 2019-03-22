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

let unopt ~default = function None -> default | Some value -> value

let string_of_ballot_vote = function
  | Yay -> "Yay"
  | Nay -> "Nay"
  | Pass -> "Pass"

let ballot_of_string = function
  | "Yay" | "yay" -> Yay
  | "Pass"| "pass" -> Pass
  | "Nay" | "nay" -> Nay
  | _ -> Nay

let int_of_ballot_vote rolls = function
  | Yay -> rolls
  | Nay -> -rolls
  | Pass -> 0

let ballot_of_int = function
  | i when i > 0 -> Yay
  | i when i < 0 -> Nay
  | _ -> Pass

let voting_period_kind_of_string = function
  | "proposal" -> NProposal
  | "testing_vote" -> NTesting_vote
  | "testing" -> NTesting
  | "promotion_vote" -> NPromotion_vote
  | _ -> assert false

let string_of_voting_period_kind = function
  | NProposal -> "proposal"
  | NTesting_vote -> "testing_vote"
  | NTesting -> "testing"
  | NPromotion_vote -> "promotion_vote"

let pp_voting_period_kind = function
  | NProposal -> "Proposal"
  | NTesting_vote -> "Exploration"
  | NTesting -> "Testing"
  | NPromotion_vote -> "Promotion"

let pp_voting_period_status = function
  | VPS_passed -> "VP passed"
  | VPS_wait -> "VP wait (future)"
  | VPS_current -> "VP current"
  | VPS_ignored -> "VP never happened"
