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

module type Constants = sig
  val foundation_and_dls : float
  val contributors_xtz : float
  val early_xtz : float
  val dls_xtz : float
  val foundation_xtz : float
  val total_supply_ico : float
  val total_ico_wallets : int
end

open Data_types

let (++) = Int64.add

let total_supply_ico ico =
  ico.ico_company_tokens ++
  ico.ico_foundation_tokens ++
  ico.ico_early_tokens ++
  ico.ico_contributors_tokens

(*
module Betanet = struct
  let foundation_and_dls = 152_863_719.602520019
  let contributors_xtz =  608_297_709.519372
  let early_xtz = 3_156_502.29410000052
  let dls_xtz = foundation_and_dls /. 2.
  let foundation_xtz = foundation_and_dls /. 2. +. 0.000008
  let total_supply_ico = foundation_and_dls +. contributors_xtz +. early_xtz
  let total_ico_wallets = 31524
end

module Alphanet = struct
  let foundation_and_dls = 0.
  let contributors_xtz =  760_000_000.0
  let early_xtz = 0.
  let dls_xtz = 0.
  let foundation_xtz = 32_000_000.0
  let total_supply_ico = foundation_and_dls +. contributors_xtz +. early_xtz
  let total_ico_wallets = 30_000
end

module Zeronet = struct
  let foundation_and_dls = 0.
  let contributors_xtz =  760_000_000.0
  let early_xtz = 0.
  let dls_xtz = 0.
  let foundation_xtz = 8_040_000.0
  let total_supply_ico = foundation_and_dls +. contributors_xtz +. early_xtz
  let total_ico_wallets = 30_000
end

module Constants = (val (match Tezos_constants.net with
    | Tezos_constants.Betanet -> (module Betanet : Constants)
    | Tezos_constants.Zeronet -> (module Zeronet : Constants)
    | Tezos_constants.Alphanet -> (module Alphanet : Constants)))
*)
