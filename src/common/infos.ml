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

let versions =
  {
    server_version = TzscanConfig.version;
    server_build = TzscanConfig.en_date;
    server_commit = TzscanConfig.commit;
  }

let betanet = {
  ico_company_tokens = 76431859_801260L ;
  ico_foundation_tokens = 76431859_801260L ;
  ico_early_tokens = 3_156_502_294100L ;
  ico_contributors_tokens = 608_297_709_519372L ;
  ico_wallets = 31524 ;
}

(*
let alphanet = {
  ico_company_tokens = 0L ;
  ico_foundation_tokens = 32_000_000_000000L ;
  ico_early_tokens = 0L ;
  ico_contributors_tokens = 760_000_000_000000L ;
  ico_wallets = 30_000 ;
}

let zeronet = {
  ico_company_tokens = 0L ;
  ico_foundation_tokens = 8_040_000_000000L ;
  ico_early_tokens = 0L ;
  ico_contributors_tokens = 760_000_000_000000L ;
  ico_wallets = 30_000 ;
}
*)

let api =
  let constants7 = Tezos_constants.constants in
  (* Before cycle 7, there were no rewards *)
  let constants0 =
    { constants7 with
      block_reward = 0L ;
      endorsement_reward = 0L ;
    } in
  let api_config =
    {
      conf_network = "Mainnet" ;
      conf_constants = [
        0, constants0 ;
        7, constants7 ;
      ] ;
      conf_ico = betanet;
      conf_rampup_cycles = 64;
      conf_has_delegation = false ;
      conf_has_marketcap = false ;
    }
  in
  let api_date = 0. in
  let api_versions  = {
    server_version = "--" ;
    server_build = "--" ;
    server_commit = "--" ;
  }
  in
  {
    api_config ;
    api_date ;
    api_versions ;
  }


let www = {
  www_currency_name = "Tezos" ;
  www_currency_short = "XTZ" ;
  www_currency_symbol =  "#xa729" ;
  www_languages = [ "English", "en" ; "Français", "fr" ] ;
  www_apis = [||] ;
  www_auth = None ;
  www_logo = "tzscan-logo.png" ;
  www_footer = "/footer.html" ;
  www_networks = [] ;
}

let save_api_config filename =
  let oc = open_out filename in
  output_string oc (EzEncoding.construct ~compact:false
                      Api_encoding.V1.Server.api_server_config
                      api.api_config);
  close_out oc

let constants ~cycle =
  let rec iter cycle c constants =
    match constants with
    | (cy , c) :: constants when cy <= cycle ->
      iter cycle c constants
    | _ -> c
  in
  match api.api_config.conf_constants with
  | [] -> assert false
  | (_,c) :: constants ->
    iter cycle c constants

let rampup ~cycle deposit =
  let rampup_cycles = api.api_config.conf_rampup_cycles in
  if cycle >= rampup_cycles then deposit
  else
    Int64.(div (mul deposit (of_int cycle)) (of_int rampup_cycles))
