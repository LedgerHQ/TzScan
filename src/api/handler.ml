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
open Db_intf
open Recaptcha

let (>>=) = Lwt.(>>=)

let max_number_of_replies = 50

(*********************************************************************)
(* Handlers                                                          *)
(*********************************************************************)
module V1 = struct

  let operations_param params =
    match EzAPI.find_param Service.param_operations params with
    | None -> None
    | Some b -> Some (bool_of_string b)

  let number_params params =
    match EzAPI.find_param Service.param_number params with
    | None -> None
    | Some i_str -> Some (min (int_of_string i_str) max_number_of_replies)

  let pagination_params params =
    let page_size = number_params params in
    let page =
      match EzAPI.find_param Service.param_page params with
      | None -> None
      | Some i_str -> Some (int_of_string i_str) in
    (page, page_size)

  let contract_params params =
    match EzAPI.find_param Service.param_contract params with
    | None -> None
    | Some b_str -> Some (bool_of_string b_str)

  let filters_params params =
    let filters = EzAPI.find_params Service.param_type params in
    let pending = EzAPI.find_param Service.param_status params = (Some "Pending") in
    filters, pending

  let peers_params params = EzAPI.find_param Service.param_peers params

  let level_param params =
    match EzAPI.find_param Service.param_level params with
    | None -> None
    | Some i_str -> Some (int_of_string i_str)

  let cycle_param params =
    match EzAPI.find_param Service.param_cycle params with
    | None -> None
    | Some b -> Some (int_of_string b)

  let delegate_params params =
    match EzAPI.find_param Service.param_delegate params with
    | None -> None
    | Some b_str -> Some (bool_of_string b_str)

  let search_filter_params params =
    EzAPI.find_param Service.param_search_filter params

  let future_params params =
    match EzAPI.find_param Service.param_future params with
    | None -> None
    | Some b_str -> Some (bool_of_string b_str)

  let tops_kind_params params =
    EzAPI.find_param Service.param_kind params

  let spendable_params params =
    match EzAPI.find_param Service.param_spendable params with
    | None -> Some true
    | Some b_str -> Some (bool_of_string b_str)

  let block_hash_params params =
    EzAPI.find_param Service.param_block_hash params

  let period_params params =
    match EzAPI.find_param Service.param_period params with
    | None -> None
    | Some p_str -> Some (int_of_string p_str)

  let period_kind_params params =
    match EzAPI.find_param Service.param_period_kind params with
    | None -> None
    | Some p_str -> Some (Tezos_utils.voting_period_kind_of_string p_str)

  let ballot_params params = EzAPI.find_param Service.param_ballot params

  (* Block *)

  (* /block/BHASH *)
  let block (params, hash) () =
    Lwt_io.printf "block/%s\n%!" hash >>= fun () ->
    let operations = operations_param params in
    Dbr.block ?operations @@ Hash hash >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block

  (* /head *)
  let head (_params:EzAPI.params) () =
    Lwt_io.printf "head/\n%!" >>= fun () ->
    Dbr.head () >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block

  (* /genesis *)
  let genesis (_params:EzAPI.params) () =
    Lwt_io.printf "genesis/\n%!" >>= fun () ->
    Dbr.block (Level 0) >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block

  (* /blocks *)
  let blocks (params:EzAPI.params) () =
    Lwt_io.printf "Blocks\n%!" >>= fun () ->
    let operations = operations_param params in
    let page, page_size = pagination_params params in
    Dbr.blocks ?page ?page_size ?operations () >>= EzAPIServer.return

  let blocks_with_pred_fitness (params:EzAPI.params) () =
    Lwt_io.printf "Blocks with predecessor fitness\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.blocks_with_pred_fitness ?page ?page_size () >>= EzAPIServer.return

  let nb_snapshot_blocks _params () =
    Lwt_io.printf "Number Snapshot Blocks\n%!" >>= fun () ->
    Dbr.nb_snapshot_blocks () >>= EzAPIServer.return

  let snapshot_blocks (params:EzAPI.params) () =
    Lwt_io.printf "Snapshot Blocks\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.snapshot_blocks ?page ?page_size () >>= EzAPIServer.return

  let snapshot_levels _params () =
    Lwt_io.printf "Snapshot Levels\n%!" >>= fun () ->
    Dbr.snapshot_levels () >>= EzAPIServer.return

  (* /heads *)
  let heads (params:EzAPI.params) () =
    Lwt_io.printf "Heads\n%!"  >>= fun () ->
    let page, page_size = pagination_params params in
    let level = level_param params in
    Dbr.heads ?page ?page_size ?level () >>= EzAPIServer.return

  let nb_heads _params () =
    Lwt_io.printf "nb_heads\n%!" >>= fun () ->
    Dbr.nb_heads () >>= EzAPIServer.return

  let heads_with_pred_fitness (params:EzAPI.params) () =
    Lwt_io.printf "Heads with predecessor fitness\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    let level = level_param params in
    Dbr.heads_with_pred_fitness ?page ?page_size ?level () >>= EzAPIServer.return

  (* /nb_uncles/LEVEL *)
  let nb_uncles (_params, level) () =
    Lwt_io.printf "nb uncles at level %d\n%!" level >>= fun () ->
    Dbr.nb_uncles ~level () >>= EzAPIServer.return

  let nb_cycle _params () =
    Lwt_io.printf "nb_cycle/\n%!" >>= fun () ->
    Dbr.nb_cycle () >>= EzAPIServer.return

  (* Account / Contract  *)

  (* /accounts *)
  let accounts (params:EzAPI.params) () =
    Lwt_io.printf "accounts/\n%!"  >>= fun () ->
    let page, page_size = pagination_params params in
    let contract = contract_params params in
    Dbr.accounts ?page ?page_size ?contract () >>= EzAPIServer.return

  (* /number_accounts/ *)
  let nb_accounts (params:EzAPI.params) () =
    Lwt_io.printf "Request: 'nb_accounts\n%!" >>= fun () ->
    let contract = contract_params params in
    Dbr.nb_accounts ?contract () >>= EzAPIServer.return

  let bonds_rewards (_params, hash) () =
    Lwt_io.printf "bonds_rewards/%s\n%!" hash >>= fun () ->
    Dbr.account_bonds_rewards hash >>= EzAPIServer.return

  let extra_bonds_rewards (_params, hash) () =
    Lwt_io.printf "extra_bonds_rewards/%s\n%!" hash >>= fun () ->
    Dbr.extra_bonds_rewards hash >>= EzAPIServer.return

  let max_roll_cycle (_params) () =
    Lwt_io.printf "max_roll_cycle\n%!" >>= fun () ->
    Dbr.max_roll_cycle () >>= EzAPIServer.return

  let rolls_distribution (_params, cycle) () =
    Lwt_io.printf "rolls_distribution for cycle %d\n%!" cycle >>= fun () ->
    Dbr.rolls_distribution cycle >>= EzAPIServer.return

  let roll_number (_params, hash) () =
    Lwt_io.printf "roll_number/%s\n%!" hash >>= fun () ->
    Dbr.roll_number hash >>= EzAPIServer.return

  let rolls_history (params, hash) () =
    Lwt_io.printf "rolls_history/%s\n%!" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.rolls_history ?page ?page_size hash >>= EzAPIServer.return

  let account_status (_params, hash) () =
    Lwt_io.printf "account_status/%s\n%!" hash >>= fun () ->
    Dbr.account_status hash >>= EzAPIServer.return

  let account_from_alias (_params, alias) () =
    Lwt_io.printf "account_from_alias/%s\n%!" alias >>= fun () ->
    Dbr.account_from_alias alias >>= EzAPIServer.return

  let votes_account (params, hash) () =
    Lwt_io.printf "votes_account/%s\n%!" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.votes_account ?page ?page_size hash >>= EzAPIServer.return

  let vote_graphs_account (_params, hash) () =
    Lwt_io.printf "vote_graphs_account/%s\n%!" hash >>= fun () ->
    Dbr.vote_graphs_account hash >>= EzAPIServer.return

  (* Rewards *)

  let nb_cycle_rewards (_params, hash) () =
    Lwt_io.printf "nb_cycle_rewards/%s\n%!" hash >>= fun () ->
    Dbr.nb_cycle_rewards hash >>= EzAPIServer.return

  let nb_delegators (params, hash) () =
    Lwt_io.printf "nb_delegators/%s\n%!" hash >>= fun () ->
    let cycle = cycle_param params in
    Dbr.nb_delegators ?cycle hash >>= EzAPIServer.return

  let rewards_split_cycles (params, hash) () =
    Lwt_io.printf "rewards_split_cycles/%s\n%!" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.delegate_rewards_split_cycles ?page ?page_size hash
    >>= EzAPIServer.return

  let rewards_split (params, hash) () =
    let page, page_size = pagination_params params in
    let cycle = cycle_param params in
    Lwt_io.printf "rewards_split/%s/%d\n%!" hash (Misc.unopt (-1) cycle) >>= fun () ->
    Dbr.delegate_rewards_split ?page ?page_size ?cycle hash >>=
    EzAPIServer.return

  let rewards_split_fast (params, hash) () =
    let page, page_size = pagination_params params in
    let cycle = cycle_param params in
    Lwt_io.printf "rewards_split_fast/%s/%d\n%!" hash (Misc.unopt (-1) cycle) >>= fun () ->
    Dbr.delegate_rewards_split_fast ?page ?page_size ?cycle hash >>=
    EzAPIServer.return

  let nb_cycle_delegator_rewards (_params, hash) () =
    Lwt_io.printf "nb_cycle_delegator_rewards/%s\n%!" hash >>= fun () ->
    Dbr.nb_cycle_delegator_rewards hash >>= EzAPIServer.return

  let delegator_rewards (params, hash) () =
    Lwt_io.printf "delegator_rewards/%s\n%!" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.delegator_rewards ?page ?page_size hash >>= EzAPIServer.return

  let delegator_rewards_with_details (params, hash) () =
    Lwt_io.printf "delegator_rewards_details/%s\n%!" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.delegator_rewards_with_details ?page ?page_size hash >>= EzAPIServer.return

  (* Operations *)

  (* /operation/OHASH *)
  let operation (params, ohash) () =
    Lwt_io.printf "operation/%s\n%!" ohash >>= fun () ->
    let block_hash = block_hash_params params in
    Dbr.operation ?block_hash ohash >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some op -> EzAPIServer.return op

  let make_operations ?(pending=false) ?hash params =
    Lwt_io.printf "Request: 'operations'\n%!" >>= fun () ->
    let filters, pending2 = filters_params params in
    let pending = pending || pending2 in
    let page, page_size = pagination_params params in
    let delegate = delegate_params params in
    let selector = match hash with
      | None ->
        if pending then Pending
        else Empty
      | Some hash -> Pg_helper.hash_selector_of_hash hash in
    Dbr.operations
      ?delegate
      ?filters
      ?page
      ?page_size
      selector

  (* /estimate_gas?token=? *)
  let estimate_gas (params:EzAPI.params) () =
    let token = EzAPI.find_param Service.param_token params in
    match token with
    | Some token -> Dbr.estimate_gas token >>= EzAPIServer.return
    | _ -> Lwt.fail EzAPI.ResultNotfound

  (* /broadcast_transaction *)
  let broadcast_transaction _ transaction =
    Node_request.broadcast_transaction transaction >>= EzAPIServer.return

  (* /counter?token=? *)
  let counter (params:EzAPI.params) () =
    let token = EzAPI.find_param Service.param_token params in
    match token with
    | Some token -> Node_request.counter token >>= EzAPIServer.return
    | _ -> Lwt.fail EzAPI.ResultNotfound

  (* /estimate_storage?token=? *)
  let estimate_storage (params:EzAPI.params) () =
    let token = EzAPI.find_param Service.param_token params in
    match token with
    | Some token -> Dbr.estimate_storage token >>= EzAPIServer.return
    | _ -> Lwt.fail EzAPI.ResultNotfound

  (* /operations *)
  let operations (params:EzAPI.params) () =
    make_operations params >>= EzAPIServer.return

  (* /operations/BHASH *)
  let operations_bh (params, block_hash) () =
    let hash = if block_hash = "" then None else Some block_hash in
    make_operations ?hash params >>= EzAPIServer.return

  (* /number_operations/HASH *)
  let nb_operations_hash (params, hash) () =
    Lwt_io.printf "Request: 'nb_operations/%s'\n%!" hash >>= fun () ->
    let filters, pending = filters_params params in
    let delegate = delegate_params params in
    let hash_selector =
      if pending then Pending else Pg_helper.hash_selector_of_hash hash
    in
    Dbr.nb_operations ?delegate ?filters hash_selector >>= EzAPIServer.return

  (* /number_operations/ *)
  let nb_operations (params:EzAPI.params) () =
    Lwt_io.printf "Request: 'nb_operations\n%!" >>= fun () ->
    let filters, pending = filters_params params in
    Dbr.nb_operations ?filters (if pending then Pending else Empty) >>= EzAPIServer.return

  (* /endorsements/level *)
  let endorsements_level (_params, level) () =
    Lwt_io.printf "Request: 'endorsements_level %i\n%!" level >>= fun () ->
    Dbr.endorsements @@ Level level >>= EzAPIServer.return

  let nonces params () =
    Lwt_io.printf "nonces/\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.nonces ?page ?page_size () >>= EzAPIServer.return

  let transaction_account_csv (params, hash) () =
    let token = EzAPI.find_param Service.param_token params in
    match token, Config.get_secret_key (), Config.get_csv_dir () with
    | Some token, Some secret_key, Some csv_dir ->
      verify secret_key token >>= fun captcha ->
      if captcha.cap_success then (
        match captcha.cap_score with
        | Some score when score > 0.5 ->
          Lwt_io.printf "transaction to csv/\n%!" >>= fun () ->
          let date = CalendarLib.Printer.Date.to_string (CalendarLib.Date.today ()) in
          let filename = Printf.sprintf "transactions_%s_%s.csv" hash date in
          let csv_files = Sys.readdir csv_dir in
          if Array.exists (fun s -> s = filename) csv_files then
            EzAPIServer.return filename
          else
            Dbr.operations ~page_size:max_int ~filters:["Transaction"] (Account hash)
            >>= fun ltr ->
            let header = [ "transaction"; "block"; "source"; "destination";
                           "amount"; "fee"; "date"; "failed"; "burned tez" ] in
            let l =
              List.fold_left (fun acc o -> acc @ (Data_string.transaction header o)) [] ltr in
            Csv_lwt.save ~quote_all:true (csv_dir ^ filename) (header :: l)
            >>= fun () -> EzAPIServer.return filename
        | _ ->
          Printf.eprintf "CSV transaction for %s failed captcha verification with token %S\n%!" hash token;
          Lwt.fail EzAPI.ResultNotfound)
      else (
        Printf.eprintf "CSV transaction for %s failed captcha verification with token %S\n%!" hash token;
        Lwt.fail EzAPI.ResultNotfound)
    | _ ->
      Printf.eprintf "No captcha or no secret key\n%!";
      Lwt.fail EzAPI.ResultNotfound


  (* Bakings *)

  let bakings (params, hash) () =
    Lwt_io.printf "Request: 'bakings/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    let cycle = cycle_param params in
    Dbr.bakings ?page ?page_size ?cycle hash >>= EzAPIServer.return

  (* /number_bakings/HASH *)
  let nb_bakings (params, hash) () =
    Lwt_io.printf "Request: 'nb_bakings/%s'\n%!" hash >>= fun () ->
    let cycle = cycle_param params in
    Dbr.nb_bakings ?cycle hash >>= EzAPIServer.return

  let nb_bakings_endorsement (params, hash) () =
    Lwt_io.printf "Request: 'nb_bakings_endorsement/%s'\n%!" hash >>= fun () ->
    let cycle = cycle_param params in
    Dbr.nb_bakings_endorsement ?cycle hash >>= EzAPIServer.return

  let bakings_endorsement (params, hash) () =
    Lwt_io.printf "Request: 'bakings endorsement/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    let cycle = cycle_param params in
    Dbr.bakings_endorsement ?page ?page_size ?cycle hash >>= EzAPIServer.return

  let cycle_bakings (params, hash) () =
    Lwt_io.printf "Request: 'cycle_bakings/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.cycle_bakings_sv ?page ?page_size hash >>= EzAPIServer.return

  let nb_cycle_bakings (_params, hash) () =
    Lwt_io.printf "Request: 'nb_cycle_bakings/%s'\n%!" hash >>= fun () ->
    Dbr.nb_cycle_bakings hash >>= EzAPIServer.return

  let cycle_endorsements (params, hash) () =
    Lwt_io.printf "Request: 'cycle_endorsements/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.cycle_endorsements_sv ?page ?page_size hash >>= EzAPIServer.return

  let nb_cycle_endorsements (_params, hash) () =
    Lwt_io.printf "Request: 'nb_cycle_endorsements/%s'\n%!" hash >>= fun () ->
    Dbr.nb_cycle_endorsements hash >>= EzAPIServer.return

  let nb_bakings_history (_params, hash) () =
    Lwt_io.printf "Request: 'nb_bakings_history/%s'\n%!" hash >>= fun () ->
    Dbr.nb_bakings_history hash >>= EzAPIServer.return

  let bakings_history (params, hash) () =
    Lwt_io.printf "Request: 'bakings_history/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.bakings_history ?page ?page_size hash >>= EzAPIServer.return

  let total_bakings (_params, hash) () =
    Lwt_io.printf "Request: 'total_bakings/%s'\n%!'" hash >>= fun () ->
    Dbr.total_bakings hash >>= EzAPIServer.return

  let nb_endorsements_history (_params, hash) () =
    Lwt_io.printf "Request: 'nb_endorsemnts_history/%s'\n%!" hash >>= fun () ->
    Dbr.nb_endorsements_history hash >>= EzAPIServer.return

  let endorsements_history (params, hash) () =
    Lwt_io.printf "Request: 'endorsements_history/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.endorsements_history ?page ?page_size hash >>= EzAPIServer.return

  let total_endorsements (_params, hash) () =
    Lwt_io.printf "Request: 'total_enodorsements/%s'\n%!'" hash >>= fun () ->
    Dbr.total_endorsements hash >>= EzAPIServer.return

  let last_baking_and_endorsement (_params, hash) () =
    Lwt_io.printf "last baking and endorsement of %s\n%!" hash >>= fun () ->
    Dbr.last_baking_and_endorsement hash >>= EzAPIServer.return

  let next_baking_and_endorsement (_params, hash) () =
    Lwt_io.printf "next baking and endorsement of %s\n%!" hash >>= fun () ->
    Dbr.next_baking_and_endorsement hash >>= EzAPIServer.return


  (* Rights *)

  (* /number_bakings/HASH *)
  let nb_cycle_rights params () =
    Lwt_io.printf "Request: 'nb_cycle_rights'\n%!">>= fun () ->
    let filter = search_filter_params params in
    let future = future_params params in
    Dbr.nb_cycle_rights ?future ?filter () >>= EzAPIServer.return

  let cycle_rights (params:EzAPI.params) () =
    Lwt_io.printf "Request: 'cycle_rights'\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    let filter = search_filter_params params in
    let future = future_params params in
    Dbr.cycle_rights ?future ?filter ?page ?page_size () >>= EzAPIServer.return

  (* /number_bakings/HASH *)
  let nb_baker_rights (params, hash) () =
    Lwt_io.printf "Request: 'nb_baking_rights/%s'\n%!" hash >>= fun () ->
    let cycle = cycle_param params in
    Dbr.nb_baker_rights ?cycle hash >>= EzAPIServer.return

  let baker_rights (params, hash) () =
    Lwt_io.printf "Request: 'baking_rights/%s'\n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    let cycle = cycle_param params in
    Dbr.baker_rights ?cycle ?page ?page_size hash >>= EzAPIServer.return

  let cycle_baker_rights (_params, hash) () =
    Lwt_io.printf "Request: 'cycle_baker_rights/%s'\n%!'" hash >>= fun () ->
    Dbr.cycle_baker_rights hash >>= EzAPIServer.return

  let cycle_all_rights (params,hash) () =
    Lwt_io.printf "Request: 'cycle_all_rights/%s'\n%!" hash >>= fun () ->
    let cycle = cycle_param params in
    let prio = number_params params in
    Dbr.cycle_all_rights ?cycle ?prio hash >>= EzAPIServer.return

  (* /number_bakings/HASH *)
  let nb_endorser_rights (params, hash) () =
    Lwt_io.printf "Request: 'nb_endorser_rights/%s'\n%!" hash >>= fun () ->
    let cycle = cycle_param params in
    Dbr.nb_endorser_rights ?cycle hash >>= EzAPIServer.return

  let endorser_rights (params, hash) () =
    Lwt_io.printf "Request: 'endorser_rights/%s'n%!'" hash >>= fun () ->
    let page, page_size = pagination_params params in
    let cycle = cycle_param params in
    Dbr.endorser_rights ?cycle ?page ?page_size hash >>= EzAPIServer.return

  let cycle_endorser_rights (_params, hash) () =
    Lwt_io.printf "Request: 'cycle_endorser_rights/%s'\n%!'" hash >>= fun () ->
    Dbr.cycle_endorser_rights hash >>= EzAPIServer.return

  let required_balance (_params, hash) () =
    Lwt_io.printf "Request: 'required_balance/%s'\n%!'" hash >>= fun () ->
    Dbr.required_balance hash >>= EzAPIServer.return


  (* Block *)

  let block_succ (_params, hash) () =
    Lwt_io.printf "succ/%s\n%!" hash >>= fun () ->
    Dbr.block_successor hash >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some hash -> EzAPIServer.return hash

  let block_pred (_params, hash) () =
    Lwt_io.printf "pred/%s\n%!" hash >>= fun () ->
    Dbr.block (Hash hash) >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block.predecessor_hash

  let timestamp (_params, hash) () =
    Lwt_io.printf "timestamp/%s\n%!" hash >>= fun () ->
    Dbr.block (Hash hash) >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block ->
      EzAPIServer.return @@
      Date.to_string block.timestamp

  let level (_params, hash) () =
    Lwt_io.printf "level/%s\n%!" hash >>= fun () ->
    Dbr.level hash >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some level -> EzAPIServer.return level

  let fitness (_params, hash) () =
    Lwt_io.printf "fitness/%s\n%!" hash >>= fun () ->
    Dbr.block (Hash hash) >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block.fitness

  let network (_params, hash) () =
    Lwt_io.printf "network/%s\n%!" hash >>= fun () ->
    Dbr.block (Hash hash) >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block.network

  let priority (_params, hash) () =
    Lwt_io.printf "priority/%s\n%!" hash >>= fun () ->
    Dbr.block (Hash hash) >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block.priority

  module VolumeCache = struct

    module StringMap = Map.Make(String)

    let cache = ref StringMap.empty

    let get hash f =
      Lwt.catch
        (fun () -> Lwt.return @@ StringMap.find hash !cache)
        (function
          | Not_found ->
            f () >>= fun x ->
            cache := StringMap.add hash x !cache;
            Lwt.return x
          | e -> Lwt.fail e
        )

  end

  let volume hash =
    make_operations ?hash (EzAPI.request [ "type", [ "Transaction" ]])
    >>= fun operations ->
    Lwt.return @@
    List.fold_left (fun sum op ->
        match op.op_type with
        | Sourced Manager (_, _, list) ->
          List.fold_left (fun sum op -> match op with
              | Transaction tr -> Int64.(add tr.tr_amount sum)
              | _ -> sum) sum list
        | _ -> sum  (* should never happen *))
      Int64.zero operations

  let volume (_params, block_hash) () =
    Lwt_io.printf "volume/%s\n%!" block_hash >>= fun () ->
    (if block_hash = "" then volume None
     else
       VolumeCache.get block_hash (fun () -> volume (Some block_hash)) )
    >>= EzAPIServer.return


  (* Level *)

  let block_level (params, level) () =
    Lwt_io.printf "block_level/%i\n%!" level >>= fun () ->
    let operations = operations_param params in
    Dbr.block ?operations @@ Level level >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block

  let block_hash_level (_params, level) () =
    Printf.printf "block_hash_level/%i\n%!" level;
    Dbr.block @@ Level level >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some block -> EzAPIServer.return block.hash


  (* Stats *)

  let marketcap _param () =
    Lwt_io.printf "marketcap/\n%!"  >>= fun () ->
    Dbr.marketcap () >>= EzAPIServer.return

  (* /number_network_peers/ *)
  let nb_network_peers params () =
    Lwt_io.printf "Request: 'nb_network_peers\n%!" >>= fun () ->
    let state = peers_params params in
    Dbr.nb_network_peers ?state () >>= EzAPIServer.return

  let network_stats (params:EzAPI.params) () =
    Lwt_io.printf "network stats/\n%!"  >>= fun () ->
    let page, page_size = pagination_params params in
    let state = peers_params params in
    Dbr.network_stats ?state ?page ?page_size () >>= EzAPIServer.return

  let country_stats (params:EzAPI.params) () =
    Lwt_io.printf "network stats/\n%!"  >>= fun () ->
    let state = peers_params params in
    Dbr.country_stats ?state () >>= EzAPIServer.return

  let baker_stats (_params, hash) () =
    Lwt_io.printf "baker stats/\n%!"  >>= fun () ->
    Dbr.baker_stats hash >>= EzAPIServer.return

  let bakers_stats params () =
    Lwt_io.printf "baker stats/\n%!"  >>= fun () ->
    let cycle = cycle_param params in
    Dbr.bakers_stats ?cycle () >>= EzAPIServer.return

  let blocks_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "blocks_per_day/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let s = Stats.stats () in
    EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
                         pd_value = s.Stats.stats_nblocks_per_day }

  let bakers_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "bakers_per_day/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let s = Stats.stats () in
    EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
                         pd_value = s.Stats.stats_nbakers_per_day }

  let priorities_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "priorities_per_day/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let s = Stats.stats () in
    EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
                         pd_value = s.Stats.stats_priorities_per_day }

  let operations_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "operations_per_day/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let s = Stats.stats () in
    EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
                         pd_value = s.Stats.stats_nops_per_day }

  let operations_per_block_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "priorities_per_day/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let s = Stats.stats () in
    EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
                         pd_value = s.Stats.stats_nops_per_block_per_day }

  let fees_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "fees_per_day/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let s = Stats.stats () in
    EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
                         pd_value = s.Stats.stats_fees_per_day }

  let volume_per_day (_params:EzAPI.params) () =
    Lwt_io.printf "volume_per_day/\n%!"  >>= fun () ->
    let translate_to_tsp date =
      let year = date / 10000 in
      let month = (date - year * 10000) / 100 in
      let day = date - year * 10000 - month * 100 in
      Printf.sprintf "%d/%02d/%02d" year month day in
    Dbr.volume_per_day () >>= fun l ->
    EzAPIServer.return
      {pd_days = Array.of_list @@ List.map (fun (date, _) -> translate_to_tsp date) l;
       pd_value = Array.of_list @@ List.map snd l}
    (* Stats.update () >>= fun () ->
     * let s = Stats.stats () in
     * EzAPIServer.return { pd_days = s.Stats.stats_name_per_day;
     *                 pd_value = Array.map (fun n ->
     *                     (\* convert to kXTZ *\)
     *                     float_of_int (n / 100_000))
     *                     s.Stats.stats_volume_per_day } *)

  let mini_stats  (_params:EzAPI.params) () =
    Lwt_io.printf "mini_stats/\n%!"  >>= fun () ->
    Stats.update () >>= fun () ->
    let ms = Stats.mini_stats () in
    EzAPIServer.return ms

  let health_stats (_params, cycle) () =
    Lwt_io.printf "health_stats/\n%!"  >>= fun () ->
    Dbr.health_stats cycle >>= EzAPIServer.return

  let context_days _params () =
    Lwt_io.printf "context_days/\n%!" >>= fun () ->
    Dbr.context_days () >>= EzAPIServer.return

  let context_stats (_params, day) () =
    Lwt_io.printf "context_stats/\n%!" >>= fun () ->
    let y, m, d = Utils.split_ymd_timestamp day in
    let day = CalendarLib.Calendar.make y m d 0 0 0 in
    Dbr.context_stats day >>= function
    | None -> Lwt.fail EzAPI.ResultNotfound
    | Some st -> EzAPIServer.return st

  let nb_tops params () =
    let kind = tops_kind_params params in
    Dbr.nb_tops ?kind () >>= EzAPIServer.return

  let tops params () =
    let kind = tops_kind_params params in
    let page, page_size = pagination_params params in
    Dbr.tops ?page ?page_size ?kind () >>= EzAPIServer.return

  let h24_stats _params () =
    Lwt_io.printf "h24_stats/\n%!"  >>= fun () ->
    Dbr.h24_stats () >>= EzAPIServer.return

  let supply _params () =
    Lwt_io.printf "supply\n%!" >>= fun () ->
    Dbr.supply () >>= EzAPIServer.return

  let activated_balances _params () =
    Lwt_io.printf "activated_balances\n%!" >>= fun () ->
    Dbr.activated_balances () >>= EzAPIServer.return

  let balance_break_down (_params, hash) () =
    Lwt_io.printf "balance_break_down\n%!" >>= fun () ->
    Dbr.balance_break_down hash >>= EzAPIServer.return

  let market_prices _params () =
    Lwt_io.printf "market_prices\n%!" >>= fun () ->
    Dbr.market_prices () >>= fun rows ->
    EzAPIServer.return @@ Array.of_list rows


  (* Search *)

  let search_block (_params, hash) () =
    Lwt_io.printf "search block %s/\n%!" hash >>= fun () ->
    Dbr.search_block hash >>= EzAPIServer.return

  let search_operation (_params, hash) () =
    Lwt_io.printf "search operation %s/\n%!" hash >>= fun () ->
    Dbr.search_operation hash >>= EzAPIServer.return

  let search_account (_params, hash) () =
    Lwt_io.printf "search account %s/\n%!" hash >>= fun () ->
    Dbr.search_account hash >>= EzAPIServer.return

  let nb_search_block (_params, hash) () =
    Lwt_io.printf "number of block search results for %s/\n%!" hash >>= fun () ->
    Dbr.nb_search_block hash >>= EzAPIServer.return

  let nb_search_operation (_params, hash) () =
    Lwt_io.printf "number of operation search results for %s/\n%!" hash >>= fun () ->
    Dbr.nb_search_operation hash >>= EzAPIServer.return

  let nb_search_account (_params, hash) () =
    Lwt_io.printf "number of search account results for %s/\n%!" hash >>= fun () ->
    Dbr.nb_search_account hash >>= EzAPIServer.return

  let alias (_params, hash) () =
    Lwt_io.printf "alias/%s\n%!" hash >>= fun () ->
    EzAPIServer.return (Alias.to_name hash).alias


  (* Node *)

  let node_timestamps _params () =
    Lwt_io.printf "timestamps for all nodes\n%!" >>= fun () ->
    Node_request.timestamps () >>= fun s ->
    EzAPIServer.return s

  let node_account (_params, hash) () =
    Lwt_io.printf "account %s from balance node/\n%!" hash >>= fun () ->
    Node_request.to_lwt Node_request.account_dec hash >>=
    fun (acc_node_timestamp,
         (manager, acc_balance, acc_spendable, dlgt, acc_script,
          acc_storage, acc_counter)) ->
    let acc_name = Alias.to_name hash in
    let acc_manager = Alias.to_name manager in
    let acc_dlgt = fst dlgt, match snd dlgt with
      | None -> None
      | Some delegate -> Some (Alias.to_name delegate) in
    EzAPIServer.return {acc_name; acc_manager; acc_balance; acc_spendable; acc_dlgt;
                        acc_script; acc_storage; acc_counter;
                        acc_node_timestamp}

  let node_deactivated_grace_period (_params, hash) () =
    Lwt_io.printf "roll_balance_history/%s\n%!" hash >>= fun () ->
    Node_request.to_lwt
      Node_request.delegate_details
      hash
    >>= fun details ->
    EzAPIServer.return
      (details.delegate_deactivated,
       details.delegate_grace_period)

  let node_nb_delegated_contracts (_params, hash) () =
    Lwt_io.printf "nb_delegated_contracts %s\n%!" hash >>= fun () ->
    Node_request.to_lwt
      Node_request.delegate_details
      hash
    >>= fun details ->
    EzAPIServer.return @@
    List.length details.delegate_delegated_contracts

  let node_delegated_contracts (params, hash) () =
    Lwt_io.printf "delegated_contracts %s\n%!" hash >>= fun () ->
    let page, page_size = pagination_params params in
    Node_request.to_lwt Node_request.delegate_details hash
    >>= fun details ->
    let deleguees = details.delegate_delegated_contracts in
    let rec sublist b e l = match l with
        [] -> []
      | h :: t ->
        let tail = if e = 0 then [] else sublist (b - 1) (e - 1) t in
        if b > 0 then tail else h :: tail in
    let p = match page with None -> 0 | Some p -> p in
    let p_s = match page_size with None -> 20 | Some p_s -> p_s in
    let start_list = p * p_s in
    let end_list = ((p + 1) * p_s) - 1 in
    EzAPIServer.return @@ sublist start_list end_list deleguees

  let node_staking_balance (_params, hash) () =
    Lwt_io.printf "staking_balance %s\n%!" hash >>= fun () ->
    Node_request.to_lwt Node_request.delegate_details hash
    >>= fun details -> EzAPIServer.return details.delegate_staking_balance


  (* Server *)

  let version  (_params:EzAPI.params) () =
    Lwt_io.printf "version/\n%!"  >>= fun () ->
    EzAPIServer.return {
      server_version = TzscanConfig.version;
      server_build = TzscanConfig.en_date;
      server_commit = TzscanConfig.commit;
    }

  (* /date *)
  let date (_params:EzAPI.params) () =
    Lwt_io.printf "date/\n%!" >>= fun () ->
    EzAPIServer.return (EzAPIServer.req_time())

  let api_server_info _req () =
    let api_date = EzAPIServer.req_time() in
    let api_versions = Infos.versions in
    let api_server_info = { Infos.api with
                            api_date ; api_versions } in
    EzAPIServer.return api_server_info


  (* Protocols *)

  let nb_protocol _params () =
    Lwt_io.printf "nb_protocol/\n%!" >>= fun () ->
    Dbr.nb_protocol () >>= EzAPIServer.return

  let protocols params () =
    Lwt_io.printf "protocols/\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    Dbr.protocols ?page ?page_size () >>= EzAPIServer.return

  let proposals params () =
    Lwt_io.printf "proposals/\n%!" >>= fun () ->
    let period = period_params params in
    let page, page_size = match period with
      | None -> pagination_params params
      | _ -> Some 0, Some max_int in
    Dbr.proposals ?period ?page ?page_size () >>= EzAPIServer.return

  let nb_proposals params () =
    Lwt_io.printf "nb_proposals/\n%!" >>= fun () ->
    let period = period_params params in
    Dbr.nb_proposals ?period () >>= EzAPIServer.return

  let voting_period_info params () =
    Lwt_io.printf "voting_period_info/\n%!" >>= fun () ->
    let period = period_params params in
    Dbr.voting_period_info ?period () >>= EzAPIServer.return

  let testing_proposal (params, period) () =
    Lwt_io.printf "testing_proposal %d/\n%!" period >>= fun () ->
    match period_kind_params params with
    | Some period_kind ->
      Dbr.testing_proposal period period_kind >>= EzAPIServer.return
    | None -> Lwt.fail EzAPI.ResultNotfound

  let ballots (params, period) () =
    Lwt_io.printf "ballots %d/\n%!" period >>= fun () ->
    match period_kind_params params with
    | Some period_kind ->
      Dbr.ballots period period_kind >>= EzAPIServer.return
    | None -> Lwt.fail EzAPI.ResultNotfound

  let nb_proposal_votes (params, hash) () =
    Lwt_io.printf "nb_proposal_votes %s/\n%!" hash >>= fun () ->
    let period = period_params params in
    Dbr.nb_proposal_votes ?period hash >>= EzAPIServer.return

  let proposal_votes (params, hash) () =
    Lwt_io.printf "proposal_votes %s/\n%!" hash >>= fun () ->
    let period = period_params params in
    let page, page_size = pagination_params params in
    Dbr.proposal_votes ?period ?page ?page_size hash >>= EzAPIServer.return

  let total_proposal_votes (_params, period) () =
    Lwt_io.printf "total_proposal_votes %d/\n%!" period >>= fun () ->
    Dbr.total_proposal_votes period >>= EzAPIServer.return

  let nb_ballot_votes (params, hash) () =
    Lwt_io.printf "nb_ballot_votes %s/\n%!" hash >>= fun () ->
    let period = period_params params in
    let ballot = ballot_params params in
    Dbr.nb_ballot_votes ?period ?ballot hash >>= EzAPIServer.return

  let ballot_votes (params, hash) () =
    Lwt_io.printf "ballot_votes %s/\n%!" hash >>= fun () ->
    let period = period_params params in
    let ballot = ballot_params params in
    let page, page_size = pagination_params params in
    Dbr.ballot_votes ?period ?ballot ?page ?page_size hash >>= EzAPIServer.return

  let total_voters (_params, period) () =
    Lwt_io.printf "total_voters %d/\n%!" period >>= fun () ->
    Dbr.total_voters period >>= EzAPIServer.return

  (* Balance updates *)

  let balance_updates_number (params,hash) () =
    Lwt_io.printf "nu_balance_updates/\n%!" >>= fun () ->
    let from = cycle_param params in
    Dbr.nb_balance_updates ?from hash >>= EzAPIServer.return

  let balance (_params,hash) () =
    Lwt_io.printf "balance/\n%!" >>= fun () ->
    Dbr.balance hash >>= EzAPIServer.return

  let balance_updates (params,hash) () =
    Lwt_io.printf "balance_updates/\n%!" >>= fun () ->
    let from = cycle_param params in
    let page, page_size = pagination_params params in
    Dbr.balance_updates ?page ?page_size ?from hash >>= EzAPIServer.return

  let active_balance_updates (params,hash) () =
    Lwt_io.printf "active_balance_updates/\n%!" >>= fun () ->
    let cycle_opt = cycle_param params in
    match cycle_opt with
      Some cycle -> Dbr.active_balance_updates cycle hash >>= EzAPIServer.return
    | None -> EzAPIServer.return []

  let balance_from_balance_updates (_params,hash) () =
    Lwt_io.printf "balance_from_balance_updates/\n%!" >>= fun () ->
    Dbr.balance_from_balance_updates hash >>= EzAPIServer.return

  let balance_history (_params,hash) () =
    Lwt_io.printf "balance_history/\n%!" >>= fun () ->
    Dbr.balance_history hash >>= EzAPIServer.return

  let cycle_frozen (params,hash) () =
     Lwt_io.printf "cycle_frozen/\n%!" >>= fun () ->
     match cycle_param params with
       None -> EzAPIServer.return
                 {b_spendable = Int64.zero;
                  b_frozen =  Int64.zero;
                  b_rewards =  Int64.zero;
                  b_fees =  Int64.zero;
                  b_deposits =  Int64.zero}
     | Some cycle ->
        (Dbr.cycle_frozen cycle hash) >>= EzAPIServer.return

  let balance_number params () =
    Lwt_io.printf "balance_number/\n%!" >>= fun () ->
    match cycle_param params with
      None -> EzAPIServer.return 0
    | Some cycle ->
       (Dbr.nb_balance (Int32.of_int cycle)) >>= EzAPIServer.return

  let balance_ranking params () =
    Lwt_io.printf "balance_ranking/\n%!" >>= fun () ->
    let page, page_size = pagination_params params in
    match cycle_param params with
      None -> EzAPIServer.return []
    | Some cycle ->
       match spendable_params params with
         None ->  EzAPIServer.return []
       | Some spendable ->
          Dbr.balance_ranking ?page ?page_size (Int32.of_int cycle) spendable >>= EzAPIServer.return

end

module V2 = V1
module V3 = V1
