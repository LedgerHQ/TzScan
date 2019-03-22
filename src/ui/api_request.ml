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

open Ocp_js
open Tezos_types
open Data_types
open EzAPI.TYPES
open Service
open Common

(* Use our own version of Ezjsonm.from_string to avoid stack overflows *)
let () = EzEncodingJS.init ()

let find_url_arg arg =
  let args =
    match Url.url_of_string (Js.to_string Dom_html.window##location##href) with
    | None -> []
    | Some (
        Url.Http { Url.hu_arguments = args; _ }
      | Url.Https { Url.hu_arguments = args; _ }
      | Url.File { Url.fu_arguments = args; _ }
      ) -> args
  in
  Misc.list_find_opt (fun (k, _v) -> k = arg) args

let find_page () =
  match Jsloc.find_arg "p" with
  | None -> 0
  | Some p_str -> try int_of_string p_str with _ -> 0

let find_level () =
  match Jsloc.find_arg "level" with
  | None -> None
  | Some p_str -> try Some (int_of_string p_str) with _ -> None

(* TODO: use `request0` and `request1` as much as possible, as it checks
  consistency between the output of the service and the encoding used
  here. *)

module Api : sig
  val request0 :
    ?error:EzRequest.error_handler ->
    'a EzAPI.service0 -> string -> ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ('a -> unit) -> unit

  val request1 : ?error:EzRequest.error_handler ->
    ('a, 'b) EzAPI.service1 ->
    string ->
    ?post:bool ->
    ?headers:(string * string) list ->
    ?params:(EzAPI.param * EzAPI.arg_value) list ->
    ('b -> unit) -> 'a -> unit

end = struct
  let request0 ?error s name ?post ?headers ?params cont =
    EzXhr.get0 ?error (api name) s name ?post ?headers ?params cont ()
  let request1 ?error s name = EzXhr.get1 ?error (api name) s name
end
open Api

let page_params page page_size =
  [Service.param_page, I page;
   Service.param_number, I page_size]

let kind_param kind = Service.param_kind, S kind

let page_request0 ?error service name ?(params=[]) =
  fun page page_size cont ->
    request0 ?error service name cont ~params:(params @ (page_params page page_size))

let page_request1 ?error service name arg ?(params=[]) =
  fun page page_size cont ->
    request1 ?error service name cont arg ~params:(params @ (page_params page page_size))

module Node_state = struct

  let request_timestamps update =
    request0 V1.node_timestamps "Node.timestamps"
      ~error:(fun _status _content -> update [])
      update

end

(** Toplevel requests with handler *)
let blocks ?(params=[]) ?(log_msg="Toplevel.blocks") handler =
    request0 ~params V1.blocks log_msg (fun blocks -> handler blocks)

let head ?(params=[]) ?(log_msg="Toplevel.head") handler =
  request0 ~params V1.head log_msg (fun h -> handler h)

(* ******* *)

module Home = struct

  let level () = request1 V1.level "Home.level" Home_ui.update_progress

  let last_volume block =
    Home_ui.update_leftbox_volume block.volume ;
    Home_ui.update_leftbox_baker block.baker

  let heads () =
    let transactions =
      request1 V1.operations_bh "Home.transactions"
        (fun ops -> Home_ui.update_leftbox_nb_transactions (List.length ops))
        ~params:[ Service.param_type, S "Transaction" ]
    in
    blocks
      ~log_msg:"Home.heads"
      ~params:[ Service.param_number, I 6 ]
      (fun blocks -> Home_ui.update_blocks blocks;
        match blocks with
        | hd :: _tl ->
          last_volume hd ;
          Home_ui.update_leftbox_timestamp @@
          Date.to_string hd.timestamp ;
          transactions hd.hash ;
          (level()) hd.hash
        | [] -> assert false)

  let marketcap ()  =
    request0 V1.marketcap "Home.marketcap"
      (fun m ->
         request0 V1.supply "Home.supply"
           (fun supply_info ->
              Home_ui.update_leftbox_marketcap
                m.price_usd m.price_btc m.volume_usd_24 m.percent_change_1
                m.percent_change_24 m.percent_change_7 m.last_updated supply_info))

  let request () =
    heads () ;
    Node_state.request_timestamps Home_ui.update_state_row ;
    if Infos.api.api_config.conf_has_marketcap then marketcap ()

  let mini_stats () =
    request0 V1.mini_stats "Charts.mini_stats" Home_ui.update_stats

  let h24_stats () =
    request0 V1.h24_stats "Home.h24_stats" Home_ui.update_h24

end

let confirmation ?head_level update bhash =
  level bhash (fun blevel ->
      match head_level with
      | None ->
        head
          ~log_msg:"Operation+Block.confirmation(head)"
          (fun head -> update bhash blevel (head.level - blevel))
      | Some head_level ->
        update bhash blevel (head_level - blevel)
    )

module Block = struct

  let block_uncles block =
    request1 V1.nb_uncles "Block.nb_uncles"
      (fun nb -> Block_ui.update_block_uncles block nb)
      block.level

  let block_succ block =
    request1 V1.block_succ "Block.succ" (Block_ui.update_succ block) block.hash

  let endorsements ~cycle hash =
    request1 V1.endorsements_level (Printf.sprintf "Block.endorsements [%s]" hash)
      (Block_ui.update_block_endorsements ~cycle hash)

  let get_operations kind update hash =
    request1 V1.nb_operations_hash (Printf.sprintf "Block.number_%s" kind)
      ~params:[ Service.param_type, S kind ]
      (fun nrows -> update ~nrows
          (page_request1 V1.operations_bh (Printf.sprintf "Block.%s" kind) hash
             ~params:[ Service.param_type, S kind ]))
      hash

  let originations =
    get_operations "Origination" Block_ui.update_block_originations

  let endorsements_in_this_block =
    get_operations "Endorsement" Block_ui.update_block_included_endorsements

  let delegations =
    get_operations "Delegation" Block_ui.update_block_delegations

  (* let nonces =
   *   get_operations "Nonce" Block_ui.update_block_nonce_revelations *)

  let activations =
    get_operations "Activation" Block_ui.update_block_activations

  let transactions =
    get_operations "Transaction" Block_ui.update_block_transactions

  let block hash =
    request1 V1.block "Block.block"
      (fun block ->
         request1 V1.level "Level.level"
           (fun level_details ->
              request0 V1.snapshot_levels "Block.snapshot_level"
                (fun snapshots ->
                   head ~log_msg:"Block.head"
                     (fun head ->
                        Block_ui.update_block_summary level_details block
                          (List.mem block.level snapshots);
                        !Block_ui.update_block_baker block ;
                        let cycle = level_details.lvl_cycle in
                        endorsements ~cycle hash block.level ;
                        if head.level <> block.level then block_succ block ;
                        block_uncles block;
                        if block.distance_level = 0 then
                          confirmation ~head_level:head.level
                            Block_ui.update_confirmation hash)
                )
               ) block.hash)
      ~error:(fun _status _content ->
          let content = Search.not_found hash in
          Common.update_main_content content)
      hash

  let request hash =
    Block_ui.update_tabs ();
    block hash ;
    transactions hash ;
    originations hash ;
    endorsements_in_this_block hash ;
    delegations hash ;
    activations hash
    (* nonces hash ; *)
end

let with_price_usd f =
  if Infos.api.api_config.conf_has_marketcap then
    request0 V1.marketcap "Home.marketcap"
      ~error:(fun _ _ -> f None)
      (fun marketcap -> f (Some marketcap.price_usd))
  else f None

module Operation = struct

  let endorsements op args block_hash =
    request1 V1.level "Operation.endorsements.level"
      (fun lvl ->
         request1 V1.endorsements_level "Operation.endorsements"
           (fun ops ->
              let ops = Common.get_endorsements ops in
              let endorse_info = (ops, lvl) in
              Operation_ui.update_operation_summary ~endorse_info op args ;
              timestamp op.op_block_hash (Operation_ui.update_timestamp op.op_hash) ;
              confirmation Operation_ui.update_confirmation op.op_block_hash)
           lvl.lvl_level) block_hash

  let operation hash args =
    let block_hash = List.find_opt (fun (k, _v) -> k = "block_hash") args in
    let params = match block_hash with
      | None -> []
      | Some (_k, v) -> [Service.param_block_hash, S v] in
    with_price_usd (fun price_usd ->
        request1 V1.operation "Operation.operation"
          ~params
          (fun op ->
             match op.op_type with
             | Sourced Consensus Endorsement endorse ->
               endorsements op args endorse.endorse_block_hash
             | _ ->
               Operation_ui.update_operation_summary
                 ?price_usd op args ;
               timestamp op.op_block_hash (Operation_ui.update_timestamp op.op_hash) ;
               confirmation Operation_ui.update_confirmation op.op_block_hash ;
               Operation_ui.update_tabs ()
          )
          ~error:(fun _status _content ->
              let content = Search.not_found hash in
              Common.update_main_content content
            )
          hash)

  let request hash args =
    operation hash args
end

module Account = struct

  let baking_required_balance hash blce delegate =
    request1 V1.required_balance "Account.required_balance"
      (fun required ->
         match delegate with
         | Some delegate when hash = delegate.tz ->
           request1 V1.node_deactivated_grace_period "Account.deactivated"
             (fun (_is_deactivated, grace_period) ->
                Account_ui.update_baking_info hash
                  ~grace_period blce required )
             hash
         | _ ->
           Account_ui.update_baking_info hash blce required)
      hash

  let bonds_rewards ?price_usd hash blce =
    request1 V1.account_bonds_rewards "Account.bonds_rewards"
      (fun br ->
         request1 V1.extra_bonds_rewards "Account.extra_bonds_rewards"
           (fun extra ->
              Account_ui.update_bonds_rewards ?price_usd hash br extra blce)
           hash)
      hash

  let account_details hash on_finish =
    request1 V1.node_account "Account.details"
      ~error:(fun _status _content ->
          Account_ui.update_details_404 @@ Common.hash_to_name hash;
          on_finish ?price_usd:None hash 0L None)
      (fun details ->
         with_price_usd
           (fun price_usd ->
              request1 V1.account_status "Account.extended_details"
                (fun ext ->
                   let delegate = snd details.acc_dlgt in
                   Account_ui.update_account_code
                     details.acc_name details.acc_script;
                   match delegate with
                   | Some dg when dg.tz = details.acc_name.tz ->
                     request1
                       V1.node_deactivated_grace_period
                       "Account.deactivated"
                       (fun (is_deactivated, grace_period) ->
                          request1 V1.operations_bh "Account.revelation"
                            (fun revelations ->
                               request1
                                 V1.operations_bh
                                 "Account.activation"
                                 (fun activates ->
                                    request1
                                      V1.node_staking_balance
                                      "Account.staking_balance"
                                      (fun staking_balance ->
                                         Account_ui.update_details
                                           ?price_usd
                                           details ext
                                           details.acc_node_timestamp
                                           is_deactivated grace_period
                                           revelations activates
                                           (Some staking_balance) ;
                                         !Account_ui.update_logo_payout hash;
                                         on_finish
                                           ?price_usd
                                           hash details.acc_balance delegate
                                      ) hash
                                      ~error:(fun _status _content ->
                                          Account_ui.update_details
                                            ?price_usd
                                            details ext
                                            details.acc_node_timestamp
                                            is_deactivated grace_period
                                            revelations activates None ;
                                          !Account_ui.update_logo_payout hash;
                                          on_finish
                                            ?price_usd
                                            hash details.acc_balance delegate)
                                 ) hash
                                 ~params:[ Service.param_type, S "Activation" ]
                            ) hash
                            ~params:[ Service.param_type, S "Reveal" ]
                       ) hash
                   | _ ->
                     request1 V1.operations_bh "Home.revelation"
                       (fun revelations ->
                          request1 V1.operations_bh "Home.activation"
                            (fun activates ->
                               Account_ui.update_details
                                 details ext details.acc_node_timestamp
                                 false 0
                                 revelations activates None ;
                               !Account_ui.update_logo_payout hash;
                               on_finish
                                 ?price_usd
                                 hash details.acc_balance delegate
                            ) hash
                            ~params:[ Service.param_type, S "Activation" ]
                       ) hash
                       ~params:[ Service.param_type, S "Reveal" ]
                ) hash)
      ) hash

  let transactions_number hash =
    request1 V1.nb_operations_hash "Account.number_transactions"
      ~params:[ Service.param_type, S "Transaction" ]
      (Account_ui.update_account_transactions_number hash)
      hash

  let transactions hash =
    request1 V1.nb_operations_hash "Account.number_transactions"
      ~params:[ Service.param_type, S "Transaction" ]
      (fun nrows ->
         Account_ui.update_account_transactions hash ~nrows
           (page_request1 V1.operations_bh "Account.transactions" hash
              ~params:[ Service.param_type, S "Transaction" ]))
      hash

  let delegations_number hash =
    request1 V1.nb_operations_hash "Account.number_delegations"
      ~params:[ Service.param_type, S "Delegation" ]
      (fun nrows_delegation ->
         request1 V1.nb_operations_hash "Account.number_originations_delegate"
           ~params:[ Service.param_type, S "Origination";
                     Service.param_delegate, S "true"]
           (fun nrows_or ->
              request1 V1.node_nb_delegated_contracts "Account.nb_delegated_contracts"
                (fun nrows_deleguees ->
                   let nrows =  max nrows_delegation @@
                     max nrows_deleguees nrows_or in
                   Account_ui.update_account_delegations_number nrows)
                hash
                ~error: (fun _status _content ->
                    let nrows = max nrows_delegation nrows_or in
                    Account_ui.update_account_delegations_number nrows))
           hash)
      hash

  let delegations hash =
    request1 V1.nb_operations_hash "Account.number_delegations"
      ~params:[ Service.param_type, S "Delegation" ]
      (fun nrows_delegation ->
         Account_ui.update_account_delegations hash ~nrows:nrows_delegation
           (page_request1 V1.operations_bh "Account.delegations" hash
              ~params:[ Service.param_type, S "Delegation" ]) ;
         request1 V1.nb_operations_hash "Account.number_originations_delegate"
           ~params:[ Service.param_type, S "Origination";
                     Service.param_delegate, S "true"]
           (fun nrows_or ->
              Account_ui.update_account_originations_delegate hash ~nrows:nrows_or
                (page_request1 V1.operations_bh "Account.originations_delegate" hash
                   ~params:[ Service.param_type, S "Origination";
                             Service.param_delegate, S "true" ]) ;
              request1 V1.node_nb_delegated_contracts "Account.nb_delegated_contracts"
                (fun nrows_deleguees ->
                   Account_ui.update_account_deleguees hash ~nrows:nrows_deleguees
                     (page_request1 V1.node_delegated_contracts
                        "Account.delegated_contracts" hash);
                )
                ~error:(fun _status _content ->
                    Account_ui.update_account_no_deleguees ())
                hash)
           hash)
      hash

  let originations_number hash =
    request1 V1.nb_operations_hash "Account.number_originations"
      ~params:[ Service.param_type, S "Origination" ]
      Account_ui.update_account_originations_number hash

  let originations hash =
    request1 V1.nb_operations_hash "Account.number_originations"
      ~params:[ Service.param_type, S "Origination" ]
      (fun nrows ->
         Account_ui.update_account_originations hash ~nrows
           (page_request1 V1.operations_bh "Account.originations" hash
              ~params:[ Service.param_type, S "Origination" ]))
      hash

  let endorsements_number hash =
    request1 V1.nb_operations_hash "Account.number_endorsements"
      ~params:[ Service.param_type, S "Endorsement" ]
      Account_ui.update_account_endorsements_number hash

  let endorsements hash =
    request1 V1.nb_operations_hash "Account.number_endorsements"
      ~params:[ Service.param_type, S "Endorsement" ]
      (fun nrows ->
         Account_ui.update_account_endorsements ~nrows
           (page_request1 V1.operations_bh "Account.endorsements" hash
              ~params:[ Service.param_type, S "Endorsement" ]))
      hash

  let bakings_status hash =
    request1 V1.nb_bakings "Account.number_bakings"
      (fun nb_bak ->
         request1 V1.nb_baker_rights "Account.number_baking_rights"
           (fun nb_bak_rights ->
              request1 V1.nb_bakings_endorsement "Account.number_baking_endorsements"
                (fun nb_end ->
                   request1 V1.nb_endorser_rights "Account.number_endorsement_rights"
                     (fun nb_end_rights ->
                        let active =
                          (nb_bak + nb_bak_rights + nb_end + nb_end_rights) <> 0 in
                        Account_ui.update_account_bakings_status active
                     ) hash
                ) hash
           ) hash
      ) hash;
    if String.length hash > 1 then
      match String.sub hash 0 2 with
      | "tz" ->
        request1 V1.nb_cycle_rewards "Account.number_cycle_rewards"
          (fun nrows -> Account_ui.update_account_rewards_status (nrows <> 0)) hash
      | "TZ" | "KT" ->
        request1 V1.nb_cycle_delegator_rewards "Account.number_cycle_delegator_rewards"
          (fun nrows ->
             Account_ui.update_account_rewards_status (nrows <> 0)) hash
      | _ -> ()

  let bakings ?cycle hash =
    let params = match cycle with
      | None -> []
      | Some cycle -> [Service.param_cycle, I cycle] in
    request1 V1.nb_bakings "Account.number_bakings" ~params
      (fun nrows ->
         Account_ui.update_account_bakings ?cycle ~nrows
           (page_request1 V1.bakings "Account.bakings" hash ~params))
      hash

  let bakings_endorsement ?cycle hash =
    let params = match cycle with
      | None -> []
      | Some cycle -> [Service.param_cycle, I cycle] in
    request1
      V1.nb_bakings_endorsement "Account.number_bakings_endorsement" ~params
      (fun nrows ->
         Account_ui.update_account_bakings_endorsement ?cycle ~nrows
           (page_request1 V1.bakings_endorsement "Account.bakings_endorsement" hash ~params))
      hash

  let baking_rights ?cycle hash =
    let params = match cycle with
      | None -> []
      | Some cycle -> [Service.param_cycle, I cycle] in
    request1
      V1.nb_baker_rights "Account.number_cycle_baker_rights" ~params
      (fun nrows -> Account_ui.update_account_baking_rights ?cycle ~nrows
          (page_request1
             V1.baker_rights "Account.cycle_baker_rights" hash ~params))
      hash

  let endorsement_rights ?cycle hash =
    let params = match cycle with
      | None -> []
      | Some cycle -> [Service.param_cycle, I cycle] in
    request1
      V1.nb_endorser_rights "Account.number_cycle_endorser_rights" ~params
      (fun nrows -> Account_ui.update_account_endorsement_rights ?cycle ~nrows
          (page_request1
             V1.endorser_rights "Account.cycle_endorser_rights" ~params
             hash))
      hash

  let cycle_rewards ?cycle hash sbalance rewards =
    let params = match cycle with
      | None -> []
      | Some cycle -> [Service.param_cycle, I cycle] in
    request1
      V1.nb_delegators "Account.number_delegators" ~params
      (fun nrows -> Account_ui.update_account_rewards ?cycle sbalance rewards ~nrows
          (page_request1
             V1.rewards_split_fast "Account.rewards_split" hash ~params))
      hash

  let rolls_history hash =
    request1 V1.rolls_history "Account.rolls_history"
      ~params:(page_params 0 50)
      (Baking_ui.update_chart_div hash)
      hash

  let bakings_tables hash =
    let rec close_bakings () =
      request1 V1.last_baking_and_endorsement "Account.last_baking_and_endorsement"
        (fun last_infos ->
           request1 V1.next_baking_and_endorsement "Account.next_baking_and_endorsement"
             (fun next_infos ->
                Account_ui.update_close_baking_and_endorsement last_infos next_infos
                  close_bakings)
             hash)
        hash in
    close_bakings ();

    request1 V1.nb_bakings_history "Account.number_bakings_history"
      (fun nrows -> let nrows = nrows in
        Account_ui.update_baking_history ~nrows
          (page_request1 V1.bakings_history "Account.bakings_history" hash)
          baking_rights bakings hash)
      hash;

    request1 V1.nb_endorsements_history "Account.number_endorsements_history"
      (fun nrows -> let nrows = nrows in
        Account_ui.update_endorsement_history ~nrows
          (page_request1 V1.endorsements_history "Account.endorsement_history" hash)
          endorsement_rights bakings_endorsement hash)
      hash;

    rolls_history hash

  let rewards_tables hash =
    if String.length hash > 1 then
      match String.sub hash 0 2 with
      | "tz" ->
        request1 V1.nb_cycle_rewards "Account.number_cycle_rewards"
          (fun nrows ->
             Account_ui.update_rewards_history ~nrows
               (page_request1 V1.rewards_split_cycles "Account.rewards_split_cycles" hash)
               cycle_rewards hash)
          hash
      | "TZ" | "KT" ->
        request1 V1.nb_cycle_delegator_rewards "Account.number_cycle_delegator_rewards"
          (fun nrows ->
             Account_ui.update_delegator_rewards_history ~nrows
               (page_request1 V1.delegator_rewards_with_details
                  "Account.delegator_rewards" hash))
          hash
      | _ -> ()

  let balance_history ?price_usd hash =
    request1
      V1.balance_history
      "Balance_update.balance_history"
      ~params:[]
      (fun hist ->
        request0
          V1.head
          "Balance_updates.level"
          (fun head ->
            let level = head.level in
            let current_cycle = Infos.cycle_from_level ~cst:(Infos.constants ~cycle:0) level in
                request1
                  V1.cycle_all_rights
                  "Balance_update.cycle_all_rights"
                  ~params:[Service.param_cycle, I current_cycle]
                  (fun (baks,ends) ->
                    request1
                      V1.cycle_frozen
                      "Balance_update.cycle_frozen"
                      ~params:[Service.param_cycle, I (current_cycle - 5)]
                      (fun upcoming_unfrozen ->
                         Balance_ui.update_balance_ui
                           ?price_usd
                           hash hist current_cycle level baks ends upcoming_unfrozen
                      )
                      hash
                  ) hash
          )
      ) hash

  let balance_update_number hash =
    request1
      V1.balance_updates_number
      "Balance_update.balance_update_number"
      ~params:[]
      (Account_ui.update_nb_balance_updates hash)
      hash

  let balance_updates hash =

    request1
      V1.balance_updates_number
      "Balance_update.balance_update_number"
      ~params:[]
      ~error:(fun _ _ -> balance_history hash)
      (fun nrows ->
         request0 V1.marketcap "Home.marketcap"
           (fun marketcap ->
              let price_usd = marketcap.price_usd in
              Account_ui.update_account_balance_updates
                ~price_usd
                hash
                ~nrows
                (page_request1
                   V1.balance_updates
                   "Balance_update.balance_updates"
                   hash
                   ~params:[];
                );
              balance_history ~price_usd hash
           )
           ~error:(fun _ _ ->
               Account_ui.update_account_balance_updates
                 hash
                 ~nrows
                 (page_request1
                    V1.balance_updates
                    "Balance_update.balance_updates"
                    hash
                    ~params:[]
                 );
               balance_history hash
             )
      )
      hash

  let votes_number hash =
    request1 V1.vote_graphs_account "Account.vote_graphs"
      (fun (proposals, ballots) ->
         let nrows = List.length ballots +
                     List.fold_left (fun acc (_, n, _) -> acc + n) 0 proposals in
         Account_ui.update_votes_number nrows)
      hash

  let votes hash =
    request1 V1.vote_graphs_account "Account.vote_graphs"
      (fun (proposals, ballots) ->
         let nrows = List.length ballots +
                     List.fold_left (fun acc (_, n, _) -> acc + n) 0 proposals in
         if nrows > 0 then (
           (* Account_ui.update_vote_graphs proposals ballots; *)
           Proposals_ui.update_account_votes ~nrows
             (page_request1 V1.votes_account "Account.votes" hash))
      )
      hash

  let request hash filters =
    transactions_number hash;
    delegations_number hash;
    originations_number hash;
    endorsements_number hash;
    bakings_status hash;
    balance_update_number hash;
    votes_number hash;
    account_details hash (fun ?price_usd hash balance delegate ->
        bonds_rewards ?price_usd hash balance ;
        baking_required_balance hash balance delegate
        (* roll_number hash (bonds_rewards hash balance) ; *)
      );
    let default = Account_ui.default_filter filters in
    Account_ui.update_transactions default hash transactions ;
    Account_ui.update_delegations default hash delegations ;
    Account_ui.update_originations default hash originations ;
    Account_ui.update_endorsements default hash endorsements ;
    Account_ui.update_bakings default hash bakings_tables ;
    Account_ui.update_balance_updates default hash balance_updates ;
    Account_ui.update_rewards default hash rewards_tables ;
    Account_ui.update_votes default hash votes
end

module Blocks = struct

  let snapshot_blocks () =
    request0 V1.nb_snapshot_blocks "Blocks.nb_snapshot_blocks"
      (fun nrows -> Blocks_ui.update_snapshot_blocks ~nrows
          (page_request0 V1.snapshot_blocks "Blocks.snapshot_blocks"))

  let request_baking_rights ?filter () =
    let params = match filter with
      | None -> []
      | Some filter -> [Service.param_search_filter, S filter] in
    request0 V1.nb_cycle_rights "Blocks.nb_pending_priorities" ~params
      (fun nrows -> Blocks_ui.update_baking_rights ~nrows
          (page_request0 V1.cycle_rights "Blocks.baking_rights" ~params));
    let params = (Service.param_future, S "false") :: params in
    request0 V1.nb_cycle_rights "Blocks.nb_pending_priorities" ~params
      (fun nrows -> Blocks_ui.update_baking_rights ~future:false ~nrows
          (page_request0 V1.cycle_rights "Blocks.baking_rights" ~params))
  let request () =
    head
      ~log_msg:"Blocks.head"
      (fun head -> let nrows = head.level+1 in
        request0 V1.snapshot_levels "Blocks.snapshot_levels"
          (fun snapshots ->
             Blocks_ui.update_blocks ~snapshots ~nrows
               (fun page page_size cont ->
                  request0 V1.blocks_with_pred_fitness "Blocks.blocks"
                    ~params:(page_params page page_size)
                    (fun l ->
                       let blocks = List.map fst l in
                       cont l; List.iter Block.block_uncles blocks))))
end

module Accounts = struct

  let account_details hash =
    if hash <> "God" then
      request1
        ~error:(fun _status _content -> Accounts_ui.update_details hash None)
        V1.node_account "Account.details"
        (fun account_details ->
           Accounts_ui.update_details hash (Some account_details)) hash

  let accounts ?(contract=None) () =
    let contract_params = match contract with
      | None -> []
      | Some contract ->
        [ Service.param_contract, S (string_of_bool contract) ] in
    request0 V1.nb_accounts "Accounts.number_accounts"
      ~params:contract_params
      (fun nrows ->
         Accounts_ui.update
           ~contract:(match contract with None -> false | Some c -> c)
           account_details
           nrows
           (page_request0 V1.accounts "Accounts.account" ~params:contract_params))

  let request ?contract () =
    accounts ?contract ()

end

module Top_accounts = struct

  let request () =
    Top_accounts_ui.update_cmd (fun selection ->
        let kind = Top_accounts_ui.selection_to_kind selection in
        match selection with
        | "Balances" ->
          request0
            V1.nb_cycle
            "Balance_updates.nb_cycle"
            (fun last_cycle ->
               Top_accounts_ui.update_ranking_table
                 (page_request0
                    V1.balance_ranking
                    "Balance_update.balance_ranking"
                    ~params:[Service.param_cycle, I last_cycle;
                             Service.param_spendable, S (string_of_bool true)])
            )
        | _ ->
          let param = kind_param kind in
          request0 V1.nb_tops "Top_accounts.nb_tops"
            (fun nrows ->
               request0 V1.context_days "Context.days"
                 (fun days -> match days with
                    | hd :: _ ->
                      request1 V1.context_stats "Context.stats"
                        (fun context ->
                           let total =
                             Top_accounts_ui.selection_to_total context selection in
                           let level = context.context_level in
                           Top_accounts_ui.update
                             nrows
                             kind
                             level
                             total
                             (page_request0 V1.tops "Top_accounts.tops" ~params:[ param ]))
                        hd
                    | _ -> ()))
            ~params:[ param ])

end

module Heads = struct

  let request () =
    match find_level () with
    | None ->
      request0
        V1.nb_heads "Heads.nb_heads"
        (fun nrows ->
           Blocks_ui.Heads.update ~nrows
             (fun page page_size cont ->
                request0
                  V1.heads_with_pred_fitness "Heads.heads"
                  ~params:(page_params page page_size)
                  (fun l -> let blocks = List.map fst l in
                    cont l; List.iter Block.block_uncles blocks)
                  ))
    | Some level ->
      (* we do this to avoid to paginate when showing uncles of a
         given level *)
      request1 V1.nb_uncles "Heads.nb_uncles"
        (fun nrows ->
           Blocks_ui.update_blocks ~alt:true ~level ~nrows
             (page_request0 V1.heads_with_pred_fitness "Heads.uncles"
                ~params:[Service.param_level, I level]))
        level

end

module Operations = struct

  (* generic updater of operations. Parameterized by the kind of the
  operation and an update function. *)
  module Updater
           (Arg :
              sig
                val op_kind : string
                val update :
                  pending:bool ->
                  nrows:int ->
                  (int -> int -> (Data_types.operation list -> unit) -> unit) ->
                  unit
              end) :
  sig
    val request : pending:bool -> unit
  end = struct

    let request ~pending =
    let params = [ Service.param_type, S Arg.op_kind ] in
    let params, kind =
      if pending then
        (Service.param_status, S "Pending") :: params,
        Printf.sprintf "Pending-%s" Arg.op_kind
      else params, Arg.op_kind
    in
    request0 V1.nb_operations (Printf.sprintf "Operations.number_%s" kind)
      ~params
      (fun nrows ->
         Arg.update ~pending ~nrows
           (page_request0 V1.operations (Printf.sprintf  "Block.%s" kind)
              ~params))
  end

  module Activations =
    Updater(struct
      let op_kind = "Activation"
      let update  = Operations_ui.Activations.update
    end)

  module Transactions =
    Updater(struct
      let op_kind = "Transaction"
      let update  = Operations_ui.Transactions.update
    end)

  module Endorsements =
    Updater(struct
      let op_kind = "Endorsement"
      let update  = Operations_ui.Endorsements.update
    end)

  module Delegations =
    Updater(struct
      let op_kind = "Delegation"
      let update  = Operations_ui.Delegations.update
    end)

  module Originations =
    Updater(struct
      let op_kind = "Origination"
      let update  = Operations_ui.Originations.update
    end)

  module Double_baking =
    Updater(struct
      let op_kind = "Double_baking_evidence"
      let update  = Operations_ui.Double_Bakings.update
    end)

  module Double_endorsement =
    Updater(struct
      let op_kind = "Double_endorsement_evidence"
      let update  = Operations_ui.Double_Endorsements.update
    end)

  module Nonces = struct

    let request () =
      request0 V1.nb_cycle "Nonces.nb_cycle"
        (fun nrows ->
           Operations_ui.Nonces.update_nonces ~nrows
             (page_request0 V1.nonces "Nonces.nonces"))
  end

  let update_activation_alert () =
    request0 V1.activated_balances "Operations.activated_balances"
      (fun blc -> Operations_ui.update_activation_alert blc)

end

module Network = struct

  let request () =
    let params = match Jsloc.find_arg "state" with
      | Some "running"
      | None -> [ Service.param_peers, S "running" ]
      | _ -> []
    in
    request0 V1.country_stats "Network_stats" ~params Network_stats_ui.update_map;
    request0 V1.nb_network_peers "Network.nb" ~params
      (fun nrows -> Network_stats_ui.update_peers ~nrows
          (page_request0 V1.network_stats "Network.stats.network" ~params))
end

module Charts = struct

  let request_bakers () =
    request0 V1.nb_cycle "Charts.nb_cycle"
      (fun last_cycle ->
         Charts_ui.update_bakers_cmd last_cycle
           (fun cycle ->
              let params =
                if cycle = "All" then []
                else [ Service.param_cycle, I (int_of_string cycle) ] in
              request0 V1.bakers "Charts.bakers"
                (fun bakers ->
                   Charts_ui.update_bakers_chart (Array.of_list bakers))
                ~params
              ))

  let request_blocks_per_day () =
    request0 V1.blocks_per_day "Charts.blocks_per_day"
      Charts_ui.update_blocks_per_day

  let request_delay_per_day () =
    request0 V1.blocks_per_day "Charts.blocks_per_day"
      Charts_ui.update_delay_per_day

  let request_bakers_per_day () =
    request0 V1.bakers_per_day "Charts.bakers_per_day"
      Charts_ui.update_bakers_per_day

  let request_priorities_per_day () =
    request0 V1.priorities_per_day "Charts.priorities_per_day"
      Charts_ui.update_priorities_per_day

  let request_operations_per_day () =
    request0 V1.operations_per_day "Charts.operations_per_day"
      Charts_ui.update_operations_per_day

  let request_operations_per_block_per_day () =
    request0 V1.operations_per_block_per_day "Charts.operations_per_block_per_day"
      Charts_ui.update_operations_per_block_per_day

  let request_fees_per_day () =
    request0 V1.fees_per_day "Charts.fees_per_day"
      Charts_ui.update_fees_per_day

  let request_volume_per_day () =
    request0 V1.volume_per_day "Charts.volume_per_day"
      Charts_ui.update_volume_per_day

  let request_market_prices () =
    request0 V1.market_prices "Charts.market_prices"
      Charts_ui.update_market_prices
end

module Server = struct

  let info redraw =
    request0 V1.api_server_info "Server.info"
      (fun info ->
         Infos.api.api_config <- info.api_config ;
         Infos.api.api_versions <- info.api_versions ;
         Infos.api.api_date <- info.api_date ;
         Format_date.set_server_date info.api_date ;
         redraw ()
      )

end

module Rolls_distribution = struct

  let request () =
    request0 V1.max_roll_cycle "Rolls_distribution.max_cycle"
      (fun max_cycle ->
         Rolls_distribution_ui.update_cmd max_cycle
           (fun cycle ->
              let cycle = int_of_string cycle in
              request1 V1.rolls_distribution "Rolls_distribution"
                (Rolls_distribution_ui.update_rolls_distrib cycle)
                cycle))
end

module Health = struct

  let stats () =
    head
      ~log_msg:"Health.head"
      (fun head ->
         request1 V1.level "Health.level"
           (fun level -> Health_stats_ui.update_cmd level.lvl_cycle
               (fun cycle ->
                  let cycle = int_of_string cycle in
                  request1 V1.health_stats "Health.stats"
                    (Health_stats_ui.update_health_page cycle)
                    cycle))
           head.hash)

  let request = stats
end

module Context = struct

  let stats () =
    request0 V1.context_days "Context.days"
      (fun days -> match days with
         | _hd :: _ ->
           Context_stats_ui.update_cmd days (fun day ->
               request1 V1.context_stats "Context.stats"
                 Context_stats_ui.update_context_page
                 day)
         | _ -> Context_stats_ui.update_context_empty ())

  let request = stats
end

module Search = struct

  let search_account update hash =
    request1 V1.nb_search_account "Search.nb.account"
      (fun nb ->
         if nb = 0 then
           update nb hash []
         else
           request1 V1.search_account "Search.account"
             (update nb hash)
             hash)
      hash

  let search hash =
    match hash.[0], hash.[1] with
    | 'o', ( 'n' | 'p' | 'q' ) ->
      request1 V1.nb_search_operation "Search.nb.operation"
        (fun nb ->
           if nb = 0 then Search.update_search 0 hash []
           else
             request1 V1.search_operation "Search.operation"
               (fun res ->
                  Search.update_search nb hash
                    (List.map (fun s ->
                         { tz = s; alias = None }, ""
                       ) res)
               )
               hash)
        hash
    | 'B', ( 'K' | 'L' | 'M' ) ->
      request1 V1.nb_search_block "Search.nb.block"
        (fun nb ->
           if nb = 0 then Search.update_search 0 hash []
           else
             request1 V1.search_block "Search.block"
               (fun res ->
                  Search.update_search nb hash
                    (List.map (fun s ->
                         { tz = s; alias = None }, ""
                       ) res)
               )
               hash)
        hash
    | _ -> search_account Search.update_search hash

  let request = search
end

module Protocols = struct
  let protocols_request () =
    request0 V1.nb_protocol "Protocol.nb_protocol"
      (fun nrows ->
         Protocols_ui.update ~nrows
           (page_request0 V1.protocols "Protocol.protocols"))

  let proposals_request args =
    let period = match List.find_opt (fun (x,_) -> x = "period") args with
      | None -> None
      | Some (_, period) -> int_of_string_opt period in
    let params, current = match period with
      | None -> [], true
      | Some period -> [ Service.param_period, I period ], false in
    request0 V1.voting_period_info "Protocol.voting_period_info" ~params
      (fun (period, voting_period_kind, cycle, level, period_max, period_status, quorum) ->
         if period = 0 then
           Proposals_ui.update_controls ~left:false ~period ()
         else if current || period_max then
           Proposals_ui.update_controls ~right:false ~period ();
         Proposals_ui.update_progress
           period voting_period_kind cycle level period_status;
         let params = [ Service.param_period, I period ] in
         match voting_period_kind with
         | NProposal ->
           request1 V1.total_proposal_votes "Protocol.total_proposal_votes"
             (fun x ->
                request0 V1.proposals "Protocol.proposals" ~params
                  (Proposals_ui.update_proposals x)) period
         | NTesting_vote | NPromotion_vote ->
           request1 V1.total_voters "Protocol.total_voters"
             (fun totals ->
                let params = [
                  Service.param_period_kind,
                  S (Tezos_utils.string_of_voting_period_kind voting_period_kind) ] in
                request1 V1.ballots "Protocol.ballots" ~params
                  (Proposals_ui.update_ballots period quorum totals) period) period
         | NTesting ->
           let params = [
             Service.param_period_kind,
             S (Tezos_utils.string_of_voting_period_kind voting_period_kind) ] in
           request1 V1.testing_proposal "Protocol.testing_proposal" ~params
             Proposals_ui.update_testing period
      )

  let all_proposals_request () =
    request0 V1.nb_proposals "Protocol.nb_proposals"
      (fun nrows ->
         Proposals_ui.update_all_proposals ~nrows
           (page_request0 V1.proposals "Protocol.proposals"))

  let proposal_votes_request ?period hash =
    let params = match period with
      | None -> []
      | Some period -> [Service.param_period, I period] in
    request1 V1.nb_proposal_votes "Protocol.nb_proposal_votes"
      (fun (nrows, nvotes) ->
         Proposals_ui.update_votes ~nrows nvotes
           (page_request1 V1.proposal_votes "Protocol.proposal_votes" ~params hash) hash
      ) hash

  let ballot_votes_request ?period ?ballot hash =
    let params, empty = match ballot, period with
      | None, _ -> [], "No ballots for this proposal"
      | Some ballot, None ->
        [Service.param_ballot, S ballot],
        Printf.sprintf "No ballot %S for this proposal" ballot
      | Some ballot, Some period ->
        [Service.param_ballot, S ballot; Service.param_period, I period],
        Printf.sprintf "No ballot %S for this proposa at period %d" ballot period
    in
    request1 V1.nb_ballot_votes "Protocol.nb_ballot_votes" ~params
      (fun (nrows, nvotes) ->
         Proposals_ui.update_votes ~empty ~nrows nvotes
           (page_request1 V1.ballot_votes "Protocol.ballot_votes" ~params hash) hash
      ) hash

  let votes_request () =
    let period = Misc.unoptf None int_of_string_opt (Jsloc.find_arg "period") in
    match Jsloc.find_arg "proposal" with
    | None -> ()
    | Some hash ->
      match Jsloc.find_arg "vote_kind" with
      | Some "proposal" -> proposal_votes_request ?period hash
      | Some "ballot" ->
        let ballot = Jsloc.find_arg "ballot" in
        ballot_votes_request ?period ?ballot hash
      | _ -> ()



end

module CSV = struct
  let transactions hash update =
    match Infos.www.www_recaptcha_key, Infos.www.www_csv_server with
    | Some site_key, Some (csv_api, _) ->
      Recaptcha.check site_key (fun token ->
          EzXhr.get1 (base_of_host csv_api)
            V1.transaction_account_csv "CSV.transaction_account"
            ~params:[ Service.param_token, S token ]
            (fun s -> update s ) hash )
    | _ -> Js_utils.log "No site key or no csv api"
end
