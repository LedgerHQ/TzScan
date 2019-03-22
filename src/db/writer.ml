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

let debug fmt = Utils.debug !Debug_constants.debug_writer fmt

let dbh = PGOCaml.connect ~database:TzscanConfig.database ()

let () =
  EzPG.upgrade_database dbh ~upgrades: Pg_update.upgrades

let pg_lock_r = ref false
let pg_lock f =
  if !pg_lock_r
  then f ()
  else
    begin
      pg_lock_r := true;
      PGSQL(dbh) "BEGIN";
      f ();
      PGSQL(dbh) "COMMIT";
      pg_lock_r := false
    end

let head () =
  PGSQL(dbh) "SELECT * FROM block \
              WHERE distance_level = 0 ORDER BY level DESC LIMIT 1"
  |> Pg_helper.rows_to_option
  |> Pg_helper.omap Pg_helper.block_of_tuple_noop

let is_block_registered hash =
  let row = PGSQL(dbh) "SELECT hash FROM block WHERE hash = $hash" in
  match row with
  | [ _hash ] -> true
  | _ -> false


let register_tezos_user hash =
  let insert hash contract =
    PGSQL(dbh) "INSERT INTO tezos_user (hash,contract) \
                VALUES ($hash,$contract) ON CONFLICT (hash) DO NOTHING" in
  let update hash edpk contract =
    PGSQL(dbh) "INSERT INTO tezos_user (hash,contract,edpk) \
                VALUES ($hash,$contract,$edpk) ON CONFLICT (hash) \
                DO UPDATE SET hash=$hash,contract=$contract,edpk=$edpk" in
  debug "[Writer] Registering user %s\n%!" hash ;
  if hash = "" || String.length hash = 1 then insert hash false (* HACK *)
  else
    begin match String.sub hash 0 2 with
      | "tz" -> insert hash false
      | "TZ" | "KT" -> insert hash true
      | "ed" -> update (Blake2b.pk_to_tz1 hash) hash false
      | _ -> debug "[Writer] invalid user hash %S\n%!" hash end;
  if PGSQL(dbh) "SELECT tz FROM operation_count_user WHERE tz = $hash" = [] then
    PGSQL(dbh) "INSERT INTO operation_count_user(tz) VALUES($hash)"

let register_cycle_count_baker cycle hash =
  if PGSQL(dbh) "SELECT cycle, tz FROM cycle_count_baker WHERE cycle = $cycle \
                 AND tz = $hash" = [] then
    PGSQL(dbh) "INSERT INTO cycle_count_baker(cycle, tz) VALUES($cycle, $hash)"

let register_header header =
  let level = Int64.of_int header.header_level in
  let proto = Int64.of_int header.header_proto in
  let predecessor = header.header_predecessor in
  let timestamp = Pg_helper.cal_of_date header.header_timestamp in
  let validation_pass = Int64.of_int header.header_validation_pass in
  let operations_hash = header.header_operations_hash in
  let fitness = header.header_fitness in
  let context = header.header_context in
  let priority = Int32.of_int header.header_priority in
  let seed = header.header_seed_nonce_hash in
  let nonce = header.header_proof_of_work_nonce in
  let signature = header.header_signature in
  let row =
    PGSQL(dbh) "SELECT * FROM header WHERE \
                level = $level AND \
                priority = $priority AND \
                fitness = $fitness AND \
                context = $context AND \
                signature = $signature" in
  match row with
  | [] ->
    PGSQL(dbh) "INSERT INTO header \
                (level, proto, predecessor, timestamp, validation_pass, \
                operations_hash, fitness, context, priority, \
                commited_nonce_hash, pow_nonce, signature) \
                VALUES \
                ($level, $proto, $predecessor, $timestamp, $validation_pass, \
                $operations_hash, $fitness, $context, $priority, \
                $seed, $nonce, $signature) \
                ON CONFLICT DO NOTHING"
  | _ -> ()

let register_protocol proto =
  let name = proto in
  let hash = proto in
  PGSQL(dbh) "INSERT INTO protocol (hash, name) \
              VALUES ($hash, $name) ON CONFLICT (hash) DO NOTHING"

let register_block block level distance operation_count volume fees =
  let hash = block.node_hash in
  let predecessor_hash = block.node_header.header_predecessor in
  let fitness = block.node_header.header_fitness in
  let baker = block.node_metadata.meta_header.header_meta_baker in
  let timestamp = Pg_helper.cal_of_date block.node_header.header_timestamp in
  let validation_pass = block.node_header.header_validation_pass in
  let protocol = block.node_protocol in
  let test_protocol = block.node_protocol in
  let network = block.node_chain_id in
  let test_network, test_network_expiration =
    match block.node_metadata.meta_test_chain_status with
    | CSNot_running -> "Not running", "Not running"
    | CSForking { protocol ; expiration } ->
      Printf.sprintf "Forking %S" protocol,
      expiration
    | CSRunning { chain_id ; genesis ; protocol ; expiration } ->
      Printf.sprintf "Running [%S][%S][%S]" chain_id genesis protocol,
      expiration in
  let priority  = Int32.of_int block.node_header.header_priority  in
  let commited_nonce_hash  = block.node_header.header_seed_nonce_hash in
  let pow_nonce = block.node_header.header_proof_of_work_nonce in
  let proto = block.node_header.header_proto in
  let data = "--" in            (* TODO remove data field *)
  let signature = block.node_header.header_signature in
  let protocol_hash = protocol in
  let test_protocol_hash = protocol in
  let validation_pass = Int64.of_int validation_pass in
  let proto = Int64.of_int proto in
  let lvl_level = Int64.of_int level.node_lvl_level in
  let lvl_level_position = Int64.of_int level.node_lvl_level_position in
  let lvl_cycle = Int64.of_int level.node_lvl_cycle in
  let lvl_cycle_position = Int64.of_int level.node_lvl_cycle_position in
  let lvl_voting_period = Int64.of_int level.node_lvl_voting_period in
  let lvl_voting_period_position = Int64.of_int level.node_lvl_voting_period_position in
  let voting_period_kind =
    Tezos_utils.string_of_voting_period_kind
      block.node_metadata.meta_header.header_meta_voting_period_kind in

  register_tezos_user baker ;
  register_cycle_count_baker lvl_cycle baker;
  register_protocol protocol;
  register_protocol test_protocol;
  PGSQL(dbh)
    "INSERT INTO block \
     (hash, predecessor, fitness, baker, timestamp, validation_pass, \
     proto, data, signature, \
     protocol, test_protocol, \
     network, test_network, test_network_expiration, level, level_position, \
     priority, cycle, cycle_position, voting_period, voting_period_position, \
     commited_nonce_hash, pow_nonce, distance_level, operation_count, \
     volume, fees, voting_period_kind) \
     VALUES \
     ($hash, $predecessor_hash, $fitness, $baker, $timestamp, $validation_pass, \
     $proto, $data, $signature, \
     $protocol_hash, \
     $test_protocol_hash, $network, $test_network, $test_network_expiration, \
     $lvl_level, $lvl_level_position, $priority, $lvl_cycle, \
     $lvl_cycle_position, $lvl_voting_period, $lvl_voting_period_position, \
     $commited_nonce_hash, $pow_nonce, $distance, $operation_count, \
     $volume, $fees, $voting_period_kind) \
     ON CONFLICT (hash) DO NOTHING";
  (* factor 2 is to take some space for endorsements and transactions of alternative
     chains that we don't read in operation_recent *)
  let endorsement_cut = match PGSQL(dbh) "SELECT MAX(id) FROM endorsement_last" with
    | [ Some i ] -> Int64.(sub i (mul 2L Pg_update.Constants.last_endorsements))
    | _ -> 0L in
  let transaction_cut = match PGSQL(dbh) "SELECT MAX(id) FROM transaction_last" with
    | [ Some i ] -> Int64.(sub i (mul 2L Pg_update.Constants.last_transactions))
    | _ -> 0L in
  let origination_cut = match PGSQL(dbh) "SELECT MAX(id) FROM origination_last" with
    | [ Some i ] -> Int64.(sub i (mul 2L Pg_update.Constants.last_originations))
    | _ -> 0L in
  let reveal_cut = match PGSQL(dbh) "SELECT MAX(id) FROM reveal_last" with
    | [ Some i ] -> Int64.(sub i (mul 2L Pg_update.Constants.last_reveals))
    | _ -> 0L in
  let delegation_cut = match PGSQL(dbh) "SELECT MAX(id) FROM delegation_last" with
    | [ Some i ] -> Int64.(sub i (mul 2L Pg_update.Constants.last_delegations))
    | _ -> 0L in
  let activation_cut = match PGSQL(dbh) "SELECT MAX(id) FROM activation_last" with
    | [ Some i ] -> Int64.(sub i (mul 2L Pg_update.Constants.last_activations))
    | _ -> 0L in
  PGSQL(dbh) "DELETE FROM endorsement_last WHERE id < $endorsement_cut";
  PGSQL(dbh) "DELETE FROM transaction_last WHERE id < $transaction_cut";
  PGSQL(dbh) "DELETE FROM origination_last WHERE id < $origination_cut";
  PGSQL(dbh) "DELETE FROM delegation_last WHERE id < $delegation_cut";
  PGSQL(dbh) "DELETE FROM activation_last WHERE id < $activation_cut";
  PGSQL(dbh) "DELETE FROM reveal_last WHERE id < $reveal_cut"

let get_status_from_manager_metadata = function
  | None -> false (* Pending *)
  | Some meta ->
    match meta.manager_meta_operation_result with
    | None -> assert false
    | Some meta ->
      match meta.meta_op_status with
      | None -> assert false
      | Some status -> status <> "applied"

let get_paid_storage_diff_size_from_manager_metadata = function
  | None -> 0L
  | Some meta ->
    match meta.manager_meta_operation_result with
    | None -> assert false
    | Some meta ->
      match meta.meta_op_paid_storage_size_diff with
      | None -> 0L
      | Some paid -> Z.to_int64 paid

let get_internal_burn = function
  | None -> 0L
  | Some meta ->
    List.fold_left (fun acc -> function
        | NTransaction transaction ->
          let paid =
            get_paid_storage_diff_size_from_manager_metadata
              transaction.node_tr_metadata in
          let burn =
            Int64.(mul
                     paid
                     (of_int Tezos_constants.Constants.cost_per_byte)) in
          Int64.add acc burn
        | NOrigination origination ->
          let paid =
            get_paid_storage_diff_size_from_manager_metadata
              origination.node_or_metadata in
          let burn =
            Int64.(mul
                     paid
                     (of_int Tezos_constants.Constants.cost_per_byte)) in
          Int64.add acc burn
        | _ -> acc)
      0L meta.manager_meta_internal_operation_results

let register_genesis genesis =
  let dummy_level = {
    node_lvl_level = 0 ;
    node_lvl_level_position = 0 ;

    node_lvl_cycle = 0 ;
    node_lvl_cycle_position = 0 ;

    node_lvl_voting_period = 0 ;
    node_lvl_voting_period_position = 0 ;

    node_lvl_expected_commitment = false ;
  } in
  let orphan = { genesis with
                 node_hash = Utils.orphan_block_hash;
                 node_header = {
                   genesis.node_header with
                   header_predecessor = Utils.orphan_block_hash } }  in
  (* Special user for genesis and first block *)
  PGSQL(dbh) "INSERT INTO tezos_user (hash,contract) \
              VALUES ('God','false')";
  register_block genesis dummy_level 0L 0L 0L 0L;
  register_block orphan dummy_level Int64.minus_one 0L 0L 0L

let rec register_operation_type ?(internal=false) ?extra_info
    timestamp_op hash block_hash operations =
  let len = List.length operations in
  List.iteri (fun i op ->
      debug "[Writer] register_operation_type %S %d / %d\n%!" hash (i + 1) len ;
      match op with
      | NSeed_nonce_revelation seed_nonce_revelation ->
        let level = Int64.of_int seed_nonce_revelation.node_seed_level in
        let nonce = seed_nonce_revelation.node_seed_nonce in
        PGSQL(dbh)
          "INSERT INTO seed_nonce_revelation\
           (hash, level, nonce)\
           VALUES\
           ($hash, $level, $nonce) ON CONFLICT (hash, level) DO NOTHING"

      (* Double baking evidence *)
      | NDouble_baking_evidence dbe ->
        let accused, denouncer, lost_deposit,
            lost_rewards, lost_fees, gain_rewards =
          match dbe.node_double_bh_metadata with
          | None -> "", "", 0L, 0L, 0L, 0L
          | Some metadata ->
            let balance_updates =
              match metadata.meta_op_balance_updates with None -> [] | Some l -> l in
            List.fold_left
              (fun (accused, denouncer, lost_deposit,
                    lost_rewards, lost_fees, gain_rewards) b ->
                match b with
                (* | Debited _ | Credited _ *) | Contract _ ->
                  accused, denouncer, lost_deposit,
                  lost_rewards, lost_fees, gain_rewards
                | Rewards (tz1, _cycle, amount) ->
                  if amount < 0L then (* accused *)
                    (tz1, denouncer,
                     lost_deposit, amount, lost_fees, gain_rewards)
                  else      (* denouncer *)
                    (accused, tz1,
                     lost_deposit, lost_rewards, lost_fees, amount)
                | Fees (tz1, _cycle, fees) ->
                  (tz1, denouncer,
                   lost_deposit, lost_rewards, fees, gain_rewards)
                | Deposits (tz1, _cycle, amount) ->
                  (tz1, denouncer,
                   amount, lost_rewards, lost_fees, gain_rewards))
              ("","", 0L, 0L, 0L, 0L) balance_updates in
        register_header dbe.node_double_bh1 ;
        register_header dbe.node_double_bh2 ;
        let h1_level =
          Int64.of_int dbe.node_double_bh1.header_level in
        let h1_priority = Int32.of_int dbe.node_double_bh1.header_priority in
        let h1_fitness = dbe.node_double_bh1.header_fitness in
        let h1_context = dbe.node_double_bh1.header_context in
        let h1_signature = dbe.node_double_bh1.header_signature in
        let h1_row =
          PGSQL(dbh)
            "SELECT id FROM header AS h \
             WHERE \
             h.level = $h1_level AND \
             h.priority = $h1_priority AND \
             h.fitness = $h1_fitness AND \
             h.context = $h1_context AND \
             h.signature = $h1_signature" in
        let h1 = match h1_row with
          | [ id ] -> Int64.to_int32 id
          | _ -> assert false in
        let h2_level =
          Int64.of_int dbe.node_double_bh2.header_level in
        let h2_priority = Int32.of_int dbe.node_double_bh2.header_priority in
        let h2_fitness = dbe.node_double_bh2.header_fitness in
        let h2_context = dbe.node_double_bh2.header_context in
        let h2_signature = dbe.node_double_bh2.header_signature in
        let h2_row =
          PGSQL(dbh)
            "SELECT id FROM header AS h \
             WHERE \
             h.level = $h2_level AND \
             h.priority = $h2_priority AND \
             h.fitness = $h2_fitness AND \
             h.context = $h2_context AND \
             h.signature = $h2_signature" in
            let h2 = match h2_row with
            | [ id ] -> Int64.to_int32 id
            | _ -> assert false in
        PGSQL(dbh)
          "INSERT INTO double_baking_evidence \
           (hash, header1, header2, accused, denouncer, lost_deposit, \
           lost_rewards, lost_fees, gain_rewards) \
           VALUES \
           ($hash, $h1, $h2, $accused, $denouncer, $lost_deposit, \
           $lost_rewards, $lost_fees, $gain_rewards) ON CONFLICT DO NOTHING"

      (* Double Endorsement Evidence *)
      | NDouble_endorsement_evidence dee ->
        let endorse1 = dee.node_double_endorsement1 in
        let endorse2 = dee.node_double_endorsement2 in
        let block1 = "--" in (* TODO *)
        let level1 = Int64.of_int endorse1.ndee_level in
        let slots1 = [] in      (* TODO  *)
        let block2 = "--" in (* TODO *)
        let level2 = Int64.of_int endorse2.ndee_level in
        let slots2 = [] in      (* TODO *)
        PGSQL(dbh)
          "INSERT INTO double_endorsement_evidence \
           (hash, block_hash1, level1, slots1, \
           block_hash2, level2, slots2) \
           VALUES \
           ($hash, $block1, $level1, $slots1, \
           $block2, $level2, $slots2) \
           ON CONFLICT DO NOTHING"

      (* Activation *)
      | NActivation act ->
        let pkh = act.node_act_pkh in
        let secret = act.node_act_secret in
        PGSQL(dbh)
          "INSERT INTO activation \
           (hash, pkh, secret) \
           VALUES \
           ($hash, $pkh, $secret) \
           ON CONFLICT DO NOTHING" ;
        let balance =
          begin match act.node_act_metadata with
          | None -> None
          | Some meta ->
            begin match meta.meta_op_balance_updates with
              | None -> Some 0L
              | Some l ->
                begin match l with
                  | [ bu ] ->
                    begin match bu with
                      | Contract (acc, amount) when acc = pkh ->
                        PGSQL(dbh)
                          "INSERT INTO activation_balance \
                           (hash, pkh, balance) \
                           VALUES \
                           ($hash, $pkh, $amount) \
                           ON CONFLICT DO NOTHING";
                        Some amount
                      | _ ->
                        debug
                          "[Writer] Weird balance_updates type for \
                           activation %S\n%!"
                          hash;
                        Some 0L
                    end
                  | _ ->
                    debug
                      "[Writer] Unexpected balance_updates length for \
                       activation %S - %S\n%!"
                      hash @@
                    String.concat
                      " " (List.map Utils.string_of_balance_update l);
                    None
                end
            end
        end in
        register_tezos_user pkh;
        let op_level, op_block_hash, network, timestamp_block, distance_level =
          match extra_info with
          | Some (op_level, _, op_block_hash, network, timestamp_block) ->
            Some (Int64.of_int op_level), Some op_block_hash, Some network,
            Some timestamp_block, Some (-1l)
          | _ -> None, None, None, None, None in
        PGSQL(dbh)
          "INSERT INTO activation_last \
           (hash, pkh, secret, balance, timestamp_op, op_level, op_block_hash, \
           distance_level, network, timestamp_block) \
           VALUES($hash, $pkh, $secret, $?balance, $timestamp_op, $?op_level, \
           $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        PGSQL(dbh)
          "INSERT INTO activation_all \
           (hash, pkh, secret, balance, timestamp_op, op_level, op_block_hash, \
           distance_level, network, timestamp_block) \
           VALUES($hash, $pkh, $secret, $?balance, $timestamp_op, $?op_level, \
           $?op_block_hash, $?distance_level, $?network, $?timestamp_block)"

      (* Endorsement *)
      | NEndorsement endorsement ->
        let src, slots =
          match endorsement.node_endorse_metadata with
          | None -> "", []
          | Some metadata ->
            begin match metadata.meta_op_delegate with
             | None -> "" | Some delegate -> delegate
            end,
            begin match metadata.meta_op_slots with
              | None -> [] | Some slots -> slots
            end in
        register_tezos_user src;
        let edpk = "--" in    (* TO REMOVE when db clean *)
        let level = Int64.of_int endorsement.node_endorse_block_level in
        let slot = List.map (fun s -> Some (Int64.of_int s)) slots in
        PGSQL(dbh)
          "INSERT INTO endorsement \
           (hash, source, block_level, block_hash, slot, edpk) \
           VALUES \
           ($hash, $src, $level, $block_hash, $slot, $edpk) \
           ON CONFLICT DO NOTHING";
        begin
          match extra_info with
          | Some (op_level, _priority, op_block_hash, network, timestamp) ->
            let op_level, op_cycle, distance_level =
              Int64.of_int op_level,
              Int64.of_int @@ Tezos_constants.cycle_from_level op_level,
              Int32.minus_one in
            let priority =
              match PGSQL(dbh)
                      "SELECT priority FROM block WHERE hash = $block_hash" with
              | [ p ] -> p
              | _ -> assert false in
            register_cycle_count_baker op_cycle src;
            let slots = List.map (fun s -> Some (Int32.of_int s)) slots in
            PGSQL(dbh)
              "INSERT INTO endorsement_last \
               (hash, source, block_hash, block_level, slots, priority, \
               op_block_hash, op_level, op_cycle, distance_level, network, timestamp) \
               VALUES \
               ($hash, $src, $block_hash, $level, $slots, $priority, \
               $op_block_hash, $op_level, $op_cycle, $distance_level, $network, $timestamp) \
               ON CONFLICT DO NOTHING";
            PGSQL(dbh)
              "INSERT INTO endorsement_all \
               (hash, source, block_hash, block_level, slots, priority, \
               op_block_hash, op_level, op_cycle, distance_level, network, timestamp) \
               VALUES \
               ($hash, $src, $block_hash, $level, $slots, $priority, \
               $op_block_hash, $op_level, $op_cycle, $distance_level, $network, $timestamp) \
               ON CONFLICT DO NOTHING"
          | _ -> ()
        end

      (* Proposals *)
      | NProposals proposals ->
        let src = proposals.node_prop_src in
        let voting = proposals.node_prop_voting_period in
        let proposals =
          List.map (fun str -> Some str) proposals.node_prop_proposals in
        PGSQL(dbh)
          "INSERT INTO proposal \
           (hash, source, voting_period, proposals) \
           VALUES \
           ($hash, $src, $voting, $proposals) \
           ON CONFLICT DO NOTHING";

      (* Ballot *)
      | NBallot ballot ->
        let src = ballot.node_ballot_src in
        let voting = ballot.node_ballot_voting_period in
        let proposal = ballot.node_ballot_proposal in
        let vote = Tezos_utils.string_of_ballot_vote ballot.node_ballot_vote in
        PGSQL(dbh)
          "INSERT INTO ballot \
           (hash, source, voting_period, proposal, ballot) \
           VALUES \
           ($hash, $src, $voting, $proposal, $vote) \
           ON CONFLICT DO NOTHING"

      (* Transaction *)
      | NTransaction transaction ->
        debug "[Writer]  transaction %s\n%!" hash;
        let source = transaction.node_tr_src in
        let counter = Z.to_int64 transaction.node_tr_counter in
        let fee = transaction.node_tr_fee in
        let gas_limit = Z.to_int64 transaction.node_tr_gas_limit in
        let storage_limit = Z.to_int64 transaction.node_tr_storage_limit in
        register_tezos_user source;
        let dst = transaction.node_tr_dst in
        register_tezos_user dst;
        let amount = transaction.node_tr_amount in
        let parameters = transaction.node_tr_parameters in
        let failed =
          get_status_from_manager_metadata transaction.node_tr_metadata in
        let paid =
          get_paid_storage_diff_size_from_manager_metadata
            transaction.node_tr_metadata in
        let burn =
          if internal then 0L else
            Int64.
              (add
                 (get_internal_burn transaction.node_tr_metadata)
                 (mul paid (of_int Tezos_constants.Constants.cost_per_byte))) in
        PGSQL(dbh)
          "INSERT INTO transaction \
           (hash, source, destination, \
           fee, counter, amount, parameters, gas_limit, storage_limit, \
           failed, internal, burn_tez) \
           VALUES \
           ($hash, $source, $dst, $fee, $counter, $amount, $?parameters, \
           $gas_limit, $storage_limit, $failed, $internal, $burn) \
           ON CONFLICT DO NOTHING" ;
        let op_level, op_block_hash, network, timestamp_block, distance_level =
          match extra_info with
          | Some (op_level, _, op_block_hash, network, timestamp_block) ->
            Some (Int64.of_int op_level), Some op_block_hash, Some network,
            Some timestamp_block, Some (-1l)
          | _ -> None, None, None, None, None in
        PGSQL(dbh)
          "INSERT INTO transaction_all \
           (hash, source, destination, fee, counter, amount, parameters, \
           gas_limit, storage_limit, failed, internal, timestamp_op, burn_tez, \
           op_level, op_block_hash, distance_level, network, timestamp_block) \
           VALUES \
           ($hash, $source, $dst, $fee, $counter, $amount, $?parameters, \
           $gas_limit, $storage_limit, $failed, $internal, $timestamp_op, $burn, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        PGSQL(dbh)
          "INSERT INTO transaction_last \
           (hash, source, destination, fee, counter, amount, parameters, \
           gas_limit, storage_limit, failed, internal, timestamp_op, burn_tez, \
           op_level, op_block_hash, distance_level, network, timestamp_block) \
           VALUES \
           ($hash, $source, $dst, $fee, $counter, $amount, $?parameters, \
           $gas_limit, $storage_limit, $failed, $internal, $timestamp_op, $burn, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        begin match transaction.node_tr_metadata with
          | None -> ()
          | Some metadata ->
            register_operation_type ~internal:true ?extra_info
              timestamp_op hash block_hash
              metadata.manager_meta_internal_operation_results
        end

      (* Origination *)
      | NOrigination origination ->
        debug "[Writer]  origination %s\n%!" hash;
        let source = origination.node_or_src in
        let counter = Z.to_int64 origination.node_or_counter in
        let fee = origination.node_or_fee in
        let gas_limit = Z.to_int64 origination.node_or_gas_limit in
        let storage_limit = Z.to_int64 origination.node_or_storage_limit in
        register_tezos_user source;
        let manager = origination.node_or_manager in
        register_tezos_user manager;
        let delegate =
          match origination.node_or_delegate with
            None -> "" | Some delegate -> delegate in
        register_tezos_user delegate ;
        let spendable = origination.node_or_spendable in
        let delegatable = origination.node_or_delegatable in
        let balance = origination.node_or_balance in
        let tz1 =
          match origination.node_or_metadata with
          | None -> Blake2b.originated_TZ1 hash
          | Some meta ->
            match meta.manager_meta_operation_result with
            | None -> assert false
            | Some meta ->
              match meta.meta_op_originated_contracts with
              | None | Some [] -> Blake2b.originated_TZ1 hash
              | Some (hd :: _ ) -> hd in
        let failed =
          get_status_from_manager_metadata origination.node_or_metadata in
        let paid =
          get_paid_storage_diff_size_from_manager_metadata
            origination.node_or_metadata in
        let burn =
          if internal then 0L
          else
            Int64.
              (add
                 (get_internal_burn origination.node_or_metadata)
                 (add
                    (mul paid (of_int Tezos_constants.Constants.cost_per_byte))
                    Tezos_constants.Constants.origination_burn)) in
        debug "[Writer] tz1 %S originated by %S\n%!" tz1 source ;
        register_tezos_user tz1;
        let sc_code, sc_storage =
        match origination.node_or_script with
          | None -> None, None
          | Some sc -> Some sc.sc_code, Some sc.sc_storage in
        PGSQL(dbh)
          "INSERT INTO origination \
           (hash, source, tz1, fee, \
           counter, manager, delegate, script_code, \
           script_storage_type, spendable, \
           delegatable, balance, gas_limit, storage_limit, failed, internal,\
           burn_tez) \
           VALUES \
           ($hash, $source, $tz1, $fee, \
           $counter, $manager, $delegate, $?sc_code, \
           $?sc_storage, $spendable, $delegatable, \
           $balance, \
           $gas_limit, $storage_limit, $failed, $internal, $burn) \
           ON CONFLICT DO NOTHING";
        let op_level, op_block_hash, distance_level, network, timestamp_block =
          match extra_info with
          | Some (op_level, _priority, op_block_hash, network, timestamp_block) ->
            Some (Int64.of_int op_level), Some op_block_hash, Some (-1l), Some network,
            Some timestamp_block
          | _ -> None, None, None, None, None in
        PGSQL(dbh)
          "INSERT INTO origination_last \
           (hash, source, tz1, fee, counter, manager, delegate, script_code, \
           script_storage_type, spendable, delegatable, balance, gas_limit, \
           storage_limit, failed, internal, burn_tez, timestamp_op, \
           op_level, op_block_hash, distance_level, network, timestamp_block) \
           VALUES($hash, $source, $tz1, $fee, $counter, $manager, $delegate, \
           $?sc_code, $?sc_storage, $spendable, $delegatable, $balance, $gas_limit, \
           $storage_limit, $failed, $internal, $burn, $timestamp_op, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        PGSQL(dbh)
          "INSERT INTO origination_all \
           (hash, source, tz1, fee, counter, manager, delegate, script_code, \
           script_storage_type, spendable, delegatable, balance, gas_limit, \
           storage_limit, failed, internal, burn_tez, timestamp_op, \
           op_level, op_block_hash, distance_level, network, timestamp_block) \
           VALUES($hash, $source, $tz1, $fee, $counter, $manager, $delegate, \
           $?sc_code, $?sc_storage, $spendable, $delegatable, $balance, $gas_limit, \
           $storage_limit, $failed, $internal, $burn, $timestamp_op, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        begin match origination.node_or_metadata with
        | None -> ()
        | Some metadata ->
          register_operation_type ~internal:true ?extra_info
            timestamp_op hash block_hash
            metadata.manager_meta_internal_operation_results
        end

      (* Delegation *)
      | NDelegation delegation ->
        let source = delegation.node_del_src in
        let counter = Z.to_int64 delegation.node_del_counter in
        let fee = delegation.node_del_fee in
        let gas_limit = Z.to_int64 delegation.node_del_gas_limit in
        let storage_limit = Z.to_int64 delegation.node_del_storage_limit in
        debug "[Writer]  delegation %s\n%!" hash;
        register_tezos_user source;
        let pubkey = "--" in    (* TODO no pubkey anymore *)
        let delegate = delegation.node_del_delegate in
        let failed =
          get_status_from_manager_metadata delegation.node_del_metadata in
        register_tezos_user delegate;
        PGSQL(dbh)
          "INSERT INTO delegation \
           (hash, source, pubkey, fee, counter, delegate, \
           gas_limit, storage_limit, failed, internal) \
           VALUES \
           ($hash, $source, $pubkey, $fee, $counter, $delegate, \
           $gas_limit, $storage_limit, $failed, $internal) \
           ON CONFLICT DO NOTHING";
        let op_level, op_block_hash, distance_level, network, timestamp_block =
          match extra_info with
          | Some (op_level, _priority, op_block_hash, network, timestamp_block) ->
            Some (Int64.of_int op_level), Some op_block_hash, Some (-1l), Some network,
            Some timestamp_block
          | _ -> None, None, None, None, None in
        PGSQL(dbh)
          "INSERT INTO delegation_last \
           (hash, source, fee, counter, delegate, gas_limit, storage_limit, \
           failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
           network, timestamp_block) \
           VALUES($hash, $source, $fee, $counter, $delegate, $gas_limit, \
           $storage_limit, $failed, $internal, $timestamp_op, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        PGSQL(dbh)
          "INSERT INTO delegation_all \
           (hash, source, fee, counter, delegate, gas_limit, storage_limit, \
           failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
           network, timestamp_block) \
           VALUES($hash, $source, $fee, $counter, $delegate, $gas_limit, \
           $storage_limit, $failed, $internal, $timestamp_op, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)"

      (* Reveal *)
      | NReveal reveal ->
        let source = reveal.node_rvl_src in
        let counter = Z.to_int64 reveal.node_rvl_counter in
        let fee = reveal.node_rvl_fee in
        let gas_limit = Z.to_int64 reveal.node_rvl_gas_limit in
        let storage_limit = Z.to_int64 reveal.node_rvl_storage_limit in
        debug "[Writer]  reveal %s\n%!" hash;
        register_tezos_user source;
        let pubkey = reveal.node_rvl_pubkey in
        let failed =
          get_status_from_manager_metadata reveal.node_rvl_metadata in
        PGSQL(dbh)
          "INSERT INTO reveal \
           (hash, source, fee, counter, pubkey, \
           gas_limit, storage_limit, failed, internal) \
           VALUES \
           ($hash, $source, $fee, $counter, $pubkey, \
           $gas_limit, $storage_limit, $failed, $internal) \
           ON CONFLICT DO NOTHING";
        let op_level, op_block_hash, distance_level, network, timestamp_block =
          match extra_info with
          | Some (op_level, _priority, op_block_hash, network, timestamp_block) ->
            Some (Int64.of_int op_level), Some op_block_hash, Some (-1l), Some network,
            Some timestamp_block
          | _ -> None, None, None, None, None in
        PGSQL(dbh)
          "INSERT INTO reveal_last \
           (hash, source, fee, counter, pubkey, gas_limit, storage_limit, \
           failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
           network, timestamp_block) \
           VALUES($hash, $source, $fee, $counter, $pubkey, $gas_limit, \
           $storage_limit, $failed, $internal, $timestamp_op, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)";
        PGSQL(dbh)
          "INSERT INTO reveal_all \
           (hash, source, fee, counter, pubkey, gas_limit, storage_limit, \
           failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
           network, timestamp_block) \
           VALUES($hash, $source, $fee, $counter, $pubkey, $gas_limit, \
           $storage_limit, $failed, $internal, $timestamp_op, \
           $?op_level, $?op_block_hash, $?distance_level, $?network, $?timestamp_block)"
      | NActivate
      | NActivate_testnet -> ())
    operations

let register_operation timestamp op =
  let op_hash = op.node_op_hash in
  let op_type = Utils.string_of_op_contents (List.hd op.node_op_contents) in
  let op_anon_type =
    Utils.opt_list @@ Utils.string_of_anonymous_op_contents op.node_op_contents in
  let op_manager_type =
    Utils.opt_list @@ Utils.string_of_manager_op_contents op.node_op_contents in
  PGSQL(dbh)
    "INSERT INTO operation \
     (hash, op_type, op_anon_type, op_manager_type, timestamp) \
     VALUES \
     ($op_hash, $op_type, $op_anon_type, $op_manager_type, $timestamp) \
     ON CONFLICT (hash) DO NOTHING"

let register_pending_operation timestamp op =
  pg_lock (fun () ->
      let op_hash = op.pending_hash in
      let op_type = Utils.string_of_op_contents (List.hd op.pending_contents) in
      let op_anon_type =
        Utils.opt_list @@
        Utils.string_of_anonymous_op_contents op.pending_contents in
      let op_manager_type =
        Utils.opt_list @@
        Utils.string_of_manager_op_contents op.pending_contents in
      PGSQL(dbh)
        "INSERT INTO operation \
         (hash, op_type, op_anon_type, op_manager_type, timestamp) \
         VALUES \
         ($op_hash, $op_type, $op_anon_type, $op_manager_type, $timestamp) \
         ON CONFLICT (hash) DO NOTHING")

let register_block_operation block_hash op =
  let op_hash = op.node_op_hash in
  debug "[Writer] register_block_operation %S %S\n%!" block_hash op_hash ;
  PGSQL(dbh)
    "INSERT INTO block_operation \
     (operation_hash, block_hash) \
     VALUES \
     ($op_hash, $block_hash)";
  Printf.printf "register block operation %s\n%!" op_hash

let register_flat_operation kind op_level op_block_hash network tsp_block op =
  let op_hash = op.node_op_hash in
  let block_hash = op.node_op_branch in
  List.iter (function
      | NTransaction transaction ->
        Printf.printf "register flat transaction %s\n%!" op_hash;
        let source = transaction.node_tr_src in
        let counter = Z.to_int64 transaction.node_tr_counter in
        let fee = transaction.node_tr_fee in
        let gas_limit = Z.to_int64 transaction.node_tr_gas_limit in
        let storage_limit = Z.to_int64 transaction.node_tr_storage_limit in
        register_tezos_user source;
        let dst = transaction.node_tr_dst in
        register_tezos_user dst;
        let amount = transaction.node_tr_amount in
        let parameters = transaction.node_tr_parameters in
        let failed =
          get_status_from_manager_metadata transaction.node_tr_metadata in
        let paid =
          get_paid_storage_diff_size_from_manager_metadata
            transaction.node_tr_metadata in
        let burn =
          Int64.(mul paid (of_int Tezos_constants.Constants.cost_per_byte)) in
        let op_level = Int64.of_int op_level in
        let timestamp_op = CalendarLib.Calendar.now () in
        if kind = "new_flat_operation" then (
          PGSQL(dbh)
            "INSERT INTO transaction_all \
             (hash, source, destination, fee, counter, amount, parameters, \
             gas_limit, storage_limit, failed, internal, timestamp_op, burn_tez, \
             op_level, op_block_hash, distance_level, network, timestamp_block) \
             VALUES \
             ($op_hash, $source, $dst, $fee, $counter, $amount, $?parameters, \
             $gas_limit, $storage_limit, $failed, false, $timestamp_op, $burn, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)";
          PGSQL(dbh)
            "INSERT INTO transaction_last \
             (hash, source, destination, fee, counter, amount, parameters, \
             gas_limit, storage_limit, failed, internal, timestamp_op, burn_tez, \
             op_level, op_block_hash, distance_level, network, timestamp_block) \
             VALUES \
             ($op_hash, $source, $dst, $fee, $counter, $amount, $?parameters, \
             $gas_limit, $storage_limit, $failed, false, $timestamp_op, $burn, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)")
        else if kind = "update_flat_operation" then (
          PGSQL(dbh)
            "UPDATE transaction_all SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash";
          PGSQL(dbh)
            "UPDATE transaction_last SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash")
      | NOrigination origination ->
        debug "[Writer]  register flat origination %s\n%!" op_hash;
        let source = origination.node_or_src in
        let counter = Z.to_int64 origination.node_or_counter in
        let fee = origination.node_or_fee in
        let gas_limit = Z.to_int64 origination.node_or_gas_limit in
        let storage_limit = Z.to_int64 origination.node_or_storage_limit in
        register_tezos_user source;
        let manager = origination.node_or_manager in
        register_tezos_user manager;
        let delegate =
          match origination.node_or_delegate with
            None -> "" | Some delegate -> delegate in
        register_tezos_user delegate ;
        let spendable = origination.node_or_spendable in
        let delegatable = origination.node_or_delegatable in
        let balance = origination.node_or_balance in
        let tz1 =
          match origination.node_or_metadata with
          | None -> Blake2b.originated_TZ1 op_hash
          | Some meta ->
            match meta.manager_meta_operation_result with
            | None -> assert false
            | Some meta ->
              match meta.meta_op_originated_contracts with
              | None | Some [] -> Blake2b.originated_TZ1 op_hash
              | Some (hd :: _ ) -> hd in
        let failed =
          get_status_from_manager_metadata origination.node_or_metadata in
        let paid =
          get_paid_storage_diff_size_from_manager_metadata
            origination.node_or_metadata in
        let burn =
          Int64.
            (add
               (get_internal_burn origination.node_or_metadata)
               (add
                  (mul paid (of_int Tezos_constants.Constants.cost_per_byte))
                  Tezos_constants.Constants.origination_burn)) in
        debug "[Writer] tz1 %S originated by %S\n%!" tz1 source ;
        register_tezos_user tz1;
        let sc_code, sc_storage =
        match origination.node_or_script with
          | None -> None, None
          | Some sc -> Some sc.sc_code, Some sc.sc_storage in
        let op_level = Int64.of_int op_level in
        let timestamp_op = CalendarLib.Calendar.now () in
        if kind = "new_flat_operation" then (
          PGSQL(dbh)
            "INSERT INTO origination_last \
             (hash, source, tz1, fee, counter, manager, delegate, script_code, \
             script_storage_type, spendable, delegatable, balance, gas_limit, \
             storage_limit, failed, internal, burn_tez, timestamp_op, \
             op_level, op_block_hash, distance_level, network, timestamp_block) \
             VALUES($op_hash, $source, $tz1, $fee, $counter, $manager, $delegate, \
             $?sc_code, $?sc_storage, $spendable, $delegatable, $balance, $gas_limit, \
             $storage_limit, $failed, false, $burn, $timestamp_op, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)";
          PGSQL(dbh)
            "INSERT INTO origination_all \
             (hash, source, tz1, fee, counter, manager, delegate, script_code, \
             script_storage_type, spendable, delegatable, balance, gas_limit, \
             storage_limit, failed, internal, burn_tez, timestamp_op, \
             op_level, op_block_hash, distance_level, network, timestamp_block) \
             VALUES($op_hash, $source, $tz1, $fee, $counter, $manager, $delegate, \
             $?sc_code, $?sc_storage, $spendable, $delegatable, $balance, $gas_limit, \
             $storage_limit, $failed, false, $burn, $timestamp_op, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)")
        else if kind = "update_flat_operation" then (
          PGSQL(dbh)
            "UPDATE origination_all SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash";
          PGSQL(dbh)
            "UPDATE origination_last SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash" )
      | NDelegation delegation ->
        let source = delegation.node_del_src in
        let counter = Z.to_int64 delegation.node_del_counter in
        let fee = delegation.node_del_fee in
        let gas_limit = Z.to_int64 delegation.node_del_gas_limit in
        let storage_limit = Z.to_int64 delegation.node_del_storage_limit in
        debug "[Writer]  delegation %s\n%!" op_hash;
        register_tezos_user source;
        let delegate = delegation.node_del_delegate in
        let failed =
          get_status_from_manager_metadata delegation.node_del_metadata in
        register_tezos_user delegate;
        let op_level = Int64.of_int op_level in
        let timestamp_op = CalendarLib.Calendar.now () in
        if kind = "new_flat_operation" then (
          PGSQL(dbh)
            "INSERT INTO delegation_last \
             (hash, source, fee, counter, delegate, gas_limit, storage_limit, \
             failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
             network, timestamp_block) \
             VALUES($op_hash, $source, $fee, $counter, $delegate, $gas_limit, \
             $storage_limit, $failed, false, $timestamp_op, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)";
          PGSQL(dbh)
            "INSERT INTO delegation_all \
             (hash, source, fee, counter, delegate, gas_limit, storage_limit, \
             failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
             network, timestamp_block) \
             VALUES($op_hash, $source, $fee, $counter, $delegate, $gas_limit, \
             $storage_limit, $failed, false, $timestamp_op, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)")
        else if kind = "update_flat_operation" then (
          PGSQL(dbh)
            "UPDATE delegation_all SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash";
          PGSQL(dbh)
            "UPDATE delegation_last SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash")
      | NReveal reveal ->
        let source = reveal.node_rvl_src in
        let counter = Z.to_int64 reveal.node_rvl_counter in
        let fee = reveal.node_rvl_fee in
        let gas_limit = Z.to_int64 reveal.node_rvl_gas_limit in
        let storage_limit = Z.to_int64 reveal.node_rvl_storage_limit in
        register_tezos_user source;
        let pubkey = reveal.node_rvl_pubkey in
        let failed =
          get_status_from_manager_metadata reveal.node_rvl_metadata in
        let op_level = Int64.of_int op_level in
        let timestamp_op = CalendarLib.Calendar.now () in
        if kind = "new_flat_operation" then (
          PGSQL(dbh)
            "INSERT INTO reveal_last \
             (hash, source, fee, counter, pubkey, gas_limit, storage_limit, \
             failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
             network, timestamp_block) \
             VALUES($op_hash, $source, $fee, $counter, $pubkey, $gas_limit, \
             $storage_limit, $failed, false, $timestamp_op, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)";
          PGSQL(dbh)
            "INSERT INTO reveal_all \
             (hash, source, fee, counter, pubkey, gas_limit, storage_limit, \
             failed, internal, timestamp_op, op_level, op_block_hash, distance_level, \
             network, timestamp_block) \
             VALUES($op_hash, $source, $fee, $counter, $pubkey, $gas_limit, \
             $storage_limit, $failed, false, $timestamp_op, \
             $op_level, $op_block_hash, -1, $network, $tsp_block)")
        else if kind = "update_flat_operation" then (
          PGSQL(dbh)
            "UPDATE reveal_all SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash";
          PGSQL(dbh)
            "UPDATE reveal_last SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash")
      | NActivation act ->
        let pkh = act.node_act_pkh in
        let secret = act.node_act_secret in
        let balance =
          begin match act.node_act_metadata with
          | None -> None
          | Some meta ->
            begin match meta.meta_op_balance_updates with
              | None -> Some 0L
              | Some l ->
                begin match l with
                  | [ bu ] ->
                    begin match bu with
                      | Contract (acc, amount) when acc = pkh -> Some amount
                      | _ ->
                        debug
                          "[Writer] Weird balance_updates type for \
                           activation %S\n%!"
                          op_hash;
                        Some 0L
                    end
                  | _ ->
                    debug
                      "[Writer] Unexpected balance_updates length for \
                       activation %S - %S\n%!"
                      op_hash @@
                    String.concat
                      " " (List.map Utils.string_of_balance_update l);
                    None
                end
            end
        end in
        register_tezos_user pkh;
        let op_level = Int64.of_int op_level in
        let timestamp_op = CalendarLib.Calendar.now () in
        if kind = "new_flat_operation" then (
          PGSQL(dbh)
            "INSERT INTO activation_last \
             (hash, pkh, secret, balance, timestamp_op, op_level, op_block_hash, \
             distance_level, network, timestamp_block) \
             VALUES($op_hash, $pkh, $secret, $?balance, $timestamp_op, $op_level, \
             $op_block_hash, -1, $network, $tsp_block)";
          PGSQL(dbh)
            "INSERT INTO activation_all \
             (hash, pkh, secret, balance, timestamp_op, op_level, op_block_hash, \
             distance_level, network, timestamp_block) \
             VALUES($op_hash, $pkh, $secret, $?balance, $timestamp_op, $op_level, \
             $op_block_hash, -1, $network, $tsp_block)")
        else if kind = "update_flat_operation" then (
          PGSQL(dbh)
            "UPDATE activation_all SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash";
          PGSQL(dbh)
            "UPDATE activation_last SET \
             op_level = $op_level, op_block_hash = $op_block_hash, \
             distance_level = -1, network = $network, \
             timestamp_block = $tsp_block WHERE \
             hash = $op_hash")
      | NEndorsement endorsement ->
        let src, slots =
          match endorsement.node_endorse_metadata with
          | None -> "", []
          | Some metadata ->
            begin match metadata.meta_op_delegate with
             | None -> "" | Some delegate -> delegate
            end,
            begin match metadata.meta_op_slots with
              | None -> [] | Some slots -> slots
            end in
        register_tezos_user src;
        let level = Int64.of_int endorsement.node_endorse_block_level in
        let slots = List.map (fun s -> Some (Int32.of_int s)) slots in
        let op_cycle = Int64.of_int @@ Tezos_constants.cycle_from_level op_level in
        let op_level = Int64.of_int op_level in
        register_cycle_count_baker op_cycle src;
        let priority =
          match PGSQL(dbh) "SELECT priority FROM block WHERE hash = $block_hash" with
          | [ p ] -> p
          | _ -> assert false in
        PGSQL(dbh)
          "INSERT INTO endorsement_last \
           (hash, source, block_hash, block_level, slots, priority, \
           op_block_hash, op_level, op_cycle, distance_level, network, timestamp) \
           VALUES \
           ($op_hash, $src, $block_hash, $level, $slots, $priority, \
           $op_block_hash, $op_level, $op_cycle, -1, $network, $tsp_block) \
           ON CONFLICT DO NOTHING";
        PGSQL(dbh)
          "INSERT INTO endorsement_all \
           (hash, source, block_hash, block_level, slots, priority, \
           op_block_hash, op_level, op_cycle, distance_level, network, timestamp) \
           VALUES \
           ($op_hash, $src, $block_hash, $level, $slots, $priority, \
           $op_block_hash, $op_level, $op_cycle, -1, $network, $tsp_block) \
           ON CONFLICT DO NOTHING"
      | _ -> ()) op.node_op_contents

let register_orphan_op op_hash =
  PGSQL(dbh)
    "INSERT INTO block_operation \
     (operation_hash, block_hash) \
     VALUES \
     ($op_hash, 'Orphan')"

let clean_orphan curr_ops new_ops =
  (* TODO mika :
     c'est super inefficace, on risque d'avoir des pbs quand le reseau
     generera beaucoup d'operations... *)
  List.iter (fun oph ->
      if List.exists (fun op -> oph = op.pending_hash) new_ops then ()
      else register_orphan_op oph)
    curr_ops

let register_pending timestamp ops =
  let db_pending_ops =
    PGSQL(dbh)
      "SELECT o.hash FROM operation AS o \
       LEFT JOIN block_operation AS bo ON o.hash = bo.operation_hash \
       WHERE bo.block_hash IS NULL" in
  clean_orphan db_pending_ops ops ;
  List.iter (fun op ->
      let hash = op.pending_hash in
      match PGSQL(dbh) "SELECT hash FROM operation WHERE hash = $hash" with
      | [] ->
        if List.for_all (function
            | NTransaction tr ->
              begin match tr.node_tr_parameters with
                | None -> true
                | Some _ -> false
              end
            | NOrigination ori ->
              begin match ori.node_or_script with
                | None -> true
                | Some _ -> false
              end
            | NDelegation _
            | NReveal _ -> true
            | _ -> false) op.pending_contents then begin
          register_pending_operation timestamp op ;
          register_operation_type
            timestamp op.pending_hash op.pending_branch op.pending_contents
        end
      | _ -> ())
    ops

let update_metadata_operation ?extra_info timestamp_op block_hash op =
  if List.for_all (function
      | NTransaction _
      | NOrigination _
      | NDelegation _
      | NReveal _ -> true
      | _ -> false) op.node_op_contents then
    let hash = op.node_op_hash in
    let len = List.length op.node_op_contents in
    List.iteri (fun i op ->
        debug "[Writer] update_metadata_operation %S %S %d / %d\n%!" block_hash hash (i + 1) len ;
        match op with
        | NTransaction trans ->
          let failed =
            get_status_from_manager_metadata trans.node_tr_metadata in
          let src = trans.node_tr_src in
          let counter = Z.to_int64 trans.node_tr_counter in
          PGSQL(dbh)
            "UPDATE transaction SET failed = $failed \
             WHERE hash = $hash AND source = $src AND counter = $counter" ;

          begin match trans.node_tr_metadata with
            | None -> ()
            | Some metadata ->
              register_operation_type ~internal:true ?extra_info
                timestamp_op hash block_hash
                metadata.manager_meta_internal_operation_results
          end

        | NOrigination ori ->
          let failed =
            get_status_from_manager_metadata ori.node_or_metadata in
          let src = ori.node_or_src in
          let counter = Z.to_int64 ori.node_or_counter in
          PGSQL(dbh)
            "UPDATE origination SET failed = $failed \
             WHERE hash = $hash AND source = $src AND counter = $counter" ;

          begin match ori.node_or_metadata with
            | None -> ()
            | Some metadata ->
              register_operation_type ~internal:true ?extra_info
                timestamp_op hash block_hash
                metadata.manager_meta_internal_operation_results
          end

        | NDelegation del ->
          let failed =
            get_status_from_manager_metadata del.node_del_metadata in
          let src = del.node_del_src in
          let counter = Z.to_int64 del.node_del_counter in
          PGSQL(dbh)
            "UPDATE delegation SET failed = $failed \
             WHERE hash = $hash AND source = $src AND counter = $counter" ;
        | NReveal rvl ->
          let failed =
            get_status_from_manager_metadata rvl.node_rvl_metadata in
          let src = rvl.node_rvl_src in
          let counter = Z.to_int64 rvl.node_rvl_counter in
          PGSQL(dbh)
            "UPDATE reveal SET failed = $failed \
             WHERE hash = $hash AND source = $src AND counter = $counter" ;
        | _ -> assert false)
      op.node_op_contents

let register_operations block ops =
  let timestamp = Pg_helper.cal_of_date block.node_header.header_timestamp in
  let block_hash = block.node_hash in
  let len = List.length ops in
  List.iteri (fun i op ->
      debug "[Writer] register_operation %S %d / %d\n%!" block_hash (i + 1) len ;
      let op_hash = op.node_op_hash in
      let oporphan =
        PGSQL(dbh)
          "SELECT operation_hash FROM block_operation WHERE \
           operation_hash = $op_hash AND block_hash = 'Orphan'"  in
      match oporphan with
      | [] ->
        begin
          let oph =
            PGSQL(dbh)
              "SELECT operation_hash FROM block_operation WHERE \
               operation_hash = $op_hash AND block_hash = $block_hash"  in
          match oph with
          | [] ->
            begin
              let oph =
                PGSQL(dbh)
                  "SELECT operation_hash FROM block_operation WHERE \
                   operation_hash = $op_hash"  in
              match oph with
              | [] ->
                begin
                  let opt =
                    PGSQL(dbh)
                      "SELECT hash FROM operation WHERE hash = $op_hash" in
                  match opt with
                  | [] ->
                    pg_lock (fun () ->
                        (* operations to be linked to a first block *)
                        register_operation timestamp op;
                        register_block_operation block_hash op;
                        register_operation_type
                          ~extra_info:(block.node_header.header_level,
                                       block.node_header.header_priority,
                                       block_hash, block.node_chain_id,
                                       Pg_helper.cal_of_date
                                         block.node_header.header_timestamp)
                          timestamp
                          op.node_op_hash op.node_op_branch op.node_op_contents )
                  | _ ->
                    pg_lock (fun () ->
                        (* operations pending to be linked to a first block *)
                        register_block_operation block_hash op;
                        register_flat_operation "update_flat_operation"
                          block.node_header.header_level block_hash
                          block.node_chain_id
                          (Pg_helper.cal_of_date block.node_header.header_timestamp) op;
                        update_metadata_operation
                          ~extra_info:(block.node_header.header_level,
                                       block.node_header.header_priority,
                                       block_hash, block.node_chain_id,
                                       Pg_helper.cal_of_date
                                         block.node_header.header_timestamp)
                          timestamp
                          block_hash op)
                end
              | _ ->
                pg_lock (fun () ->
                    (* operations to be linked to a new block *)
                    register_block_operation block_hash op;
                    register_flat_operation "new_flat_operation"
                      block.node_header.header_level block_hash
                      block.node_chain_id
                      (Pg_helper.cal_of_date
                         block.node_header.header_timestamp) op
                  )
            end
          | _ -> ()
        end
      | _ ->
        PGSQL(dbh)
          "UPDATE block_operation SET block_hash = $block_hash \
           WHERE operation_hash = $op_hash AND block_hash = 'Orphan'"
    ) ops

let string_operation (op : node_operation_type option) : string =
  match op with
  | None -> "Header"
  | Some op -> match op with
      NTransaction _ -> "Transaction"
    | NOrigination _ -> "Origination"
    | NReveal _ -> "Reveal"
    | NDelegation _ -> "Delegation"
    | NSeed_nonce_revelation _ -> "Seed nonce revelation"
    | NActivation _ -> "Activation"
    | NDouble_endorsement_evidence _ -> "Double endorsement evidence"
    | NDouble_baking_evidence _ -> "Double baking evidence"
    | NEndorsement _ -> "Endorsement"
    | NProposals _ -> "Proposals"
    | NBallot _ -> "Ballot"
    | NActivate -> "Activate"
    | NActivate_testnet -> "Activate testnet"

let bu_to_bu_info ?op may_burn bu_internal bu_level bu_date bu bu_block_hash =
  let bu_op_type = string_operation op in
  match bu with
    Contract (bu_account, bu_diff) ->
    Some
      {bu_account;
       bu_block_hash;
       bu_diff;
       bu_date;
       bu_update_type = "Contract";
       bu_op_type;
       bu_internal;
       bu_level;
       bu_frozen = false;
       bu_burn=may_burn} (* Contracts in originations/transactions may burn tez *)
  | Rewards (bu_account,_,bu_diff)->
    Some
      {bu_account;
       bu_block_hash;
       bu_diff;
       bu_date;
       bu_update_type = "Reward";
       bu_op_type;
       bu_internal;
       bu_level;
       bu_frozen = true;
       bu_burn=false}

  | Fees (bu_account,_,bu_diff)->
    Some
      {bu_account;
       bu_block_hash;
       bu_diff;
       bu_date;
       bu_update_type = "Fee";
       bu_op_type;
       bu_internal;
       bu_level;
       bu_frozen = true;
       bu_burn=false}

  | Deposits (bu_account,_,bu_diff) ->
    Some
      {bu_account;
       bu_block_hash;
       bu_diff;
       bu_date;
       bu_update_type = "Deposit";
       bu_op_type;
       bu_internal;
       bu_level;
       bu_frozen = true;
       bu_burn=false}

let get_op_bal_update = function
  | Some {meta_op_status;meta_op_balance_updates = Some l;_} ->
    begin match meta_op_status with Some "applied" | None -> l | _ -> [] end
  | _ -> []

let rec get_man_bal_update ?op header_bu_infos man_mtdt is_orig internal date level block_hash =
  let b_u = match man_mtdt.manager_meta_balance_updates with
      Some b_u -> b_u
    | None -> []
  in
  let bu_info_internal : balance_update_info list =
    List.flatten
      (List.map
         (get_balance_update_info ~header_bu_infos date level true block_hash)
         man_mtdt.manager_meta_internal_operation_results) in
  let all_bu =
    b_u
    @ (get_op_bal_update man_mtdt.manager_meta_operation_result) in

  bu_info_internal @
  (
    List.fold_left
      (fun acc bu ->
         let bu_info =
           match op with
             None -> bu_to_bu_info is_orig internal level date bu block_hash
           | Some op -> bu_to_bu_info ~op is_orig internal level date bu block_hash in

         match bu_info with
           None -> acc
         | Some bu -> bu :: acc) [] all_bu)

and get_balance_update_info
    ?(header_bu_infos=[])
    (date : Date.t)
    (level : int32)
    (internal:bool)
    (block_hash : block_hash)
    (op : node_operation_type) =
  match op with
    NTransaction {node_tr_metadata = Some man_mtdt; _ }
  | NReveal {node_rvl_metadata = Some man_mtdt; _ }
  | NDelegation {node_del_metadata = Some man_mtdt; _ } ->
    get_man_bal_update ~op header_bu_infos man_mtdt false internal date level block_hash

  | NOrigination {node_or_metadata = Some man_mtdt; node_or_manager = manager; _} ->
     (* Burns tez *)
    let (bu_info : balance_update_info list) =
      get_man_bal_update ~op header_bu_infos man_mtdt true internal date level block_hash in
    (* Burned tez appear as contracts. For usual operations, contracts go by pairs
         (one is debited, the other credited).
         Burnt tez are the only unpaired contracts.*)
    let () =
      List.iter
        (fun (info : balance_update_info) ->
           if not info.bu_burn || not (String.equal info.bu_account manager)
           then () (* It has either been treated, or is not a burn contract *)
           else
             let opp_val = Int64.neg info.bu_diff in
             let is_not_burn : bool =
               List.exists
                 (fun (i:balance_update_info) ->
                    if Int64.equal opp_val i.bu_diff
                    then let () = i.bu_burn <- false in true
                    else false
                 )
                 bu_info
             in
             info.bu_burn <- not is_not_burn
        )
        (bu_info@header_bu_infos)
    in bu_info

  | NSeed_nonce_revelation {node_seed_metadata = op_mtdt ; _ }
  | NActivation {node_act_metadata = op_mtdt ; _ }
  | NDouble_endorsement_evidence {node_double_endorsement_metadata = op_mtdt ; _ }
  | NDouble_baking_evidence {node_double_bh_metadata = op_mtdt ; _ }
  | NEndorsement {node_endorse_metadata = op_mtdt ; _ }
  | NProposals {node_prop_metadata = op_mtdt ; _ }
  | NBallot {node_ballot_metadata = op_mtdt ; _ } ->
    List.fold_left
      (fun acc bu ->
         match bu_to_bu_info ~op false internal level date bu block_hash with
           None -> acc
         | Some bu -> bu :: acc)
      []
      (get_op_bal_update op_mtdt)

  | _ -> []

let insert_bu_info bu =
  let (hash : string) = bu.bu_account in
  let (diff : int64) = bu.bu_diff in
  let (date : CalendarLib.Calendar.t) = Pg_helper.cal_of_date bu.bu_date in
  let (update_type : string) = bu.bu_update_type in
  let (internal : bool) = bu.bu_internal in
  let (level : int32) = bu.bu_level in
  let (frozen: bool) = bu.bu_frozen in
  let (burn : bool) = bu.bu_burn in
  let (block_hash: block_hash) = bu.bu_block_hash in
  let (op_type : string) = bu.bu_op_type in
  PGSQL(dbh)
    "INSERT INTO balance_updates \
     (hash, block_hash, diff, date, update_type, operation_type, internal, level, frozen, burn) \
     VALUES \
     ($hash, $block_hash, $diff, $date, $update_type, $op_type, $internal, $level, $frozen, $burn)"

let bu_infos_from_block block =

  let date = block.node_header.header_timestamp in
  let ops = List.flatten block.node_operations in
  let level = Int32.of_int block.node_header.header_level in
  let block_hash = block.node_hash in
  let header_bu_infos =
    match block.node_metadata.meta_header.header_meta_balance_updates with
      None -> []
    | Some l ->
      List.fold_left
        (fun acc bu ->
           match bu_to_bu_info true true level date bu block_hash with
             None -> acc
           | Some bu_info -> bu_info :: acc) [] l in
    List.fold_left
      (fun acc op ->
         List.flatten (
             List.map
               (get_balance_update_info ~header_bu_infos date level false block_hash)
               op.node_op_contents) @ acc)
      header_bu_infos
      ops

let register_balance_updates_info balance_update_infos =
  List.iter insert_bu_info balance_update_infos

module StrMap =
    Map.Make(
    struct
      type t = string
      let compare =
        String.compare end)

let update_new_cycle cycle =
  let next_cycle = Int32.add cycle Int32.one in
  (* New cycle for balance history *)

  let accounts =
  PGSQL(dbh)
        "SELECT hash,spendable_balance,frozen,rewards,fees,deposits \
         FROM balance_from_balance_updates \
         WHERE cycle=$cycle" in
  List.iter
    (fun (hash,sp_bal,frz,rew,fees,deps) ->
       PGSQL(dbh)
         "INSERT INTO balance_from_balance_updates \
         (hash,spendable_balance,frozen,rewards,fees,deposits,cycle) \
         VALUES \
         ($hash,$sp_bal,$frz,$rew,$fees,$deps,$next_cycle)")
    accounts;

  (* There are too much balance updates for the db to be efficient.
     Every 5 cycle, sanitize the table balance_updates. *)

  if Int32.compare cycle @@ Int32.of_int 6 < 0
  then () (* Do nothing *)
  else (* sanitize *)
    let level_min =
      Int32.mul
        (Int32.sub cycle @@ Int32.of_int 6)
        (Int32.of_int Tezos_constants.Constants.block_per_cycle)
    in
    let level_max = Int32.add level_min @@ Int32.of_int Tezos_constants.Constants.block_per_cycle
    in
    PGSQL(dbh)
        "DELETE FROM balance_updates \
         WHERE level<=$level_max AND level>=$level_min"

let update_balance_from_balance_updates
      cycle
      hash
      total_diff =
    let old_balances =
      PGSQL(dbh)
        "SELECT spendable_balance,frozen,rewards,fees,deposits,cycle \
         FROM balance_from_balance_updates WHERE hash=$hash \
         ORDER BY cycle DESC LIMIT 1" in

    match old_balances with
    | [] ->
      let next_cycle = Int32.add cycle Int32.one in
      let s_diff = total_diff.b_spendable
      and f_diff = total_diff.b_frozen
      and rew_diff = total_diff.b_rewards
      and fee_diff = total_diff.b_fees
      and dep_diff = total_diff.b_deposits in

          PGSQL(dbh)
            "INSERT INTO balance_from_balance_updates \
             (hash,spendable_balance,frozen,rewards,fees,deposits,cycle) \
             VALUES \
             ($hash,0,0,0,0,0,$cycle)";
          PGSQL(dbh)
            "INSERT INTO balance_from_balance_updates \
             (hash,spendable_balance,frozen,rewards,fees,deposits,cycle) \
             VALUES \
             ($hash,$s_diff,$f_diff,$rew_diff, $fee_diff,$dep_diff,$next_cycle)"

    | (sb,frz,rew,fees,deps,last_cycle) :: _ ->
      let new_spd_bal =
        Int64.add sb total_diff.b_spendable
      in
      let new_frz_bal =
        Int64.add frz total_diff.b_frozen
      in
      let new_rw_bal =
        Int64.add rew total_diff.b_rewards
      in
      let new_fee_bal =
        Int64.add fees total_diff.b_fees
      in
      let new_dep_bal =
        Int64.add deps total_diff.b_deposits
      in
      PGSQL(dbh)
        "UPDATE balance_from_balance_updates \
         SET \
         spendable_balance=$new_spd_bal, \
         frozen=$new_frz_bal, \
         rewards=$new_rw_bal, \
         fees=$new_fee_bal, \
         deposits=$new_dep_bal \
         WHERE \
         hash=$hash AND cycle=$last_cycle"

let register_balance_from_balance_updates level (bu_list : (account_hash * int64 * bool * string) list) =
  let update_map acc bu  =
    let hash,diff,frz,update_type = bu in
    StrMap.update
      hash
      (fun old ->
         let diff_balance =
           {b_spendable = if frz then Int64.zero else diff;
            b_frozen = if frz then diff else Int64.zero;
            b_rewards = if update_type = "Reward" then diff else Int64.zero;
            b_fees = if update_type = "Fee" then diff else Int64.zero;
            b_deposits = if update_type = "Deposit" then diff else Int64.zero;
           }
         in
          match old with
            None -> Some diff_balance
          | Some bal ->

            let (+) = Int64.add in

            Some (
              {b_spendable = bal.b_spendable + diff_balance.b_spendable;
               b_frozen = bal.b_frozen + diff_balance.b_frozen;
               b_rewards =  bal.b_rewards + diff_balance.b_rewards;
               b_fees =  bal.b_fees + diff_balance.b_fees;
               b_deposits =  bal.b_deposits + diff_balance.b_deposits;
           }))
      acc
  in
  let bal_map =
    List.fold_left
      update_map
      StrMap.empty
      bu_list
  in
  let cycle = Int32.(div level @@ of_int Tezos_constants.Constants.block_per_cycle) in

  StrMap.iter
    (update_balance_from_balance_updates cycle)
    bal_map;

  if Int32.(equal (mul cycle @@ of_int Tezos_constants.Constants.block_per_cycle) level)
  (*i.e. this is a new cycle *)
  then update_new_cycle cycle

let register_init_balance hash init date level =
  let bu_info =
    {
      bu_account = hash;
      bu_block_hash = "";
      bu_diff = init;
      bu_date = date;
      bu_update_type = "Initialization";
      bu_op_type = "";
      bu_internal = true;
      bu_level = Int32.of_int level;
      bu_frozen = false;
      bu_burn = false}
  in
  insert_bu_info bu_info;
  update_balance_from_balance_updates
    Int32.zero
    hash
    {b_spendable = init;
     b_frozen = Int64.zero;
     b_rewards = Int64.zero;
     b_fees = Int64.zero;
     b_deposits =Int64.zero}

let register_balances_updates block =
  let bu_infos = bu_infos_from_block block
  in
  register_balance_updates_info bu_infos


  (*register_balance_from_balance_updates bu_infos
    (Int32.of_int block.node_header.header_level) *)

let compute_volume_fees ops =
  List.fold_left (fun (acc_vol, acc_fee) op ->
      List.fold_left (fun (acc_vol, acc_fee) op -> match op with
          | NTransaction tr ->
            let amount = tr.node_tr_amount in
            let fee = tr.node_tr_fee in
            let amount =
              if get_status_from_manager_metadata tr.node_tr_metadata then
                acc_vol
              else
                Int64.add amount acc_vol in
            amount, Int64.add fee acc_fee
          | NOrigination ori ->
            let balance = ori.node_or_balance in
            let fee = ori.node_or_fee in
            let balance =
              if get_status_from_manager_metadata ori.node_or_metadata then
                acc_vol
              else
                Int64.add balance acc_vol in
            balance, Int64.add fee acc_fee
          | NDelegation del ->
            let fee = del.node_del_fee in
            acc_vol, Int64.add fee acc_fee
          | NReveal rvl ->
            let fee = rvl.node_rvl_fee in
            acc_vol, Int64.add fee acc_fee
          | _ -> acc_vol, acc_fee)
        (acc_vol, acc_fee) op.node_op_contents)
    (0L, 0L) ops

let register_all block lvl ops =
  let volume, fees = compute_volume_fees ops in
  let operation_count = Int64.of_int @@ List.length ops in
  pg_lock (fun () ->
      register_header block.node_header;
      register_block block lvl (-1L) operation_count volume fees;
      register_operations block ops;
      register_balances_updates block
    )

let update_count_info ?(force=false) hash level info =
  match PGSQL(dbh) "SELECT level FROM count_info WHERE info = $info" with
  | [ level0 ] ->
    if (info = "lowest" && level0 > level) ||
       (info = "highest" && level0 < level) || force then
      PGSQL(dbh)
        "UPDATE count_info SET hash = $hash, level = $level WHERE info = $info"
  | _ ->
    PGSQL(dbh) "INSERT INTO count_info (hash, level, info) \
                VALUES($hash, $level, $info)"

let update_counts ?(force=false) hash sign =
  let time_b_blocks = Tezos_constants.Constants.time_between_blocks in
  let time_b_prio = Tezos_constants.Constants.time_delay_for_priority 1 - time_b_blocks in
  let itv_be_blocks = CalendarLib.Calendar.Period.second time_b_blocks in
  let itv_be_prio = CalendarLib.Calendar.Period.second time_b_prio in
  let alt = if sign = 1L then 0L else 1L in
  match PGSQL(dbh) "SELECT bl.cycle, bl.level, bl.priority, bl.fees, bl.baker, \
                    bake_time(bl.timestamp - pred.timestamp, bl.priority, \
                    $itv_be_blocks, $itv_be_prio) \
                    FROM block AS bl INNER JOIN block AS pred ON \
                    pred.hash = bl.predecessor WHERE bl.hash = $hash" with
  | [ cycle, level, priority, fees, baker, Some bktime ] ->
    debug "[Writer] [update_counts_main] \
           Update counts for block %s at level %Ld with factor %Ld\n%!" hash level sign;
    let bktime = Int64.to_float sign *. bktime in
    begin match PGSQL(dbh) "SELECT cycle FROM cycle_count WHERE cycle = $cycle" with
      | [] -> PGSQL(dbh) "INSERT INTO cycle_count(cycle) VALUES($cycle)"
      | _ -> () end;
    PGSQL(dbh) (* cycle counts *)
      "WITH tr(nb) AS ( \
       SELECT COUNT( * ) FROM transaction_all WHERE op_block_hash = $hash), \
       dlg(nb) AS ( \
       SELECT COUNT( * ) FROM delegation AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       ori(nb) AS ( \
       SELECT COUNT( * ) FROM origination AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       act(nb) AS ( \
       SELECT COUNT( * ) FROM activation AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       rvl(nb) AS ( \
       SELECT COUNT( * ) FROM reveal AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       dbe(nb) AS ( \
       SELECT COUNT( * ) FROM double_baking_evidence AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       dee(nb) AS ( \
       SELECT COUNT( * ) FROM double_endorsement_evidence AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       nonce(nb) AS ( \
       SELECT COUNT( * ) FROM seed_nonce_revelation AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       WHERE bo.block_hash = $hash), \
       endo(nba, nb_op) AS ( \
       SELECT agg_prio(priority, array_length(slots, 1), true), count(*) \
       FROM endorsement_all WHERE op_block_hash = $hash) \
       UPDATE cycle_count SET \
       nb_transaction = nb_transaction + $sign * tr.nb, \
       nb_delegation = nb_delegation + $sign * dlg.nb, \
       nb_origination = nb_origination + $sign * ori.nb, \
       nb_activation = nb_activation + $sign * act.nb, \
       nb_reveal = nb_reveal + $sign * rvl.nb, \
       nb_dbe = nb_dbe + $sign * dbe.nb, \
       nb_dee = nb_dee + $sign * dee.nb, \
       nb_nonce = nb_nonce + $sign * nonce.nb, \
       nb_endorsement = array_lin(nb_endorsement, endo.nba, 1, $sign), \
       nb_endorsement_op = nb_endorsement_op + $sign * endo.nb_op,
       nb_prio = array_lin(nb_prio, init_array_prio($priority, 1), 1, $sign) \
       FROM tr, dlg, ori, act, rvl, dbe, dee, nonce, endo \
       WHERE cycle = $cycle";
    PGSQL(dbh) (* missed endorsement *)
      "WITH endo(tz, nb) AS ( \
       SELECT UNNEST(endorsers), UNNEST(slots) FROM level_rights \
       WHERE level = $level - 1::bigint) \
       UPDATE cycle_count_baker AS ccb SET \
       nb_miss_endorsement = nb_miss_endorsement + $sign::bigint * COALESCE(endo.nb, 0) \
       FROM endo WHERE endo.tz = ccb.tz AND cycle = $cycle";
    PGSQL(dbh) (* endorsement *)
      "WITH endo(tz, nba, nb) AS ( \
       SELECT source, agg_prio(priority, array_length(slots, 1), true), \
       SUM(array_length(slots, 1)) \
       FROM endorsement_all WHERE op_block_hash = $hash GROUP BY source) \
       UPDATE cycle_count_baker AS ccb SET \
       nb_endorsement = array_lin(nb_endorsement, endo.nba, 1, $sign), \
       nb_miss_endorsement = nb_miss_endorsement - $sign * endo.nb, \
       nb_alt_endorsement = nb_alt_endorsement + $alt \
       FROM endo WHERE endo.tz = ccb.tz AND cycle = $cycle";
    PGSQL(dbh) (* baking *)
      "UPDATE cycle_count_baker SET \
       nb_baking = array_lin(nb_baking, init_array_prio($priority, 1), 1, $sign), \
       fees = fees + $sign::bigint * $fees, \
       time = time + $bktime, \
       nb_alt_baking = nb_alt_baking + $alt \
       WHERE cycle = $cycle AND tz = $baker";
    PGSQL(dbh) (* missed baking *)
      "UPDATE cycle_count_baker SET nb_miss_baking = nb_miss_baking + $sign \
       FROM (SELECT UNNEST(bakers), UNNEST(bakers_priority) \
       FROM level_rights WHERE level = $level) AS lr(baker, prio) \
       WHERE prio < $priority AND tz = baker AND cycle = $cycle";
    PGSQL(dbh) (* transaction as source *)
      "WITH tr_src(tz, nb) AS ( \
       SELECT source, COUNT( * ) FROM transaction_all WHERE op_block_hash = $hash \
       GROUP BY source) \
       UPDATE operation_count_user AS ocu SET \
       nb_transaction_src = nb_transaction_src + $sign * coalesce(tr_src.nb,0) \
       FROM tr_src WHERE ocu.tz = tr_src.tz";
    PGSQL(dbh) (* transaction as destination without source *)
      "WITH tr_dst(tz, nb) AS ( \
       SELECT destination, COUNT( * ) FROM transaction_all \
       WHERE op_block_hash = $hash AND source <> destination \
       GROUP BY destination) \
       UPDATE operation_count_user AS ocu SET \
       nb_transaction_dst = nb_transaction_dst + $sign * coalesce(tr_dst.nb,0) \
       FROM tr_dst WHERE ocu.tz = tr_dst.tz";
    PGSQL(dbh) (* delegation as source *)
      "WITH dlg_src(tz, nb) AS ( \
       SELECT source, COUNT( * ) FROM delegation \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash GROUP BY source) \
       UPDATE operation_count_user AS ocu SET \
       nb_delegation_src = nb_delegation_src + $sign * coalesce(dlg_src.nb,0) \
       FROM dlg_src WHERE ocu.tz = dlg_src.tz";
    PGSQL(dbh) (* delegation as delegate without source *)
      "WITH dlg_dlg(tz, nb) AS ( \
       SELECT delegate, COUNT( * ) FROM delegation \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash  AND delegate <> source \
       GROUP BY delegate) \
       UPDATE operation_count_user AS ocu SET \
       nb_delegation_dlg = nb_delegation_dlg + $sign * coalesce(dlg_dlg.nb,0) \
       FROM dlg_dlg WHERE ocu.tz = dlg_dlg.tz";
    PGSQL(dbh) (* origination as source *)
      "WITH ori_src(tz, nb) AS ( \
       SELECT source, COUNT( * ) FROM origination \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash \
       GROUP BY source) \
       UPDATE operation_count_user AS ocu SET \
       nb_origination_src = nb_origination_src + $sign * coalesce(ori_src.nb,0) \
       FROM ori_src WHERE ocu.tz = ori_src.tz";
    PGSQL(dbh) (* origination as manager without source *)
      "WITH ori_man(tz, nb) AS ( \
       SELECT manager, COUNT( * ) FROM origination \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash AND manager <> source \
       GROUP BY manager) \
       UPDATE operation_count_user AS ocu SET \
       nb_origination_man = nb_origination_man + $sign * coalesce(ori_man.nb,0) \
       FROM ori_man WHERE ocu.tz = ori_man.tz";
    PGSQL(dbh) (* origination as tz1 *)
      "WITH ori_tz1(tz, nb) AS ( \
       SELECT tz1, COUNT( * ) FROM origination \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash \
       GROUP BY tz1) \
       UPDATE operation_count_user AS ocu SET \
       nb_origination_tz1 = nb_origination_tz1 + $sign * coalesce(ori_tz1.nb,0) \
       FROM ori_tz1 WHERE ocu.tz = ori_tz1.tz";
    PGSQL(dbh) (* origination as delegate *)
      "WITH ori_dlg(tz, nb) AS ( \
       SELECT delegate, COUNT( * ) FROM origination  \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash \
       GROUP BY delegate) \
       UPDATE operation_count_user AS ocu SET \
       nb_origination_dlg = nb_origination_dlg + $sign * coalesce(ori_dlg.nb,0) \
       FROM ori_dlg WHERE ocu.tz = ori_dlg.tz";
    PGSQL(dbh) (* activation *)
      "WITH act(tz, nb) AS ( \
       SELECT pkh, COUNT( * ) FROM activation \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash \
       GROUP BY pkh) \
       UPDATE operation_count_user AS ocu SET \
       nb_activation = nb_activation + $sign * coalesce(act.nb,0) \
       FROM act WHERE ocu.tz = act.tz";
    PGSQL(dbh) (* reveal *)
      "WITH rvl(tz, nb) AS ( \
       SELECT source, COUNT( * ) FROM reveal \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash \
       GROUP BY source) \
       UPDATE operation_count_user AS ocu SET \
       nb_reveal = nb_reveal + $sign * coalesce(rvl.nb,0) \
       FROM rvl WHERE ocu.tz = rvl.tz";
    PGSQL(dbh) (* double baking evidence as denouncer/baker*)
      "WITH dbe_bk(tz, nb) AS ( \
       SELECT denouncer, COUNT( * ) FROM double_baking_evidence \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash \
       GROUP BY denouncer) \
       UPDATE operation_count_user AS ocu SET \
       nb_dbe_bk = nb_dbe_bk + $sign * coalesce(dbe_bk.nb,0) \
       FROM dbe_bk WHERE ocu.tz = dbe_bk.tz";
    PGSQL(dbh) (* double baking evidence as accused *)
      "WITH dbe_acc(tz, nb) AS ( \
       SELECT accused, COUNT( * ) FROM double_baking_evidence \
       INNER JOIN block_operation ON hash = operation_hash \
       WHERE block_hash = $hash AND accused <> denouncer \
       GROUP BY accused) \
       UPDATE operation_count_user AS ocu SET \
       nb_dbe_acc = nb_dbe_acc + $sign * coalesce(dbe_acc.nb,0) \
       FROM dbe_acc WHERE ocu.tz = dbe_acc.tz";
    PGSQL(dbh) (* nonce revelation *)
      "WITH nonce(tz, nb) AS ( \
       SELECT bl2.baker, COUNT( * ) FROM seed_nonce_revelation AS a \
       INNER JOIN block_operation AS bo ON a.hash = bo.operation_hash \
       INNER JOIN block AS bl2 ON bl2.level = a.level \
       WHERE block_hash = $hash \
       GROUP BY baker) \
       UPDATE operation_count_user AS ocu SET \
       nb_nonce = nb_nonce + $sign * coalesce(nonce.nb,0) \
       FROM nonce WHERE ocu.tz = nonce.tz";
    PGSQL(dbh) (* endorsement *)
      "WITH endo(tz, nb) AS ( \
       SELECT source, COUNT( * ) FROM endorsement_all \
       WHERE op_block_hash = $hash GROUP BY source ) \
       UPDATE operation_count_user AS ocu SET \
       nb_endorsement = nb_endorsement + $sign * coalesce(endo.nb,0) \
       FROM endo WHERE ocu.tz = endo.tz";
    update_count_info ~force hash level "highest";
  | _ -> debug "[Writer] [update_counts_main] block not registered"

let reset_balance_from_balance_updates account diff frz up_type level  =
  debug "[Writer] [reset_balance] %s : undoing %s balance_update : diff = %s"
    account (if frz then "frozen" else "contract") (Int64.to_string diff);
  let cycle_in_bfbu =
    (* We add one because the index of balance_from_balance_updates has an offset of 1 *)
    Int32.(add (div level (of_int Tezos_constants.Constants.block_per_cycle)) Int32.one) in
  if not frz then
    PGSQL(dbh)
      "UPDATE balance_from_balance_updates \
       SET spendable_balance=spendable_balance - $diff \
       WHERE hash=$account AND cycle>=$cycle_in_bfbu"
  else
    let () =
      PGSQL(dbh)
        "UPDATE balance_from_balance_updates \
         SET frozen=frozen - $diff \
         WHERE hash=$account AND cycle>=$cycle_in_bfbu"
    in
    match up_type with
      "Reward" ->
      PGSQL(dbh)
        "UPDATE balance_from_balance_updates \
         SET rewards=rewards - $diff \
         WHERE hash=$account AND cycle>=$cycle_in_bfbu"
    | "Fee" ->
      PGSQL(dbh)
        "UPDATE balance_from_balance_updates \
         SET fees=fees - $diff \
         WHERE hash=$account AND cycle>=$cycle_in_bfbu"
    | "Deposit" ->
      PGSQL(dbh)
        "UPDATE balance_from_balance_updates \
         SET deposits=deposits - $diff \
         WHERE hash=$account AND cycle>=$cycle_in_bfbu"
    | s -> debug "[Writer] [update_balance_updates] %s undefined" s

let update_distance_level_alt level =
  PGSQL(dbh) "UPDATE block SET distance_level = -1 WHERE level > $level" ;
  PGSQL(dbh) "UPDATE endorsement_all SET distance_level = -1 WHERE op_level > $level" ;
  PGSQL(dbh) "UPDATE endorsement_last SET distance_level = -1 WHERE op_level > $level";
  PGSQL(dbh) "UPDATE transaction_all SET distance_level = -1 WHERE op_level > $level" ;
  PGSQL(dbh) "UPDATE transaction_last SET distance_level = -1 WHERE op_level > $level";
  PGSQL(dbh) "UPDATE origination_all SET distance_level = -1 WHERE op_level > $level" ;
  PGSQL(dbh) "UPDATE origination_last SET distance_level = -1 WHERE op_level > $level";
  PGSQL(dbh) "UPDATE delegation_all SET distance_level = -1 WHERE op_level > $level" ;
  PGSQL(dbh) "UPDATE delegation_last SET distance_level = -1 WHERE op_level > $level";
  PGSQL(dbh) "UPDATE activation_all SET distance_level = -1 WHERE op_level > $level" ;
  PGSQL(dbh) "UPDATE activation_last SET distance_level = -1 WHERE op_level > $level";
  PGSQL(dbh) "UPDATE reveal_all SET distance_level = -1 WHERE op_level > $level" ;
  PGSQL(dbh) "UPDATE reveal_last SET distance_level = -1 WHERE op_level > $level";
  let level32= Int64.to_int32 level in
  let bad_bal_updt =
    PGSQL(dbh) "SELECT hash,diff,frozen,update_type FROM balance_updates \
                WHERE level > $level32 AND distance_level = 0" in
  List.iter
    (fun (account,diff,frz,up_type) ->
       reset_balance_from_balance_updates account diff frz up_type level32)
    bad_bal_updt;

  PGSQL(dbh) "UPDATE balance_updates SET distance_level = -1 WHERE level > $level32"

let update_balances level block_hash =
  debug "[Writer] [update_balance_updates]";
  let level = Int32.of_int level in
  let good_bal_updts =
    PGSQL(dbh) "SELECT hash,diff,frozen,update_type FROM balance_updates \
                WHERE block_hash=$block_hash AND level=$level"
  in
  register_balance_from_balance_updates level good_bal_updts

let reset_main_chain count start_level level64 =
  debug "[Writer] [reset_main_chain] %d %Ld\n%!" start_level level64 ;
  if count then
    begin match PGSQL(dbh) "SELECT hash, level FROM block WHERE distance_level = 0 \
                            ORDER BY level DESC LIMIT 1" with
    | [ hash, level ] ->
      let rec aux hash level =
        if level > level64 then (
          update_counts ~force:true hash (-1L) ;
          match PGSQL(dbh) "SELECT predecessor FROM block \
                            WHERE hash = $hash" with
          | [ pred ] -> aux pred (Int64.pred level)
          | _ -> ()) in
      aux hash level
    | _ -> () end;
  update_distance_level_alt level64;
  let start_level64 = Int64.of_int start_level in
  let cycle =
    PGSQL(dbh) "SELECT cycle FROM block WHERE level = $start_level64 LIMIT 1" in
  begin match cycle with
    | [ cycle ] ->
      PGSQL(dbh) "INSERT INTO switch (cycle, switch_count, longest_alt_chain) \
                  VALUES ($cycle, 0, 0) ON CONFLICT DO NOTHING"
    | _ -> ()
  end ;
  let level = Int64.to_int level64 in
  let chain_depth = start_level - level in
  let chain_depth64 = Int64.of_int chain_depth in
  if chain_depth > 1 then
    begin match cycle with
      | [ cycle ] ->
        let row =
          PGSQL(dbh) "SELECT cycle, switch_count, longest_alt_chain FROM switch \
                      WHERE cycle = $cycle" in
        begin match row with
        | [] ->
          PGSQL(dbh) "INSERT INTO switch \
                      (cycle, switch_count, longest_alt_chain) \
                      VALUES \
                      ($cycle, 1, $chain_depth64)"
        | [ (_, switch_count, longest_alt_chain) ] ->
          let longest = max longest_alt_chain chain_depth64 in
          let switch_count = Int64.add switch_count 1L in
          PGSQL(dbh) "UPDATE switch SET \
                      switch_count = $switch_count, \
                      longest_alt_chain = $longest \
                      WHERE cycle = $cycle"
        | _ -> ()
        end
      | _ -> ()
    end

let update_distance_level_main count hash =
  PGSQL(dbh) "UPDATE block SET distance_level = 0 WHERE hash = $hash";
  PGSQL(dbh) "UPDATE endorsement_all SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE endorsement_last SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE transaction_all SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE transaction_last SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE origination_all SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE origination_last SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE delegation_all SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE delegation_last SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE activation_all SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE activation_last SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE reveal_all SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE reveal_last SET distance_level = 0 WHERE op_block_hash = $hash";
  PGSQL(dbh) "UPDATE balance_updates SET distance_level = 0 WHERE block_hash = $hash";
  if count then update_counts ~force:true hash 1L


let register_main_chain count block =
  pg_lock (fun () ->
      let hash = block.node_hash in
      let pred_hash = block.node_header.header_predecessor in
      let start_level = block.node_header.header_level in
      let rec register_aux hash pred_hash curr_level =
        debug "[Writer] [register_main_chain] %d\n%!" curr_level ;

        match  PGSQL(dbh) "SELECT predecessor, level, distance_level \
                           FROM block WHERE hash = $pred_hash" with
        | [ _, level, 0L ] ->
          reset_main_chain count start_level level ;
          update_balances curr_level hash;
          update_distance_level_main count hash
        | [ pred, level, _ ] ->
          register_aux pred_hash pred @@ Int64.to_int level ;
          update_balances curr_level hash;
          update_distance_level_main count hash
        | _ ->
          debug "[Writer] [main_chain] Can't recover main chain status for %s\n%!"
            pred_hash in
      register_aux hash pred_hash start_level;
    )

let peer_to_string peer =
  match peer with
  | None -> ""
  | Some s -> s

let last_connection =
  function
  | None -> "", ""
  | Some (point, date) -> peer_to_string (Some point), date


let get_country point =
  if point = "" then "", ""
  else begin
    try
      let ip =
        try
          Scanf.sscanf point "::ffff:%[0-9.]:%_d" (fun ip -> ip)
        with _ ->
          Scanf.sscanf point "%[0-9a-fA-F:]" (fun ip -> ip)
      in
      let gi = Geoip.init_exn Geoip.GEOIP_MEMORY_CACHE in
      let country_name = Utils.unopt ~default:"" @@ Geoip.country_name_by_name gi ip in
      let country_code = Utils.unopt ~default:"" @@ Geoip.country_code_by_name gi ip in
      Geoip.close gi ;
      country_name, country_code
    with _ ->
      debug "[Writer] Found unparseable ip: %s\n%!" point;
      "", ""
  end

let register_network_stats stats =
  pg_lock (fun () ->
      PGSQL(dbh) "UPDATE peers SET state= 'disconnected'" ;
      List.iter
        (fun { peer_id; country=_; score ; trusted ; conn_metadata ;
               state ; id_point ; stat ;
               last_failed_connection ; last_rejected_connection ;
               last_established_connection ; last_disconnection ;
               last_seen ; last_miss } ->
          let _conn_metadata = conn_metadata in (* TODO add to db *)
          let point_id = peer_to_string id_point in
          let state =
            match state with
            | Accepted -> "accepted"
            | Running -> "running"
            | Disconnected -> "disconnected" in
          let last_failed_connection_point, last_failed_connection_date =
            last_connection last_failed_connection in
          let last_rejected_connection_point, last_rejected_connection_date =
            last_connection last_rejected_connection in
          let last_established_connection_point, last_established_connection_date =
            last_connection last_established_connection in
          let last_disconnection_point, last_disconnection_date =
            last_connection last_disconnection in
          let last_seen_point, last_seen_date = last_connection last_seen in
          let last_miss_point, last_miss_date = last_connection last_miss in
          let total_sent = stat.total_sent in
          let total_recv = stat.total_recv in
          let current_inflow = Int64.of_int stat.current_inflow in
          let current_outflow = Int64.of_int  stat.current_outflow in
          match PGSQL(dbh) "SELECT peer_id, point_id, country_name, country_code \
                            FROM peers \
                            WHERE peer_id = $peer_id" with
          | [] -> (* No entry just insert a new row *)
            let country_name, country_code = get_country point_id  in
            PGSQL(dbh)
              "INSERT INTO peers \
               (peer_id, country_name, country_code, point_id, trusted, score, state, total_sent, \
               total_received, current_inflow, current_outflow, \
               last_failed_connection_point, last_failed_connection_date, \
               last_rejected_connection_point, last_rejected_connection_date, \
               last_established_connection_point,last_established_connection_date, \
               last_disconnection_point, last_disconnection_date, \
               last_seen_point, last_seen_date, \
               last_miss_point, last_miss_date) \
               VALUES \
               ($peer_id, $country_name, $country_code, $point_id, $trusted, $score, $state, \
               $total_sent, $total_recv, $current_inflow, $current_outflow, \
               $last_failed_connection_point, $last_failed_connection_date, \
               $last_rejected_connection_point, $last_rejected_connection_date, \
               $last_established_connection_point, $last_established_connection_date, \
               $last_disconnection_point, $last_disconnection_date, \
               $last_seen_point, $last_seen_date, \
               $last_miss_point, $last_miss_date)"
          | [ _, point_id_db, country_name, country_code ] -> (* Known peer, juste update info *)
            if point_id = "" then
              PGSQL(dbh)
                "UPDATE peers \
                 SET \
                 trusted = $trusted, \
                 score = $score, \
                 state = $state, \
                 total_sent = $total_sent, \
                 total_received = $total_recv, \
                 current_inflow = $current_inflow, \
                 current_outflow = $current_outflow, \
                 last_failed_connection_point = $last_failed_connection_point, \
                 last_failed_connection_date = $last_failed_connection_date, \
                 last_rejected_connection_point = $last_rejected_connection_point, \
                 last_rejected_connection_date = $last_rejected_connection_date, \
                 last_established_connection_point = $last_established_connection_point, \
                 last_established_connection_date = $last_established_connection_date, \
                 last_disconnection_point = $last_disconnection_point, \
                 last_disconnection_date = $last_disconnection_date, \
                 last_seen_point = $last_seen_point, \
                 last_seen_date = $last_seen_date, \
                 last_miss_point = $last_miss_point, \
                 last_miss_date = $last_miss_date WHERE peer_id = $peer_id"
            else
              let country_name, country_code =
                match point_id_db with
                | None -> country_name, country_code
                | Some point_id_db ->
                  if point_id_db = point_id then country_name, country_code
                  else get_country point_id in
              PGSQL(dbh)
                "UPDATE peers \
                 SET \
                 country_name = $country_name, \
                 country_code = $country_code, \
                 point_id = $point_id, \
                 trusted = $trusted, \
                 score = $score, \
                 state = $state, \
                 total_sent = $total_sent, \
                 total_received = $total_recv, \
                 current_inflow = $current_inflow, \
                 current_outflow = $current_outflow, \
                 last_failed_connection_point = $last_failed_connection_point, \
                 last_failed_connection_date = $last_failed_connection_date, \
                 last_rejected_connection_point = $last_rejected_connection_point, \
                 last_rejected_connection_date = $last_rejected_connection_date, \
                 last_established_connection_point = $last_established_connection_point, \
                 last_established_connection_date = $last_established_connection_date, \
                 last_disconnection_point = $last_disconnection_point, \
                 last_disconnection_date = $last_disconnection_date, \
                 last_seen_point = $last_seen_point, \
                 last_seen_date = $last_seen_date, \
                 last_miss_point = $last_miss_point, \
                 last_miss_date = $last_miss_date WHERE peer_id = $peer_id"
          | _ -> () )
        stats)

let block_hash level =
  debug "[Writer] block_hash/%i\n%!" level;
  let level = Int64.of_int level in
  match PGSQL(dbh)
          "SELECT hash FROM block WHERE level = $level \
           AND distance_level = 0" with
  | [ hash ] -> hash
  | _ -> assert false


let register_crawler_activity name delay =
  let timestamp = Unix.gettimeofday () in
  let delay = Int32.of_int delay in
  PGSQL(dbh) "INSERT INTO crawler_activity (name, timestamp, delay) \
              VALUES ($name, $timestamp, $delay) ON CONFLICT (name) \
              DO UPDATE SET timestamp = $timestamp, delay = $delay"

let update_alias ?(verbose=true) hash alias =
  let query_user = PGSQL(dbh) "SELECT alias FROM user_alias WHERE tz = $hash" in
  let user_has_alias, old_alias = match query_user with
    | [ old_alias ] -> true, old_alias
    | _ -> false, "" in
  let alias_in_use_by =
    match alias with
    | None -> ""
    | Some alias ->
      let query_alias =
        PGSQL(dbh) "SELECT tz FROM user_alias WHERE alias = $alias" in
      match query_alias with
      | [ user ] -> user
      | _ -> "" in
  if alias_in_use_by <> "" then
    (if verbose then debug "[Writer] alias already in use by %s\n%!" alias_in_use_by)
  else
    match user_has_alias, alias with
    | false, Some alias when String.length alias > 0 ->
      PGSQL(dbh) "INSERT INTO user_alias (tz, alias) VALUES ($hash, $alias)";
      PGSQL(dbh) "UPDATE tezos_user SET alias = $alias WHERE hash = $hash"
    | true, Some alias when String.length alias > 0 && alias <> old_alias ->
      PGSQL(dbh) "UPDATE user_alias SET alias = $alias WHERE tz = $hash";
      PGSQL(dbh) "UPDATE tezos_user SET alias = $alias WHERE hash = $hash"
    | true, None | true, Some "" ->
      PGSQL(dbh) "DELETE FROM user_alias WHERE tz = $hash";
      PGSQL(dbh) "UPDATE tezos_user SET alias = NULL WHERE hash = $hash"
    | _ -> ()


(* update highest info and counts if in alternative branch *)
let update_count_info_highest_alt () =
  let rec aux hash =
    match PGSQL(dbh) "SELECT distance_level, predecessor, level \
                      FROM block WHERE hash = $hash" with
    | [ -1L, pred, _ ] ->
      update_counts hash (-1L);
      aux pred
    | [ _, _, level ] ->
      PGSQL(dbh) "UPDATE count_info SET \
                  hash = $hash, level = $level WHERE info = 'highest'"
    | _ -> () in
  match PGSQL(dbh) "SELECT hash FROM count_info WHERE info = 'highest'" with
  | [ hash ] -> aux hash
  | _ -> ()

(* counts from a hash (head in None) down to a level *)
let reset_counts start_level end_hash =
  let rec aux hash level acc =
    Printf.printf "%Ld\n%!" level;
    if level >= start_level then (
      begin match PGSQL(dbh) "SELECT predecessor FROM block WHERE hash = $hash" with
        | [ pred ] -> aux pred (Int64.pred level) (hash :: acc)
        | _ -> assert false end )
    else (
      List.iter (fun hash -> update_counts hash 1L) acc;
      List.nth_opt acc 0)
  in
  match end_hash with
  | None ->
    begin match PGSQL(dbh) "SELECT hash, level FROM block WHERE distance_level = 0 \
                            ORDER BY level DESC LIMIT 1" with
    | [ hash, level ] ->
      let start_hash = aux hash level [] in
      begin match start_hash with
        | None -> ()
        | Some start_hash -> update_count_info start_hash start_level "lowest" end;
      update_count_info hash level "highest";
      List.iter (fun hash -> update_counts hash 0L)
        (PGSQL(dbh) "SELECT hash FROM block WHERE distance_level <> 0 \
                     AND level >= $start_level")
    | _ -> () end
  | Some end_hash ->
     begin match PGSQL(dbh) "SELECT level FROM block WHERE hash = $end_hash" with
    | [ level ] ->
      let start_hash = aux end_hash level [] in
      begin match start_hash with
        | None -> ()
        | Some start_hash -> update_count_info start_hash start_level "lowest" end;
      update_count_info end_hash level "highest";
      List.iter (fun hash -> update_counts hash 0L)
        (PGSQL(dbh) "SELECT hash FROM block WHERE distance_level <> 0 \
                     AND level >= $start_level AND level <= $level")
    | _ -> () end
(* extend count range if needed *)
let counts_downup start_level end_level =
  let start_level = Int64.of_int start_level in
  let end_level = Int64.of_int end_level in
  update_count_info_highest_alt ();
  let lowest_level, lowest_pred =
    match PGSQL(dbh) "SELECT ci.level, bl.predecessor FROM count_info AS ci \
                      INNER JOIN block AS bl ON bl.hash = ci.hash \
                      WHERE ci.info = 'lowest'" with
    | [ level, pred ] -> Some level, Some pred
    | _ ->
      PGSQL(dbh) "INSERT INTO count_info (hash, level, info) \
                  SELECT hash, $start_level, 'lowest' FROM block \
                  WHERE level = $start_level AND distance_level = 0";
      None, None in
  let highest_level =
    match PGSQL(dbh) "SELECT level FROM count_info WHERE info = 'highest'" with
    | [ level ] -> Some level
    | _ ->
      PGSQL(dbh) "INSERT INTO count_info (hash, level, info) \
                  SELECT hash, $end_level, 'highest' FROM block \
                  WHERE level = $end_level AND distance_level = 0";
      None in
  let get_end_hash end_level =
    if end_level = -1L then None
    else
      match PGSQL(dbh) "SELECT hash FROM block WHERE level = $end_level AND \
                        distance_level = 0" with
      | [ hash ] -> Some hash
      | _ -> None in
  let pairs =
    match lowest_level, lowest_pred, highest_level with
    | Some lowest_level, Some lowest_pred, Some highest_level ->
      let low_pair =
        if start_level < lowest_level then
          [start_level, Some lowest_pred]
        else [] in
      let high_pair =
        if end_level = -1L || end_level > highest_level then
          [Int64.succ highest_level, get_end_hash end_level]
        else [] in
      low_pair @ high_pair
    | _ ->  [start_level, get_end_hash end_level] in
  List.iter (fun (start_level, end_hash) -> reset_counts start_level end_hash) pairs
