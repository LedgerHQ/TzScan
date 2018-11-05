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

let search_limit = 20
let debug fmt = Utils.debug !Debug_constants.debug_reader fmt

module Reader_generic (M : Db_intf.MONAD) = struct
  module Monad = M
  open M
  module PGOCaml = PGOCaml_generic.Make(M)

  let dbh_pool =
    let validate conn =
      PGOCaml.alive conn >>= fun is_alive ->
      debug "[Reader] Validate connection : [%b]\n%!" is_alive ;
      M.return is_alive in
    let check _conn is_ok =
      debug "[Reader] Check connection.\n%!" ;
      is_ok false in
    let dispose conn =
      debug "[Reader] Dispose connection.\n%!" ;
      PGOCaml.close conn in
    M.pool_create ~check ~validate ~dispose 20 (fun () ->
      PGOCaml.connect ~database:TzscanConfig.database ())

  let with_dbh f = M.pool_use dbh_pool f

  let (>>>) f g = f g

  let of_count_opt = function
    | [ Some count ] -> return (Int64.to_int count)
    | _ -> return 0

   let of_count = function
    | [ count ] -> return (Int64.to_int count)
    | _ -> return 0

  let test_opt f = function
    | None -> None, true
    | Some x -> Some (f x), false

  let block_successor hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT bl.hash FROM block as bl \
       INNER JOIN block as bl_pred ON bl_pred.hash = bl.predecessor \
       WHERE bl.predecessor = $hash AND bl.hash <> $hash AND \
       bl.distance_level = bl_pred.distance_level" >>= function
    | [] -> return None
    | h :: _ -> return (Some h)

  let level hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT level, level_position, cycle, cycle_position, voting_period, voting_period_position \
       FROM block WHERE hash = $hash" >>=
    fun rows ->
    match Pg_helper.rows_to_option rows with
    | None -> return None
    | Some ( lvl_level, lvl_level_position,
             lvl_cycle, lvl_cycle_position,
             lvl_voting_period, lvl_voting_period_position ) ->
      let lvl_level = Int64.to_int lvl_level in
      let lvl_level_position = Int64.to_int lvl_level_position in
      let lvl_cycle = Int64.to_int lvl_cycle in
      let lvl_cycle_position = Int64.to_int lvl_cycle_position in
      let lvl_voting_period = Int64.to_int lvl_voting_period in
      let lvl_voting_period_position = Int64.to_int lvl_voting_period_position in
      return (Some { lvl_level; lvl_level_position; lvl_cycle; lvl_cycle_position;
                     lvl_voting_period; lvl_voting_period_position  })

  let nb_cycle () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT MAX(cycle) + 1 FROM block" >>= of_count_opt

  let nonces ?(page=0) ?(page_size=20) () =
    nb_cycle () >>= fun nb_cycle ->
    let max_cycle = nb_cycle - page * page_size - 1 in
    let min_cycle = nb_cycle - (page + 1) * page_size - 1 in
    let block_per_cycle = Tezos_constants.Constants.block_per_cycle in
    let max_level = Int64.of_int @@ (max_cycle + 1) * block_per_cycle in
    let min_level = Int64.of_int @@ (min_cycle + 1) * block_per_cycle in
    debug "[Reader] test %d %d %Ld %Ld\n%!" min_cycle max_cycle min_level max_level;
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
        "SELECT o.hash, array_agg(s.level ORDER BY s.level DESC) FROM block AS b \
         INNER JOIN block_operation AS bo ON b.hash = bo.block_hash \
         INNER JOIN operation AS o ON bo.operation_hash = o.hash \
         INNER JOIN seed_nonce_revelation AS s ON s.hash = o.hash \
         WHERE b.distance_level = 0 AND \
         array_position(o.op_anon_type, 'Nonce', 1) > 0 AND \
         s.level > $min_level AND s.level <= $max_level \
         GROUP BY o.hash" >>= fun rows ->
    let n = max 0 (min page_size (max_cycle + 1)) in
    return @@ Misc.list_init n (fun i ->
        let cycle_i = max_cycle - i in
        cycle_i,
        List.fold_left (fun acc (hash, levels) ->
            match levels with
            | None -> acc
            | Some arr -> match List.rev @@ Misc.unopt_list Int64.to_int arr with
              | lv0 :: _ as levels when (lv0 - 1)/ block_per_cycle = cycle_i ->
                (hash, levels) :: acc
              | _ -> acc) [] rows)


  let block ?(operations=false) selector =
    with_dbh >>> fun dbh ->
    if operations
    then
      begin
        match selector with
        | Hash hash ->
          PGSQL(dbh) "SELECT b.*, \
                      p.name AS protocol_name, pt.name AS test_protocol_name, \
                      array_remove(array_agg(cast(o.hash as VARCHAR)),NULL) AS ohash, \
                      array_remove(array_agg(o.op_type),NULL) AS op_type \
                      FROM block AS b \
                      INNER JOIN protocol AS p ON b.protocol = p.hash \
                      INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
                      LEFT JOIN \
                      (block_operation AS bo INNER JOIN operation AS o \
                      ON o.hash = bo.operation_hash) \
                      ON b.hash = bo.block_hash \
                      WHERE b.hash = $hash \
                      GROUP BY b.hash, protocol_name, test_protocol_name"
        | Level level ->
          let level = Int64.of_int level in
          PGSQL(dbh) "SELECT b.*, \
                      p.name AS protocol_name, pt.name AS test_protocol_name, \
                      array_remove(array_agg(cast(o.hash as VARCHAR)),NULL) AS ohash, \
                      array_remove(array_agg(o.op_type),NULL) AS op_type \
                      FROM block AS b \
                      INNER JOIN protocol AS p ON b.protocol = p.hash \
                      INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
                      LEFT JOIN \
                      (block_operation AS bo INNER JOIN operation AS o \
                      ON o.hash = bo.operation_hash) \
                      ON b.hash = bo.block_hash AND b.distance_level = 0 \
                      WHERE level = $level \
                      GROUP BY b.hash, protocol_name, test_protocol_name"
      end >>= fun rows ->
      match Pg_helper.rows_to_option rows with
      | Some row ->
        return @@ Some (Pg_helper.block_of_tuple_with_ops row)
      | None -> return None
    else
      begin
        match selector with
        | Hash hash ->
          PGSQL(dbh) "SELECT b.*, \
                      p.name AS protocol_name, pt.name AS test_protocol_name \
                      FROM block AS b \
                      INNER JOIN protocol AS p ON b.protocol = p.hash \
                      INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
                      WHERE b.hash = $hash"
        | Level level ->
          let level = Int64.of_int level in
          PGSQL(dbh) "SELECT b.*, \
                      p.name AS protocol_name, pt.name AS test_protocol_name \
                      FROM block AS b \
                      INNER JOIN protocol AS p ON b.protocol = p.hash \
                      INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
                      WHERE level = $level AND distance_level = 0"
      end >>= fun rows ->
      return (
        rows
        |> Pg_helper.rows_to_option
        |> Pg_helper.omap Pg_helper.block_of_tuple_noop)


  let blocks ?(page=0) ?(page_size=20) ?(operations=false) () =
    with_dbh >>> fun dbh ->
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    if operations
    then
      PGSQL(dbh)
        "SELECT b.*, \
         p.name AS protocol_name, pt.name AS test_protocol_name, \
         array_remove(array_agg(cast(o.hash AS VARCHAR)),NULL) AS ohash, \
         array_remove(array_agg(o.op_type),NULL) AS op_type \
         FROM block AS b \
         LEFT JOIN
          (block_operation AS bo INNER JOIN operation AS o \
         ON o.hash = bo.operation_hash) \
         ON b.hash = bo.block_hash AND b.distance_level = 0 \
         INNER JOIN protocol AS p ON b.protocol = p.hash \
         INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
         GROUP BY b.hash, protocol_name, test_protocol_name \
         ORDER BY level DESC \
         OFFSET $offset LIMIT $limit"
      >>= fun rows ->
      return @@ List.map Pg_helper.block_of_tuple_with_ops rows
    else
      PGSQL(dbh)
        "SELECT b.*, \
         p.name AS protocol_name, pt.name AS test_protocol_name \
         FROM block as b \
         INNER JOIN protocol AS p ON b.protocol = p.hash \
         INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
         WHERE distance_level = 0 \
         ORDER BY level DESC \
         OFFSET $offset LIMIT $limit" >>= fun rows ->
      return @@ List.map Pg_helper.block_of_tuple_noop rows

  let level_from_cycle_index cycle index =
    index * Tezos_constants.Constants.blocks_per_roll_snapshot +
    (Tezos_constants.Constants.blocks_per_roll_snapshot - 1) +
    (cycle - (Tezos_constants.Constants.allowed_fork + 2)) *
    Tezos_constants.Constants.block_per_cycle + 1

  let snapshot_blocks  ?(page=0) ?(page_size=20) () =
    with_dbh >>> fun dbh ->
    let offset = Int64.of_int (page * page_size) in
    let limit = Int64.of_int page_size in
    PGSQL(dbh)
      "SELECT cycle, index, rolls_count FROM snapshot_rolls \
       WHERE cycle > 6 \
       ORDER BY cycle DESC \
       OFFSET $offset LIMIT $limit" >>= fun rows ->
    return @@
    List.map (fun (cycle, index, rc) ->
        let index = Int32.to_int index in
        let cycle = Int64.to_int cycle in
        let rc = Int32.to_int rc in
        {
          snap_cycle = cycle ;
          snap_index = index ;
          snap_level = level_from_cycle_index cycle index ;
          snap_rolls = rc
        })
      rows

  let nb_snapshot_blocks () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(DISTINCT cycle) FROM snapshot_rolls WHERE cycle > 6"
    >>= of_count_opt

  let snapshot_levels () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT index, cycle FROM snapshot_rolls WHERE cycle > 6" >>= fun rows ->
    return @@ List.map (fun (index, cycle) ->
        let index = Int32.to_int index in
        let cycle = Int64.to_int cycle in
        level_from_cycle_index cycle index) rows

  let head () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT b.*, \
                p.name AS protocol_name, pt.name AS test_protocol_name \
                FROM block AS b \
                INNER JOIN protocol AS p ON b.protocol = p.hash \
                INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
                WHERE distance_level = 0 ORDER BY level DESC LIMIT 1"
    >>= fun rows ->
    return
      ( rows
        |> Pg_helper.rows_to_option
        |> Pg_helper.omap Pg_helper.block_of_tuple_noop )

  let heads ?(page=0) ?(page_size=20) ?level () =
    with_dbh >>> fun dbh ->
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    let level, nolevel = test_opt Int64.of_int level in
    PGSQL(dbh)
      "SELECT b.*, \
       p.name AS protocol_name, pt.name AS test_protocol_name \
       FROM block AS b \
       INNER JOIN protocol AS p ON b.protocol = p.hash \
       INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
       WHERE ($nolevel AND distance_level <> 0 AND level <> 0) OR \
       (NOT $nolevel AND level = $?level) \
       ORDER BY level DESC \
       OFFSET $offset LIMIT $limit"
    >>= fun rows ->
    return @@ List.map Pg_helper.block_of_tuple_noop rows

  let nb_heads () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT count(*) FROM block WHERE distance_level <> 0 AND level <> 0"
    >>= of_count_opt

  let nb_uncles ~level () =
    with_dbh >>> fun dbh ->
    let lvl = Int64.of_int level in
    if level = 0 then return 0 else
      PGSQL(dbh) "SELECT count (*) \
                  FROM block \
                  WHERE (distance_level <> 0  AND level = $lvl)"
      >>= of_count_opt

  let nb_operation_block hash filters =
    with_dbh >>> fun dbh ->
    match filters with
    | [] ->
      PGSQL(dbh) "SELECT operation_count from block WHERE hash = $hash" >>=
      begin function
        | [ count ] -> return @@ Int64.to_int count
        | _ -> return 0
      end
    | [ "Nonce" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN seed_nonce_revelation AS n ON n.hash = o.hash" >>= of_count_opt
    | [ "Activation" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN activation AS a ON a.hash = o.hash" >>= of_count_opt
    | [ "Transaction" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM transaction_all WHERE op_block_hash = $hash"
      >>= of_count_opt
    | [ "Delegation" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN delegation AS d ON d.hash = o.hash" >>= of_count_opt
    | [ "Origination" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN origination AS ori ON ori.hash = o.hash" >>= of_count_opt
    | [ "Reveal" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN reveal AS r ON r.hash = o.hash" >>= of_count_opt
    | [ "Endorsement" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM endorsement_all WHERE op_block_hash = $hash"
      >>= of_count_opt
    | [ "Double_baking_evidence" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN double_baking_evidence AS e ON e.hash = o.hash" >>= of_count_opt
    | [ "Double_endorsement_evidence" ] ->
      PGSQL(dbh)
        "SELECT COUNT(*) FROM operation AS o \
         INNER JOIN (SELECT operation_hash AS oph FROM \
         block_operation WHERE block_hash = $hash) AS op ON op.oph = o.hash \
         INNER JOIN double_endorsement_evidence AS e ON e.hash = o.hash" >>= of_count_opt
    | filters ->
      let (seed, activation, transaction, origination,
           delegation, endorsement, _, _, _, _, reveal) =
        Pg_helper.filters_flag filters in
      PGSQL(dbh)
        "SELECT COUNT(o.hash) FROM operation AS o \
         INNER JOIN block_operation AS bo ON o.hash = bo.operation_hash \
         WHERE \
         bo.block_hash = $hash AND \
         (($seed AND array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
         ($activation AND array_position(o.op_anon_type, 'Activation', 1) > 0) OR \
         ($transaction AND array_position(o.op_manager_type, 'Transaction', 1) > 0) OR \
         ($origination AND array_position(o.op_manager_type, 'Origination', 1) > 0) OR \
         ($delegation AND array_position(o.op_manager_type, 'Delegation', 1) > 0) OR \
         ($reveal AND array_position(o.op_manager_type, 'Reveal', 1) > 0) OR \
         ($endorsement AND o.op_type = 'Endorsement'))" >>= of_count_opt

  let nb_operation_account ~delegate hash filters =
    with_dbh >>> fun dbh -> match filters with
      | [ "Activation" ] ->
        PGSQL(dbh) "SELECT nb_activation \
                    FROM operation_count_user WHERE tz = $hash" >>= of_count
      | [ "Transaction" ] ->
        PGSQL(dbh) "SELECT nb_transaction_src + nb_transaction_dst \
                    FROM operation_count_user WHERE tz = $hash" >>= of_count_opt
      | [ "Delegation" ] ->
        PGSQL(dbh) "SELECT nb_delegation_src + nb_delegation_dlg \
                    FROM operation_count_user WHERE tz = $hash" >>= of_count_opt
      | [ "Origination" ] ->
        if not delegate then (
          PGSQL(dbh) "SELECT nb_origination_src + nb_origination_man + \
                      nb_origination_tz1 FROM operation_count_user WHERE tz = $hash"
            >>= of_count_opt)
        else (
          PGSQL(dbh) "SELECT nb_origination_dlg \
                      FROM operation_count_user WHERE tz = $hash" >>= of_count)
      | [ "Reveal" ] ->
        PGSQL(dbh) "SELECT nb_reveal FROM operation_count_user WHERE tz = $hash"
          >>= of_count
      | [ "Endorsement" ] ->
        PGSQL(dbh) "SELECT nb_endorsement FROM operation_count_user WHERE tz = $hash"
          >>= of_count
      | [ "Double_baking_evidence" ] ->
        PGSQL(dbh) "SELECT nb_dbe_acc + nb_dbe_bk \
                    FROM operation_count_user WHERE tz = $hash" >>= of_count_opt
      | [ "Nonce" ] ->
        PGSQL(dbh) "SELECT nb_nonce FROM operation_count_user WHERE tz = $hash"
        >>= of_count
      | filters ->
        let (_seed, activation, transaction, origination,
             delegation, endorsement, _, _, _, _, reveal) =
          Pg_helper.filters_flag filters in
        PGSQL(dbh)
          "SELECT (CASE WHEN $activation THEN nb_activation ELSE 0 END) + \
           (CASE WHEN $transaction THEN nb_transaction_src + nb_transaction_dst ELSE 0 END) +\
           (CASE WHEN $origination THEN nb_origination_src ELSE 0 END) + \
           (CASE WHEN $delegation THEN nb_delegation_src + nb_delegation_dlg ELSE 0 END) + \
           (CASE WHEN $endorsement THEN nb_endorsement ELSE 0 END) + \
           (CASE WHEN $reveal THEN nb_reveal ELSE 0 END) \
           FROM operation_count_user WHERE tz = $hash" >>= of_count_opt

  let nb_operation_empty pending filters =
    with_dbh >>> fun dbh -> begin match filters with
      | [ "Activation" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM activation AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_activation)::bigint FROM cycle_count"
      | [ "Transaction" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM transaction AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_transaction)::bigint FROM cycle_count"
      | [ "Delegation" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM delegation AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_delegation)::bigint FROM cycle_count"
      | [ "Origination" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM origination AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_origination)::bigint FROM cycle_count"
      | [ "Reveal" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM reveal AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_reveal)::bigint FROM cycle_count"
      | [ "Endorsement" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM endorsement AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_endorsement_op)::bigint FROM cycle_count"
      | [ "Double_baking_evidence" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM double_baking_evidence AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash  \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_dbe)::bigint FROM cycle_count"
      | [ "Double_endorsement_evidence" ] ->
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(*) FROM double_endorsement_evidence AS a \
             LEFT JOIN block_operation AS bo \
             ON a.hash = bo.operation_hash  \
             WHERE bo.block_hash IS NULL"
        else
          PGSQL(dbh) "SELECT SUM(nb_dee)::bigint FROM cycle_count"
      | filters ->
        let (seed, activation, transaction, origination,
             delegation, endorsement, _proposal, _ballot,
             _double_bake, _double_endorse, reveal) =
          Pg_helper.filters_flag filters in
        if pending then
          PGSQL(dbh)
            "SELECT COUNT(o.hash) FROM operation AS o \
             LEFT JOIN block_operation AS bo ON o.hash = bo.operation_hash \
             WHERE \
             ( bo.block_hash IS NULL AND \
             (($seed AND array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
             ($activation AND array_position(o.op_anon_type, 'Activation', 1) > 0) OR \
             ($transaction AND array_position(o.op_manager_type, 'Transaction', 1) > 0) OR \
             ($origination AND array_position(o.op_manager_type, 'Origination', 1) > 0) OR \
             ($delegation AND array_position(o.op_manager_type, 'Delegation', 1) > 0) OR \
             ($reveal AND array_position(o.op_manager_type, 'Reveal', 1) > 0) OR \
             ($endorsement AND o.op_type = 'Endorsement')))"
        else
          PGSQL(dbh)
            "SELECT SUM((CASE WHEN $activation THEN nb_activation ELSE 0 END) + \
             (CASE WHEN $transaction THEN nb_transaction ELSE 0 END) +\
             (CASE WHEN $origination THEN nb_origination ELSE 0 END) + \
             (CASE WHEN $delegation THEN nb_delegation ELSE 0 END) + \
             (CASE WHEN $endorsement THEN array_sum(nb_endorsement, 1) ELSE 0 END) + \
             (CASE WHEN $reveal THEN nb_reveal ELSE 0 END))::bigint \
             FROM cycle_count"
    end >>= of_count_opt

  let nb_operations ?(delegate=false) ?(filters=[]) selector =
    match selector with
    | Block hash -> nb_operation_block hash filters
    | Account hash -> nb_operation_account ~delegate hash filters
    | Empty -> nb_operation_empty false filters
    | Pending -> nb_operation_empty true filters

  let transaction_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT hash, source, destination, fee, counter, \
       amount, parameters, gas_limit, storage_limit, failed, internal, burn_tez, \
       op_level, timestamp_op \
       FROM transaction_all WHERE hash = $op_hash" >>= fun rows ->
    return @@ Pg_helper.transaction_from_db rows

  let origination_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT o.* \
       FROM origination AS o \
       WHERE o.hash = $op_hash" >>= fun rows ->
    return @@ Pg_helper.origination_from_db rows

  let delegation_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT d.* \
       FROM delegation AS d \
       WHERE d.hash = $op_hash" >>= fun rows ->
    return @@ Pg_helper.delegation_from_db rows

  let reveal_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT r.* \
       FROM reveal AS r \
       WHERE r.hash = $op_hash" >>= fun rows ->
    return @@ Pg_helper.reveal_from_db rows

  let activation_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT a.* \
       FROM activation AS a \
       WHERE a.hash = $op_hash" >>= fun rows ->
    return @@ Pg_helper.activation_from_db rows

  let nonce_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT s.* \
       FROM seed_nonce_revelation AS s \
       WHERE s.hash = $op_hash" >>= fun rows ->
    return @@ Pg_helper.nonce_from_db rows

  let double_endorsement_evidence_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT dee.* \
       FROM double_endorsement_evidence AS dee \
       WHERE dee.hash = $op_hash" >>= fun rows ->
    return @@ List.map (fun row -> Pg_helper.dee_from_db (row, -1L)) rows

  let double_baking_evidence_operations op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT bl.signature, dbe.*, h1.*, h2.* \
       FROM double_baking_evidence AS dbe \
       INNER JOIN header AS h1 ON h1.id = dbe.header1 \
       INNER JOIN header AS h2 ON h2.id = dbe.header2 \
       INNER JOIN block AS bl  ON h1.level = bl.level \
       WHERE dbe.hash = $op_hash AND bl.distance_level = 0" >>= fun rows ->
    return @@ List.map Pg_helper.dbe_from_db rows

  let anon_operation op_hash op_anon_types =
    let op_anon_types =
      List.fold_left (fun acc op_type -> match op_type with
          | None -> acc
          | Some t -> t :: acc)
        [] op_anon_types in
    let nonce, activation, dbe, dee = Pg_helper.anon_types op_anon_types in
    (if activation then activation_operations op_hash else return [])
    >>= fun act_ops ->
    (if nonce then nonce_operations op_hash else return [])
    >>= fun nonce_ops ->
    (if dbe then double_baking_evidence_operations op_hash else return [])
    >>= fun dbe_ops ->
    (if dee then double_endorsement_evidence_operations op_hash else return [])
    >>= fun dee_ops ->
    return @@ Some (Anonymous (act_ops @ nonce_ops @ dbe_ops @ dee_ops))

  let manager_operation op_hash op_manager_types =
    let op_manager_types =
      List.fold_left (fun acc op_type -> match op_type with
          | None -> acc
          | Some t -> t :: acc)
        [] op_manager_types in
    let transaction, origination, delegation, reveal =
      Pg_helper.manager_types op_manager_types in
    if not transaction && not origination && not delegation && not reveal then
      return None
    else
      (if transaction then
         transaction_operations op_hash
       else return ("", []))
      >>= fun (tr_src, tr_ops) ->
      (if origination then
         origination_operations op_hash
       else return ("", []))
      >>= fun (ori_src, ori_ops) ->
      (if delegation then
         delegation_operations op_hash
       else return ("", []))
      >>= fun (del_src, del_ops) ->
      (if reveal then
         reveal_operations op_hash
       else return ("", []))
      >>= fun (rvl_src, rvl_ops) ->
      let ops = tr_ops @ ori_ops @ del_ops @ rvl_ops in
      let source =
        if tr_ops <> [] then tr_src
        else
        if ori_ops <> [] then ori_src
        else
        if del_ops <> [] then del_src
        else
        if rvl_ops <> [] then rvl_src
        else "" in
      return @@
      Some
        (Sourced
           (Manager
              ("manager", Alias.to_name source, ops)))

  let operation op_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT o.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, o.op_type, \
       o.op_manager_type, o.op_anon_type \
       FROM operation AS o \
       LEFT JOIN block_operation AS bo ON o.hash = bo.operation_hash \
       LEFT JOIN block AS bl ON bl.hash = bo.block_hash \
       WHERE o.hash = $op_hash ORDER BY bl.distance_level DESC LIMIT 1"
    >>= fun rows -> match rows with
    | [ _, Some op_bhash, Some op_nhash,"Anonymous", _, Some anon_types ] ->
      begin
        anon_operation op_hash anon_types >>= function
        | None -> return None
        | Some op_type ->
          return @@
          Some {
            op_hash;
            op_block_hash = op_bhash;
            op_network_hash = op_nhash;
            op_type }
      end
    | [ _, Some op_bhash, Some op_nhash, "Endorsement", _, _ ] ->
      begin
        PGSQL(dbh)
          "SELECT source, block_level, block_hash, slots, op_level, priority, \
           timestamp FROM endorsement_all \
           WHERE hash = $op_hash" >>= function
        | [ endorse_src, endorse_block_level, endorse_block_hash,
            endorse_slot, endorse_op_level, endorse_priority, endorse_timestamp ] ->
          let endorse_block_level = Int64.to_int endorse_block_level in
          let endorse_op_level = Int64.to_int endorse_op_level in
          let endorse_priority = Int32.to_int endorse_priority in
          let endorse_slot = Misc.unopt_list Int32.to_int endorse_slot in
          let endorse_timestamp = Pg_helper.string_of_cal endorse_timestamp in
          let endorse = {endorse_src = Alias.to_name endorse_src ;
                         endorse_block_hash ; endorse_block_level ; endorse_slot;
                         endorse_op_level; endorse_priority; endorse_timestamp} in
          return @@
          Some {
            op_hash;
            op_block_hash = op_bhash;
            op_network_hash = op_nhash;
            op_type = Sourced (Consensus (Endorsement endorse)) }
        | _ -> return None
      end
    | [ _, _op_bhash, _op_nhash, "Proposals", _, _ ]
    | [ _, _op_bhash, _op_nhash, "Ballot", _, _ ] ->
      return None
    | [ _, _op_bhash, _op_nhash, "Activate", _, _ ]
    | [ _, _op_bhash, _op_nhash, "Actvate_testnet", _, _ ] ->
      return None
    | [ _, Some op_bhash, Some op_nhash, "Manager", Some mana_types, _ ] ->
      begin
        manager_operation op_hash mana_types >>= function
        | None -> return None
        | Some op_type ->
          return @@
          Some {
            op_hash;
            op_block_hash = op_bhash;
            op_network_hash = op_nhash;
            op_type }
      end
    | _ -> return None

  let operations_from_block filters page page_size block_hash =
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    match filters with
    (* Anonymous *)
    | [ "Nonce" ] ->
      PGSQL(dbh)
        "WITH op_join AS \
         (SELECT bo.operation_hash, bo.block_hash, bl.network, o.op_type \
         FROM block_operation AS bo \
         INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
         INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
         WHERE bo.block_hash = $block_hash AND \
         array_position(o.op_anon_type, 'Nonce', 1) > 0) \
         SELECT opj.*, \
         s.* \
         FROM op_join AS opj \
         LEFT JOIN seed_nonce_revelation AS s \
         ON opj.operation_hash = s.hash \
         ORDER BY s.hash LIMIT $limit OFFSET $offset" >>= fun rows ->
      let grouped =
        List.fold_left
          (fun acc (oph, op_bhash, op_nhash, _, _, level, nonce) ->
             let level = Int64.to_int level in
             try
               let (op_bhash, op_nhash, levelsnonces) =
                 List.assoc oph acc in
               (oph, (op_bhash, op_nhash,
                      (Seed_nonce_revelation
                         { seed_level = level;
                           seed_nonce = nonce })
                      :: levelsnonces)) ::
               (List.remove_assoc oph acc)
             with Not_found ->
               (oph,
                (op_bhash,
                 op_nhash,
                 [ Seed_nonce_revelation
                     { seed_level = level;
                       seed_nonce = nonce }])) :: acc
          ) [] rows in
      return @@
      List.map (fun (op_hash, (op_block_hash, op_network_hash, seed_op)) ->
          { op_hash; op_block_hash; op_network_hash;
            op_type = Anonymous seed_op })
        grouped

    | [ "Activation" ] ->
      PGSQL(dbh)
        "WITH op_join AS \
         (SELECT bo.operation_hash, bo.block_hash, bl.network, o.op_type \
         FROM block_operation AS bo \
         INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
         INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
         WHERE bo.block_hash = $block_hash AND \
         array_position(o.op_anon_type, 'Activation', 1) > 0) \
         SELECT opj.*, \
         a.* \
         FROM op_join AS opj \
         LEFT JOIN activation AS a \
         ON opj.operation_hash = a.hash \
         ORDER BY a.hash LIMIT $limit OFFSET $offset" >>= fun rows ->
      let grouped =
        List.fold_left
          (fun acc (oph, op_bhash, op_nhash, _, _, pkh, secret) ->
             try
               let (op_bhash, op_nhash, pkhsecret) =
                 List.assoc oph acc in
               (oph, (op_bhash, op_nhash,
                      (Activation
                         { act_pkh = Alias.to_name pkh;
                           act_secret = secret })
                      :: pkhsecret)) ::
               (List.remove_assoc oph acc)
             with Not_found ->
               (oph,
                (op_bhash,
                 op_nhash,
                 [ Activation
                     { act_pkh = Alias.to_name pkh;
                       act_secret = secret }])) :: acc
          ) [] rows in
      return @@
      List.map (fun (op_hash, (op_block_hash, op_network_hash, act_op)) ->
          { op_hash; op_block_hash; op_network_hash;
            op_type = Anonymous act_op })
        grouped

    | [ "Double_baking_evidence" ] ->
      debug  "[Reader] TODO : Double_baking_evidence\n%!";
      return []

    | [ "Double_endorsement_evidence" ] ->
      debug  "[Reader] TODO : Double_endorsement_evidence\n%!";
      return []

    (* Consensus *)
    | [ "Endorsement" ] ->
      PGSQL(dbh)
        "SELECT hash, op_block_hash, network, source, block_hash, slots, \
         block_level, op_level, priority, timestamp \
         FROM endorsement_all WHERE op_block_hash = $block_hash \
         ORDER BY priority, slots LIMIT $limit OFFSET $offset"
      >>= fun rows ->
      return @@
      List.map
        (fun (op_hash, op_block_hash, op_network_hash, endorse_src,
              endorse_block_hash, endorse_slot, endorse_block_level,
              endorse_op_level, endorse_priority, endorse_timestamp) ->
          let endorse_block_level = Int64.to_int endorse_block_level in
          let endorse_op_level = Int64.to_int endorse_op_level in
          let endorse_priority = Int32.to_int endorse_priority in
          let endorse_slot = Misc.unopt_list Int32.to_int endorse_slot in
          let endorse_timestamp = Pg_helper.string_of_cal endorse_timestamp in
          let e = { endorse_src = Alias.to_name endorse_src; endorse_block_hash ;
                    endorse_block_level; endorse_slot; endorse_op_level;
                    endorse_priority; endorse_timestamp} in
          { op_hash; op_block_hash; op_network_hash;
            op_type = Sourced (Consensus (Endorsement e)) }
        ) rows

    (* Manager *)
    | [ "Transaction" ] ->
      PGSQL(dbh)
        "SELECT hash, op_block_hash, network, source, destination, fee, counter, \
         amount, parameters, gas_limit, storage_limit, failed, internal, burn_tez, \
         op_level, timestamp_block \
         FROM transaction_all WHERE op_block_hash = $block_hash \
         ORDER BY amount, hash LIMIT $limit OFFSET $offset"
      >>= fun rows ->
      let grouped =
        List.fold_left
          (fun acc (oph, op_bhash, op_nhash, tr_src, tr_dst, tr_fee,
                    tr_counter, tr_amount, tr_parameters, gas_limit, storage_limit,
                    tr_failed, tr_internal, tr_burn, tr_op_level, tr_timestamp) ->
            let tr_gas_limit =
              match gas_limit with
              | None -> Z.zero
              | Some gas_limit -> Z.of_int64 gas_limit in
            let tr_storage_limit =
              match storage_limit with
              | None -> Z.zero
              | Some storage_limit -> Z.of_int64 storage_limit in
            let tr_counter = Int64.to_int32 tr_counter in
            let tr_op_level = Misc.unoptf (-1) Int64.to_int tr_op_level in
            let tr_timestamp = Misc.unoptf ""
                Pg_helper.string_of_cal tr_timestamp in
            match List.assoc_opt oph acc with
            | Some (op_bhash, op_nhash, src, trans) ->
              (oph, (op_bhash, op_nhash, src,
                     (Transaction
                        { tr_src = Alias.to_name tr_src ;
                          tr_dst = Alias.to_name tr_dst; tr_amount ;
                          tr_parameters ; tr_failed ; tr_internal; tr_counter;
                          tr_fee; tr_gas_limit; tr_storage_limit; tr_burn;
                          tr_op_level; tr_timestamp })
                     :: trans)) ::
              (List.remove_assoc oph acc)
            | None ->
              (oph, (op_bhash, op_nhash, tr_src,
                     [ Transaction
                         { tr_src = Alias.to_name tr_src ;
                           tr_dst = Alias.to_name tr_dst ; tr_amount ;
                           tr_parameters ; tr_failed ; tr_internal; tr_counter;
                           tr_fee; tr_gas_limit; tr_storage_limit; tr_burn ;
                           tr_op_level; tr_timestamp } ])) :: acc
          ) [] rows in
      return @@
      List.rev @@ List.fold_left
        (fun acc row -> match row with
           | (op_hash, (Some op_block_hash, Some op_network_hash, src, tr_ops)) ->
             { op_hash; op_block_hash; op_network_hash;
               op_type =
                 Sourced (Manager ("manager", Alias.to_name src, tr_ops)) } :: acc
           | _ -> acc )
        [] grouped

    | [ "Delegation" ] ->
      PGSQL(dbh)
        "WITH op_join AS \
         (SELECT bo.operation_hash, bo.block_hash, bl.network, o.op_type \
         FROM block_operation AS bo \
         INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
         INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
         WHERE bo.block_hash = $block_hash AND \
         array_position(o.op_manager_type, 'Delegation', 1) > 0) \
         SELECT opj.*, \
         d.* \
         FROM op_join AS opj \
         LEFT JOIN delegation AS d \
         ON opj.operation_hash = d.hash \
         ORDER BY d.hash, d.counter LIMIT $limit OFFSET $offset" >>= fun rows ->
      let grouped =
        List.fold_left
          (fun acc (oph, op_bhash, op_nhash, _,
                    _, del_src, _, del_fee, del_counter, del_delegate,
                    gas_limit, storage_limit, del_failed, del_internal) ->
            let del_delegate = Utils.unopt del_delegate ~default:"" in
            let del_gas_limit =
              match gas_limit with
              | None -> Z.zero
              | Some gas_limit -> Z.of_int64 gas_limit in
            let del_storage_limit =
              match storage_limit with
              | None -> Z.zero
              | Some storage_limit -> Z.of_int64 storage_limit in
            let del_counter = Int64.to_int32 del_counter in
            try
              let (op_bhash, op_nhash, src, dels) =
                List.assoc oph acc in
              (oph, (op_bhash, op_nhash, src,
                     (Delegation
                        { del_src = Alias.to_name del_src ;
                          del_delegate = Alias.to_name del_delegate;
                          del_counter ; del_fee ;
                          del_gas_limit ; del_storage_limit ;
                          del_failed ; del_internal }) :: dels)) ::
              (List.remove_assoc oph acc)
            with Not_found ->
              (oph,
               (op_bhash, op_nhash, del_src,
                [ Delegation
                    { del_src = Alias.to_name del_src ;
                      del_delegate = Alias.to_name del_delegate;
                      del_counter ; del_fee ;
                      del_gas_limit ; del_storage_limit ;
                      del_failed ; del_internal } ])) :: acc) [] rows in
      return @@
      List.map
        (fun (op_hash,
              (op_block_hash, op_network_hash, src, del_ops)) ->
          { op_hash; op_block_hash; op_network_hash;
            op_type =
              Sourced (Manager ("manager", Alias.to_name src, del_ops)) })
        grouped

    | [ "Reveal" ] ->
      PGSQL(dbh)
        "WITH op_join AS \
         (SELECT bo.operation_hash, bo.block_hash, bl.network, o.op_type \
         FROM block_operation AS bo \
         INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
         INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
         WHERE bo.block_hash = $block_hash AND \
         array_position(o.op_manager_type, 'Reveal', 1) > 0) \
         SELECT opj.*, \
         r.* \
         FROM op_join AS opj \
         LEFT JOIN reveal AS r \
         ON opj.operation_hash = r.hash \
         ORDER BY r.hash, r.counter LIMIT $limit OFFSET $offset" >>= fun rows ->
      let grouped =
        List.fold_left
          (fun acc (oph, op_bhash, op_nhash, _,
                    _, rvl_src, rvl_fee, rvl_counter, rvl_key, gas_limit,
                    storage_limit, rvl_failed, rvl_internal) ->
            let rvl_pubkey = Utils.unopt rvl_key ~default:"" in
            let rvl_gas_limit =
              match gas_limit with
              | None -> Z.zero
              | Some gas_limit -> Z.of_int64 gas_limit in
            let rvl_storage_limit =
              match storage_limit with
              | None -> Z.zero
              | Some storage_limit -> Z.of_int64 storage_limit in
            let rvl_counter = Int64.to_int32 rvl_counter in
            try
              let (op_bhash, op_nhash, src, rvls) =
                List.assoc oph acc in
              (oph, (op_bhash, op_nhash, src,
                     (Reveal
                        { rvl_src = Alias.to_name rvl_src ; rvl_pubkey ;
                          rvl_counter ; rvl_fee ;
                          rvl_gas_limit ; rvl_storage_limit ;
                          rvl_failed ; rvl_internal }) :: rvls)) ::
              (List.remove_assoc oph acc)
            with Not_found ->
              (oph,
               (op_bhash, op_nhash, rvl_src,
                [ Reveal
                    { rvl_src = Alias.to_name rvl_src ; rvl_pubkey ;
                      rvl_counter ; rvl_fee ;
                      rvl_gas_limit ; rvl_storage_limit ;
                      rvl_failed ; rvl_internal } ])) :: acc
          ) [] rows in
      return @@
      List.map
        (fun (op_hash,
              (op_block_hash, op_network_hash, src, rvl_ops)) ->
          { op_hash; op_block_hash; op_network_hash;
            op_type =
              Sourced (Manager ("manager", Alias.to_name src, rvl_ops)) })
        grouped

    | [ "Origination" ] ->
      PGSQL(dbh)
        "WITH op_join AS \
         (SELECT bo.operation_hash, bo.block_hash, bl.network, o.op_type \
         FROM block_operation AS bo \
         INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
         INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
         WHERE bo.block_hash = $block_hash AND \
         array_position(o.op_manager_type, 'Origination', 1) > 0) \
         SELECT opj.*, \
         ori.* \
         FROM op_join AS opj \
         LEFT JOIN Origination AS ori \
         ON opj.operation_hash = ori.hash \
         ORDER BY ori.hash, ori.counter LIMIT $limit OFFSET $offset" >>= fun rows ->
      let grouped =
        List.fold_left
          (fun acc (oph, op_bhash, op_nhash, _,
                    _, or_src, or_tz1, or_fee, or_counter,or_manager, or_delegate,
                    sc_code, sc_storage, or_spendable,
                    or_delegatable, or_balance, gas_limit, storage_limit,
                    or_failed, or_internal, or_burn) ->
            let or_delegate =
              match or_delegate with None -> or_src | Some del -> del in
            let sc_code = match sc_code with
              | None -> "{}"
              | Some code -> code in
            let sc_storage = match sc_storage with
              | None -> "{}"
              | Some st -> st in
            let or_script = Some { sc_code; sc_storage }  in
            let or_gas_limit =
              match gas_limit with
              | None -> Z.zero
              | Some gas_limit -> Z.of_int64 gas_limit in
            let or_storage_limit =
              match storage_limit with
              | None -> Z.zero
              | Some storage_limit -> Z.of_int64 storage_limit in
            let or_counter = Int64.to_int32 or_counter in
            let origi = {
              or_src = Alias.to_name or_src ;
              or_tz1 = Alias.to_name or_tz1;
              or_manager = Alias.to_name or_manager;
              or_delegate = Alias.to_name or_delegate;
              or_counter ; or_fee ; or_gas_limit ; or_storage_limit ;
              or_script;
              or_spendable; or_delegatable; or_balance ;
              or_failed ;
              or_internal ; or_burn } in
            try
              let (op_bhash, op_nhash, src, oris) =
                List.assoc oph acc in
              (oph, (op_bhash, op_nhash, src, (Origination origi) :: oris)) ::
              (List.remove_assoc oph acc)
            with Not_found ->
              (oph,
               (op_bhash, op_nhash, or_src,
                [ Origination origi ])) :: acc) [] rows in
      return @@
      List.map
        (fun (op_hash,
              (op_block_hash, op_network_hash, src, ori_ops)) ->
          { op_hash; op_block_hash; op_network_hash;
            op_type =
              Sourced (Manager ("manager", Alias.to_name src, ori_ops)) })
        grouped

    (* Dictator *)
    | [ "Activate" ] | [ "Activate_test" ] ->
      debug "[Reader] TODO : Dictator ops\n%!";
      return []

    (* Amendement*)
    | [ "Proposal" ] | [ "Ballot" ] ->
      debug "[Reader] TODO : Amendement ops\n%!";
      return []

    | filters ->
      let (seed, activation, transaction, origination,
           delegation, endorsement, _, _, _, _, reveal) =
        Pg_helper.filters_flag filters in
      PGSQL(dbh)
        "nullable-results"
        "WITH op_join AS \
         (SELECT bo.operation_hash, bo.block_hash, bl.network, o.op_type, \
         bl.level
         FROM block_operation AS bo \
         INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
         INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
         WHERE bo.block_hash = $block_hash AND \
         (($transaction AND \
         array_position(o.op_manager_type, 'Transaction', 1) > 0) OR \
         ($delegation AND \
         array_position(o.op_manager_type, 'Delegation', 1) > 0) OR \
         ($origination AND \
         array_position(o.op_manager_type, 'Origination', 1) > 0) OR \
         ($reveal AND \
         array_position(o.op_manager_type, 'Reveal', 1) > 0) OR \
         ($seed AND \
         array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
         ($activation AND \
         array_position(o.op_anon_type, 'Activation', 1) > 0) OR \
         ($endorsement AND o.op_type = 'Endorsement'))) \
         SELECT o.*, \
         s.*, \
         a.*, \
         t.*, \
         ori.*, \
         d.*, \
         e.*, \
         r.* \
         FROM op_join AS o \
         LEFT JOIN seed_nonce_revelation AS s ON (o.operation_hash = s.hash) \
         LEFT JOIN activation AS a ON (o.operation_hash = a.hash) \
         LEFT JOIN transaction AS t ON (o.operation_hash = t.hash) \
         LEFT JOIN origination AS ori ON (o.operation_hash = ori.hash) \
         LEFT JOIN delegation AS d ON (o.operation_hash = d.hash) \
         LEFT JOIN endorsement AS e ON (o.operation_hash = e.hash) \
         LEFT JOIN reveal AS r ON (o.operation_hash = r.hash) \
         ORDER BY o.operation_hash LIMIT $limit OFFSET $offset"
      >>= fun rows ->
      return @@ Pg_helper.operation_from_db_list rows

  let operations_from_account filters page page_size hash =
    with_dbh >>> fun dbh ->
    debug "[Reader] [operations_from_account %s %d %d\n%!" hash page page_size ;
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    let (_seed, activation, transaction, origination,
         delegation, endorsement, _, _, _, _, reveal) =
      Pg_helper.filters_flag filters in
    let seed = false in
    PGSQL(dbh)
      "nullable-results"
      "WITH op AS (\
       (SELECT o.hash, 'prevalidation' AS block_hash, \
       'prevalidation' AS network_hash, o.op_type, -1::bigint, \
       s.*, \
       f.*, \
       t.*, \
       ori.*, \
       d.*, \
       e.*, \
       r.* \
       FROM operation AS o \
       LEFT JOIN block_operation AS bo ON (o.hash = bo.operation_hash) \
       LEFT JOIN seed_nonce_revelation AS s ON (o.hash = s.hash) \
       LEFT JOIN activation AS f ON (o.hash = f.hash) \
       LEFT JOIN transaction AS t ON (o.hash = t.hash) \
       LEFT JOIN origination AS ori ON (o.hash = ori.hash) \
       LEFT JOIN delegation AS d ON (o.hash = d.hash) \
       LEFT JOIN endorsement AS e ON o.hash = e.hash \
       LEFT JOIN reveal AS r ON (o.hash = r.hash) \
       WHERE bo.block_hash IS NULL AND \
       (($seed AND array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
       ($activation AND array_position(o.op_anon_type, 'Activation', 1) > 0 \
       AND f.pkh = $hash) OR \
       ($transaction AND array_position(o.op_manager_type, 'Transaction', 1) > 0 \
       AND (t.source = $hash OR t.destination = $hash)) OR \
       ($origination AND array_position(o.op_manager_type, 'Origination', 1) > 0 \
       AND ori.source = $hash) OR \
       ($delegation AND array_position(o.op_manager_type, 'Delegation', 1) > 0 \
       AND (d.source = $hash OR d.delegate = $hash)) OR \
       ($endorsement AND o.op_type = 'Endrosement' \
       AND e.source = $hash) OR \
       ($reveal AND array_position(o.op_manager_type, 'Reveal', 1) > 0 \
       AND r.source = $hash))) \
       UNION ALL \
       (SELECT o.hash, bo.block_hash, bl.network, o.op_type, bl.level,  \
       s.*, \
       f.*, \
       t.*, \
       ori.*, \
       d.*, \
       e.*, \
       r.* \
       FROM block_operation AS bo \
       INNER JOIN operation AS o ON o.hash = bo.operation_hash \
       INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
       LEFT JOIN seed_nonce_revelation AS s ON \
       (o.hash = s.hash) \
       LEFT JOIN activation AS f ON \
       (o.hash = f.hash) \
       LEFT JOIN transaction AS t ON \
       (o.hash = t.hash) \
       LEFT JOIN origination AS ori ON \
       (o.hash = ori.hash) \
       LEFT JOIN delegation AS d ON \
       (o.hash = d.hash) \
       LEFT JOIN endorsement AS e \
       ON o.hash = e.hash \
       LEFT JOIN reveal AS r ON \
       (o.hash = r.hash) \
       WHERE bl.distance_level = 0 AND \
       (($seed AND array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
       ($activation AND  array_position(o.op_anon_type, 'Activation', 1) > 0 \
       AND f.pkh = $hash) OR \
       ($transaction AND  array_position(o.op_manager_type, 'Transaction', 1) > 0 \
       AND (t.source = $hash OR t.destination = $hash)) OR \
       ($delegation AND  array_position(o.op_manager_type, 'Delegation', 1) > 0 \
       AND (d.source = $hash OR d.delegate = $hash)) OR \
       ($origination AND  array_position(o.op_manager_type, 'Origination', 1) > 0 \
       AND ori.source =$hash) OR \
       ($endorsement AND o.op_type = 'Endorsement' \
       AND e.source = $hash) OR \
       ($reveal AND  array_position(o.op_manager_type, 'Reveal', 1) > 0 \
       AND r.source = $hash)) \
       ORDER BY bl.level DESC)) \
       SELECT * FROM op LIMIT $page_size64 OFFSET $offset" >>= fun rows ->
    return @@ Pg_helper.operation_from_db_list rows

  let transaction_from_account page page_size hash =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "SELECT hash, op_block_hash, network, source, destination, fee, counter, \
       amount, parameters, gas_limit, storage_limit, failed, internal, \
       burn_tez, op_level, timestamp_block \
       FROM transaction_all WHERE source = $hash OR destination = $hash \
       AND distance_level = 0 \
       ORDER BY op_level DESC LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.transaction_from_db_list rows

  let delegation_from_account page page_size hash =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "WITH ops AS \
       (WITH op AS (SELECT * FROM delegation WHERE source = $hash OR delegate = $hash) \
       SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
       op.source, op.pubkey, op.fee, op.counter, op.delegate, \
       op.gas_limit, op.storage_limit, op.failed, op.internal \
       FROM op \
       LEFT JOIN block_operation AS bo ON op.hash = bo.operation_hash \
       LEFT JOIN block AS bl ON (bl.hash = bo.block_hash) \
       WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
       SELECT ops.hash, ops.block_hash, ops.network_hash, \
       ops.source, ops.pubkey, ops.fee, ops.counter, ops.delegate, \
       ops.gas_limit, ops.storage_limit, ops.failed, ops.internal \
       FROM ops \
       ORDER BY ops.level DESC, ops.hash NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.delegation_from_db_list rows

  let reveal_from_account page page_size hash =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "WITH ops AS \
       (WITH op AS (SELECT * FROM reveal WHERE source = $hash) \
       SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
       op.source, op.fee, op.counter, op.pubkey, \
       op.gas_limit, op.storage_limit, op.failed, op.internal \
       FROM op \
       LEFT JOIN block_operation AS bo ON op.hash = bo.operation_hash \
       LEFT JOIN block AS bl ON (bl.hash = bo.block_hash) \
       WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
       SELECT ops.hash, ops.block_hash, ops.network_hash, \
       ops.source, ops.fee, ops.counter, ops.pubkey, \
       ops.gas_limit, ops.storage_limit, ops.failed, ops.internal \
       FROM ops \
       ORDER BY ops.level DESC, ops.hash NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.reveal_from_db_list rows

  let activation_from_account page page_size hash =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "WITH ops AS \
       (WITH op AS (SELECT * FROM activation WHERE pkh = $hash) \
       SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
       op.pkh, op.secret \
       FROM op \
       LEFT JOIN block_operation AS bo ON op.hash = bo.operation_hash \
       LEFT JOIN block AS bl ON (bl.hash = bo.block_hash) \
       WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
       SELECT ops.hash, ops.block_hash, ops.network_hash, \
       ops.pkh, ops.secret \
       FROM ops \
       ORDER BY ops.level DESC, ops.hash NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.activation_from_db_list rows

  let endorsement_from_account page page_size hash =
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "SELECT hash, op_block_hash, network, source, block_hash, slots, \
       block_level, op_level, priority, timestamp \
       FROM endorsement_all WHERE distance_level = 0 AND source = $hash \
       ORDER BY block_level DESC LIMIT $limit OFFSET $offset"
    >>= fun rows ->
    return @@ Pg_helper.endorsement_from_db_list rows

  let origination_from_account ~delegate page page_size hash =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    begin
      if not delegate then
        PGSQL(dbh)
          "WITH ops AS \
           (WITH op AS (SELECT * FROM origination \
           WHERE (source = $hash OR tz1 = $hash OR manager = $hash)) \
           SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
           COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
           op.source, op.tz1, op.fee, op.counter, op.manager, op.delegate, \
           op.script_code, op.script_storage_type, op.spendable, op.delegatable, \
           op.balance, op.gas_limit, op.storage_limit, op.failed, op.internal, \
           op.burn_tez \
           FROM op \
           LEFT JOIN block_operation AS bo ON op.hash = bo.operation_hash \
           LEFT JOIN block AS bl ON (bl.hash = bo.block_hash) \
           WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
           SELECT ops.hash, ops.block_hash, ops.network_hash, ops.source, \
           ops.tz1, ops.fee, ops.counter, ops.manager, ops.delegate, \
           ops.script_code, ops.script_storage_type, \
           ops.spendable, ops.delegatable, ops.balance, ops.gas_limit, \
           ops.storage_limit, ops.failed, ops.internal, \
           ops.burn_tez
           FROM ops \
           ORDER BY ops.level DESC, ops.hash NULLS FIRST \
           LIMIT $page_size64 OFFSET $offset"
      else
        PGSQL(dbh)
          "WITH ops AS \
           (WITH op AS (SELECT * FROM origination \
           WHERE delegate = $hash) \
           SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
           COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
           op.source, op.tz1, op.fee, op.counter, op.manager, op.delegate, \
           op.script_code, op.script_storage_type, op.spendable, op.delegatable, \
           op.balance, op.gas_limit, op.storage_limit, op.failed, op.internal, \
           op.burn_tez \
           FROM op \
           LEFT JOIN block_operation AS bo ON op.hash = bo.operation_hash \
           LEFT JOIN block AS bl ON (bl.hash = bo.block_hash) \
           WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
           SELECT ops.hash, ops.block_hash, ops.network_hash, ops.source, \
           ops.tz1, ops.fee, ops.counter, ops.manager, ops.delegate, \
           ops.script_code, ops.script_storage_type, \
           ops.spendable, ops.delegatable, ops.balance, ops.gas_limit, \
           ops.storage_limit, ops.failed, ops.internal, ops.burn_tez \
           FROM ops \
           ORDER BY ops.level DESC, ops.hash NULLS FIRST \
           LIMIT $page_size64 OFFSET $offset"
    end
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.origination_from_db_list rows

  let operations_from_pending filters page page_size =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    let (seed, activation, transaction, origination,
         delegation, endorsement, _, _, _, _, reveal) =
      Pg_helper.filters_flag filters in
    PGSQL(dbh)
      "nullable-results"
      "WITH op_join AS \
       (SELECT o.hash, o.op_type \
       FROM operation AS o \
       LEFT JOIN block_operation AS bo ON (o.hash = bo.operation_hash) \
       WHERE bo.block_hash IS NULL AND \
       (($seed AND array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
       ($activation AND array_position(o.op_anon_type, 'Activation', 1) > 0) OR \
       ($transaction AND array_position(o.op_manager_type, 'Transaction', 1) > 0) OR \
       ($origination AND array_position(o.op_manager_type, 'Origination', 1) > 0) OR \
       ($delegation AND array_position(o.op_manager_type, 'Delegation', 1) > 0) OR \
       ($endorsement AND o.op_type = 'Endorsement') OR \
       ($reveal AND array_position(o.op_manager_type, 'Reveal', 1) > 0)) \
       LIMIT $page_size64 OFFSET $offset) \
       SELECT opj.hash, 'prevalidation' :: text, 'prevalidation' :: text, \
       opj.op_type, -1::bigint, \
       s.*, \
       f.*, \
       t.*, \
       ori.*, \
       d.*, \
       e.*, \
       r.* \
       FROM op_join AS opj \
       LEFT JOIN seed_nonce_revelation AS s ON ($seed AND opj.hash = s.hash) \
       LEFT JOIN activation AS f ON ($activation AND opj.hash = f.hash) \
       LEFT JOIN transaction AS t ON ($transaction AND opj.hash = t.hash) \
       LEFT JOIN origination AS ori ON ($origination AND opj.hash = ori.hash) \
       LEFT JOIN delegation AS d ON ($delegation AND opj.hash = d.hash) \
       LEFT JOIN endorsement AS e ON ($endorsement AND opj.hash = e.hash) \
       LEFT JOIN reveal AS r ON ($reveal AND opj.hash = r.hash)"
    >>= fun rows ->
    return @@ Pg_helper.operation_from_db_list rows

  let transaction_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    begin
      if Int64.add page_size64 offset < Pg_update.Constants.last_transactions then
        PGSQL(dbh)
          "SELECT hash, op_block_hash, network, source, destination, fee, counter, \
           amount, parameters, gas_limit, storage_limit, failed, internal, burn_tez, \
           op_level, timestamp_block \
           FROM transaction_last WHERE distance_level = 0 \
           ORDER BY op_level DESC LIMIT $page_size64 OFFSET $offset"
      else
        PGSQL(dbh)
          "SELECT hash, op_block_hash, network, source, destination, fee, counter, \
           amount, parameters, gas_limit, storage_limit, failed, internal, burn_tez, \
           op_level, timestamp_block \
           FROM transaction_all WHERE distance_level = 0 \
           ORDER BY op_level DESC LIMIT $page_size64 OFFSET $offset"
    end
    >>= fun rows ->
    debug "[Reader] ROWS %d\n%!" (List.length rows);
    return @@ List.rev @@ Pg_helper.transaction_from_db_list rows

  let delegation_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "WITH ops AS \
       (WITH op AS (SELECT * FROM delegation) \
       SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
       op.source, op.pubkey, op.fee, op.counter, op.delegate, \
       op.gas_limit, op.storage_limit, op.failed, op.internal \
       FROM op \
       INNER JOIN block_operation AS bo ON op.hash = bo.operation_hash \
       INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
       WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
       SELECT ops.hash, ops.block_hash, ops.network_hash, \
       ops.source, ops.pubkey, ops.fee, ops.counter, ops.delegate, \
       ops.gas_limit, ops.storage_limit, ops.failed, ops.internal \
       FROM ops \
       ORDER BY ops.level DESC, ops.hash NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.delegation_from_db_list rows

  let reveal_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "WITH ops AS \
       (WITH op AS (SELECT * FROM reveal) \
       SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
       op.source, op.fee, op.counter, op.pubkey, \
       op.gas_limit, op.storage_limit, op.failed, op.internal \
       FROM op \
       INNER JOIN block_operation AS bo ON op.hash = bo.operation_hash \
       INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
       WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
       SELECT ops.hash, ops.block_hash, ops.network_hash, \
       ops.source, ops.fee, ops.counter, ops.pubkey, \
       ops.gas_limit, ops.storage_limit, ops.failed, ops.internal \
       FROM ops \
       ORDER BY ops.level DESC, ops.hash NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.reveal_from_db_list rows

  let endorsement_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    begin
      if Int64.add offset limit > Pg_update.Constants.last_transactions then
        PGSQL(dbh)
          "SELECT hash, op_block_hash, network, source, block_hash, slots, \
           block_level, op_level, priority, timestamp \
           FROM endorsement_all where distance_level = 0 \
           ORDER BY block_level DESC LIMIT $limit OFFSET $offset"
      else
        PGSQL(dbh)
          "SELECT hash, op_block_hash, network, source, block_hash, slots, \
           block_level, op_level, priority, timestamp \
           FROM endorsement_last where distance_level = 0 \
           ORDER BY block_level DESC LIMIT $limit OFFSET $offset"
    end
    >>= fun rows ->
    return @@ Pg_helper.endorsement_from_db_list rows

  let origination_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "WITH ops AS \
       (WITH op AS (SELECT * FROM origination) \
       SELECT op.hash, COALESCE(bo.block_hash,'prevalidation') AS block_hash, \
       COALESCE(bl.network,'prevalidation') AS network_hash, bl.level, \
       op.source, op.tz1, op.fee, op.counter, op.manager, op.delegate, \
       op.script_code, op.script_storage_type, op.spendable, \
       op.delegatable, op.balance, \
       op.gas_limit, op.storage_limit, op.failed, op.internal, \
       op.burn_tez \
       FROM op \
       INNER JOIN block_operation AS bo ON op.hash = bo.operation_hash \
       INNER JOIN block AS bl ON (bl.hash = bo.block_hash) \
       WHERE bl.distance_level = 0 OR bl.distance_level IS NULL) \
       SELECT ops.hash, ops.block_hash, ops.network_hash, ops.source, \
       ops.tz1, ops.fee, ops.counter, ops.manager, ops.delegate, \
       ops.script_code, ops.script_storage_type, ops.spendable, \
       ops.delegatable, ops.balance, \
       ops.gas_limit, ops.storage_limit, ops.failed, ops.internal, \
       ops.burn_tez \
       FROM ops \
       ORDER BY ops.level DESC, ops.hash \
       NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ List.rev @@ Pg_helper.origination_from_db_list rows

  let operations_from_recent filters page page_size =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    let (seed, activation, transaction, origination,
         delegation, endorsement, _, _, _, _, reveal) =
      Pg_helper.filters_flag filters in
    PGSQL(dbh)
      "nullable-results"
      "WITH op_join AS (SELECT bo.operation_hash, bo.block_hash, bl.network, \
       o.op_type, bl.level \
       FROM block_operation AS bo \
       INNER JOIN block AS bl ON (bl.hash = bo.block_hash AND bl.distance_level = 0) \
       INNER JOIN operation AS o ON (o.hash = bo.operation_hash) \
       WHERE \
       (($seed AND array_position(o.op_anon_type, 'Nonce', 1) > 0) OR \
       ($activation AND array_position(o.op_anon_type, 'Activation', 1) > 0) OR \
       ($transaction AND array_position(o.op_manager_type, 'Transaction', 1) > 0) OR \
       ($origination AND array_position(o.op_manager_type, 'Origination', 1) > 0) OR \
       ($delegation AND array_position(o.op_manager_type, 'Delegation', 1) > 0) OR \
       ($endorsement AND o.op_type = 'Endorsement') OR \
       ($reveal AND array_position(o.op_manager_type, 'Reveal', 1) > 0)) \
       ORDER BY bl.level DESC, bo.operation_hash LIMIT $page_size64 OFFSET $offset) \
       SELECT opj.operation_hash, opj.block_hash, opj.network, opj.op_type, opj.level, \
       s.*, \
       f.*, \
       t.*, \
       ori.*, \
       d.*, \
       e.*, \
       r.* \
       FROM op_join AS opj \
       LEFT JOIN seed_nonce_revelation AS s ON ($seed AND opj.operation_hash = s.hash) \
       LEFT JOIN activation AS f ON ($activation AND opj.operation_hash = f.hash) \
       LEFT JOIN transaction AS t ON ($transaction AND opj.operation_hash = t.hash) \
       LEFT JOIN origination AS ori ON ($origination AND opj.operation_hash = ori.hash) \
       LEFT JOIN delegation AS d ON ($delegation AND opj.operation_hash = d.hash) \
       LEFT JOIN endorsement AS e ON ($endorsement AND opj.operation_hash = e.hash) \
       LEFT JOIN reveal AS r ON ($reveal AND opj.operation_hash = r.hash) \
       ORDER BY opj.level DESC" >>= fun rows ->
    return @@ List.rev @@ Pg_helper.operation_from_db_list rows

  let double_baking_evidence_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int (page * page_size) in
    PGSQL(dbh)
      "SELECT dbe.hash, bl.hash, bl.network \
       FROM double_baking_evidence AS dbe \
       INNER JOIN block_operation AS bo ON bo.operation_hash = dbe.hash \
       INNER JOIN block AS bl ON bl.hash = bo.block_hash \
       WHERE bl.distance_level = 0 \
       ORDER BY bl.level DESC LIMIT $limit OFFSET $offset" >>= fun bl_rows ->
    PGSQL(dbh)
      "SELECT bl2.signature, dbe.*, h1.*, h2.* \
       FROM double_baking_evidence AS dbe \
       INNER JOIN header AS h1 ON h1.id = dbe.header1 \
       INNER JOIN header AS h2 ON h2.id = dbe.header2 \
       INNER JOIN block_operation AS bo ON bo.operation_hash = dbe.hash \
       INNER JOIN block AS bl ON bl.hash = bo.block_hash \
       INNER JOIN block AS bl2 ON h1.level = bl2.level \
       WHERE bl.distance_level = 0 AND bl2.distance_level = 0\
       ORDER BY bl.level DESC LIMIT $limit OFFSET $offset" >>= fun rows ->
    return @@ List.map2 (fun (op_hash, op_block_hash, op_network_hash) row ->
        {op_hash; op_block_hash; op_network_hash;
         op_type = Anonymous [Pg_helper.dbe_from_db row]}) bl_rows rows

  let double_endorsement_evidence_from_recent page page_size =
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int (page * page_size) in
    PGSQL(dbh)
      "SELECT dee.hash, bl.hash, bl.network, bl.level \
       FROM double_endorsement_evidence AS dee \
       INNER JOIN block_operation AS bo ON bo.operation_hash = dee.hash \
       INNER JOIN block AS bl ON bl.hash = bo.block_hash \
       WHERE bl.distance_level = 0 \
       ORDER BY bl.level DESC LIMIT $limit OFFSET $offset" >>= fun bl_rows ->
    PGSQL(dbh)
      "SELECT dee.* \
       FROM double_endorsement_evidence AS dee \
       INNER JOIN block_operation AS bo ON bo.operation_hash = dee.hash \
       INNER JOIN block AS bl ON bl.hash = bo.block_hash \
       WHERE bl.distance_level = 0 \
       ORDER BY bl.level DESC LIMIT $limit OFFSET $offset" >>= fun rows ->
    return @@ List.map2 (fun (op_hash, op_block_hash, op_network_hash, op_level) row ->
        {op_hash; op_block_hash; op_network_hash;
         op_type = Anonymous [Pg_helper.dee_from_db (row, op_level)]}) bl_rows rows

  let operations ?(delegate=false) ?(filters=[]) ?(page=0) ?(page_size=20) selector =
    match selector with
    | Block hash ->
      operations_from_block filters page page_size hash

    | Account hash ->
      begin match filters with
        | [ "Transaction" ] -> transaction_from_account page page_size hash
        | [ "Delegation" ] -> delegation_from_account page page_size hash
        | [ "Endorsement" ] -> endorsement_from_account page page_size hash
        | [ "Origination" ] -> origination_from_account ~delegate page page_size hash
        | [ "Reveal" ] -> reveal_from_account page page_size hash
        | [ "Activation" ] -> activation_from_account page page_size hash
        | _ -> operations_from_account filters page page_size hash
      end

    | Pending -> operations_from_pending filters page page_size

    | Empty ->
      begin match filters with
        | [ "Transaction" ] -> transaction_from_recent page page_size
        | [ "Delegation" ] -> delegation_from_recent page page_size
        | [ "Endorsement" ] -> endorsement_from_recent page page_size
        | [ "Origination" ] -> origination_from_recent page page_size
        | [ "Reveal" ] -> reveal_from_recent page page_size
        | [ "Double_baking_evidence" ] ->
          double_baking_evidence_from_recent page page_size
        | [ "Double_endorsement_evidence" ] ->
          double_endorsement_evidence_from_recent page page_size
        | _ -> operations_from_recent filters page page_size
      end

  (* Temporary *)
  let head_level () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT b.level \
       FROM block AS b \
       INNER JOIN protocol AS p ON b.protocol = p.hash \
       INNER JOIN protocol AS pt ON b.test_protocol = pt.hash \
       WHERE distance_level = 0 ORDER BY level DESC LIMIT 1"
    >>= function
    | [ number ] -> return number
    | _ -> return Int64.zero

  let current_cycle () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT MAX(cycle) FROM block WHERE distance_level = 0"
    >>= function
    | [ Some c ] -> return c
    | _ -> return 0L

  (* Baking Section *)

  let nb_bakings ?(rights=true) ?cycle hash =
    with_dbh >>> fun dbh ->
    let cycle, nocycle = test_opt Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT array_sum(nb_baking, 1) + (CASE WHEN $rights THEN nb_miss_baking ELSE 0 END) \
       FROM cycle_count_baker \
       WHERE tz = $hash AND ($nocycle or cycle = $?cycle)"
    >>= of_count_opt

  let bakings ?(page=0) ?(page_size=20) ?cycle hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int (page * page_size) in
    let cycle, nocycle = test_opt Int64.of_int cycle in
    let time_b_blocks = Tezos_constants.Constants.time_between_blocks in
    let time_b_prio = Tezos_constants.Constants.time_delay_for_priority 1 - time_b_blocks in
    let interval_b_blocks = CalendarLib.Calendar.Period.second time_b_blocks in
    let interval_b_prio = CalendarLib.Calendar.Period.second time_b_prio in
    PGSQL(dbh)
      "SELECT b.hash, b.baker, b.level, b.cycle, b.priority, \
       b.distance_level, b.fees, \
       bake_time(b.timestamp - pred.timestamp, b.priority, $interval_b_blocks, \
       $interval_b_prio), \
       b.baker = $hash \
       FROM block AS b \
       LEFT JOIN block AS pred ON b.predecessor = pred.hash \
       INNER JOIN level_rights AS lr ON b.level = lr.level \
       WHERE (b.baker = $hash OR \
       lr.bakers_priority[array_position(lr.bakers, $hash)] <= b.priority) \
       AND ($nocycle OR b.cycle = $?cycle) AND lr.level <= $head_lvl \
       ORDER BY b.level DESC NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ Pg_helper.bakings_from_db_list rows

  let nb_cycle_bakings hash =
    current_cycle () >>= fun head_cycle ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT COUNT(*) FROM cycle_count_baker \
                WHERE tz = $hash AND cycle <= $head_cycle AND \
                array_sum(nb_baking, 1) + nb_miss_baking <> 0"
    >>= of_count_opt

  let cycle_bakings offset limit hash =
    current_cycle () >>= fun head_cycle ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT cycle, array_sum(nb_baking,1), array_sum(nb_baking,2), \
       nb_miss_baking, array_wavg(nb_baking,1), fees, time FROM cycle_count_baker \
       WHERE tz = $hash AND cycle <= $head_cycle \
       ORDER BY cycle DESC LIMIT $limit OFFSET $offset"
       >>= fun rows ->
       current_cycle () >>= fun current_cycle ->
       return (Pg_helper.cycle_bakings_from_db_list (Int64.to_int current_cycle) rows)

  let total_bakings hash =
    let block_reward = Tezos_constants.Constants.block_reward in
    let block_deposit = Tezos_constants.Constants.block_security_deposit in
    let ramp_up_cycle = Int64.of_int Tezos_constants.ramp_up_cycles in
    current_cycle () >>= fun head_cycle ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT( * ), SUM(array_sum(nb_baking,1))::bigint, \
       SUM(array_sum(nb_baking,2))::bigint, SUM(nb_miss_baking)::bigint, \
       SUM(array_wavg(nb_baking,1)), SUM(fees)::bigint, SUM(time), \
       SUM(bk_rewards(array_sum(nb_baking,1) * $block_reward, cycle, true))::bigint, \
       SUM(deposits(array_sum(nb_baking,1) * $block_deposit, cycle, $ramp_up_cycle, \
       1, true))::bigint \
       FROM cycle_count_baker \
       WHERE tz = $hash AND cycle <= $head_cycle"
    >>= function
    | [ (Some a, b, c, Some d, Some e, Some f, Some g, rewards, deposits) ] ->
      let count_float = (Int64.to_float a) in
      (let row = Pg_helper.cycle_bakings_from_db_list 0
           [0L, b, c, d, Some (e /. count_float), f, g ] in
      match rewards, deposits, row with
      | Some rewards, Some deposits, [total_bk] ->
        return [{total_bk with cbk_tez = {tez_fee = total_bk.cbk_tez.tez_fee;
                                         tez_reward = rewards;
                                         tez_deposit = deposits}}]
      | _, _, _ -> return [])
    | _ -> return []

  let nb_baker_rights ?cycle hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let cycle, nocycle = test_opt Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT COUNT (level) FROM level_rights \
       WHERE $hash = ANY (bakers) AND level > $head_lvl AND \
       ($nocycle OR cycle = $?cycle)"
    >>= of_count_opt

  let baker_rights ?cycle ?(page=0) ?(page_size=20) hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    let cycle, nocycle = test_opt Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT level, cycle, bakers_priority[ARRAY_POSITION(bakers, $hash)], \
       level - $head_lvl \
       FROM level_rights \
       WHERE $hash = ANY (bakers) AND level > $head_lvl AND \
       ($nocycle OR cycle = $?cycle) \
       ORDER BY level ASC NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ Pg_helper.baker_rights_from_db_list rows

  let nb_cycle_baker_rights hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(DISTINCT cycle) FROM level_rights \
       WHERE $hash = ANY (bakers)  and level > $head_lvl" >>= of_count_opt

  let cycle_baker_rights hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
       "WITH t(cycle, prio) AS ( \
        SELECT cycle, bakers_priority[ARRAY_POSITION(bakers, $hash)] \
        FROM level_rights \
        WHERE $hash = ANY (bakers)  and level > $head_lvl) \
        SELECT cycle, SUM(CASE WHEN prio = 0 THEN 1 ELSE 0 END), \
        cast (AVG(prio) as FLOAT) FROM t
        GROUP BY cycle ORDER BY cycle DESC"
    >>= fun rows ->
    return @@ List.rev @@ List.fold_left
      (fun acc cr -> match cr with
         | (cr_cycle, Some cr_nblocks, Some cr_priority) ->
           {cr_cycle = Int64.to_int cr_cycle; cr_nblocks = Int64.to_int cr_nblocks;
            cr_priority} :: acc
         | _ -> acc) [] rows

  let nb_bakings_history hash =
    nb_cycle_baker_rights hash >>= fun nb_rights ->
    nb_cycle_bakings hash >>= fun nb_cycle_bakings ->
    if nb_rights + nb_cycle_bakings = 0 then return 0
    else return (1 + nb_rights + nb_cycle_bakings)

  let bakings_history ?(page=0) ?(page_size=20) hash =
    total_bakings hash >>= fun tot_row ->
    cycle_baker_rights hash >>= fun rights_rows ->
    let offset = Int64.of_int @@
      if page = 0 then 0 else page * page_size - 1 - List.length rights_rows in
    let limit = Int64.of_int @@
      if page = 0 then page_size - 1 - List.length rights_rows else page_size in
    if offset < 0L then return (tot_row, rights_rows, []) else
      cycle_bakings offset limit hash >>= fun rows ->
      return (tot_row, (if page = 0 then rights_rows else []), rows)

  (* Baking Endorsements Section *)

  let nb_bakings_endorsement ?cycle hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let cycle, nocycle = test_opt Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT count(*) FROM level_rights \
       WHERE $hash = any(endorsers) AND ($nocycle or cycle = $?cycle) \
       AND level <= $head_lvl"
    >>= of_count_opt

  let bakings_endorsement ?(page=0) ?(page_size=20) ?cycle hash =
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int (page * page_size) in
    let cycle, nocycle = test_opt Int64.of_int cycle in
    let block_per_cycle =
      Int64.of_int Tezos_constants.Constants.block_per_cycle in
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "nullable-results"
      "WITH lr(level, nslot) AS ( \
       SELECT level, slots[array_position(endorsers, $hash)] \
       FROM level_rights \
       WHERE $hash = any(endorsers) AND \
       ($nocycle OR level >= ($?cycle * $block_per_cycle::bigint)::bigint \
       AND level < ($?cycle + 1::bigint) * $block_per_cycle) \
       AND level < $head_lvl), \
       p(level, hash, endorser, cycle, priority, distance_level, slots) AS ( \
       SELECT block_level, op_block_hash, source, op_cycle, priority, distance_level, slots \
       FROM endorsement_all \
       WHERE source = $hash AND ( $nocycle OR op_cycle = $?cycle ) \
       AND distance_level = 0) \
       SELECT lr.level, lr.nslot, p.hash, p.endorser, p.cycle, p.priority, \
       p.distance_level, p.slots \
       FROM lr AS lr LEFT JOIN p AS p ON lr.level = p.level \
       ORDER BY lr.level DESC LIMIT $limit OFFSET $offset"
    >>= fun rows ->
    return @@ Pg_helper.bakings_endorsement_from_db_list rows

  let nb_cycle_endorsements hash =
    current_cycle () >>= fun head_cycle ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM cycle_count_baker \
       WHERE tz = $hash AND cycle <= $head_cycle AND \
       array_sum(nb_endorsement, 1) + nb_miss_endorsement <> 0"
    >>= of_count_opt

  let cycle_endorsements offset limit hash =
    current_cycle () >>= fun head_cycle ->
    with_dbh >>> fun dbh ->
    let reward = Tezos_constants.Constants.endorsement_reward_coeff in
    PGSQL(dbh)
      "SELECT cycle, array_sum(nb_endorsement,1), nb_miss_endorsement, \
       array_wavg(nb_endorsement, 1), end_rewards_array($reward, cycle, nb_endorsement) \
       FROM cycle_count_baker WHERE tz = $hash AND cycle <= $head_cycle \
       ORDER BY cycle DESC LIMIT $limit OFFSET $offset"
    >>= fun rows ->
    current_cycle () >>= fun current_cycle ->
    return (Pg_helper.cycle_endorsements_from_db_list (Int64.to_int current_cycle) rows)

  let total_endorsements hash =
    let reward = Tezos_constants.Constants.endorsement_reward_coeff in
    let deposit = Tezos_constants.Constants.endorsement_security_deposit in
    let ramp_up_cycle = Int64.of_int Tezos_constants.ramp_up_cycles in
    current_cycle () >>= fun head_cycle ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT( * ), SUM(array_sum(nb_endorsement,1))::bigint, \
       SUM(nb_miss_endorsement)::bigint, SUM(array_wavg(nb_endorsement,1)), \
       SUM(end_rewards_array($reward, cycle, nb_endorsement))::bigint, \
       SUM(deposits(array_sum(nb_endorsement,1) * $deposit, cycle, $ramp_up_cycle, \
       1, true))::bigint \
       FROM cycle_count_baker \
       WHERE tz = $hash AND cycle <= $head_cycle"
    >>= function
    | [ (Some a, b, Some c, Some d, e, Some deposits) ] ->
      let count_float = Int64.to_float a in
      (let row = Pg_helper.cycle_endorsements_from_db_list 0
           [(0L, b, c, Some (d /. count_float), e)] in
       match row with
       | [ total_ed ] ->
         return
           [ {total_ed  with ced_tez =
                               {tez_fee=0L;
                                tez_reward = total_ed.ced_tez.tez_reward;
                                tez_deposit = deposits} } ]
       | _ -> return [])
    | _ -> return []

  let nb_endorser_rights ?cycle hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let cycle, nocycle = test_opt Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT COUNT (level) FROM level_rights \
       WHERE $hash = ANY (endorsers) AND level > $head_lvl AND \
       ($nocycle OR cycle = $?cycle)"
    >>= of_count_opt

  let endorser_rights ?cycle ?(page=0) ?(page_size=20) hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    let cycle, nocycle = test_opt Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT level, cycle, slots[ARRAY_POSITION(endorsers, $hash)], \
       level - $head_lvl FROM level_rights \
       WHERE $hash = ANY (endorsers)  AND level > $head_lvl AND \
       ($nocycle OR cycle = $?cycle) \
       ORDER BY level ASC NULLS FIRST LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@ Pg_helper.endorser_rights_from_db_list rows

  let nb_cycle_endorser_rights hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(DISTINCT cycle) FROM level_rights \
       WHERE $hash = ANY (endorsers)  and level > $head_lvl" >>= of_count_opt

  let cycle_endorser_rights hash =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT cycle, COUNT(level), \
       SUM( slots[ARRAY_POSITION(endorsers, $hash)]) \
       FROM level_rights \
       WHERE $hash = ANY (endorsers)  and level > $head_lvl \
       GROUP BY cycle ORDER BY cycle DESC"
    >>= fun rows ->
    return @@ List.rev @@ List.fold_left
      (fun acc cr -> match cr with
         | (cr_cycle, Some cr_nblocks, Some cr_priority) ->
           {cr_cycle = Int64.to_int cr_cycle; cr_nblocks = Int64.to_int cr_nblocks;
            cr_priority = Int64.to_float cr_priority} :: acc
         | _ -> acc) [] rows

  let nb_endorsements_history hash =
    nb_cycle_endorser_rights hash >>= fun nb_rights ->
    nb_cycle_endorsements hash >>= fun nb_cycle_endorsements ->
    if nb_rights + nb_cycle_endorsements = 0 then return 0
    else return (1 + nb_rights + nb_cycle_endorsements)

  let endorsements_history ?(page=0) ?(page_size=20) hash =
    total_endorsements hash >>= fun tot_row ->
    cycle_endorser_rights hash >>= fun rights_rows ->
    let offset = Int64.of_int @@
      if page = 0 then 0 else page * page_size - 1 - List.length rights_rows in
    let limit = Int64.of_int @@
      if page = 0 then page_size - 1 - List.length rights_rows else page_size in
    if offset < 0L then return (tot_row, rights_rows, []) else
      cycle_endorsements offset limit hash >>= fun rows ->
      return (tot_row, (if page = 0 then rights_rows else []), rows)

  let nb_cycle_rights ?(future=true) ?filter () =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let filter, nofilter = test_opt (fun x -> x) filter in
    let level, notlevel =
      test_opt (fun x -> x)
        (match filter with None -> None | Some filter -> Int64.of_string_opt filter) in
    PGSQL(dbh)
      "SELECT COUNT(level) FROM level_rights \
       WHERE ($future AND level > $head_lvl \
       OR NOT $future AND level <= $head_lvl) \
       AND ($nofilter OR ( NOT $notlevel AND level = $?level \
       OR $notlevel AND bakers_priority[ARRAY_POSITION(bakers, $?filter)] < 4))"
    >>= of_count_opt

  let cycle_rights ?(future=true) ?filter ?(page=0) ?(page_size=20) () =
    head_level () >>= fun head_lvl ->
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    let filter, nofilter = test_opt (fun x -> x) filter in
    let level, notlevel =
      test_opt (fun x -> x)
        (match filter with None -> None | Some filter -> Int64.of_string_opt filter) in
    PGSQL(dbh)
      "nullable-results"
      "SELECT lr.level, bakers, endorsers, bakers_priority, bl.priority, bl.baker \
       FROM level_rights AS lr \
       LEFT JOIN block AS bl ON (bl.level = lr.level AND bl.distance_level = 0) \
       WHERE ($future AND lr.level > $head_lvl \
       OR NOT $future AND lr.level <= $head_lvl) \
       AND ($nofilter OR ( NOT $notlevel AND lr.level = $?level \
       OR $notlevel AND bakers_priority[ARRAY_POSITION(bakers, $?filter)] < 4)) \
       ORDER BY \
       CASE WHEN $future THEN lr.level END ASC, \
       CASE WHEN NOT $future THEN lr.level END DESC \
       LIMIT $limit OFFSET $offset"
    >>= fun rows ->
    return @@ Pg_helper.rights_from_db_list rows

  let required_balance hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT MAX(cycle), MAX(level) FROM block WHERE distance_level = 0"
    >>= fun current ->
    let current_cycle, head_lvl =  match current with
      | [ Some cycle, Some level ] -> cycle, level
      | _ -> 0L, 0L in
    PGSQL(dbh)
      "WITH rights AS \
       (SELECT lr.cycle, \
       SUM(CASE WHEN \
       bakers_priority[ARRAY_POSITION(bakers, $hash)] = 0 \
       THEN 1 ELSE 0 END) AS bcount, \
       SUM(slots[ARRAY_POSITION(endorsers, $hash)]) AS ecount \
       FROM level_rights AS lr \
       WHERE (lr.level > $head_lvl) AND \
       ($hash = ANY (bakers) OR $hash = ANY (endorsers)) \
       GROUP BY lr.cycle ORDER BY lr.cycle ASC) \
       SELECT r.cycle, r.bcount, r.ecount, \
       COALESCE((SELECT count FROM snapshot_owner WHERE tz1 = $hash \
       AND id = (SELECT id FROM snapshot_rolls WHERE cycle = r.cycle))), \
       COALESCE((SELECT rolls_count FROM snapshot_rolls WHERE cycle = r.cycle)) \
       FROM rights AS r"
    >>= fun rights_rows ->
    let allowed_fork = Tezos_constants.Constants.allowed_fork in
    cycle_bakings 0L (Int64.of_int (allowed_fork + 3)) hash >>= fun cbks ->
    let cbks = List.map
        (fun cbk ->
           cbk.cbk_cycle, List.fold_left Int64.add 0L
             [cbk.cbk_tez.tez_deposit; cbk.cbk_tez.tez_reward; cbk.cbk_tez.tez_fee]) cbks in
    cycle_endorsements 0L (Int64.of_int (allowed_fork + 3)) hash >>= fun ceds ->
    let ceds = List.map
        (fun ced ->
           ced.ced_cycle, List.fold_left Int64.add 0L
             [ced.ced_tez.tez_deposit; ced.ced_tez.tez_reward; ced.ced_tez.tez_fee]) ceds in
    let _, l = List.fold_left
        (fun (s, acc) (cycle, nbak, nend, roll, total) ->
           let cyclei = Int64.to_int cycle in
           match (nbak, nend) with
           | Some nbak, Some nend ->
             let b_security_deposit =
               Tezos_constants.cycle_deposits
                 Tezos_constants.Constants.block_security_deposit cyclei in
             let e_security_deposit =
               Tezos_constants.cycle_deposits
                 Tezos_constants.Constants.endorsement_security_deposit cyclei in
             let tez = Int64.(
                 add (mul nbak b_security_deposit)
                   (mul nend e_security_deposit))
             in
             let roll = match roll with None -> -1 | Some r -> Int32.to_int r in
             let back =
               if cycle = current_cycle then 0L
               else (
                 let back_bk = List.fold_left (fun acc cbk ->
                     if (fst cbk) = cyclei - allowed_fork - 1 then
                       Int64.add acc (snd cbk)
                     else acc ) 0L cbks in
                 let back_end = List.fold_left (fun acc ced ->
                     if (fst ced) = cyclei - allowed_fork - 1 then
                       Int64.add acc (snd ced)
                     else acc ) 0L ceds in
                 Int64.add back_bk back_end) in
             let total =
               match total with None -> 0 | Some t -> Int32.to_int t in
             let s = Int64.(add (sub s back) tez) in
             (s, (cyclei, tez, back, s, roll, total) :: acc)
           | _ -> (s, acc))
        (0L, []) rights_rows in
    return @@ List.rev l

  let endorsements selector =
    with_dbh >>> fun dbh ->
    begin
      match selector with
      | Hash block_hash ->
        PGSQL(dbh)
          "SELECT hash, op_block_hash, network, source, slots, block_hash, \
           block_level, op_level, priority, timestamp FROM endorsement_all \
           WHERE block_hash = $block_hash AND distance_level = 0 \
           ORDER BY priority, slots"
      | Level level ->
        let level64 = Int64.of_int level in
        PGSQL(dbh)
          "SELECT hash, op_block_hash, network, source, slots, block_hash, \
           block_level, op_level, priority, timestamp FROM endorsement_all \
           WHERE block_level = $level64 AND distance_level = 0 AND \
           op_block_hash <> 'Orphan' \
           ORDER BY priority, slots"
    end
    >>= fun rows ->
    return @@ List.map
      (fun (op_hash, op_block_hash, op_network_hash, endorse_src, endorse_slot,
            endorse_block_hash, endorse_block_level, endorse_op_level,
            endorse_priority, endorse_timestamp) ->
        let endorse_slot = Misc.unopt_list Int32.to_int endorse_slot in
        let endorse_block_level = Int64.to_int endorse_block_level in
        let endorse_op_level = Int64.to_int endorse_op_level in
        let endorse_priority = Int32.to_int endorse_priority in
        let endorse_src = Alias.to_name endorse_src in
        let endorse_timestamp = Pg_helper.string_of_cal endorse_timestamp in
        let endorse =
          { endorse_src; endorse_block_hash; endorse_slot;
            endorse_block_level; endorse_op_level; endorse_priority;
            endorse_timestamp } in
        { op_hash; op_block_hash; op_network_hash;
          op_type = Sourced (Consensus (Endorsement endorse)) }) rows

  let protocol protocol_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT * from protocol WHERE hash = $protocol_hash" >>= function
    | [] -> return None
    | [ (proto_hash, proto_name) ] -> return @@ Some { proto_hash; proto_name }
    | rows ->
      begin
        debug
          "[Reader] [protocol] Primary key constraint failed ? %S has %d entries\n%!"
          protocol_hash
          (List.length rows);
        try assert false with e -> fail e
      end

  let account_status hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT rev.hash \
       FROM reveal AS rev \
       INNER JOIN operation AS op ON rev.hash = op.hash \
       WHERE rev.pubkey = $hash \
       ORDER BY op.timestamp ASC LIMIT 1" >>= fun reveal ->
    PGSQL(dbh)
      "SELECT ori.hash \
       FROM origination AS ori \
       WHERE ori.tz1 = $hash LIMIT 1" >>= fun origin ->
    (match reveal, origin with
       rev :: _, ori :: _ ->
       { account_status_hash = Alias.to_name hash;
         account_status_revelation = Some rev;
         account_status_origination = Some ori }
     | rev :: _, [] ->
       { account_status_hash = Alias.to_name hash;
         account_status_revelation = Some rev;
         account_status_origination = None }
     | [], ori :: _ ->
       { account_status_hash = Alias.to_name hash;
         account_status_revelation = None;
         account_status_origination = Some ori }
     | [], [] ->
       { account_status_hash = Alias.to_name hash;
         account_status_revelation = None;
         account_status_origination = None }) |> return

  let account selector =
    match selector with
    | Account hash ->
      with_dbh >>> fun dbh ->
      PGSQL(dbh)
        "SELECT ori.tz1, ori.manager, ori.script_code, \
         ori.spendable, ori.delegatable \
         FROM origination AS ori WHERE ori.tz1 = $hash" >>=
      begin function
        | [ (account_hash, account_manager, _,
             account_spendable, account_delegatable) ] ->
          return @@ Some
            { account_hash = Alias.to_name account_hash;
              account_manager = Alias.to_name account_manager;
              account_spendable ;
              account_delegatable ;
            }
          | _ ->
            return @@ Some
              { account_hash = Alias.to_name hash;
                account_manager = Alias.to_name hash;
                account_spendable = false ;
                account_delegatable = false ;
              }
      end
    | _ -> return None


  let accounts ?(page=0) ?(page_size=20) ?(contract=false) () =
    with_dbh >>> fun dbh ->
    let page_size64 = Int64.of_int page_size in
    let offset = Int64.of_int(page * page_size) in
    PGSQL(dbh)
      "SELECT t.hash, t.alias \
       FROM tezos_user AS t \
       WHERE t.contract = $contract \
       AND hash <> 'God' AND hash <> '' \
       ORDER BY id DESC LIMIT $page_size64 OFFSET $offset"
    >>= fun rows ->
    return @@
    List.map (fun (tz, alias) ->
        { account_hash = {tz; alias};
          account_manager = Alias.to_name "";
          account_spendable = false ;
          account_delegatable = false ;
        }) rows

  let nb_accounts ?(contract=false) () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "nullable-results"
      "SELECT COUNT(t.hash) \
       FROM tezos_user AS t \
       WHERE t.contract = $contract \
       AND t.hash <> 'God' AND t.hash <> ''"
    >>= of_count_opt

  let marketcap () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT * FROM marketcap ORDER By last_updated DESC LIMIT 1" >>= function
    | [] -> begin try assert false with e -> fail e end
    | ((mc_id, name, symbol, rank, price_usd, price_btc,
        volume_usd_24, market_cap_usd, available_supply,
        total_supply, max_supply, percent_change_1,
        percent_change_24, percent_change_7, last_updated, _valid)) :: _
      ->
      let last_updated = Int64.to_string last_updated in
      return @@
      { mc_id ;
        name ;
        symbol ;
        rank ;
        price_usd ;
        price_btc ;
        volume_usd_24 ;
        market_cap_usd ;
        available_supply ;
        total_supply ;
        max_supply ;
        percent_change_1 ;
        percent_change_24 ;
        percent_change_7 ;
        last_updated ;
      }

  let to_peer point_id = point_id

  let nb_network_peers ?state () =
    with_dbh >>> fun dbh ->
    let state, nostate = test_opt (fun s -> s) state in
    PGSQL(dbh) "SELECT COUNT(*) FROM peers WHERE ($nostate OR state = $?state)"
    >>= of_count_opt

  let network_stats ?state ?(page=0) ?(page_size=20) () =
    with_dbh >>> fun dbh ->
    let limit = Int64.of_int page_size in
    let offset = Int64.of_int (page * page_size) in
    let state, nostate = test_opt (fun s -> s) state in
    PGSQL(dbh)
      "SELECT * FROM peers \
       WHERE ($nostate OR state = $?state) \
       ORDER BY state DESC, total_sent + total_received DESC \
       OFFSET $offset LIMIT $limit"
    >>= function
    | [] -> return []
    | results ->
      return @@
      List.map (fun (_, peer_id, country_name, country_code, point_id, trusted, score, state,
                     total_sent, total_recv, current_inflow, current_outflow,
                     last_failed_connection_point, last_failed_connection_date,
                     last_rejected_connection_point, last_rejected_connection_date,
                     last_established_connection_point, last_established_connection_date,
                     last_disconnection_point, last_disconnection_date,
                     last_seen_point, last_seen_date,
                     last_miss_point, last_miss_date) ->
                 let state =
                   match state with
                   | "accepted" -> Accepted
                   | "running"  -> Running
                   | "disconnected" -> Disconnected
                   | _ -> assert false in
                 let to_peer = function None -> "" | Some point_id -> to_peer point_id in
                 let id_point = Some (to_peer point_id) in
                 let to_date = function None -> "" | Some date -> date in
                 let last_failed_connection =
                   Some (to_peer last_failed_connection_point, to_date last_failed_connection_date) in
                 let last_rejected_connection =
                   Some (to_peer last_rejected_connection_point, to_date last_rejected_connection_date) in
                 let last_established_connection =
                   Some (to_peer last_established_connection_point, to_date last_established_connection_date) in
                 let last_disconnection =
                   Some (to_peer last_disconnection_point, to_date last_disconnection_date) in
                 let last_seen = Some (to_peer last_seen_point, to_date last_seen_date) in
                 let last_miss = Some (to_peer last_miss_point, to_date last_miss_date) in
                 let country = country_name, country_code in
                 { peer_id; country; score ; trusted ; conn_metadata = None ;
                   state ; id_point ;
                   stat = { total_sent; total_recv ;
                            current_inflow = Int64.to_int current_inflow ;
                            current_outflow = Int64.to_int current_outflow } ;
                   last_failed_connection ; last_rejected_connection ;
                   last_established_connection ; last_disconnection ;
                   last_seen; last_miss } )
        results

  let country_stats ?state () =
    with_dbh >>> fun dbh ->
    begin match state with
      | Some "running" ->
        PGSQL(dbh)
          "SELECT country_name, country_code, COUNT(*) FROM peers \
           WHERE state = 'running' GROUP BY (country_name, country_code)"
      | _ ->
        PGSQL(dbh)
          "SELECT country_name, country_code, COUNT(*) FROM peers \
           GROUP BY (country_name, country_code)"
    end  >>= fun rows ->
    return @@
    List.map (fun (country_name, country_code, total) ->
        let total = match total with None -> 0 | Some t -> Int64.to_int t in
        {country_name; country_code; total})
      rows

  let baker_stats hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT b.baker, COUNT( b.hash ), SUM(b.volume)::bigint AS volume, \
       SUM(b.fees)::bigint AS fees \
       FROM block AS b \
       WHERE b.distance_level = 0 AND b.baker = $hash \
       GROUP BY baker ORDER BY COUNT(*) DESC" >>= fun rows ->
    return @@ match rows with
    |  [ baker_hash, Some nb, vol_opt, fees_opt ] ->
      let nb_blocks = Int64.to_int nb in
      let volume_total =
        match vol_opt with
        | Some i -> i
        | None -> 0L in
      let fees_total =
        match fees_opt with
        | Some i -> i
        | None -> 0L in
      { baker_hash = Alias.to_name baker_hash; nb_blocks; volume_total; fees_total }
    | _ ->
      { baker_hash = Alias.to_name hash; nb_blocks = 0; volume_total = 0L; fees_total = 0L }

  let bakers_stats ?cycle () =
    with_dbh >>> fun dbh ->
    (match cycle with
     | None ->
       PGSQL(dbh)
         "SELECT b.baker, COUNT( b.hash ), SUM(b.volume)::bigint AS volume, \
          SUM(b.fees)::bigint AS fees \
          FROM block AS b \
          WHERE b.distance_level = 0 AND b.baker <> 'God' \
          GROUP BY baker ORDER BY COUNT(*) DESC"
     | Some cycle ->
       let cycle = Int64.of_int cycle in
       PGSQL(dbh)
         "SELECT b.baker, COUNT( b.hash ), SUM(b.volume)::bigint AS volume, \
          SUM(b.fees)::bigint AS fees \
          FROM block AS b \
          WHERE b.distance_level = 0 AND b.baker <> 'God' AND b.cycle = $cycle \
          GROUP BY baker ORDER BY COUNT(*) DESC")
    >>= fun rows ->
    return @@
    List.map (function
        | (baker_hash, Some nb, vol_opt, fees_opt) ->
          let nb_blocks = Int64.to_int nb in
          let volume_total =
            match vol_opt with
            | Some i -> i
            | None -> 0L in
          let fees_total =
            match fees_opt with
            | Some i -> i
            | None -> 0L in
          { baker_hash = Alias.to_name baker_hash; nb_blocks; volume_total; fees_total }
        | _ -> assert false)
      rows

  let calculate_revelation_rate level_per_cycle row =
    let revelation_max_number =
      float_of_int @@ Tezos_constants.nb_revelations_per_cycle () in
    if level_per_cycle = 0. then 0.
    else
      match row with
      | [ Some i ] ->
        ((Int64.to_float i) /. revelation_max_number) *. 100.
      | _ -> 0.

  let health_stats cycle =
    with_dbh >>> fun dbh ->
    let cycle = Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT priority FROM block \
       WHERE distance_level = 0 AND cycle = $cycle" >>= fun priorities ->
    PGSQL(dbh)
      "SELECT SUM(priority)/COUNT(*) AS mean, \
       SUM(volume)::bigint AS volume, \
       SUM(fees)::bigint AS fees \
       FROM block WHERE distance_level = 0 AND cycle = $cycle"
    >>= fun cycle_stats ->
    PGSQL(dbh)
      "SELECT switch_count, longest_alt_chain \
       FROM switch WHERE cycle = $cycle"
    >>= fun switch_stats ->
    PGSQL(dbh)
      "SELECT level, EXTRACT(YEAR FROM timestamp), EXTRACT(MONTH FROM timestamp), \
       EXTRACT(DAY FROM timestamp) FROM block \
       WHERE cycle = $cycle AND \
       cycle_position = 0 AND level <> 0"
    >>= fun cycle_start ->
    PGSQL(dbh)
      "SELECT level, EXTRACT(YEAR FROM timestamp), EXTRACT(MONTH FROM timestamp), \
       EXTRACT(DAY FROM timestamp) FROM block \
       WHERE cycle = $cycle \
       ORDER BY level DESC LIMIT 1"
    >>= fun cycle_end ->
    PGSQL(dbh)
      "SELECT COUNT( * ) FROM cycle_count_baker \
       WHERE cycle = $cycle AND nb_baking <> '{}'"
    >>= fun cycle_bakers ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM cycle_count_baker \
       WHERE cycle = $cycle AND nb_endorsement <> '{}'"
    >>= fun cycle_endorsers ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM block \
       WHERE distance_level <> 0 AND \
       cycle = $cycle" >>= fun number_alt_heads ->
    PGSQL(dbh)
      "SELECT COUNT(s.level) FROM seed_nonce_revelation AS s \
       INNER JOIN block_operation AS bo ON bo.operation_hash = s.hash \
       INNER JOIN block AS bl ON bl.hash = bo.block_hash \
       WHERE s.level IN (SELECT level FROM block WHERE cycle = $cycle) \
       AND bl.distance_level = 0" >>= fun cycle_revelations_number ->
    PGSQL(dbh)
      "SELECT SUM(CASE WHEN distance_level = 0 THEN array_length(slots, 1) ELSE 0 END), \
       SUM(CASE WHEN distance_level <> 0 THEN array_length(slots, 1) ELSE 0 END) \
       FROM endorsement_all WHERE op_cycle = $cycle"
    >>= fun cycle_endorsements ->
    PGSQL(dbh)
      "SELECT hash, level, volume \
       FROM block WHERE \
       volume = (SELECT max(volume) FROM block WHERE cycle = $cycle) \
       AND distance_level = 0 \
       AND cycle = $cycle LIMIT 1" >>= fun biggest_block_volume ->
    PGSQL(dbh)
      "SELECT hash, level, fees \
       FROM block WHERE \
       fees = (SELECT max(fees) FROM block WHERE cycle = $cycle) \
       AND distance_level = 0 \
       AND cycle = $cycle LIMIT 1" >>= fun biggest_block_fees ->
    PGSQL(dbh)
      "SELECT baker, COUNT(*) FROM block WHERE \
       cycle = $cycle GROUP BY baker \
       ORDER BY COUNT(*) DESC LIMIT 1" >>= fun top_baker ->

    let slot_number =
      float_of_int Tezos_constants.Constants.endorsers_per_block in

    let cycle_start_level, year_start, month_start, day_start  = match cycle_start with
      | (level, Some year, Some month, Some day) :: _
        -> Int64.to_int level, int_of_float year, int_of_float month, int_of_float day
      | _ -> -1, -1, -1, -1 in
    let cycle_end_level, year_end, month_end, day_end = match cycle_end with
       | (level, Some year, Some month, Some day) :: _
      -> Int64.to_int level, int_of_float year, int_of_float month, int_of_float day
      | _ -> -1, -1, -1, -1 in
    let cycle_volume = match cycle_stats with
      | [ _, Some f, _ ] -> f
      | _ -> 0L in
    let cycle_fees = match cycle_stats with
      | [ _, _, Some f ] -> f
      | _ -> 0L in
    let cycle_bakers = match cycle_bakers with
      | [ Some i ] -> Int64.to_int i
      | _ -> -1 in
    let cycle_endorsers = match cycle_endorsers with
      | [ Some i ] -> Int64.to_int i
      | _ -> -1 in
    let cycle_position =
      float_of_int @@ (cycle_end_level - cycle_start_level) + 1 in
    let total_endorsements, main_endorsements_rate,
        alt_endorsements_rate = match cycle_endorsements with
      | [main_endo, alt_endo] ->
        let main_endo = Misc.unoptf 0. Int64.to_float main_endo in
        let alt_endo = Misc.unoptf 0. Int64.to_float alt_endo in
        int_of_float (main_endo +. alt_endo),
        main_endo /. (alt_endo +. main_endo),
        (alt_endo /. (cycle_position *. slot_number)) *. 100.
      | _ -> 0, 0., 0. in
    let endorsements_rate =
      ((float total_endorsements) /. (cycle_position *. slot_number)) *. 100. in
    let empty_endorsements_rate = 100. -. endorsements_rate in
    let main_revelation_rate =
      calculate_revelation_rate cycle_position cycle_revelations_number in
    let alternative_heads_number = match number_alt_heads with
      | [ Some i ] -> Int64.to_int i
      | _ -> 0 in
    let double_endorsements = 0 in
    let switch_number = match switch_stats with
      | [ i, _ ] -> Int64.to_int i
      | _ -> -1 in
    let longest_switch_depth = match switch_stats with
      | [ _, i ] -> Int64.to_int i
      | _ -> -1 in
    let mean_priority =
      (List.fold_left (fun acc p -> Int32.to_float p +. acc) 0. priorities) /.
      (float (List.length priorities)) in
    let score_priority =
      let cycle_size =
        float_of_int @@
        (cycle_end_level - cycle_start_level + 1) in
      let m_priority = 16. in    (* TODO to delete *)
      let m2 = m_priority ** 2. in
      let m = m2 *. cycle_size in
      let s =
        List.fold_left (fun acc p ->
            let p = min (Int32.to_float p) m_priority in
            (p ** 2.) +. acc)
          0. priorities in
      ((m -. s) /. m) *. 100. in
    let biggest_block_volume = match biggest_block_volume with
      | ( hash, level, _ ) :: _ -> hash, Int64.to_int level
      | _ -> "No one", -1 in
    let biggest_block_fees = match biggest_block_fees with
      | ( hash, level, _ ) :: _ -> hash, Int64.to_int level
      | _ -> "No one", -1 in
    let top_baker = match top_baker with
      | ( top_baker, _ ) :: _ -> top_baker
      | _ -> "No one" in
    return {
      cycle_start_level ;
      cycle_end_level ;
      cycle_volume ;
      cycle_fees ;
      cycle_bakers ;
      cycle_endorsers ;
      cycle_date_start = (year_start, month_start, day_start) ;
      cycle_date_end = (year_end, month_end, day_end) ;

      endorsements_rate ;
      main_endorsements_rate ;
      alt_endorsements_rate ;
      empty_endorsements_rate ;
      double_endorsements ;

      main_revelation_rate ;

      alternative_heads_number ;
      switch_number ;
      longest_switch_depth ;

      mean_priority ;
      score_priority ;

      biggest_block_volume ;
      biggest_block_fees ;
      top_baker = Alias.to_name top_baker ;
    }

  let context_days () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT day FROM day_context ORDER BY day DESC" >>= fun days ->
    return @@
    List.map (fun d ->
        CalendarLib.Printer.Calendar.sprint "%Y-%m-%d" d) days

  let context_stats day =
    let day_before = CalendarLib.Calendar.prev day `Day in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT id FROM day_context WHERE day = $day" >>= function
    | [ id ] ->
      PGSQL(dbh)
        "SELECT * FROM context_totals WHERE id = $id" >>= begin function
        | [ _id, Some context_hash, _context_period, _context_period_kind,
            Some context_addresses, Some context_keys, Some context_revealed,
            Some context_originated, Some context_contracts, Some context_roll_owners,
            Some context_rolls, Some context_delegated, Some context_delegators,
            Some context_deleguees, Some context_self_delegates,
            Some context_multi_deleguees, Some context_current_balances,
            Some context_full_balances, Some context_staking_balances,
            Some context_frozen_balances, Some context_frozen_deposits,
            Some context_frozen_rewards, Some context_frozen_fees,
            Some context_paid_bytes, Some context_used_bytes ] ->
          level context_hash >>= fun context_level ->
          PGSQL(dbh)
            "SELECT id FROM day_context WHERE day = $day_before" >>= begin function
            | [ id_before ] ->
              PGSQL(dbh)
                "SELECT * FROM context_totals WHERE id = $id_before" >>= begin function
                | [ _id, _context_hash2, _context_period2, _context_period_kind2,
                    Some context_addresses2, Some context_keys2, Some context_revealed2,
                    Some context_originated2, Some context_contracts2, Some context_roll_owners2,
                    Some context_rolls2, Some context_delegated2, Some context_delegators2,
                    Some context_deleguees2, Some context_self_delegates2,
                    Some context_multi_deleguees2, Some context_current_balances2,
                    Some context_full_balances2, Some context_staking_balances2,
                    Some context_frozen_balances2, Some context_frozen_deposits2,
                    Some context_frozen_rewards2, Some context_frozen_fees2,
                    Some context_paid_bytes2, Some context_used_bytes2 ] ->
                  return @@
                  Some {
                    context_level ;

                    context_addresses =
                      Int32.to_int context_addresses ;
                    context_addresses_diff =
                      if context_addresses = 0l then 0.
                      else
                        ((Int32.to_float context_addresses) -.
                         (Int32.to_float context_addresses2)) /.
                        (Int32.to_float context_addresses) *. 100. ;
                    context_keys =
                      Int32.to_int context_keys ;
                    context_keys_diff =
                      if context_keys = 0l then 0.
                      else
                        ((Int32.to_float context_keys) -.
                         (Int32.to_float context_keys2)) /.
                        (Int32.to_float context_keys) *. 100. ;
                    context_revealed =
                      Int32.to_int context_revealed ;
                    context_revealed_diff =
                      if context_revealed = 0l then 0.
                      else
                        ((Int32.to_float context_revealed) -.
                         (Int32.to_float context_revealed2)) /.
                        (Int32.to_float context_revealed) *. 100. ;
                    context_originated =
                      Int32.to_int context_originated ;
                    context_originated_diff =
                      if context_originated = 0l then 0.
                      else
                        ((Int32.to_float context_originated) -.
                         (Int32.to_float context_originated2)) /.
                        (Int32.to_float context_originated) *. 100. ;
                    context_contracts =
                      Int32.to_int context_contracts ;
                    context_contracts_diff =
                      if context_contracts = 0l then 0.
                      else
                        ((Int32.to_float context_contracts) -.
                         (Int32.to_float context_contracts2)) /.
                        (Int32.to_float context_contracts) *. 100. ;
                    context_roll_owners =
                      Int32.to_int context_roll_owners ;
                    context_roll_owners_diff =
                      if context_roll_owners = 0l then 0.
                      else
                        ((Int32.to_float context_roll_owners) -.
                         (Int32.to_float context_roll_owners2)) /.
                        (Int32.to_float context_roll_owners) *. 100. ;
                    context_rolls =
                      Int32.to_int context_rolls ;
                    context_rolls_diff =
                      if context_rolls = 0l then 0.
                      else
                        ((Int32.to_float context_rolls) -.
                         (Int32.to_float context_rolls2)) /.
                        (Int32.to_float context_rolls) *. 100. ;
                    context_delegated ;
                    context_delegated_diff =
                      if context_delegated = 0L then 0.
                      else
                        ((Int64.to_float context_delegated) -.
                         (Int64.to_float context_delegated2)) /.
                        (Int64.to_float context_delegated) *. 100. ;
                    context_delegators =
                      Int32.to_int context_delegators ;
                    context_delegators_diff =
                      if context_delegators = 0l then 0.
                      else
                        ((Int32.to_float context_delegators) -.
                         (Int32.to_float context_delegators2)) /.
                        (Int32.to_float context_delegators) *. 100. ;
                    context_deleguees =
                      Int32.to_int context_deleguees ;
                    context_deleguees_diff =
                      if context_deleguees = 0l then 0.
                      else
                        ((Int32.to_float context_deleguees) -.
                         (Int32.to_float context_deleguees2)) /.
                        (Int32.to_float context_deleguees) *. 100. ;
                    context_self_delegates =
                      Int32.to_int context_self_delegates ;
                    context_self_delegates_diff =
                      if context_self_delegates = 0l then 0.
                      else
                        ((Int32.to_float context_self_delegates) -.
                         (Int32.to_float context_self_delegates2)) /.
                        (Int32.to_float context_self_delegates) *. 100. ;
                    context_multi_deleguees =
                      Int32.to_int context_multi_deleguees ;
                    context_multi_deleguees_diff =
                      if context_multi_deleguees = 0l then 0.
                      else
                        ((Int32.to_float context_multi_deleguees) -.
                         (Int32.to_float context_multi_deleguees2)) /.
                        (Int32.to_float context_multi_deleguees) *. 100. ;
                    context_current_balances ;
                    context_current_balances_diff =
                      if context_current_balances = 0L then 0.
                      else
                        ((Int64.to_float context_current_balances) -.
                         (Int64.to_float context_current_balances2)) /.
                        (Int64.to_float context_current_balances) *. 100. ;
                    context_full_balances ;
                    context_full_balances_diff =
                      if context_full_balances = 0L then 0.
                      else
                        ((Int64.to_float context_full_balances) -.
                         (Int64.to_float context_full_balances2)) /.
                        (Int64.to_float context_full_balances) *. 100. ;
                    context_staking_balances ;
                    context_staking_balances_diff =
                      if context_staking_balances = 0L then 0.
                      else
                        ((Int64.to_float context_staking_balances) -.
                         (Int64.to_float context_staking_balances2)) /.
                        (Int64.to_float context_staking_balances) *. 100. ;
                    context_frozen_balances ;
                    context_frozen_balances_diff =
                      if context_frozen_balances = 0L then 0.
                      else
                        ((Int64.to_float context_frozen_balances) -.
                         (Int64.to_float context_frozen_balances2)) /.
                        (Int64.to_float context_frozen_balances) *. 100. ;
                    context_frozen_deposits ;
                    context_frozen_deposits_diff =
                      if context_frozen_deposits = 0L then 0.
                      else
                        ((Int64.to_float context_frozen_deposits) -.
                         (Int64.to_float context_frozen_deposits2)) /.
                        (Int64.to_float context_frozen_deposits) *. 100. ;
                    context_frozen_rewards ;
                    context_frozen_rewards_diff =
                      if context_frozen_rewards = 0L then 0.
                      else
                        ((Int64.to_float context_frozen_rewards) -.
                         (Int64.to_float context_frozen_rewards2)) /.
                        (Int64.to_float context_frozen_rewards) *. 100. ;
                    context_frozen_fees ;
                    context_frozen_fees_diff =
                      if context_frozen_fees = 0L then 0.
                      else
                        ((Int64.to_float context_frozen_fees) -.
                         (Int64.to_float context_frozen_fees2)) /.
                        (Int64.to_float context_frozen_fees) *. 100. ;
                    context_paid_bytes ;
                    context_paid_bytes_diff =
                      if context_paid_bytes = 0L then 0.
                      else
                        ((Int64.to_float context_paid_bytes) -.
                         (Int64.to_float context_paid_bytes2)) /.
                        (Int64.to_float context_paid_bytes) *. 100. ;
                    context_used_bytes ;
                    context_used_bytes_diff =
                      if context_used_bytes = 0L then 0.
                      else
                        ((Int64.to_float context_used_bytes) -.
                         (Int64.to_float context_used_bytes2)) /.
                        (Int64.to_float context_used_bytes) *. 100. ;
                  }
                | _ ->
                  return @@
                  Some {
                    context_level ;
                    context_addresses = Int32.to_int context_addresses ;
                    context_addresses_diff = 100. ;
                    context_keys = Int32.to_int context_keys ;
                    context_keys_diff = 100. ;
                    context_revealed = Int32.to_int context_revealed ;
                    context_revealed_diff = 100. ;
                    context_originated = Int32.to_int context_originated ;
                    context_originated_diff = 100. ;
                    context_contracts = Int32.to_int context_contracts ;
                    context_contracts_diff = 100. ;
                    context_roll_owners = Int32.to_int context_roll_owners ;
                    context_roll_owners_diff = 100. ;
                    context_rolls = Int32.to_int context_rolls ;
                    context_rolls_diff = 100. ;
                    context_delegated ;
                    context_delegated_diff = 100. ;
                    context_delegators = Int32.to_int context_delegators ;
                    context_delegators_diff = 100. ;
                    context_deleguees = Int32.to_int context_deleguees ;
                    context_deleguees_diff = 100. ;
                    context_self_delegates = Int32.to_int context_self_delegates ;
                    context_self_delegates_diff = 100. ;
                    context_multi_deleguees = Int32.to_int context_multi_deleguees ;
                    context_multi_deleguees_diff = 100. ;
                    context_current_balances ;
                    context_current_balances_diff = 100. ;
                    context_full_balances ;
                    context_full_balances_diff = 100. ;
                    context_staking_balances ;
                    context_staking_balances_diff = 100. ;
                    context_frozen_balances ;
                    context_frozen_balances_diff = 100. ;
                    context_frozen_deposits ;
                    context_frozen_deposits_diff = 100. ;
                    context_frozen_rewards ;
                    context_frozen_rewards_diff = 100. ;
                    context_frozen_fees ;
                    context_frozen_fees_diff = 100. ;
                    context_paid_bytes ;
                    context_paid_bytes_diff = 100. ;
                    context_used_bytes ;
                    context_used_bytes_diff = 100. ;
                  }
              end
            | _ ->
              return @@
              Some {
                context_level ;
                context_addresses = Int32.to_int context_addresses ;
                context_addresses_diff = 100. ;
                context_keys = Int32.to_int context_keys ;
                context_keys_diff = 100. ;
                context_revealed = Int32.to_int context_revealed ;
                context_revealed_diff = 100. ;
                context_originated = Int32.to_int context_originated ;
                context_originated_diff = 100. ;
                context_contracts = Int32.to_int context_contracts ;
                context_contracts_diff = 100. ;
                context_roll_owners = Int32.to_int context_roll_owners ;
                context_roll_owners_diff = 100. ;
                context_rolls = Int32.to_int context_rolls ;
                context_rolls_diff = 100. ;
                context_delegated ;
                context_delegated_diff = 100. ;
                context_delegators = Int32.to_int context_delegators ;
                context_delegators_diff = 100. ;
                context_deleguees = Int32.to_int context_deleguees ;
                context_deleguees_diff = 100. ;
                context_self_delegates = Int32.to_int context_self_delegates ;
                context_self_delegates_diff = 100. ;
                context_multi_deleguees = Int32.to_int context_multi_deleguees ;
                context_multi_deleguees_diff = 100. ;
                context_current_balances ;
                context_current_balances_diff = 100. ;
                context_full_balances ;
                context_full_balances_diff = 100. ;
                context_staking_balances ;
                context_staking_balances_diff = 100. ;
                context_frozen_balances ;
                context_frozen_balances_diff = 100. ;
                context_frozen_deposits ;
                context_frozen_deposits_diff = 100. ;
                context_frozen_rewards ;
                context_frozen_rewards_diff = 100. ;
                context_frozen_fees ;
                context_frozen_fees_diff = 100. ;
                context_paid_bytes ;
                context_paid_bytes_diff = 100. ;
                context_used_bytes ;
                context_used_bytes_diff = 100. ;
              }
          end
        | _ -> return None
      end
    | _ -> return None

  let nb_tops ?(kind="balances") () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT id FROM day_context \
       WHERE day = (SELECT MAX(day) FROM day_context)" >>= begin function
      | [ id ] -> return id
      | _ -> return Int64.minus_one
    end >>= fun id ->
    match kind with
    | "balances" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_balances \
         WHERE id = $id" >>= of_count_opt
    | "frozen_balances" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_frozen_balances \
         WHERE id = $id" >>= of_count_opt
    | "frozen_deposits" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_frozen_deposits \
         WHERE id = $id" >>= of_count_opt
    | "frozen_rewards" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_frozen_rewards \
         WHERE id = $id" >>= of_count_opt
    | "paid_bytes" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_paid_bytes \
         WHERE id = $id" >>= of_count_opt
    | "staking_balances" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_staking_balances \
         WHERE id = $id" >>= of_count_opt
    | "total_balances" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_total_balances \
         WHERE id = $id" >>= of_count_opt
    | "total_delegated" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_total_delegated \
         WHERE id = $id" >>= of_count_opt
    | "total_delegators" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_total_delegators \
         WHERE id = $id" >>= of_count_opt
    | "total_frozen_fees" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_total_frozen_fees \
         WHERE id = $id" >>= of_count_opt
    | "used_bytes" ->
      PGSQL(dbh)
        "SELECT COUNT(hash) FROM top_used_bytes \
         WHERE id = $id" >>= of_count_opt
    | _ -> return (-1)

  let tops ?(page=0) ?(page_size=20) ?(kind="balances") () =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT id FROM day_context \
       WHERE day = (SELECT MAX(day) FROM day_context)" >>= begin function
      | [ id ] -> return id
      | _ -> return Int64.minus_one
    end >>= fun id ->
    match kind with
    | "balances" ->
      PGSQL(dbh)
        "SELECT hash, balance FROM top_balances \
         WHERE id = $id ORDER BY balance DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "frozen_balances" ->
      PGSQL(dbh)
        "SELECT hash, frozen_balance FROM top_frozen_balances \
         WHERE id = $id ORDER BY frozen_balance DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "frozen_deposits" ->
      PGSQL(dbh)
        "SELECT hash, frozen_deposits FROM top_frozen_deposits \
         WHERE id = $id ORDER BY frozen_deposits DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "frozen_rewards" ->
      PGSQL(dbh)
        "SELECT hash, frozen_rewards FROM top_frozen_rewards \
         WHERE id = $id ORDER BY frozen_rewards DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "paid_bytes" ->
      PGSQL(dbh)
        "SELECT hash, paid_bytes FROM top_paid_bytes \
         WHERE id = $id ORDER BY paid_bytes DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "staking_balances" ->
      PGSQL(dbh)
        "SELECT hash, staking_balance FROM top_staking_balances \
         WHERE id = $id ORDER BY staking_balance DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "total_balances" ->
      PGSQL(dbh)
        "SELECT hash, total_balance FROM top_total_balances \
         WHERE id = $id ORDER BY total_balance DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "total_delegated" ->
      PGSQL(dbh)
        "SELECT hash, total_delegated FROM top_total_delegated \
         WHERE id = $id ORDER BY total_delegated DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "total_delegators" ->
      PGSQL(dbh)
        "SELECT hash, total_delegators FROM top_total_delegators \
         WHERE id = $id ORDER BY total_delegators DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "total_frozen_fees" ->
      PGSQL(dbh)
        "SELECT hash, total_frozen_fees FROM top_total_frozen_fees \
         WHERE id = $id ORDER BY total_frozen_fees DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | "used_bytes" ->
      PGSQL(dbh)
        "SELECT hash, used_bytes FROM top_used_bytes \
         WHERE id = $id ORDER BY used_bytes DESC, hash OFFSET $offset LIMIT $limit"
      >>= return
    | _ -> return []

  let account_bonds_rewards hash =
    with_dbh >>> fun dbh ->
    let allowed_fork64 = Int64.of_int Tezos_constants.Constants.allowed_fork in
    let ramp_up_cycle = Int64.of_int Tezos_constants.ramp_up_cycles in
    let b_reward = Tezos_constants.Constants.block_reward in
    let e_reward = Tezos_constants.Constants.endorsement_reward_coeff in
    let b_security_deposit = Tezos_constants.Constants.block_security_deposit in
    let e_security_deposit = Tezos_constants.Constants.endorsement_security_deposit in
    PGSQL(dbh)
      "SELECT MAX(cycle) FROM block" >>= fun current_cycle ->
    let cycle_limit = match current_cycle with
      | [ Some c ] ->
        if c < allowed_fork64 then 0L
        else Int64.sub c allowed_fork64
      | _ -> assert false in
    PGSQL(dbh)
      "SELECT SUM(bk_rewards($b_reward, cycle, true) * array_sum(nb_baking,1))::bigint, \
       SUM(deposits($b_security_deposit, cycle, $ramp_up_cycle, 1, true) \
       * array_sum(nb_baking,1))::bigint, \
       SUM(fees)::bigint \
       FROM cycle_count_baker \
       WHERE cycle >= $cycle_limit AND tz = $hash"
    >>= fun block_res ->
    PGSQL(dbh)
      "SELECT SUM(end_rewards_array($e_reward, cycle, nb_endorsement))::bigint, \
       SUM(deposits($e_security_deposit, cycle, $ramp_up_cycle, array_sum(nb_endorsement,1)::int, true))::bigint \
       FROM cycle_count_baker \
       WHERE tz = $hash AND cycle >= $cycle_limit"
    >>= fun endorsement_res ->
    let acc_b_rewards, acc_b_deposits, acc_fees = match block_res with
      | [ Some acc_b_rewards, Some acc_b_deposits, Some acc_fees ] ->
        acc_b_rewards, acc_b_deposits, acc_fees
      | _ -> 0L, 0L, 0L in
    let acc_e_rewards, acc_e_deposits = match endorsement_res with
       | [ Some acc_e_rewards, Some acc_e_deposits ] ->
        acc_e_rewards, acc_e_deposits
       | _ -> 0L, 0L in
    return {acc_b_rewards; acc_b_deposits; acc_fees; acc_e_rewards; acc_e_deposits}

  let max_roll_cycle () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT MAX(cycle) FROM snapshot_rolls" >>= of_count_opt

  let rolls_distribution cycle =
    let cycle = Int64.of_int cycle in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT tz1, count FROM snapshot_owner AS so \
       INNER JOIN snapshot_rolls AS sr ON sr.id = so.id \
       WHERE sr.cycle = $cycle ORDER BY count, tz1 DESC" >>= fun rows ->
    return @@
    List.map (fun (tz, rolls) -> Alias.to_name tz, Int32.to_int rolls) rows

  let roll_number account_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT count FROM snapshot_owner AS so \
       INNER JOIN \
       (SELECT id FROM snapshot_rolls AS sr ORDER BY sr.cycle DESC LIMIT 1) \
       AS max_id  ON max_id.id = so.id \
       WHERE tz1 = $account_hash" >>= function
    | [ count ] -> return @@ Int32.to_int count
    | _ -> return 0

  let rolls_history ?(page=0) ?(page_size=20) account_hash =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "nullable-results"
      "SELECT sr.cycle, COALESCE(so.count, 0), COALESCE(sr.rolls_count, 0) FROM snapshot_rolls AS sr \
       LEFT JOIN snapshot_owner AS so ON sr.id = so.id AND so.tz1 = $account_hash \
       ORDER BY sr.cycle DESC OFFSET $offset LIMIT $limit" >>= fun list ->
    return @@
    List.map (fun (cycle, rolls, rolls_total) ->
        (match cycle with None -> 0L | Some c -> c),
        (match rolls with None -> 0l | Some r -> r),
        (match rolls_total with None -> 0l | Some r -> r))
      list

  let all_deleguees_count_by_cycle_count () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(DISTINCT id) FROM snapshot_deleguees" >>= of_count_opt

  let all_deleguees_count_by_cycle ?(page=0) ?(page_size=20) () =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT sr.cycle, COUNT(deleguee) FROM snapshot_deleguees AS sd \
       INNER JOIN snapshot_rolls AS sr ON sd.id = sr.id \
       GROUP BY sr.cycle ORDER BY sr.cycle DESC OFFSET $offset LIMIT $limit"
    >>= fun list ->
    return @@
    List.rev @@
    List.fold_left (fun acc (cycle, count_opt) ->
        match count_opt with
        | None -> acc
        | Some c -> (cycle, c) :: acc) [] list

  let deleguees_count_by_cycle_count account_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(DISTINCT id) FROM snapshot_deleguees AS sd \
       WHERE sd.tz1 = $account_hash" >>= of_count_opt

  let deleguees_count_by_cycle ?(page=0) ?(page_size=20) account_hash =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT sr.cycle, COUNT(deleguee) FROM snapshot_deleguees AS sd \
       INNER JOIN snapshot_rolls AS sr ON sd.id = sr.id \
       WHERE sd.tz1 = $account_hash \
       GROUP BY sr.cycle ORDER BY sr.cycle DESC OFFSET $offset LIMIT $limit"
    >>= fun list ->
    return @@
    List.rev @@
    List.fold_left (fun acc (cycle, count_opt) ->
        match count_opt with
        | None -> acc
        | Some c -> (cycle, c) :: acc) [] list

  let deleguees_count account_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(deleguee) FROM snapshot_deleguees AS sd \
       INNER JOIN \
       (SELECT id FROM snapshot_rolls AS sr ORDER BY sr.cycle DESC LIMIT 1) \
       AS max_id  ON max_id.id = sd.id \
       WHERE tz1 = $account_hash" >>= of_count_opt

  let deleguees ?(page=0) ?(page_size=20) account_hash =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT deleguee FROM snapshot_deleguees AS sd \
       INNER JOIN \
       (SELECT id FROM snapshot_rolls AS sr ORDER BY sr.cycle DESC LIMIT 1) \
       AS max_id  ON max_id.id = sd.id \
       WHERE tz1 = $account_hash OFFSET $offset LIMIT $limit"

  let nb_delegators hash cycle =
    let cycle = Int64.of_int cycle in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT count(*) \
       FROM snapshot_deleguees AS sd \
       INNER JOIN snapshot_rolls AS sr ON sd.id = sr.id
       WHERE tz1 = $hash AND sr.cycle = $cycle" >>= function
    | [ Some count ] -> return @@ Int64.to_int count
    | _ -> return 0

  let nb_cycle_rewards hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(sr.cycle) FROM snapshot_owner AS so \
       INNER JOIN snapshot_rolls AS sr ON sr.id = so.id \
       WHERE so.tz1 = $hash" >>= function
      | [ Some count ] -> return @@ Int64.to_int count
      | _ -> return 0

  let delegate_rewards_split_cycles ?(page=0) ?(page_size=20) hash =
    head_level () >>= fun head_lvl ->
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    let b_reward = Tezos_constants.Constants.block_reward in
    let e_reward = Tezos_constants.Constants.endorsement_reward_coeff in
    let r_reward = Tezos_constants.Constants.revelation_reward in
    let blocks_between_revelations =
      Int64.of_int Tezos_constants.Constants.blocks_between_revelations in
    let block_per_cycle = Int64.of_int Tezos_constants.Constants.block_per_cycle in
    with_dbh >>> fun dbh ->
    (* assume endorsed_level = endorsing_level - 1 for rights *)
    PGSQL(dbh)
      "SELECT MAX(cycle) FROM block" >>= begin function
      | [ Some current_cycle ] -> return current_cycle
      | _ -> return 0L end >>= fun current_cycle ->
    PGSQL(dbh)
      "nullable-results"
      "WITH block_rew(cycle, fees, rewards) AS \
       (SELECT cycle, fees, array_sum(nb_baking,1) * bk_rewards($b_reward, cycle, true) \
       FROM cycle_count_baker \
       WHERE tz = $hash), \
       endorsement_rew(cycle, rewards) AS \
       (SELECT cycle, end_rewards_array($e_reward, cycle, nb_endorsement) \
       FROM cycle_count_baker \
       WHERE tz = $hash), \
       delegators AS \
       (SELECT sd.id, sd.tz1, count(*) AS count \
       FROM snapshot_deleguees AS sd WHERE tz1 = $hash GROUP BY id, tz1), \
       bk_rights AS \
       (SELECT cycle, SUM(bk_rewards($b_reward, cycle, true))::bigint AS reward \
       FROM level_rights WHERE bakers_priority[ARRAY_POSITION(bakers, $hash)] = 0 \
       AND level > $head_lvl \
       GROUP BY cycle ORDER BY cycle ASC), \
       endo_rights AS \
       (SELECT level / $block_per_cycle AS endo_cycle, \
       SUM(end_rewards($e_reward, level / $block_per_cycle, \
       slots[ARRAY_POSITION(endorsers, $hash)], 0, true))::bigint AS reward \
       FROM level_rights WHERE $hash = ANY(endorsers) AND level > $head_lvl \
       GROUP BY endo_cycle ORDER BY endo_cycle ASC), \
       gain_rewards AS \
       (SELECT bl.cycle, \
       SUM(CASE WHEN dbe.denouncer = $hash THEN dbe.gain_rewards ELSE 0 END) AS gain, \
       SUM(CASE WHEN dbe.accused = $hash THEN dbe.lost_deposit ELSE 0 END) AS lost_deposits, \
       SUM(CASE WHEN dbe.accused = $hash THEN dbe.lost_rewards ELSE 0 END) AS lost_rewards, \
       SUM(CASE WHEN dbe.accused = $hash THEN dbe.lost_fees ELSE 0 END) AS lost_fees \
       FROM double_baking_evidence as dbe \
       INNER JOIN block_operation as bo on bo.operation_hash = dbe.hash \
       INNER JOIN block as bl on bl.hash = bo.block_hash \
       WHERE dbe.denouncer = $hash OR dbe.accused = $hash GROUP BY bl.cycle), \
       revelation_rewards(cycle, reward, lost_reward, lost_fees) AS ( \
       SELECT bl.cycle, \
       SUM(CASE WHEN s.hash IS NOT NULL THEN $r_reward ELSE 0::bigint END)::bigint, \
       SUM(CASE WHEN s.hash IS NULL THEN $b_reward ELSE 0::bigint END)::bigint, \
       SUM(CASE WHEN s.hash IS NULL THEN bl.fees ELSE 0 END)::bigint \
       FROM block AS bl \
       LEFT JOIN seed_nonce_revelation AS s ON s.level = bl.level \
       WHERE bl.baker = $hash AND bl.level % $blocks_between_revelations = 0 \
       AND bl.cycle < $current_cycle - 1::bigint \
       GROUP BY bl.cycle) \
       SELECT sr.cycle, so.staking_balance, so.delegated_balance, \
       d.count, br.rewards, br.fees, er.rewards, bkr.reward, endr.reward::bigint, \
       den.gain::bigint, den.lost_deposits::bigint, den.lost_rewards::bigint, \
       den.lost_fees::bigint, rv.reward, rv.lost_reward, rv.lost_fees \
       FROM snapshot_owner AS so \
       INNER JOIN snapshot_rolls AS sr ON sr.id = so.id \
       LEFT JOIN delegators AS d ON d.id = sr.id \
       LEFT JOIN block_rew AS br ON br.cycle = sr.cycle \
       LEFT JOIN endorsement_rew AS er ON er.cycle = sr.cycle \
       LEFT JOIN gain_rewards AS den ON den.cycle = sr.cycle \
       LEFT JOIN bk_rights AS bkr ON bkr.cycle = sr.cycle \
       LEFT JOIN endo_rights AS endr ON endr.endo_cycle = sr.cycle \
       LEFT JOIN revelation_rewards AS rv ON rv.cycle = sr.cycle \
       WHERE so.tz1 = $hash ORDER BY sr.cycle DESC \
       OFFSET $offset LIMIT $limit" >>= fun row ->
    return @@ List.rev @@ List.fold_left
      (fun acc ars -> match ars with
           (Some ars_cycle, Some staking_balance, Some delegated_balance,
            nb_del, brewards, ars_fees, erewards,
            ars_baking_rights, ars_endorsing_rights, gain,
            lost_deposit, lost_rewards, lost_fees,
            rv_reward, lost_rv_reward, lost_rv_fees) ->
           let ars_cycle = Int64.to_int ars_cycle in
           let ars_delegate_staking_balance = staking_balance in
           let ars_delegate_delegated_balance = delegated_balance in
           let ars_delegators_nb = Misc.unoptf 0 Int64.to_int nb_del in
           let ars_block_rewards = Misc.unopt 0L brewards in
           let ars_fees = Misc.unopt 0L ars_fees in
           let ars_gain_from_denounciation = Misc.unopt 0L gain in
           let ars_lost_deposit = Misc.unopt 0L lost_deposit in
           let ars_lost_rewards = Misc.unopt 0L lost_rewards in
           let ars_lost_fees = Misc.unopt 0L lost_fees in
           let ars_endorsement_rewards = Misc.unopt 0L erewards in
           let ars_baking_rights_rewards = Misc.unopt 0L ars_baking_rights in
           let ars_endorsing_rights_rewards = Misc.unopt 0L ars_endorsing_rights in
           let ars_rv_rewards = Misc.unopt 0L rv_reward in
           let ars_rv_lost_rewards = Misc.unopt 0L lost_rv_reward in
           let ars_rv_lost_fees = Misc.unopt 0L lost_rv_fees in
           let current_cycle = Int64.to_int current_cycle in
           let unfrozen_cycle_offset =
             current_cycle - Tezos_constants.Constants.allowed_fork in
           let ars_status = match ars_cycle - current_cycle with
             | 0 -> Cycle_in_progress
             | diff when diff > 0 -> Cycle_pending
             | diff
               when diff < 0 && ars_cycle >= unfrozen_cycle_offset ->
               Rewards_pending
             | diff
               when diff < 0 && ars_cycle < unfrozen_cycle_offset ->
               Rewards_delivered
             | _ -> assert false (* Cannot happen *) in
           { ars_cycle ;
             ars_delegate_staking_balance ;
             ars_delegators_nb ;
             ars_delegate_delegated_balance ;
             ars_block_rewards ; ars_fees ; ars_endorsement_rewards ;
             ars_baking_rights_rewards ; ars_endorsing_rights_rewards ;
             ars_status ;
             ars_gain_from_denounciation ;
             ars_lost_deposit ; ars_lost_rewards ; ars_lost_fees ;
             ars_rv_rewards; ars_rv_lost_rewards; ars_rv_lost_fees
           } :: acc
         | _ -> acc) [] row

  let delegate_rewards_split ?(page=0) ?(page_size=20) hash cyclei =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    let cycle = Int64.of_int cyclei in
    let b_reward = Tezos_constants.Constants.block_reward in
    let e_reward = Tezos_constants.Constants.endorsement_reward_coeff in
    let r_reward = Tezos_constants.Constants.revelation_reward in
    let blocks_between_revelations =
      Int64.of_int Tezos_constants.Constants.blocks_between_revelations in
    let block_per_cycle = Int64.of_int Tezos_constants.Constants.block_per_cycle in
    with_dbh >>> fun dbh ->
    head_level () >>= fun head_lvl ->
    PGSQL(dbh)
      "SELECT bk_rewards($b_reward, cycle, true) * array_sum(nb_baking, 1), fees \
       FROM cycle_count_baker \
       WHERE cycle = $cycle AND tz = $hash"
    >>= fun block_res ->
    PGSQL(dbh)
      "SELECT end_rewards_array($e_reward, cycle, nb_endorsement) \
       FROM cycle_count_baker \
       WHERE tz = $hash AND cycle = $cycle"
    >>= fun endorsement_res ->
    PGSQL(dbh)
      "SELECT SUM(bk_rewards($b_reward, cycle, true))::bigint \
       FROM level_rights WHERE bakers_priority[ARRAY_POSITION(bakers, $hash)] = 0 \
       AND cycle = $cycle AND level > $head_lvl"
    >>= fun bk_rights ->
    PGSQL(dbh)
      "SELECT SUM(end_rewards($e_reward, level / $block_per_cycle, \
       slots[ARRAY_POSITION(endorsers, $hash)], 0, true))::bigint \
       FROM level_rights WHERE $hash = ANY(endorsers) AND \
       level / $block_per_cycle = $cycle AND level > $head_lvl"
    >>= fun endo_rights ->
    PGSQL(dbh)
      "SELECT COUNT(deleguee) FROM snapshot_deleguees AS sd \
       WHERE sd.tz1 = $hash AND \
       sd.id = (SELECT id FROM snapshot_rolls WHERE cycle = $cycle)"
    >>= fun nb_del ->
    PGSQL(dbh)
      "SELECT deleguee, balance FROM snapshot_deleguees AS sd \
       WHERE sd.tz1 = $hash \
       AND sd.id = (SELECT id FROM snapshot_rolls WHERE cycle = $cycle) \
       ORDER BY balance DESC, deleguee OFFSET $offset LIMIT $limit"
    >>= fun rs_delegators_balance ->
    PGSQL(dbh)
        "SELECT staking_balance FROM snapshot_owner AS so \
         WHERE so.tz1 = $hash \
         AND so.id = (SELECT id FROM snapshot_rolls WHERE cycle = $cycle)"
    >>= fun sbalance ->
    PGSQL(dbh)
      "SELECT \
       CAST(SUM(CASE WHEN dbe.denouncer = $hash THEN \
       dbe.gain_rewards ELSE 0 END) AS bigint) AS gain, \
       CAST(SUM(CASE WHEN dbe.accused = $hash THEN \
       dbe.lost_deposit ELSE 0 END) AS bigint) AS lost_deposits, \
       CAST(SUM(CASE WHEN dbe.accused = $hash THEN \
       dbe.lost_rewards ELSE 0 END) AS bigint) AS lost_rewards, \
       CAST(SUM(CASE WHEN dbe.accused = $hash THEN \
       dbe.lost_fees ELSE 0 END) AS bigint) AS lost_fees \
       FROM double_baking_evidence as dbe \
       INNER JOIN block_operation as bo on bo.operation_hash = dbe.hash \
       INNER JOIN block as bl on bl.hash = bo.block_hash \
       WHERE (dbe.denouncer = $hash OR dbe.accused = $hash) \
       AND bl.cycle = $cycle"
    >>= fun dbe_res ->
    PGSQL(dbh)
      "SELECT MAX(cycle) from block" >>= begin function
    | [ Some cycle ] -> return cycle
    | _ -> return 0L end >>= fun current_cycle ->
    PGSQL(dbh)
      "SELECT
       SUM(CASE WHEN s.hash IS NOT NULL THEN $r_reward ELSE 0::bigint END)::bigint, \
       SUM(CASE WHEN s.hash IS NULL THEN $b_reward ELSE 0::bigint END)::bigint, \
       SUM(CASE WHEN s.hash IS NULL THEN bl.fees ELSE 0 END)::bigint \
       FROM block AS bl \
       LEFT JOIN seed_nonce_revelation AS s ON s.level = bl.level \
       WHERE bl.baker = $hash AND bl.level % $blocks_between_revelations = 0 \
       AND bl.cycle = $cycle AND bl.cycle < $current_cycle - 1::bigint"
    >>= fun rv_res ->
    let rs_delegators_nb = match nb_del with
      | [ Some count ] -> Int64.to_int count
      | _ -> -1 in
    let rs_block_rewards, rs_fees = match block_res with
      | [ Some b_rewards, fees ] -> b_rewards, fees
      | _ -> 0L, 0L in
    let rs_endorsement_rewards = match endorsement_res with
      | [ Some e_rewards ] -> e_rewards
      | _ -> 0L in
    let rs_delegate_staking_balance = match sbalance with
      | [ i64 ] -> i64
      | _ -> 0L in
    let rs_baking_rights_rewards = match bk_rights with
      | [ Some rewards ] -> rewards
      | _ -> 0L in
    let rs_endorsing_rights_rewards = match endo_rights with
      | [ Some rewards ] -> rewards
      | _ -> 0L in
    let rs_delegators_balance =
      List.map (fun (del, bal) ->
          Alias.to_name del, bal) rs_delegators_balance in
    let ( rs_gain_from_denounciation, rs_lost_deposit,
          rs_lost_rewards, rs_lost_fees ) = match dbe_res with
      | [ Some gain, Some ldepo, Some lrew, Some lfees ] -> gain, ldepo, lrew, lfees
      | _ -> 0L, 0L, 0L, 0L in
    let rs_rv_rewards, rs_rv_lost_rewards, rs_rv_lost_fees = match rv_res with
      | [ Some rv_reward, Some rv_lost_reward, Some rv_lost_fees ] ->
        rv_reward, rv_lost_reward, rv_lost_fees
      | _ -> 0L, 0L, 0L in
    return
      { rs_delegate_staking_balance ; rs_delegators_nb ; rs_delegators_balance ;
        rs_block_rewards ; rs_fees ; rs_endorsement_rewards ;
        rs_baking_rights_rewards ; rs_endorsing_rights_rewards ;
        rs_gain_from_denounciation ; rs_lost_deposit ; rs_lost_rewards; rs_lost_fees;
        rs_rv_rewards; rs_rv_lost_rewards; rs_rv_lost_fees
      }

  let delegate_rewards_split_fast ?(page=0) ?(page_size=20) hash cyclei =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    let cycle = Int64.of_int cyclei in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT deleguee, balance FROM snapshot_deleguees AS sd \
       WHERE sd.tz1 = $hash \
       AND sd.id = (SELECT id FROM snapshot_rolls WHERE cycle = $cycle) \
       ORDER BY balance DESC, deleguee OFFSET $offset LIMIT $limit"
    >>= fun rs_delegators_balance ->
    return  @@ List.map (fun (del, bal) ->
        Alias.to_name del, bal) rs_delegators_balance

   let nb_cycle_delegator_rewards hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(cycle) FROM snapshot_rolls AS sr \
       INNER JOIN snapshot_deleguees AS sd ON sd.id = sr.id \
       WHERE sd.deleguee = $hash" >>= of_count_opt

  let delegator_rewards ?(page=0) ?(page_size=20) hash =
    head_level () >>= fun head_lvl ->
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    let b_reward = Tezos_constants.Constants.block_reward in
    let e_reward = Tezos_constants.Constants.endorsement_reward_coeff in
    let r_reward = Tezos_constants.Constants.revelation_reward in
    let blocks_between_revelations =
      Int64.of_int Tezos_constants.Constants.blocks_between_revelations in
    let block_per_cycle = Int64.of_int Tezos_constants.Constants.block_per_cycle in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT MAX(cycle) FROM block" >>= begin function
    | [ Some cycle ] -> return cycle
    | _ -> return 0L end >>= fun current_cycle ->
    PGSQL(dbh)
      "nullable-results"
      "WITH delegate AS \
       (SELECT sr.cycle, tz1, sd.id, balance \
       FROM snapshot_deleguees AS sd \
       INNER JOIN snapshot_rolls AS sr ON sr.id = sd.id \
       WHERE deleguee = $hash), \
       block_rew(cycle, fees, rewards) AS \
       (SELECT ccb.cycle, fees, array_sum(nb_baking,1) * \
       bk_rewards($b_reward, ccb.cycle, true) \
       FROM cycle_count_baker AS ccb \
       INNER JOIN delegate AS dl ON dl.tz1 = ccb.tz AND dl.cycle = ccb.cycle), \
       endorsement_rew(cycle, rewards) AS \
       (SELECT ccb.cycle, end_rewards_array($e_reward, ccb.cycle, nb_endorsement) \
       FROM cycle_count_baker AS ccb \
       INNER JOIN delegate AS dl ON dl.tz1 = ccb.tz AND dl.cycle = ccb.cycle), \
       bk_rights(cycle, rewards) AS \
       (SELECT lr.cycle, SUM(bk_rewards($b_reward, lr.cycle, true))::bigint \
       FROM level_rights AS lr
       INNER JOIN delegate AS dl ON dl.cycle = lr.cycle AND \
       dl.tz1 = ANY (bakers) \
       WHERE bakers_priority[ARRAY_POSITION(bakers::varchar[], dl.tz1::varchar)] = 0 \
       AND level > $head_lvl \
       GROUP BY lr.cycle ORDER BY lr.cycle ASC), \
       endo_rights(endo_cycle, rewards) AS \
       (SELECT level / $block_per_cycle AS endo_cycle, \
       SUM(end_rewards($e_reward, level / $block_per_cycle, \
       slots[ARRAY_POSITION(endorsers::varchar[], dl.tz1::varchar)], 0, true))::bigint \
       FROM level_rights AS lr \
       INNER JOIN delegate AS dl ON dl.cycle = level / $block_per_cycle \
       AND dl.tz1 = ANY (endorsers) \
       WHERE level > $head_lvl
       GROUP BY endo_cycle ORDER BY endo_cycle ASC), \
       dbe_rewards(cycle, gain, lost_deposits, lost_rewards, lost_fees) AS \
       (SELECT bl.cycle, \
       SUM(CASE WHEN dbe.denouncer = dl.tz1 THEN dbe.gain_rewards ELSE 0 END)::bigint, \
       SUM(CASE WHEN dbe.accused = dl.tz1 THEN dbe.lost_deposit ELSE 0 END)::bigint, \
       SUM(CASE WHEN dbe.accused = dl.tz1 THEN dbe.lost_rewards ELSE 0 END)::bigint, \
       SUM(CASE WHEN dbe.accused = dl.tz1 THEN dbe.lost_fees ELSE 0 END)::bigint \
       FROM double_baking_evidence as dbe \
       INNER JOIN block_operation as bo on bo.operation_hash = dbe.hash \
       INNER JOIN block as bl on bl.hash = bo.block_hash \
       INNER JOIN delegate AS dl ON dl.cycle = bl.cycle AND \
       (dl.tz1 = dbe.denouncer OR dl.tz1 = dbe.accused) \
       GROUP BY bl.cycle), \
       revelation_rewards(cycle, reward, lost_reward, lost_fees) AS ( \
       SELECT bl.cycle, \
       SUM(CASE WHEN s.hash IS NOT NULL THEN $r_reward ELSE 0::bigint END)::bigint, \
       SUM(CASE WHEN s.hash IS NULL THEN $b_reward ELSE 0::bigint END)::bigint, \
       SUM(CASE WHEN s.hash IS NULL THEN bl.fees ELSE 0 END)::bigint \
       FROM block AS bl \
       INNER JOIN delegate AS dl ON dl.cycle = bl.cycle \
       LEFT JOIN seed_nonce_revelation AS s ON s.level = bl.level \
       WHERE bl.baker = dl.tz1 AND bl.level % $blocks_between_revelations = 0 \
       AND bl.cycle < $current_cycle - 1::bigint \
       GROUP BY bl.cycle) \
       SELECT dl.cycle, dl.tz1, dl.balance, so.staking_balance, \
       br.rewards, br.fees, er.rewards, bkr.rewards, endr.rewards, \
       dbe.gain, dbe.lost_deposits, dbe.lost_rewards, \
       dbe.lost_fees, rv.reward, rv.lost_reward, rv.lost_fees \
       FROM delegate AS dl \
       INNER JOIN snapshot_owner AS so ON (so.id = dl.id AND so.tz1 = dl.tz1) \
       LEFT JOIN block_rew AS br ON br.cycle = dl.cycle \
       LEFT JOIN endorsement_rew AS er ON er.cycle = dl.cycle \
       LEFT JOIN bk_rights AS bkr ON bkr.cycle = dl.cycle \
       LEFT JOIN endo_rights AS endr ON endr.endo_cycle = dl.cycle \
       LEFT JOIN dbe_rewards AS dbe ON dbe.cycle = dl.cycle \
       LEFT JOIN revelation_rewards AS rv ON rv.cycle = dl.cycle \
       ORDER BY dl.cycle DESC \
       OFFSET $offset LIMIT $limit"
    >>= fun rows ->
    return @@ List.rev @@ List.fold_left
      (fun acc dor -> match dor with
         | (Some dor_cycle, Some dor_delegate, Some dor_balance,
            Some dor_staking_balance, brewards, fees, erewards, brewards_rights,
            erewards_rights,
            dbe_rewards, dbe_lost_deposits, dbe_lost_rewards, dbe_lost_fees,
            rv_rewards, rv_lost_rewards, rv_lost_fees) ->
           let dor_cycle = Int64.to_int dor_cycle in
           let dor_delegate = Alias.to_name dor_delegate in
           let dor_rewards =
             List.fold_left Int64.add 0L [
               Misc.unopt 0L brewards; Misc.unopt 0L erewards;
               Misc.unopt 0L brewards_rights;
               Misc.unopt 0L erewards_rights;
               Misc.unopt 0L fees ] in
           let dor_extra_rewards =
             List.fold_left Int64.add 0L [
               Misc.unopt 0L dbe_rewards; Misc.unopt 0L rv_rewards ] in
           let dor_losses =
             List.fold_left Int64.add 0L [
               Misc.unopt 0L dbe_lost_deposits;
               Misc.unopt 0L dbe_lost_rewards;
               Misc.unopt 0L dbe_lost_fees;
               Misc.unopt 0L rv_lost_rewards;
               Misc.unopt 0L rv_lost_fees ] in
           let current_cycle = Int64.to_int current_cycle in
           let unfrozen_cycle_offset =
             current_cycle - Tezos_constants.Constants.allowed_fork in
           let dor_status = match dor_cycle - current_cycle with
             | 0 -> Cycle_in_progress
             | diff when diff > 0 -> Cycle_pending
             | diff when diff < 0 && dor_cycle >= unfrozen_cycle_offset ->
               Rewards_pending
             | diff when diff < 0 && dor_cycle < unfrozen_cycle_offset ->
               Rewards_delivered
             | _ -> assert false (* Cannot happen *) in
           { dor_cycle; dor_delegate; dor_staking_balance; dor_balance;
             dor_rewards; dor_extra_rewards; dor_losses; dor_status } :: acc
         | _ -> acc
      ) [] rows


  let rewards_stats ?cycle _hash =
    match cycle with
    | None | Some _ ->
      return {
        rstats_staking_balance = 0L ;
        rstats_delegators_nb = 0 ;
        rstats_rewards = 0L;
        rstats_pc_blocks = 0. ;
        rstats_pc_endorsements = 0. ;
      }

  let search_block ?(limit=20) str =
    let limit = Int64.of_int (min limit search_limit) in
    let str2 = Printf.sprintf "%s%%" str in
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT b.hash FROM block AS b WHERE b.hash LIKE $str2 LIMIT $limit"

  let search_operation ?(limit=20) str =
    let limit = Int64.of_int (min limit search_limit) in
    let str2 = Printf.sprintf "%s%%" str in
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT o.hash FROM operation AS o WHERE o.hash LIKE $str2 LIMIT $limit"

  let search_account ?(limit=20) str =
    let limit = Int64.of_int (min limit search_limit) in
    let str2 = Printf.sprintf "%s%%" str in
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT hash, alias, (CASE WHEN hash ILIKE $str2 \
                THEN 'account' ELSE 'alias' END) FROM tezos_user \
                WHERE hash ILIKE $str2 OR alias ILIKE $str2 LIMIT $limit"
    >>= fun rows ->
    return @@ List.fold_left (fun acc row -> match row with
        | (tz, alias, Some kind) -> ({tz; alias}, kind) :: acc
        | _ -> acc) [] rows


  let nb_search_block str =
    let str2 = Printf.sprintf "%s%%" str in
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT COUNT(b.hash) FROM block AS b WHERE b.hash LIKE $str2"
    >>= of_count_opt

  let nb_search_operation str =
    let str2 = Printf.sprintf "%s%%" str in
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT COUNT(o.hash) FROM operation AS o WHERE o.hash LIKE $str2"
    >>= of_count_opt

  let nb_search_account str =
    let str2 = Printf.sprintf "%s%%" str in
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT COUNT(*) FROM tezos_user \
                WHERE hash ILIKE $str2 OR alias ILIKE $str2" >>= of_count_opt

  let activated_balances () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(balance)::bigint FROM activation_balance" >>= function
    | [ Some sum ] -> return sum
    | _ -> return 0L

  (* let supply_activated_balances () =
   *   with_dbh >>> fun dbh ->
   *   PGSQL(dbh) "SELECT SUM(balance)::bigint FROM activation_balance" >>= function
   *   | [ Some sum ] ->
   *     return sum
   *   | _ -> return 0L *)

  let h_activated_balances hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT balance FROM activation_balance WHERE pkh = $hash" >>= function
    | [ tez ] ->
      return tez
    | _ -> return 0L

  (* let activated_balances_at_level lvl =
   *   with_dbh >>> fun dbh ->
   *   PGSQL(dbh)
   *     "SELECT SUM(balance)::bigint FROM activation_balance AS ab \
   *      INNER JOIN block_operation AS bo ON bo.operation_hash = ab.hash \
   *      INNER JOIN block AS b ON bo.block_hash = b.hash \
   *      WHERE b.level <= $lvl" >>= function
   *   | [ Some sum ] ->
   *      return Int64.(to_float @@ div sum Tezos_constants.Constants.tez_units)
   *   | _ -> return 0. *)

  let unfrozen_rewards cycle_limit =
    let b_reward = Tezos_constants.Constants.block_reward in
    let e_reward = Tezos_constants.Constants.endorsement_reward_coeff in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT SUM(array_sum(nb_prio,1) * $b_reward + \
       end_rewards_array($e_reward, cycle, nb_endorsement))::bigint \
       FROM cycle_count \
       WHERE cycle < $cycle_limit AND cycle > 6" >>= function
    | [ Some rewards ] -> return rewards
    | _ -> return 0L

  let h_unfrozen_rewards cycle_limit hash =
    let b_reward = Tezos_constants.Constants.block_reward in
    let e_reward = Tezos_constants.Constants.endorsement_reward_coeff in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT SUM(CASE WHEN cycle > 6 then array_sum(nb_baking,1) * $b_reward + \
       end_rewards_array($e_reward, cycle, nb_endorsement) ELSE 0 END)::bigint, \
       SUM(fees)::bigint FROM cycle_count_baker \
       WHERE cycle < $cycle_limit AND tz = $hash" >>= function
    | [ Some rewards, Some fees ] -> return (rewards, fees)
    | [ Some rewards, _ ] -> return (rewards, 0L)
    | [ _, Some fees ] -> return (0L, fees)
    | _ -> return (0L, 0L)

  let revelations cycle_limit =
    let r_reward = Tezos_constants.Constants.revelation_reward in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT SUM(nb_nonce)::bigint * $r_reward FROM cycle_count \
       WHERE cycle < $cycle_limit - 1::bigint" >>= begin function
      | [ Some rewards ] -> return rewards
      | _ -> return 0L end
    >>= fun revelations_rewards ->
    (* Missing a revelation will instantly warrant the burn of the rewards
       and fees for the level of the missing revelation *)
    let commitments =
      Int64.of_int Tezos_constants.Constants.blocks_between_revelations in
    PGSQL(dbh)
      "SELECT COUNT(b.fees), SUM(b.fees + $r_reward)::bigint FROM block AS b \
       LEFT JOIN seed_nonce_revelation AS s ON s.level = b.level \
       WHERE b.level > 0 AND b.level % $commitments = 0 \
       AND b.cycle < $cycle_limit AND s.hash IS NULL"
    >>= function
    | [ Some nb_missed, Some burn ] ->
      return (revelations_rewards, Int64.to_int nb_missed, burn)
    | _ -> return (revelations_rewards, 0, 0L)

  let h_revelations cycle_limit hash =
    let r_reward = Tezos_constants.Constants.revelation_reward in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT(s.level) * $r_reward FROM seed_nonce_revelation AS s \
       INNER JOIN block AS bl ON bl.level = s.level AND distance_level = 0 \
       INNER JOIN block_operation As bo ON bo.operation_hash = s.hash \
       INNER JOIN block AS bl2 ON bo.block_hash = bl2.hash \
       WHERE bl.cycle < $cycle_limit - 1::bigint AND bl2.baker = $hash"
    >>= begin function
      | [ Some rewards ] -> return rewards
      | _ -> return 0L end >>= fun revelations_rewards ->
    (* Missing a revelation will instantly warrant the burn of the rewards
       and fees for the level of the missing revelation.
       Since a baker has to reveal the nonce on the next cycle, we look
       for the missing revelations until current_cycle - 2 *)
    let commitments =
      Int64.of_int Tezos_constants.Constants.blocks_between_revelations in
    PGSQL(dbh)
      "SELECT COUNT(b.fees), SUM(b.fees + $r_reward)::bigint FROM block AS b \
       LEFT JOIN seed_nonce_revelation AS s ON s.level = b.level \
       WHERE b.level > 0 AND b.level % $commitments = 0 \
       AND cycle < $cycle_limit AND s.hash IS NULL AND b.baker = $hash"
    >>= function
    | [ Some nb_missed, Some burn ] ->
      return (revelations_rewards, Int64.to_int nb_missed, burn)
    | _ -> return (revelations_rewards, 0, 0L)

  let burned_ori_tez () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(burn_tez) FROM origination WHERE NOT failed"
    >>= function
    | [ Some count ] -> return @@ Int64.of_string count
    | _ -> return 0L

  let ori_tez hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(burn_tez) FROM origination WHERE NOT failed AND source = $hash"
    >>= begin function
      | [ Some count ] -> return @@ Int64.of_string count
      | _ -> return 0L
    end
    >>= fun burn ->
    PGSQL(dbh) "SELECT SUM(balance + fee) FROM origination WHERE NOT failed AND source = $hash"
    >>= begin function
      | [ Some count ] -> return @@ Int64.of_string count
      | _ -> return 0L
    end >>= fun send ->
    PGSQL(dbh) "SELECT SUM(fee) FROM origination WHERE source = $hash AND failed"
    >>= begin function
      | [ Some count ] -> return @@ Int64.of_string count
      | _ -> return 0L
    end >>= fun fees ->
    PGSQL(dbh) "SELECT SUM(balance) FROM origination WHERE NOT failed AND tz1 = $hash"
    >>= function
    | [ Some count ] -> return (burn, Int64.add fees send, Int64.of_string count)
    | _ -> return (burn, Int64.add fees send, 0L)

  (* let burned_ori_tez_at_level lvl =
   *   with_dbh >>> fun dbh ->
   *   PGSQL(dbh)
   *     "SELECT SUM(burn_tez) FROM origination AS o \
   *      INNER JOIN block_operation AS bo ON bo.operation_hash = o.hash \
   *      INNER JOIN block AS b ON bo.block_hash = b.hash \
   *      WHERE b.level <= $lvl AND NOT o.failed" >>= function
   *   | [ Some count ] ->
   *     return @@ (float_of_string count /.
   *                Int64.to_float Tezos_constants.Constants.tez_units)
   *   | _ -> return 0. *)

  let burned_tr_tez () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(burn_tez) FROM transaction WHERE NOT failed"
    >>= function
    | [ Some count ] -> return @@ Int64.of_string count
    | _ -> return 0L

  let tr_tez hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(burn_tez) FROM transaction WHERE NOT failed AND source = $hash"
    >>= begin function
      | [ Some count ] -> return @@ Int64.of_string count
      | _ -> return 0L
    end >>= fun burn ->
    PGSQL(dbh) "SELECT SUM(amount + fee) FROM transaction WHERE NOT failed AND source = $hash"
    >>= begin function
      | [ Some count ] -> return @@ Int64.of_string count
      | _ -> return 0L
    end >>= fun send ->
    PGSQL(dbh) "SELECT SUM(fee) FROM transaction WHERE source = $hash AND failed"
    >>= begin function
      | [ Some count ] -> return @@ Int64.of_string count
      | _ -> return 0L
    end >>= fun fees ->
    PGSQL(dbh) "SELECT SUM(amount) FROM transaction WHERE NOT failed AND destination = $hash"
    >>= function
    | [ Some count ] -> return (burn, Int64.add fees send, Int64.of_string count)
    | _ -> return (burn, Int64.add fees send, 0L)

  (* let burned_tr_tez_at_level lvl =
   *   with_dbh >>> fun dbh ->
   *   PGSQL(dbh)
   *     "SELECT SUM(burn_tez) FROM transaction AS o \
   *      INNER JOIN block_operation AS bo ON bo.operation_hash = o.hash \
   *      INNER JOIN block AS b ON bo.block_hash = b.hash \
   *      WHERE b.level <= $lvl AND NOT o.failed" >>= function
   *   | [ Some count ] ->
   *     return @@ (float_of_string count /.
   *                Int64.to_float Tezos_constants.Constants.tez_units)
   *   | _ -> return 0. *)

  let burned_double_baking cycle_limit =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT CAST(SUM(lost_rewards) AS bigint), \
                CAST(SUM(lost_deposit) AS bigint), \
                CAST(SUM(lost_fees) AS bigint) \
                FROM double_baking_evidence AS dbe \
                INNER JOIN header AS h ON h.id = dbe.header1 \
                INNER JOIN block AS bl ON bl.level = h.level \
                WHERE cycle < $cycle_limit" >>= begin function
      | [ Some lost_rewards, Some lost_deposit, Some lost_fees] ->
        return @@
        Int64.(Int64.abs
                 (add lost_rewards
                    (div (add lost_deposit lost_fees) 2L)))
      | _ -> return 0L
    end >>= fun burn ->
    PGSQL(dbh) "SELECT CAST(SUM(lost_deposit) AS bigint), \
                CAST(SUM(lost_fees) AS bigint) \
                FROM double_baking_evidence AS dbe \
                INNER JOIN header AS h ON h.id = dbe.header1 \
                INNER JOIN block AS bl ON bl.level = h.level \
                WHERE cycle >= $cycle_limit" >>= function
    | [ Some lost_deposit, Some lost_fees] ->
      return @@
      Int64.(sub burn (add lost_deposit lost_fees))
    | _ -> return burn

  let double_baking cycle_limit hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT CAST(SUM(CASE WHEN cycle < $cycle_limit THEN lost_rewards ELSE 0 END) AS bigint), \
                CAST(SUM(lost_deposit) AS bigint), \
                CAST(SUM(CASE WHEN cycle < $cycle_limit THEN lost_fees ELSE 0 END) AS bigint) \
                FROM double_baking_evidence AS dbe \
                INNER JOIN header AS h ON h.id = dbe.header1 \
                INNER JOIN block AS bl ON bl.level = h.level \
                WHERE accused = $hash"
    >>= begin function
      | [ Some lost_rewards, Some lost_deposit, Some lost_fees] ->
        return @@
        Int64.(Int64.abs
                 (add lost_rewards
                    (add lost_deposit lost_fees)))
      | _ -> return 0L
    end >>= fun burn ->
    PGSQL(dbh) "SELECT CAST(SUM(gain_rewards) AS bigint) \
                FROM double_baking_evidence AS dbe \
                INNER JOIN block_operation AS bo ON bo.operation_hash = dbe.hash \
                INNER JOIN block AS bl ON bl.hash = bo.block_hash \
                WHERE cycle < $cycle_limit AND denouncer = $hash"
    >>= function
    | [ Some gains] ->
      return (burn, gains)
    | _ -> return (burn, 0L)

  (* let burned_double_baking_at_level lvl =
   *   with_dbh >>> fun dbh ->
   *   PGSQL(dbh) "SELECT CAST(SUM(lost_rewards) AS bigint), \
   *               CAST(SUM(lost_deposit) AS bigint), \
   *               CAST(SUM(lost_fees) AS bigint) \
   *               FROM double_baking_evidence AS dbe \
   *               INNER JOIN block_operation AS bo ON bo.operation_hash = dbe.hash \
   *               INNER JOIN block AS b ON bo.block_hash = b.hash \
   *               WHERE b.level <= $lvl" >>= begin function
   *     | [ Some lost_rewards, Some lost_deposit, Some lost_fees] ->
   *       return @@
   *       (Int64.(to_float @@
   *               Int64.abs
   *                 (add lost_rewards (div (add lost_deposit lost_fees) 2L))) /.
   *        Int64.to_float Tezos_constants.Constants.tez_units)
   *     | _ -> return 0.
   *   end *)

  let del_tez hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(fee) FROM delegation WHERE source = $hash"
    >>= function
    | [ Some count ] -> return @@ Int64.of_string count
    | _ -> return 0L

  let rvl_tez hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT SUM(fee) FROM reveal WHERE source = $hash"
    >>= function
    | [ Some count ] -> return @@ Int64.of_string count
    | _ -> return 0L

  let supply () =
  let ico = Infos.api.api_config.conf_ico in
  let dls = ico.ico_company_tokens in
  let foundation = ico.ico_foundation_tokens in
  let early_bakers = ico.ico_early_tokens in
  let contributors = ico.ico_contributors_tokens in

    with_dbh >>> fun dbh ->
    let allowed_fork64 = Int64.of_int Tezos_constants.Constants.allowed_fork in
    PGSQL(dbh)
      "SELECT MAX(cycle) FROM block" >>= fun current_cycle ->
    let cycle_limit = match current_cycle with
      | [ Some c ] ->
        if c < allowed_fork64 then 0L
        else Int64.sub c allowed_fork64
      | _ -> assert false in

    unfrozen_rewards cycle_limit >>= fun unfrozen_rewards ->
    revelations cycle_limit >>=
    fun (revelation_rewards, missing_revelations, burned_tez_revelation) ->
    burned_ori_tez () >>= fun burned_tez_origination ->
    burned_tr_tez () >>= fun burned_tez_transaction ->
    burned_double_baking cycle_limit >>= fun burned_tez_double_baking ->

    let total_supply_ico = Ico_constants.total_supply_ico ico in
    let burned_total =
      Int64.(add
               burned_tez_revelation
               (add burned_tez_origination
                  (add burned_tez_transaction
                     burned_tez_double_baking))) in
    let current_circulating_supply =
      Int64.(add
               total_supply_ico
               (sub
                  (add unfrozen_rewards revelation_rewards)
                  burned_total)) in
    return
      { dls ; foundation ; early_bakers ; contributors ;
        unfrozen_rewards ;
        missing_revelations ;
        revelation_rewards ; burned_tez_revelation ;
        burned_tez_origination ; burned_tez_double_baking ;
        total_supply_ico ; current_circulating_supply  }

  let balance_break_down hash =
    with_dbh >>> fun dbh ->
    let allowed_fork64 = Int64.of_int Tezos_constants.Constants.allowed_fork in
    PGSQL(dbh)
      "SELECT MAX(cycle) FROM block" >>= fun current_cycle ->
    let cycle_limit = match current_cycle with
      | [ Some c ] ->
        if c < allowed_fork64 then 0L
        else Int64.sub c allowed_fork64
      | _ -> assert false in

    h_activated_balances hash >>= fun h_activated_balance ->
    h_unfrozen_rewards cycle_limit hash >>= fun (h_unfrozen_rewards, fees) ->
    h_revelations cycle_limit hash >>=
    fun (h_revelation_rewards, h_missing_revelations, h_burned_tez_revelation) ->
    ori_tez hash >>= fun (h_burned_tez_origination, h_tez_origination_send, h_tez_origination_recv) ->
    tr_tez hash >>= fun (h_burned_tez_transaction, h_tez_transaction_send, h_tez_transaction_recv) ->
    double_baking cycle_limit hash >>= fun (h_burned_tez_double_baking, h_tez_dbe_rewards) ->
    del_tez hash >>= fun h_del_fees ->
    rvl_tez hash >>= fun h_rvl_fees ->

    let plus_total =
      Int64.(add
               h_activated_balance
               (add
                  h_unfrozen_rewards
                  (add
                     fees
                     (add
                        h_revelation_rewards
                        (add
                           h_tez_origination_recv
                           (add
                              h_tez_transaction_recv
                              h_tez_dbe_rewards)))))) in
    let minus_total =
      Int64.(add
               h_burned_tez_revelation
               (add
                  h_tez_origination_send
                  (add
                     h_burned_tez_origination
                     (add
                        h_burned_tez_transaction
                        (add
                           h_tez_transaction_send
                           (add
                              h_del_fees
                              (add
                                 h_rvl_fees
                                 h_burned_tez_double_baking))))))) in

    let h_total = Int64.sub plus_total minus_total in

    return
      {
        h_activated_balance ;
        h_unfrozen_rewards ;
        h_revelation_rewards ;
        h_missing_revelations ;
        h_burned_tez_revelation ;
        h_burned_tez_origination ;
        h_tez_origination_recv ;
        h_tez_origination_send ;
        h_burned_tez_transaction ;
        h_tez_transaction_recv ;
        h_tez_transaction_send ;
        h_burned_tez_double_baking ;
        h_tez_dbe_rewards ;
        h_total ;
      }

  let h24_stats () =
    (* To speed up the query a bit *)
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT COUNT( DISTINCT b.hash ), \
       SUM(CASE WHEN b.priority = 0 THEN 1 else 0 END) \
       FROM block AS b \
       WHERE timestamp >= NOW() - '24 hours'::INTERVAL AND \
       distance_level = 0"
    >>= fun blocks_res ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM transaction_all \
       WHERE timestamp_op >= NOW() - '24 hours'::INTERVAL \
       AND distance_level = 0"
    >>= of_count_opt >>= fun tr_count ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM origination AS t \
       INNER JOIN operation AS o ON o.hash = t.hash \
       WHERE o.timestamp >= NOW() - '24 hours'::INTERVAL"
    >>= of_count_opt >>= fun or_count ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM delegation AS t \
       INNER JOIN operation AS o ON o.hash = t.hash \
       WHERE o.timestamp >= NOW() - '24 hours'::INTERVAL"
    >>= of_count_opt >>= fun del_count ->
    PGSQL(dbh)
      "SELECT COUNT(*) FROM activation AS t \
       INNER JOIN operation AS o ON o.hash = t.hash \
       WHERE o.timestamp >= NOW() - '24 hours'::INTERVAL"
    >>= of_count_opt >>= fun act_count ->
    PGSQL(dbh)
      "SELECT SUM(array_length(slots, 1)) \
       FROM endorsement_all WHERE timestamp >= NOW() - '24 hours'::INTERVAL \
       AND distance_level = 0"
    >>= of_count_opt >>= fun end_count ->
    let blocks, blocks0 = match blocks_res with
      | [ Some blocks_count, Some blocks0_count ] ->
        Int64.to_int blocks_count, Int64.to_int blocks0_count
      | _ -> 0, 0 in
    let h24_max_end = blocks * Tezos_constants.Constants.endorsers_per_block in
    let h24_end_rate = (float end_count) /. (float h24_max_end) *. 100. in
    let h24_block_0_rate = (float blocks0) /. (float blocks) *. 100. in
    PGSQL(dbh) "SELECT staking_balances FROM context_totals \
                      ORDER BY period DESC LIMIT 1"
    >>= begin function
      | [ Some result ] -> return result
      | _ -> return 0L end
    >>= fun h24_baking_rate ->
    begin PGSQL(dbh) "select cycle,index from snapshot_rolls \
                WHERE cycle = (SELECT MAX(cycle) FROM snapshot_rolls)"
    >>= function
    | [ cycle, index ] -> return (Int64.to_int cycle, Int32.to_int index)
    | _ -> return (0, 0) end >>= fun (cycle, _index) ->
    PGSQL(dbh)
      "SELECT current_balances + frozen_fees + frozen_deposits \
       FROM context_totals ORDER BY period DESC LIMIT 1" >>= begin function
      | [ Some supply ] ->
        return (Int64.to_float supply /.
                (Int64.to_float Tezos_constants.Constants.tez_units))
      | _ -> return 0. end >>= fun circulating_supply ->
    let h24_baking_rate =
      if circulating_supply = 0. then 0. else
        ((Int64.to_float h24_baking_rate /.
          Int64.to_float Tezos_constants.Constants.tez_units) /.
         circulating_supply) *. 100. in
    let cycle64 = Int64.of_int cycle in
    PGSQL(dbh)
      "SELECT COUNT(DISTINCT tz1) FROM snapshot_owner AS so \
       INNER JOIN snapshot_rolls AS sr ON sr.id = so.id \
       WHERE sr.cycle = $cycle64"
    >>= of_count_opt >>= fun h24_active_baker ->
    return { h24_end_rate ;
             h24_block_0_rate ;
             h24_transactions = tr_count ;
             h24_originations = or_count ;
             h24_delegations = del_count ;
             h24_activations = act_count ;
             h24_baking_rate ;
             h24_active_baker }

  let crawler_activity () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT * FROM crawler_activity" >>= fun list ->
    return @@
    List.map (fun (crawler_name, crawler_timestamp, crawler_delay ) ->
        let crawler_delay = Int32.to_int crawler_delay in
        { crawler_name ; crawler_timestamp ; crawler_delay })
      list

  let volume_per_day () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT EXTRACT(year FROM timestamp) * 10000 + \
       EXTRACT(month FROM timestamp) * 100 + \
       EXTRACT(day FROM timestamp) AS d, SUM(volume)::bigint FROM block \
       WHERE volume <> 0 and distance_level = 0\
       GROUP BY d ORDER BY d DESC"
    >>= fun rows ->
    return @@ List.fold_left (fun acc (day, volume) ->
        match (day, volume) with
        | Some day, Some volume -> (int_of_float day, volume) :: acc
        | _ -> acc) [] rows

  let alias account_hash =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT alias FROM user_alias WHERE tz = $account_hash"
    >>= function
    | [ ] -> return None
    | alias :: _  -> return (Some alias)

  let account_from_alias alias =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT tz FROM user_alias WHERE alias = $alias"
    >>= function
    | [] -> return None
    | account_hash :: _ -> return (Some account_hash)

  let all_aliases () =
    with_dbh >>> fun dbh -> PGSQL(dbh) "SELECT tz, alias FROM user_alias"

  let nb_protocol () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT COUNT(*) FROM protocol" >>= of_count_opt

  let protocols ?(page=0) ?(page_size=20) () =
    let offset = Int64.of_int (page * page_size)
    and limit = Int64.of_int page_size in
    with_dbh >>> fun dbh ->
    PGSQL(dbh)
      "SELECT p.name, b.protocol, MIN(b.level) as block_start, MAX(b.level) FROM block as b \
       INNER JOIN protocol as p ON b.protocol = p.hash \
       GROUP by b.protocol, p.name ORDER BY block_start DESC OFFSET $offset LIMIT $limit"
    >>= fun rows ->
    let n = List.length rows in
    return @@ List.rev @@ snd @@ List.fold_left (fun (i, acc) row -> match row with
        | (prt_name, prt_hash, Some bl_start, Some bl_end) ->
          let prt_index = page * page_size + n - i - 2 in
          let prt_end = if i = 0 && page = 0 then (-1) else Int64.to_int bl_end in
          i + 1, {prt_index; prt_hash; prt_name;
                  prt_start = Int64.to_int bl_start; prt_end} :: acc
        | _ -> i, acc) (0, []) rows

  let market_prices () =
    with_dbh >>> fun dbh ->
    PGSQL(dbh) "SELECT last_updated/3600 as time, avg(float4(price_usd)), \
                avg(float4(price_btc)) FROM marketcap \
                WHERE valid \
                GROUP BY time ORDER BY time DESC"
    >>= fun rows ->
    return @@ List.fold_left (fun acc row -> match row with
        | (Some seconds, Some usd, Some btc) ->
          let tsp = CalendarLib.Calendar.from_unixfloat
              (Int64.to_float seconds *. 3600.) in
          (CalendarLib.Printer.Calendar.sprint "%Y-%m-%dT%H:00:00Z" tsp,
           [|"price_usd", usd; "price_btc", btc |]) :: acc
        | _ -> acc) [] rows
end
