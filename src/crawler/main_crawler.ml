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

open StringCompat
open Data_types
open Tezos_types
open Options

let rec register_genesis config =
  let url = choose_node config in
  try
    let genesis = Request.genesis url in
    if not @@ Dbw.is_block_registered genesis.node_hash then begin
      Dbw.register_genesis genesis ;
      let operations = List.flatten @@ Request.operations url genesis.node_hash in
      Dbw.register_operations genesis operations
    end
  with
  (* Try with another node, maybe the node is to busy *)
  | EzEncoding.DestructError -> register_genesis config
  (* Try with another node, this one not responding *)
  | Curl.CurlException _ ->
    (* Try to remove locally this node, maybe it will respond later *)
    let nodes =
      List.filter (fun { address ; port = _ } -> address <> url.address)
        config.nodes in
    register_genesis { config with nodes }
  | Failure msg ->
    Printf.eprintf "[Crawler] Catch failure %S\n%!" msg ;
    let nodes =
      List.filter (fun { address ; port = _ } -> address <> url.address)
        config.nodes in
    register_genesis { config with nodes }

let rec register main config hash =
  let predecessor_hash = Request.predecessor config hash in
  if !quit then exit 1 ;
  begin
    if not @@ Dbw.is_block_registered predecessor_hash then begin
      debug "[Crawler] Found block %s\n%!" predecessor_hash ;
      register main config predecessor_hash
    end
  end ;
  if !quit then exit 1 ;
  if not @@ Dbw.is_block_registered hash then begin
    (* Register head
       - register protocol
       - register test protocol
       - register block
       - register detailed operations (endorsement, transaction, etc) *)
    let level = Request.level config hash in
    debug "[Crawler] Found level %d\n%!" level.node_lvl_level ;
    let block = Request.block config hash in
    let date = block.node_header.header_timestamp in
    let operations = List.flatten @@ block.node_operations in
    debug "[Crawler] Found operations %d\n%!" @@ List.length operations ;
    let t1 = Unix.gettimeofday () in
    debug "[Crawler] [%d] Registering block %s\n%!" block.node_header.header_level hash ;
    begin
      if level.node_lvl_level = 1
      then
        let first_contracts = Request.node_contracts config hash in
        List.iter
          (fun (h,b) ->
            Dbw.register_init_balance
              h
              b
              date
              level.node_lvl_level)
          first_contracts
    end;
    Dbw.register_all block level operations ;
    if main then
      Dbw.register_main_chain !count block;
    let t2 = Unix.gettimeofday () in
    debug "[Crawler] [%d] Ok in %fs\n%!" block.node_header.header_level (t2 -. t1)
  end

let make_catchup_levels url hash level =
  let result = ref [] in
  let nb_steps = level / catchup_step in
  (* Computing max_steps by looking at the last registered block in the db *)
  let max_steps =
    match Dbw.head () with
    | None -> nb_steps
    | Some block -> (level - block.level) / catchup_step in
  for i = 0 to max_steps do
    let i = i * catchup_step in
    let block = Request.block url (Printf.sprintf "%s~%d" hash i) in
    result := (block.node_hash, block.node_header.header_level) :: !result
  done ;
  !result

let main_chain config =
  let url = choose_node config in
  try
    let register_or_not ?(main = false) url hash =
      if not @@ Dbw.is_block_registered hash then begin
        (* New block found ! *)
        let node_level = Request.level url hash in
        let current_level = node_level.node_lvl_level in
        let catchup_levels = make_catchup_levels in
        List.iter (fun (hash, level) ->
            debug "[Crawler] ========= \
                   Catching block %s at level %d / %d \
                   =========\n%!" hash level current_level;
            register main url hash)
          (catchup_levels url hash current_level)
      end
      (* Now we are up to date, we can register alt heads *)
      else
        alternative_heads_flag := true in
    (* Try to register the current head (if predecessor doesn't exist,
       try to get it first recursively). If until_mode is enabled, use
       the hash given in CLI as new head. *)
    let head_hash =
      match !new_head with
      | None ->
        Request.get_head_hash ?block:(!new_head) url
      | Some hash -> hash in
    (* Wait to be up to date before registering alt head (Cf. issue
       when catching new chain from scratch) *)
    alternative_heads_flag := false ;
    register_or_not ~main:true url head_hash ;
    debug "[Crawler] Registered block %s\n%!" head_hash ;
    if !until_mode then exit 1 ;

    if !alternative_heads_flag then begin
      (* Registering alternative heads and operations in their branches *)
      match Request.get_alternative_heads_hashes url with
      | [] -> ()
      | _ :: heads -> (* Ignore the first block *)
        debug "[Crawler] Found heads %d\n%!" @@ List.length heads ;
        List.iter (register_or_not url) @@ List.flatten heads ;
        debug "[Crawler] Registered heads\n%!" ;
    end;

    if !pending_operations_flag then begin
      (* Registering pending operations *)
      try
        let pending_operations = Request.pending_operations url in
        debug "[Crawler] Found pending_operations %d\n%!" @@
        List.length pending_operations.applied ;
        Dbw.register_pending
          (CalendarLib.Calendar.now ())
          pending_operations.applied
      with exn ->
        Printf.eprintf
          "[Crawler] Something went wrong when \
           registering the pending operations\n  %S\n%!"
          (Printexc.to_string exn)
    end;
    Dbw.register_crawler_activity "chain" sleep

  with
  (* Try next time, maybe the node is broken, or we need to edit
     Data_encoding with the broken json *)
  | EzEncoding.DestructError -> assert false
  (* Try next time with another node (maybe this one timeout, or is
     just unavailable. *)
  | Curl.CurlException _ -> ()
  (* Something goes wrong with the current node, we should definitely
     do something, but for now try with another node. *)
  | Failure msg -> Printf.eprintf "[Crawler] Catch failure %S\n%!" msg


let main_msg = "main operations"

let main_init () =
  if !count && (!start_level_count <> -1) then
    Dbw.counts_downup !start_level_count !end_level_count;
  register_genesis @@ get_crawler ()


let init () =
  crawlers := StringMap.add
      "main" (main_init, main_chain, sleep, main_msg) !crawlers
