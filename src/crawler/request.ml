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
open EzAPI.TYPES

let spf = Printf.sprintf

let chain_id = "main"
let root = spf "/chains/%s" chain_id
let head = "head"
let genesis = "genesis"

let debug fmt = Utils.debug !Debug_constants.debug_request fmt

let request ~post ?(data="{}") url path =
  let url = Printf.sprintf "%s:%d%s" url.address url.port path in
  (* a hack because EzCurl to keep the requests synchronous to avoid
     changing too much code.*)
  let result = ref None in
  let on_success r = result := Some r in
  try
    begin
      let url = URL url in
      if post then EzCurl.post ~content:data "" url on_success
      else EzCurl.get "" url on_success
    end;
    match !result with
    | Some json -> json
    | None -> failwith url
  with Curl.CurlException (_code, i, s) as exn->
    Printf.eprintf "[Request] [%d] %s Curl exception: %s\n%!"
      i url s ;
    raise exn

let cachable = true

let request ?(post=false) ~cachable url path =
  Cache.with_cache ~debug:"crawler" ~cachable ~key:path ~request:(fun () ->
      request ~post ~data:"{}" url path)

(* We cache baking_rights and other ones even if block=head, because
   they are not supposed to change between two different heads. *)

let level url block =
  let path = spf "%s/blocks/%s/helpers/current_level" root block in
  debug "[Request] level %s\n%!" path ;
  EzEncoding.destruct Tezos_encoding.Encoding.Level.encoding @@
  request ~cachable:(block <> head) url path


let operations url block =
  let path = spf "%s/blocks/%s/operations" root block in
  debug"[Request] operations %s\n%!" path ;
  EzEncoding.destruct Tezos_encoding.Encoding.Operation.encoding @@
  request ~cachable:(block <> head) url path

let pending_operations url =
  let path =
    match Tezos_constants.net with
    | Tezos_constants.Zeronet ->  spf "%s/mempool/pending_operations" root
    | _ -> spf "%s/mempool" root in
  debug "[Request] pending_ops %s\n%!" path;
  EzEncoding.destruct
    Tezos_encoding.Encoding.Operation.pending_operation @@
  request ~cachable:false url path

let predecessor url block =
  (* Should be this RPC, it is not registered properly in the node *)
  (* let path = spf "%s/blocks/%s/header/shell/predecessor" root hash in *)
  let path = spf "%s/blocks/%s/header/shell" root block in
  debug "[Request] predecessor %s\n%!" path ;
  EzEncoding.destruct Tezos_encoding.Encoding.Predecessor.encoding @@
  request ~cachable:(block <> head) url path

let block url hash =
  let path = spf "%s/blocks/%s" root hash in
  debug "[Request] block %s\n%!" path ;
  let buf = request ~cachable:(hash <> head) url path in
  try
    EzEncoding.destruct Tezos_encoding.Encoding.Block.encoding buf
  with EzEncoding.DestructError ->
    EzEncoding.destruct Tezos_encoding.Encoding.Block.genesis_encoding buf

let genesis url =
  let path = spf "%s/blocks/%s" root genesis in
  debug "[Request] genesis %s\n%!" path ;
  EzEncoding.destruct Tezos_encoding.Encoding.Block.genesis_encoding @@
  request ~cachable url path

let get_head_hash ?(block=head) url =
  let path = spf "%s/blocks/%s/hash" root block in
  (* HACK !!!!  *)
  let block_hash = request ~cachable:(block <> head) url path in
  String.sub block_hash 1 (String.length block_hash - 3)

let get_alternative_heads_hashes url =
  let path = spf "%s/blocks" root in
  EzEncoding.destruct
    Tezos_encoding.Encoding.Alternative_blocks.encoding @@
  request ~cachable:false url path

(* Shortcuts on [head] *)
let current_level ?(block=head) url = level url block
let head_block ?(block_hash=head) url = block url block_hash
          
let node_balance url block hash =
  let path =
    spf
      "%s/blocks/%s/context/contracts/%s" root block hash in
  (* debug "[crawler] node_balance %s\n%!" path ; *)
  EzEncoding.destruct
    Api_encoding.V1.Account_details.node_encoding @@
  request ~cachable:(block <> head) url path
                                    
let node_contracts url bhash : (string * int64) list =  
  let contracts = 
    let path =
      spf
        "%s/blocks/%s/context/contracts/" root bhash in
    EzEncoding.destruct
      Tezos_encoding.Encoding.Contracts.encoding @@ request ~cachable:(bhash<>head) url path
  in
  List.map
    (fun contract ->
       let (_,b,_,_,_,_,_) = node_balance url bhash contract in contract,b)
    contracts

let db_balance url account =
  let path= spf "/v1/balance_from_balance_updates/%s" account in
  EzEncoding.destruct (Api_encoding.V1.Balance.encoding) @@
    request ~cachable:false url path
