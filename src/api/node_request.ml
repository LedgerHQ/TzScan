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

open StringCompat (* for StringMap *)
open Tezos_types

let () =
  Lwt.async_exception_hook :=
    function exn ->
      Printf.eprintf "\nServer Error: %s\n\n%!" (Printexc.to_string exn)

let request title url ~error f =
  EzCohttp.get title ~error:(fun code _content -> error code)
    (EzAPI.TYPES.URL url) f

let request_enc title url ~error enc f =
  request title url ~error
    (fun json ->
       match EzEncoding.destruct enc json with
       | exception _exn -> error (-1)
       | res -> f res
    )

type node_state = {
  mutable current : string option ;
  mutable next_req : float ;
  mutable requested : bool ;
  mutable waiting : (string option -> unit) list;
}

let node_states = ref StringMap.empty

(* Query a node for the timestamp of the last block. Do it at most
   once per minute. We don't need to be more accurate, as the interface
   displays a green light if we are less that 9 minutes late.
*)
let get_timestamp address f =
  let time = EzAPIServer.req_time () in
  let state =
    try
      StringMap.find address !node_states
    with Not_found ->
      let state = {
        current = None ;
        next_req = 0. ;
        requested = false ;
        waiting = [] ;
      } in
      node_states := StringMap.add address state !node_states;
      state
  in
  if state.next_req < time then begin
    state.waiting <- f :: state.waiting;
    if not state.requested then
      let url = Printf.sprintf "%s/chains/main/blocks/head" address in
      let received current =
        state.requested <- false;
        state.current <- current;
        let waiting = state.waiting in
        state.waiting <- [];
        List.iter (fun f ->
            try f current with _ -> ()) waiting
      in
      state.requested <- true ;
      state.next_req <- time +. 60.;
      try
      request_enc "Node.timestamp" url
        ~error:(fun _ ->
            received None)
        Tezos_encoding.Encoding.Block.encoding
        (fun block ->
           let timestamp = Date.to_string block.node_header.header_timestamp in
           received (Some timestamp)
        )
      with exn ->
        Printf.eprintf "Exception %S\n%!" (Printexc.to_string exn)
  end else begin
    f state.current
  end

let timestamps () =
  let waiter, notifier = Lwt.wait () in
  let rec iter list res =
    match list with
    | [] ->
      Printf.eprintf "wakeup\n%!";
      Lwt.wakeup notifier res
    | (kind, address) :: list ->
      get_timestamp address
        (fun current ->
           Printf.eprintf "current\n%!";
           iter list ( (kind, current) :: res ))
  in
  iter (Config.get_addresses()) [];
  waiter

let with_timestamped_address f =
  let address = Config.get_api_address () in
  get_timestamp address
    (fun current -> f address current)

let account_dec hash ~error f =
  with_timestamped_address (fun address timestamp ->
      let url =
        Printf.sprintf "%s/chains/main/blocks/head/context/contracts/%s"
          address hash in
      request_enc "Node.account" url ~error
        Api_encoding.V1.Account_details.node_encoding
        (fun res -> f (timestamp, res)))

let delegate_details hash ~error f =
  let address = Config.get_api_address () in
  let url =
    Printf.sprintf
      "%s/chains/main/blocks/head/context/delegates/%s"
      address hash in
  Printf.eprintf "delegate_details %s\n%!" url ;
  request_enc "Node.delegate_details" url ~error
    Tezos_encoding.Encoding.Delegate.encoding f

exception HttpErrorCode of int

let to_lwt f arg =
  let waiter, notifier = Lwt.wait () in
  f arg ~error:(fun code ->
      Lwt.wakeup_exn notifier (HttpErrorCode code)
    )
    (fun x -> Lwt.wakeup notifier x);
  waiter
