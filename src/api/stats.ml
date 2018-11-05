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

open EzAPI.TYPES
open EzAPIServer

 (* All services must be defined at this point. We work by side-effects,
    using service ids. *)
let () = Service.init ()

module StringSet = Set.Make(String)

let rec list_split4 = function
    [] -> ([],[],[],[])
  | (x1,x2,x3,x4)::l ->
     let (rx1, rx2, rx3, rx4) = list_split4 l
     in
     (x1::rx1, x2::rx2, x3::rx3, x4::rx4)

open Tezos_types
open Data_types
open Db_intf

module M = Dbr.Monad

let (>>=) = M.(>>=)

type stats = {
    stats_level : int;
    stats_time0 : float;
    stats_time1 : float;
    stats_name_per_day : string array;
    stats_nblocks_per_day : int array; (* ndays entries *)
    stats_nbakers_per_day : int array; (* ndays entries *)
    stats_priorities_per_day : float array; (* ndays entries *)
    stats_nops_per_day : int array;
    stats_nops_per_block_per_day : int array;
    stats_fees_per_day : int64 array;
    stats_volume_per_day : int64 array;
  }

type account_info = {
    ai_address : string;
    mutable ai_baked : int; (* number of baked blocks *)
  }

type block_info = {
    mutable bi_prev : block_info option;
    bi_level : int;
    bi_hash : block_hash;
    bi_time : float;
    bi_baker : account_info;
    bi_priority : int;
    bi_nb_operations : int;

    (* not yet filled *)
    bi_nb_endorsements : int;
    bi_nb_transactions : int;
    bi_nb_originations : int;
    bi_volume : int64;
    bi_fees : int64;
  }

let last_head = ref {
                    bi_prev = None;
                    bi_level = 0;
                    bi_hash = "";
                    bi_time = 0.;
                    bi_baker = { ai_address = ""; ai_baked = 0 };
                    bi_nb_operations = 0;
                    bi_nb_endorsements = 0;
                    bi_nb_originations = 0;
                    bi_nb_transactions = 0;
                    bi_volume = 0L;
                    bi_fees = 0L;
                    bi_priority = 0;
                  }
let account_by_hash = Hashtbl.create 1000
let block_by_level = Hashtbl.create 1000
let block_by_hash = Hashtbl.create 1000
let current_stats = ref {
                        stats_level = -1;
                        stats_time0 = 0.;
                        stats_time1 = 0.;
                        stats_name_per_day = [||];
                        stats_nblocks_per_day = [||];
                        stats_nbakers_per_day = [||];
                        stats_priorities_per_day = [||];
                        stats_nops_per_day = [||];
                        stats_nops_per_block_per_day = [||];
                        stats_volume_per_day = [||];
                        stats_fees_per_day = [||];
                      }




(* recompute stats every 60 minutes at most *)
let recompute_stats () =
  Printf.eprintf "Computing statistics\n%!";
  let level = (!last_head).bi_level in

  if level > (!current_stats).stats_level + 60 then
    let time0 = (try
                   Hashtbl.find block_by_level 1
                 with Not_found ->
                   Hashtbl.find block_by_level 0
                ).bi_time in
    let time1 = (!last_head).bi_time in
    let nsecs = int_of_float (time1 -. time0) in (* since blockchain launch *)
    let ndays = nsecs / GMTime.nsecs_per_day in

    let name_per_day =
      Array.init ndays (fun day ->
                   let time = time0 +. float_of_int (day * GMTime.nsecs_per_day) in
                   let tm = GMTime.tm_of_time time in
                   Printf.sprintf "%04d/%02d/%02d"
                                  (tm.Unix.tm_year+1900)
                                  (tm.Unix.tm_mon+1)
                                  tm.Unix.tm_mday
                 ) in

    let nblocks_per_day = Array.make ndays 0 in
    let bakers_per_day = Array.make ndays StringSet.empty in
    let priorities_per_day = Array.make ndays 0. in
    let volume_per_day = Array.make ndays 0L in
    let fees_per_day = Array.make ndays 0L in
    let nops_per_day = Array.make ndays 0 in
    for i = 1 to level do
      try
        let bi = Hashtbl.find block_by_level i in
        let day = int_of_float (bi.bi_time -. time0) / GMTime.nsecs_per_day in
        if day < ndays then begin
            nblocks_per_day.(day) <- nblocks_per_day.(day) + 1;
            priorities_per_day.(day) <- priorities_per_day.(day) +.
                                          float_of_int bi.bi_priority;
            fees_per_day.(day) <- Int64.add fees_per_day.(day) bi.bi_fees;
            volume_per_day.(day) <- Int64.add volume_per_day.(day) bi.bi_volume;
            bakers_per_day.(day) <-
              StringSet.add bi.bi_baker.ai_address bakers_per_day.(day);
            nops_per_day.(day) <- nops_per_day.(day) + bi.bi_nb_operations
          end
      with Not_found ->
        Printf.eprintf "Stats.recompute_stats: block at level %d not found\n%!"
                       i
    done;

    let nbakers_per_day = Array.map StringSet.cardinal bakers_per_day in

    for day = 0 to ndays -1 do
      if nblocks_per_day.(day) <> 0 then
        priorities_per_day.(day) <- priorities_per_day.(day) /.
                                      float_of_int nblocks_per_day.(day)
    done;

    let nops_per_block_per_day =
      Array.mapi (fun day nops ->
          if nops <> 0 then
            nops / nblocks_per_day.(day)
          else 0
        ) nops_per_day in


    current_stats := {
        stats_level = level;
        stats_time0 = time0;
        stats_time1 = time1;
        stats_name_per_day = name_per_day;
        stats_nblocks_per_day = nblocks_per_day;
        stats_priorities_per_day = priorities_per_day;
        stats_nbakers_per_day = nbakers_per_day;
        stats_nops_per_day = nops_per_day;
        stats_nops_per_block_per_day = nops_per_block_per_day;
        stats_volume_per_day = volume_per_day;
        stats_fees_per_day = fees_per_day;
      }

let add_baker ai_address =
  try
    let ai = Hashtbl.find account_by_hash ai_address in
    ai.ai_baked <- ai.ai_baked + 1;
    ai
  with Not_found ->
       let ai = {
           ai_address;
           ai_baked = 1;
         } in
       Hashtbl.add account_by_hash ai_address ai;
       ai

(*
let remove_baker ai_address =
  try
    let ai = Hashtbl.find account_by_hash ai_address in
    ai.ai_baked <- ai.ai_baked - 1
  with Not_found -> () (* weird *)
*)

let insert b =
  let bi_prev =
    let prev_level = b.level -1 in
    try
      Some (Hashtbl.find block_by_level prev_level)
    with Not_found ->
      Printf.eprintf "Warning: no block at level %d\n%!" prev_level;
      None
  in
  let bi = {
      bi_prev;
      bi_level = b.level;
      bi_hash = b.hash;
      bi_time =
        b.timestamp |>
        Date.to_string |>
        GMTime.tm_of_date |>
        GMTime.time_of_tm;
      bi_priority = b.priority;
      bi_baker = add_baker b.baker.tz;
      bi_nb_operations = b.nb_operations;

      bi_nb_endorsements = 0;
      bi_nb_transactions = 0;
      bi_nb_originations = 0;
      bi_volume = b.volume;
      bi_fees = b.fees;
    } in
  Hashtbl.add block_by_level b.level bi;
  Hashtbl.add block_by_hash b.hash bi;
  bi

(* levels before this value have been completely computed *)
let done_level = ref 0

(*  <--- trop lent pour l'instant
let update_transactions _by =
  let page_size = 10000 in
  let rec iter page =
    DBR.operations ~page ~page_size Empty >>=
      function ops ->
               Printf.eprintf "[%d]%!" (List.length ops);
               match ops with
               | [] -> M.return ()
               | _ ->
                  iter (page + 1)
  in
  iter 0
 *)

let update_transactions _by = M.return ()


let next_update = ref 0.
let update ?(by = 20) () =
  Printf.eprintf "Updating statistics ?\n%!";

  if EzAPIServer.req_time() < !next_update then
    M.return ()
  else
    let next = EzAPIServer.req_time() +. 120. in
    next_update := next; (* no check for 2 minutes *)
    Dbr.head () >>= function
    | None -> assert false
    | Some head ->
       if (*head.hash <> (!last_head).bi_hash && *)
         head.level > (!last_head).bi_level + 10
       then begin
           Printf.eprintf "Updating statistics data\n%!";
           let todo = ref [] in
           let page_size = by in

           let finish_update () =
             begin try
                 List.iter (fun bs ->
                     List.iter (fun b ->
                         if b.level < !done_level then done_level := b.level;
                         begin
                           try
                             let bi = Hashtbl.find block_by_level b.level in
                             Hashtbl.remove block_by_level b.level;
                             Hashtbl.remove block_by_hash bi.bi_hash;
                             bi.bi_baker.ai_baked <- bi.bi_baker.ai_baked - 1
                           with Not_found -> ()
                         end;
                         let (bi : block_info) = insert b in
                         (* Printf.eprintf "[%d]%!" b.level; *)
                         last_head := bi
                       ) (List.rev bs)
                   ) !todo;
               with exn ->
                 Printf.eprintf
                   "Warning: caught exception %S in Stats.finish_update\n%!"
                   (Printexc.to_string exn)
             end;
             update_transactions by
             >>= function () ->
                          M.return (recompute_stats ())
           in

           let rec iter page =
             Dbr.blocks ~page ~page_size () >>= function
             |  [] -> finish_update ()
             | (b :: _) as bs ->
                let continue =
                  try
                    let bi = Hashtbl.find block_by_level b.level in
                    bi.bi_hash <> b.hash
                  with Not_found -> true
                in
                if continue then begin
                    todo := bs :: !todo;

                    iter (page+1)
                  end else
                  finish_update ()
           in
           iter 0
         end else

         M.return ()

let init () =
  Printf.eprintf "Initializing statistics\n%!";
  Dbr.block (Level 0) >>= function
  | None -> assert false
  | Some genesis ->
     last_head := insert genesis;
     update ~by:5000 ()

let stats () = !current_stats


let services = EzAPI.services ()
let nservices = Array.length services

let clients with_ip =
  let clients = ref [] in
  Hashtbl.iter (fun _ ip ->
      clients := { ip with
                   ip_ip = (if with_ip then ip.ip_ip else "---");
                   ip_last = EzAPIServer.req_time() -. ip.ip_last } :: !clients
    ) EzAPIServer.req_ips;
  Array.of_list !clients

open Timings

type period = T_MINS | T_HOURS | T_DAYS
let timing_stats period =
  let ts = EzAPIServer.timings in
  let t1 = EzAPIServer.req_time() in
  let update = Timings.get t1 in
  let timing_period = match period with
    | T_MINS -> "Minute"
    | T_HOURS -> "Hour"
    | T_DAYS -> "Day"
  in
  let timing_services =
  Array.init nservices (fun i ->
               let ok = update ts.timings_ok.(i) in
               let fail = update ts.timings_fail.(i) in
               let ok, fail =
                 match period with
                 | T_MINS -> ok.minutes, fail.minutes
                 | T_HOURS -> ok.hours, fail.hours
                 | T_DAYS -> ok.days, fail.days
               in
               { service_name = Printf.sprintf "[%d] : %s" i services.(i);
                 service_ok_days_nb = ok.nb;
                 service_ok_days_dt = ok.dt;
                 service_fail_days_nb = fail.nb;
                 service_fail_days_dt = fail.dt;
               }
             )
  in
  let timing_uptime = GMTime.date_of_tm (GMTime.tm_of_time t0) in
  { timing_uptime;
    timing_period;
    timing_services;
  }

let mini_stats () =
  let t1 = GMTime.time () in
  let head = !last_head in
  let rec iter t0 bi ((nblocks, nops, volume, fees) as v)  periods rem =
    if bi.bi_time > t0 then
      iter2 t0 bi (nblocks+1,
                   nops+bi.bi_nb_operations,
                   Int64.add volume bi.bi_volume,
                   (Int64.add fees bi.bi_fees))  periods rem
    else
      let rem = v :: rem in
      match periods with
      | [] -> List.rev rem
      | p :: periods ->
         let t0 = t1 -. p in
         iter t0 bi v periods rem

  and iter2 t0 bi v  periods rem =
    match bi.bi_prev with
    | Some bi ->
       iter t0 bi v periods rem
    | None ->
       let rem = v :: rem in
       match periods with
       | [] -> List.rev rem
       | p :: periods ->
          let t0 = t1 -. p in
          iter2 t0 bi v  periods rem
  in
  let res =
  iter (t1 -. 3600.) head (0, 0, 0L, 0L)
       [
         3600. *. 6.; (* 6 hours *)
         3600. *. 12.; (* 12 hours *)
         3600. *. 24.; (* 1 day *)
         3600. *. 24. *. 7.; (* 7 days *)
         3600. *. 24. *. 30.; (* 1 month *)
       ] []
  in
  let ms_period = [|
      "1 hour";
      "6 hours";
      "12 hours";
      "1 day";
      "7 days";
      "1 month";
    |]
  in
  let ms_nhours = [|
      1;
      6;
      12;
      24;
      7 * 24;
      30 * 24;
    |]
  in
  let (nblocks, nops, volume, fees) = list_split4 res in
  {
    ms_period;
    ms_nhours;
    ms_nblocks = Array.of_list nblocks;
    ms_nops = Array.of_list nops;
    ms_volume = Array.of_list volume;
    ms_fees = Array.of_list fees;
  }
