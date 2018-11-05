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

let exec ?(verbose=true) dbh ?callback query =
  try
    if verbose then
      Printf.eprintf "exec: %s\n%!" query;
    ignore (() = PGOCaml.prepare dbh ~query ());
    let (rows : PGOCaml.result list list) =
      PGOCaml.execute dbh ~params:[] () in
    match callback with
    | None -> ()
    | Some f ->
       let rows =
         List.map (fun cols ->
             List.map (fun res ->
                 match res with
                 | None -> ""
                 | Some s -> s
               ) cols
           ) rows
       in
       f (Some rows)
  with exn ->
       if not verbose then
         Printf.eprintf "exec: %s\n%!" query;
       Printf.eprintf "Pgsql.error: %s\n%!"
                      (Printexc.to_string exn);
       match callback with
       | None -> ()
       | Some f -> f None

let execs ?verbose dbh queries =
  List.iter (fun query ->
      exec ?verbose dbh query) queries


let createdb ?(verbose=true) database =
  let (dbh : unit PGOCaml.t PGOCaml.monad) =
    PGOCaml.connect ~database:"postgres" ()
  in
  Printf.kprintf (exec ~verbose dbh)
                 "CREATE DATABASE %s" database;
  PGOCaml.close dbh

let dropdb ?(verbose=true) database =
  let (dbh : unit PGOCaml.t PGOCaml.monad) =
    PGOCaml.connect ~database:"postgres" ()
  in
  Printf.kprintf (exec ~verbose dbh)
                 "DROP DATABASE %s" database;
  PGOCaml.close dbh

let begin_tr dbh = exec dbh "BEGIN"
let end_tr dbh = exec dbh "COMMIT"
let abort_tr dbh = exec dbh "ABORT"

let in_tr dbh f =
  let should_abort = ref true in
  try
    begin_tr dbh;
    f dbh;
    should_abort := false;
    end_tr dbh
  with exn ->
       if !should_abort then
         abort_tr dbh;
       raise exn

let touch_witness ?witness version =
    match witness with
    | None -> ()
    | Some file ->
       let oc = open_out file in
       Printf.fprintf oc "%d\n" version;
       close_out oc

let init_version0 dbh =
  exec dbh "CREATE SCHEMA db";
  exec dbh "SET search_path TO db,public";
  exec dbh {|
            CREATE TABLE info (name VARCHAR PRIMARY KEY, value INTEGER)
            |};
  exec dbh {|
            INSERT INTO info VALUES ('version',0)
            |};
  ()

let set_version version =
  Printf.kprintf (exec dbh)
                 "UPDATE info SET value = %d WHERE name = 'version'" version

let update_version ~target ?witness dbh version versions =
  let version = ref version in
  while !version < target do
    Printf.eprintf "version = %d\n%!" !version;
    begin
      match !version with
      | 0 ->
         in_tr dbh init_version1;
         version := 1
      | _ ->
         try
           let f = List.assoc !version versions in
           begin_tr dbh;
           f dbh;
           set_version (!version+1);
           end_tr dbh;
           touch_witness ?witness !version;
           version := !version +1;
         with Not_found ->
           Printf.eprintf "Your database version %d is unsupported.\n" !version;
           Printf.eprintf "Maximal supported version is %d.\n%!" target;
           Printf.eprintf "Aborting.\n%!";
           exit 2
    end;
  done;

  exec ~verbose:false dbh {|
                           SELECT value FROM info WHERE name = 'version'
                           |} ~callback:(fun res ->
         let version =
           match res with
           | Some [[ version ]] -> (try int_of_string version with _ -> -1)
           | _ -> -1
         in
         if version <> target then begin
             Printf.eprintf "Error: database update failed.\n%!";
             Printf.eprintf "  Cannot run on this database schema.\n%!";
             exit 2
           end;
         Printf.printf "Pg_update: database is up-to-date at version %d\n%!"
                       target);
  ()

let update ?(verbose=false)
           ~versions
           ?(target = List.length versions + 1) ?witness dbh =

  exec ~verbose dbh {|
            SELECT value FROM info WHERE name = 'version'
            |} ~callback:(fun res ->
         let version =
           match res with
           | Some [[ "" ]] -> 0
           | Some [[ version ]] -> int_of_string version
           | Some [] -> 0
           | Some _ -> 0
           | None -> 0
         in
         update_version ~target ?witness dbh version versions
       )
