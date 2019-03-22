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

let alias_table : (string, string) Hashtbl.t = Hashtbl.create 1000

let add_alias {tz; alias} =
  match alias with
  | None -> ()
  | Some alias -> Hashtbl.add alias_table tz alias

let get_alias_tbl tz = Hashtbl.find_opt alias_table tz

let reset () = Hashtbl.reset alias_table

let to_name ?alias tz =
  if tz = "" then {tz = ""; alias = None}
  else
    let alias = if alias = None then get_alias_tbl tz else alias in
    { tz ; alias }


let change_aliases_from_json ?(with_table=true) ?update_db encoding json =
  let alias_added = ref 0 in
  let alias_removed = ref 0 in
  try
    let services = Json_encoding.destruct encoding json in
    reset ();
    List.iter (fun s ->
        match s.srv_aliases, s.srv_sponsored with
        | None, _ | _, None  -> ()
        | Some aliases, Some timestamp ->
          let date =
            CalendarLib.Printer.Calendar.from_fstring
              "%Y-%m-%dT%H:%M:%SZ" timestamp in
          let now = CalendarLib.Calendar.now () in
          let outdated = date < now in
          List.iter (fun account ->
              if not outdated && account.alias <> None then incr alias_added;
              if outdated then incr alias_removed;
              if not outdated && with_table then add_alias account;
              match update_db with
              | None -> ()
              | Some update_db -> update_db outdated account
            ) aliases)
      services ;
    Printf.eprintf "%d alias added | %d alias removed\n%!"
      !alias_added !alias_removed
  with
  | exn ->
    Printf.eprintf "Fatal error while reading services.json: %s\n%!"
      (Printexc.to_string exn)
