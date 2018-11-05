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

let alias_table : (string, string) Hashtbl.t = Hashtbl.create 10000

let change_alias = Hashtbl.add alias_table

let get_alias_tbl = Hashtbl.find_opt alias_table

let reset () = Hashtbl.reset alias_table

let dbh = PGOCaml.connect ~database:TzscanConfig.database ()

let get_alias_db hash =
  let query =
    PGSQL(dbh) "SELECT alias FROM user_alias WHERE tz = $hash" in
  match query with
  | [ alias ] -> Some alias
  | _ -> None

let to_name ?(db=false) ?alias tz =
  if tz = "" then {tz = ""; alias = None}
  else
    let alias =
      if alias = None then
        begin
          if db then
            get_alias_db tz
          else
            get_alias_tbl tz
        end
      else
        alias in
    {tz; alias}
