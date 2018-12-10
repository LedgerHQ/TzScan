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

let change_alias tz alias = Hashtbl.add alias_table tz alias

let get_alias_tbl tz = Hashtbl.find_opt alias_table tz

let reset () = Hashtbl.reset alias_table

let to_name ?alias tz =
  if tz = "" then {tz = ""; alias = None}
  else
    let alias = if alias = None then get_alias_tbl tz else alias in
    { tz ; alias }
