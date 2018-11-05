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

let print_comment ppf text =
  Format.fprintf ppf "/* @[<h>%a@] */" Format.pp_print_text text

let print_string ppf text =
  Format.fprintf ppf "\"" ;
  String.iter (function
      | '"' -> Format.fprintf ppf "\\\""
      | '\n' -> Format.fprintf ppf "\\n"
      | '\r' -> Format.fprintf ppf "\\r"
      | '\b' -> Format.fprintf ppf "\\b"
      | '\t' -> Format.fprintf ppf "\\t"
      | '\\' -> Format.fprintf ppf "\\\\"
      | c -> Format.fprintf ppf "%c" c)
    text ;
  Format.fprintf ppf "\""

let location = function
  | Int (loc, _) -> loc
  | String (loc, _) -> loc
  | Bytes (loc, _) -> loc
  | Seq (loc, _) -> loc
  | Prim (loc, _, _, _) -> loc

let print_annotations =
  Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string

let preformat root =
  let preformat_loc (_, _, comment) = match comment with
    | None -> (false, 0)
    | Some text ->
      (String.contains text '\n', String.length text + 1) in
  let preformat_annots = function
    | [] -> 0
    | annots -> String.length (String.concat " " annots) + 2 in
  let rec preformat_expr = function
    | Int (((_, _, comment) as loc), value) ->
      let cml, csz = preformat_loc loc in
      Int ((cml, String.length (Z.to_string value) + csz, comment), value)
    | String ((_, _, comment) as loc, value) ->
      let cml, csz = preformat_loc loc in
      String ((cml, String.length value + csz, comment), value)
    | Bytes ((_, _, comment) as loc, value) ->
      let cml, _csz = preformat_loc loc in
      Bytes ((cml, 0, comment), value)
    | Prim ((_, _, comment) as loc, name, items, annots) ->
      let cml, csz = preformat_loc loc in
      let asz = preformat_annots annots in
      let items = List.map preformat_expr items in
      let ml, sz =
        List.fold_left
          (fun (tml, tsz) e ->
             let (ml, sz, _) = location e in
             (tml || ml, tsz + 1 + sz))
          (cml, String.length name + csz + asz)
          items in
      Prim ((ml, sz, comment), name, items, annots)
    | Seq ((_, _, comment) as loc, items) ->
      let cml, csz = preformat_loc loc in
      let items = List.map preformat_expr items in
      let ml, sz =
        List.fold_left
          (fun (tml, tsz) e ->
             let (ml, sz, _) = location e in
             (tml || ml, tsz + 3 + sz))
          (cml, 4 + csz)
          items in
      Seq ((ml, sz, comment), items) in
  preformat_expr root

let rec print_expr_unwrapped ppf = function
  | Prim ((ml, s, comment ), name, args, annot) ->
    let name = match annot with
      | [] -> name
      | annots ->
        Format.asprintf "%s @[<h>%a@]" name print_annotations annots in
    if not ml && s < 80 then begin
      if args = [] then
        Format.fprintf ppf "%s" name
      else
        Format.fprintf ppf "@[<h>%s %a@]" name (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr) args ;
      begin match comment with
        | None -> ()
        | Some text -> Format.fprintf ppf "@ /* %s */" text
      end ;
    end else begin
      if args = [] then
        Format.fprintf ppf "%s" name
      else if String.length name <= 4 then
        Format.fprintf ppf "%s @[<v 0>%a@]" name (Format.pp_print_list print_expr) args
      else
        Format.fprintf ppf "@[<v 2>%s@,%a@]" name (Format.pp_print_list print_expr) args ;
      begin match comment with
        | None -> ()
        | Some comment -> Format.fprintf ppf "@ %a" print_comment comment
      end
    end
  | Int ((_, _, comment), value) ->
    begin match comment with
      | None -> Format.fprintf ppf "%s" (Z.to_string value)
      | Some comment -> Format.fprintf ppf "%s@ %a" (Z.to_string value) print_comment comment
    end
  | String ((_, _, comment), value) ->
    begin match comment with
      | None -> print_string ppf value
      | Some comment -> Format.fprintf ppf "%a@ %a" print_string value print_comment comment
    end
  | Bytes ((_, _, comment), value) ->
      begin match comment with
        | None -> Format.fprintf ppf "0x%s" (Bytes.to_string value)
        | Some comment -> Format.fprintf ppf "0x%s@ %a" (Bytes.to_string value) print_comment comment
      end
  | Seq ((_, _, None), []) ->
    Format.fprintf ppf "{}"
  | Seq ((ml, s, comment), items) ->
    if not ml && s < 80 then
      Format.fprintf ppf "{ @[<h 0>"
    else
      Format.fprintf ppf "{ @[<v 0>" ;
    begin match comment, items with
      | None, _ -> ()
      | Some comment, [] -> Format.fprintf ppf "%a" print_comment comment
      | Some comment, _ -> Format.fprintf ppf "%a@ " print_comment comment
    end ;
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf " ;@ ")
      print_expr_unwrapped
      ppf items ;
    Format.fprintf ppf "@] }"

and print_expr ppf = function
  | Prim (_, _, _ :: _, _)
  | Prim (_, _, [], _ :: _) as expr ->
    Format.fprintf ppf "(%a)" print_expr_unwrapped expr
  | expr -> print_expr_unwrapped ppf expr

let print_expr_unwrapped ppf expr =
  print_expr_unwrapped ppf (preformat expr)

let print_expr ppf expr =
  print_expr ppf (preformat expr)

let to_string expr =
  let flush expr =
    print_expr_unwrapped Format.str_formatter expr ;
    Format.flush_str_formatter () in
  match expr with
  | Seq (_, exprs) ->
    (String.concat ";\n"
       (List.map flush exprs))
| Prim _ | Int _ | String _ | Bytes _ -> flush expr
