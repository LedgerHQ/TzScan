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

open Ocp_js
open Html
open Js_utils
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Button
open Bootstrap_helpers.Color
open Bootstrap_helpers.Grid
open Tezos_types
open Data_types
open Lang
open Text
open Common

let blocks_id = "blocks"
let heads_id = "heads"

(* TODO: move this version into a config file *)
let baker_version = 3

(* Blocks Pages *)

module BlocksTable = struct
  let name = "Blocks"
  let theads () = tr [
      th @@ cl_icon cube_icon (t_ s_level);
      th @@ cl_icon clock_icon (t_ s_age);
      th @@ cl_icon priority_icon (t_ s_priority);
      th @@ cl_icon (number_icon cube_icon) (t_ s_nbops);
      th @@ cl_icon Tez.icon (t_ s_volume);
      th @@ cl_icon bill_icon (t_ s_fees);
      th @@ cl_icon balance_icon (t_ s_fitness);
      th @@ cl_icon uncle_icon (t_ s_uncles);
      th @@ cl_icon cookie_icon (t_ s_baker);
      th @@ cl_icon ruler_icon (t_ s_protocol);
      th @@ cl_icon priority_icon (t_ s_baker_version);
    ]

  let page_size = 20
  let table_class = "default-table"
end

let s_blocks = ss_ "Blocks"

module BlocksPanel =
  Panel.MakePageTable(struct
                  include BlocksTable
                  let title_span nb =
                    Panel.title_nb s_blocks
                      ~help:Glossary_doc.HBlock nb
                end)

let make_blocks () = BlocksPanel.make_clg12 ~footer:true ()

module UnclesPanel = struct

  let uncles_title_span level _nb =
    span [ txt
             (Printf.sprintf "Blocks at level %d" level) ;
           Glossary_doc.(help HAlternatives) (* TODO ? *)
         ]

  include Panel.MakePageTable
            (struct
              include BlocksTable
              let title_span _nb = span [ ] (* redefined *)
            end)
end

let to_rows ?(snapshots=[]) ?(uncles=false) blocks =
  List.map (fun (block, pred_fitness) ->
      let timestamp_str = Date.to_string block.timestamp in
      let td_timestamp = td [ ] in
      Manip.appendChild td_timestamp
        (Format_date.auto_updating_timespan timestamp_str);
      let td_priority = td [ txt @@ string_of_int block.priority ] in
      let td_baker_version =
        let version = "0x" ^ String.sub block.pow_nonce 0 8  in
        match int_of_string_opt version with
        | Some version ->
          if version =  baker_version then
            td ~a:[ a_title @@ Printf.sprintf "%S: up to date" block.pow_nonce ] [
              span ~a:[ a_class [ green ] ] [ check_icon () ]
            ]
          else
            td ~a:[ a_title (Printf.sprintf "%S: not running the \
                                             latest version of the baker."
                               block.pow_nonce) ] [
              span ~a:[ a_class [ "yellow" ] ] [ exclamation_icon ()  ]
            ]
        | None ->
          td ~a:[ a_title (Printf.sprintf "No version found.") ] [
            span ~a:[ a_class [ "red" ] ] [ cross_icon ()  ]
          ] in
      let is_snapshot = List.mem block.level snapshots in
      let camera = if is_snapshot then [space_icon (); camera_icon ()] else [] in
      let cls =
        if uncles && block.distance_level = 0 then
          [ a_class [ "bg-block-main-chain" ] ]
        else if is_snapshot then
          [ a_class ["success"]; a_title "snapshot" ]
        else
          [ ]
      in
      let h_link = if uncles then block.hash else string_of_int block.level in
      let link =
        if block.distance_level = 0 then
          string_of_int block.level
        else block.hash in
      let fitness, pred_fitness =
        get_fitness block.fitness, get_fitness pred_fitness in
      let diff_fitness = fitness - pred_fitness in
      tr ~a:cls
         [
          td (make_link h_link ~path:link :: camera) ;
          td_timestamp ;
          td_priority ;
          td [ txt @@ string_of_int block.nb_operations ] ;
          td [Tez.pp_amount ~width:6 block.volume] ;
          td [Tez.pp_amount block.fees] ;
          td [ txt @@ Printf.sprintf "%d (+%d)" fitness diff_fitness ];
          begin
            if uncles then
              td [ txt_ () ]
            else
              td ~a:[ a_id @@ block_uncles_id block.hash ]
                 [ txt_ () ]
          end;
          account_w_blockies ~aclass:["no-overflow"]
            ~crop_len:20 ~crop_limit:md_size block.baker;
          td [ txt (crop_hash ~crop_len:10 block.protocol.proto_name) ] ;
          td_baker_version ;
    ]) blocks

let update_blocks ?(snapshots=[]) ?(alt=false) ?level ?nrows xhr =
  if alt then
    match level with
    | None -> ()
    | Some lvl ->
       UnclesPanel.paginate_fun (to_rows ~uncles:true) ?nrows xhr
         ~title_span:(UnclesPanel.uncles_title_span lvl)
  else
    BlocksPanel.paginate_fun (to_rows ~snapshots) ?nrows xhr

module Heads = struct

  module HeadsPanel =
    Panel.MakePageTable
      (struct
        include BlocksTable
        let title_span = Panel.title_nb s_alternative_heads
            ~help:Glossary_doc.HAlternatives
      end)

  let make () = HeadsPanel.make ()
  let to_rows txs = to_rows txs

  let update ~nrows xhr =
    HeadsPanel.paginate_fun to_rows ~nrows xhr

end

(* Snapshot Page *)

module SnapshotBlocksPanel =
  Panel.MakePageTable(struct
                  let title_span nb =
                    Panel.title_nb s_snapshot_blocks
                                   ~help:Glossary_doc.HBlock nb
                  let name = "Snapshots"
                  let theads () = tr [
                      th @@ cl_icon cube_icon "Cycle";
                      th @@ cl_icon cube_icon "Level";
                      th @@ cl_icon clock_icon "Index";
                      th @@ cl_icon Tez.icon "Rolls";
                    ]

                  let page_size = 20
                  let table_class = "default-table"
                end)

let make_snapshot_blocks () = SnapshotBlocksPanel.make ()

let snapshots_to_rows snapshots =
  List.map (fun snapshot ->
      let level_str = string_of_int snapshot.snap_level in
      tr
        [
          td [ txt @@ string_of_int snapshot.snap_cycle] ;
          td (make_link level_str :: [space_icon (); camera_icon ()]) ;
          td [ txt @@ string_of_int snapshot.snap_index ] ;
          td [ txt @@ string_of_int snapshot.snap_rolls ] ;
        ]) snapshots

let update_snapshot_blocks ?nrows xhr =
  SnapshotBlocksPanel.paginate_fun snapshots_to_rows ?nrows xhr

(* Baking Rights Page *)
let columns () =
  tr [
    th [ Lang.txt_t s_level ] ;
    th [ Lang.txt_t s_first_priority ] ;
    th [ Lang.txt_t s_second_priority ] ;
    th [ Lang.txt_t s_third_priority ] ;
    th [ Lang.txt_t s_fourth_priority ] ;
  ]

module BakingRightsPanel =
  Panel.MakePageTable(struct
    let name = "baking-rights"
    let title_span nb = Panel.title_nb s_baking_rights nb
    let page_size = 10
    let theads = columns
    let table_class = "default-table"
  end)

module PassedBakingRightsPanel =
  Panel.MakePageTable(struct
    let name = "passed-baking-rights"
    let title_span nb = Panel.title_nb  s_passed_baking_rights nb
    let page_size = 10
    let theads = columns
    let table_class = "default-table"
  end)

let to_rows_priorities prs =
  let rec list_index_opt i l x = match (i, l) with
    | _, [] -> None
    | i,  h :: _ when x = h -> Some i
    | i, _ :: q -> list_index_opt (i + 1) q x in
  let rec mk_bakers prio bkrs priorities baked =
    if prio < 0 then []
    else
      let baked_prio, baker = match baked with
        | None -> -1, {tz=""; alias=None}
        | Some (baker, baked_prio) -> baked_prio, baker in

      let baker_td = match list_index_opt 0 priorities prio with
        | None when prio = baked_prio ->
          account_w_blockies ~after:[space_icon (); cookie_icon ()] baker
        | None -> td []
        | Some i ->
          begin match List.nth_opt bkrs i with
            | _ when prio = baked_prio ->
              account_w_blockies
                ~crop_len:15 ~crop_limit:md_size
                ~after:[space_icon (); cookie_icon ()] baker
            | None -> td []
            | Some bk ->
              account_w_blockies ~crop_len:15 ~crop_limit:md_size bk
          end in
      baker_td :: mk_bakers  (prio - 1) bkrs priorities baked in
  List.map (fun pr ->
      tr (
        (td [ txt @@ string_of_int pr.r_level ]) ::
        (List.rev @@ mk_bakers 3 pr.r_bakers pr.r_bakers_priority pr.r_baked))
      ) prs

let make_baking_rights () =
  div [
    form ~a:[ a_id "filter-form"; a_autocomplete false; a_role ["search"]] [
      div ~a:[ a_class ["autocomplete"] ] [
        input ~a:[ a_id "filter"; a_class ["form-control"; "search-input"];
                                  a_placeholder (t_ s_account_hash_alias);
                                  a_input_type `Search;
                 a_name "q"] ()];
      button ~a:[ a_id "filter-button"; a_class [btn; btn_default] ]
        [ txt_t s_filter]];
    BakingRightsPanel.make_clg12 ();
    PassedBakingRightsPanel.make_clg12 ();
  ]

let update_baking_rights ?(future=true) ?nrows xhr =
  if future then
    BakingRightsPanel.paginate_fun to_rows_priorities ?nrows xhr
  else
    PassedBakingRightsPanel.paginate_fun to_rows_priorities ?nrows xhr


let filter_handler xhr_search xhr_filter =
  Search.search_handler ~input_id:"filter" ~button_id:"filter-button"
    ~list_id:"filter-list"
    ~onclick:(fun e ->
        Dom.preventDefault e;
        let filter_input = find_component "filter" in
        let filter = String.trim @@ Manip.value filter_input in
        let filter = if filter = "" then None else Some filter in
        xhr_filter ?filter () ; true)
    xhr_search
    ()

let update_filter _nb hash results =
  Search.update_search ~input_id:"filter" ~button_id:"filter-button"
    ~list_id:"filter-list" (-1) hash results
