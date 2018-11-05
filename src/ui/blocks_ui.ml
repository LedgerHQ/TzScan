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

open Lang
open Tyxml_js.Html5
open Data_types
open Js_utils
open Common
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Button
open Tezos_types
open Text

let blocks_id = "blocks"
let heads_id = "heads"

(* Blocks Pages *)

module BlocksTable = struct
  let name = "Blocks"
  let theads () = tr [
      th @@ cl_icon cube_icon (t_ s_level);
      th @@ cl_icon clock_icon (t_ s_age);
      th @@ cl_icon (number_icon cube_icon) (t_ s_nbops);
      th @@ cl_icon Tez.icon (t_ s_volume);
      th @@ cl_icon bill_icon (t_ s_fees);
      th @@ cl_icon balance_icon (t_ s_fitness);
      th @@ cl_icon uncle_icon (t_ s_uncles);
      th @@ cl_icon cookie_icon (t_ s_baker);
      th @@ cl_icon ruler_icon (t_ s_protocol);
    ]

  let page_size = 20
  let table_class = "blocks-table"
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
    span [ pcdata
             (Printf.sprintf "Blocks at level %d" level) ;
           Glossary_doc.(help HAlternatives) (* TODO ? *)
         ]

  include Panel.MakePageTable
            (struct
              include BlocksTable

              let theads () = tr [
                  th @@ cl_icon cube_icon (t_ s_hash);
                  th @@ cl_icon clock_icon (t_ s_age);
                  th @@ cl_icon (number_icon cube_icon) (t_ s_nbops);
                  th @@ cl_icon Tez.icon (t_ s_volume);
                  th @@ cl_icon bill_icon (t_ s_fees);
                  th @@ cl_icon balance_icon (t_ s_fitness);
                  th @@ cl_icon priority_icon (t_ s_priority);
                  th @@ cl_icon cookie_icon (t_ s_baker);
                  th @@ cl_icon ruler_icon (t_ s_protocol);
                ]

              let title_span _nb = span [ ] (* redefined *)
            end)
end

let to_rows ?(snapshots=[]) ?(uncles=false) blocks =
  List.map (fun block ->
      let timestamp_str = Date.to_string block.timestamp in
      let td_timestamp = td [ ] in
      Manip.appendChild td_timestamp
        (Format_date.auto_updating_timespan timestamp_str);
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
      tr ~a:cls
         [
          td (Common.make_link h_link ~path:link :: camera) ;
          td_timestamp;
          td [ pcdata @@ string_of_int block.nb_operations ] ;
          td [Tez.pp_amount ~width:6 block.volume] ;
          td [Tez.pp_amount block.fees] ;
          td [ pcdata @@ string_of_int @@ Common.get_fitness block.fitness ] ;
          begin
            if uncles then
              td [ pcdata @@ string_of_int block.priority ]
            else
              td ~a:[ a_id @@ Common.block_uncles_id block.hash ]
                 [ Common.pcdata_ () ]
          end;
          Common.account_w_blockies ~aclass:["no-overflow"] block.baker;
          td [ pcdata block.protocol.proto_name ] ;
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
                  let table_class = "blocks-table"
                end)

let make_snapshot_blocks () = SnapshotBlocksPanel.make ()

let snapshots_to_rows snapshots =
  List.map (fun snapshot ->
      let level_str = string_of_int snapshot.snap_level in
      tr
        [
          td [ pcdata @@ string_of_int snapshot.snap_cycle] ;
          td (Common.make_link level_str :: [space_icon (); camera_icon ()]) ;
          td [ pcdata @@ string_of_int snapshot.snap_index ] ;
          td [ pcdata @@ string_of_int snapshot.snap_rolls ] ;
        ]) snapshots

let update_snapshot_blocks ?nrows xhr =
  SnapshotBlocksPanel.paginate_fun snapshots_to_rows ?nrows xhr

(* Baking Rights Page *)
let columns () =
  tr [
    th [ Lang.pcdata_t s_level ] ;
    th [ Lang.pcdata_t s_first_priority ] ;
    th [ Lang.pcdata_t s_second_priority ] ;
    th [ Lang.pcdata_t s_third_priority ] ;
    th [ Lang.pcdata_t s_fourth_priority ] ;
  ]

module BakingRightsPanel =
  Panel.MakePageTable(struct
    let name = "baking-rights"
    let title_span nb = Panel.title_nb s_baking_rights nb
    let page_size = 10
    let theads = columns
    let table_class = "blocks-table"
  end)

module PassedBakingRightsPanel =
  Panel.MakePageTable(struct
    let name = "passed-baking-rights"
    let title_span nb = Panel.title_nb  s_passed_baking_rights nb
    let page_size = 10
    let theads = columns
    let table_class = "blocks-table"
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
          Common.account_w_blockies ~after:[space_icon (); cookie_icon ()] baker
        | None -> td []
        | Some i ->
          begin match List.nth_opt bkrs i with
            | _ when prio = baked_prio ->
              Common.account_w_blockies ~after:[space_icon (); cookie_icon ()] baker
            | None -> td []
            | Some bk ->
              Common.account_w_blockies bk
          end in
      baker_td :: mk_bakers  (prio - 1) bkrs priorities baked in
  List.map (fun pr ->
      tr (
        (td [ pcdata @@ string_of_int pr.r_level ]) ::
        (List.rev @@ mk_bakers 3 pr.r_bakers pr.r_bakers_priority pr.r_baked))
      ) prs

let make_baking_rights () =
  div [
    form ~a:[ a_id "filter-form"; a_autocomplete false; a_role ["search"]] [
      div ~a:[ a_class ["autocomplete"] ] [
        Tyxml_js.Html5.input ~a:[ a_id "filter"; a_class ["form-control"; "search-input"];
                                  a_placeholder (t_ s_account_hash_alias);
                                  a_input_type `Search;
                 a_name "q"] ()];
      button ~a:[ a_id "filter-button"; a_class [btn; btn_default] ]
        [ pcdata_t s_filter]];
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
