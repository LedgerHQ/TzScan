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

open Tyxml_js.Html5
open Data_types
open Common
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Lang
open Text

module ProtocolsTable = struct
  let name = "Protocols"
  let theads () = tr [
      th ~a:[ a_class [ cxs2 ] ] @@
      cl_icon (fun () -> span [pcdata "#"]) (t_ s_number);
      th ~a:[ a_class [ cxs6 ] ] @@ cl_icon ruler_icon (t_ s_hash);
      th ~a:[ a_class [ cxs2 ] ] @@ cl_icon play_icon (t_ s_start);
      th ~a:[ a_class [ cxs2 ] ] @@ cl_icon stop_icon (t_ s_end);
    ]
  let page_size = 20
  let table_class = "blocks-table"
end

module ProtocolsPanel =
  Panel.MakePageTable(struct
    include ProtocolsTable
    let title_span nb = Panel.title_nb s_protocols nb
  end)

let make = ProtocolsPanel.make ~footer:true

let to_rows =
  List.map (fun prt ->
      let elt_start = Common.make_link @@ string_of_int prt.prt_start
      and elt_end =
        if prt.prt_end = -1 then pcdata Common.bullshit_s
        else Common.make_link @@ string_of_int prt.prt_end in
      let link_proto, elt_index =
        if prt.prt_index = -1 then pcdata prt.prt_hash, pcdata Common.bullshit_s
        else
          a ~a:[ a_href @@ Printf.sprintf
                   "https://gitlab.com/tezos/tezos/tree/%s/src/proto_%03d_%s/lib_protocol/src"
                   TzscanConfig.database prt.prt_index (String.sub prt.prt_hash 0 8);
                 a_target "_blank"] [ pcdata prt.prt_hash ],
          pcdata @@ string_of_int prt.prt_index in
      tr [
        td [ elt_index ];
        td [ link_proto ];
        td [ elt_start ];
        td [ elt_end ]
      ])

let update ?nrows xhr =
  ProtocolsPanel.paginate_fun to_rows ?nrows xhr
