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
open Tyxml_js.Html5
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Button
open Tezos_types
open Lang
open Text

let network_stats_id = "network-stats"
let map_id = "map-id"
let connected_btn_id = "btn-running"
let all_btn_id = "btn-all"

module PeersPanel =
  Panel.MakePageTable(struct
                  let name = "Peer"
                  let title_span nb =
                    span [ pcdata
                             (if nb < 0 then
                                t_ s_network_stats
                              else
                                Printf.sprintf
                                  "%s (%d %s)" (t_ s_network_stats)
                                  nb (t_ s_peers)
                             ); Glossary_doc.(help HNetwork) ]
                  let theads = Panel.theads_of_strings [
                      s_peer,  3;
                      s_point, 2;
                      s_country, 1;
                      s_trusted, 1;
                      s_score, 1;
                      s_state, 1;
                      s_total_sent, 1;
                      s_total_recv, 1;
                      s_inflow, 1;
                      s_outflow, 1;
                    ]

                  let page_size = 20
                  let table_class = "blocks-table"
                end)

let string_of_state = function
  | Accepted -> s_accepted
  | Running -> s_running
  | Disconnected -> s_disconnected

let enable_buttons () =
  let btn1 = find_component connected_btn_id in
  let btn2 = find_component all_btn_id in
  Manip.removeClass btn1 "disabled";
  Manip.removeClass btn2 "disabled"

let disable_buttons state =
  enable_buttons () ;
  let btn = find_component @@ "btn-" ^ state in
  Manip.addClass btn "disabled"

let update_peers =
  PeersPanel.paginate_fun
    (fun stats ->
      List.map (fun stat ->
          let point =
            match stat.last_seen with
            | None -> Common.bullshit_s
            | Some (point, _date) -> point
          in
          tr [
              td [ pcdata @@ stat.peer_id ] ;
              td [ pcdata @@ point ] ;
              td [ stat.country |> fst |> pcdata ] ;
              td [ pcdata @@ string_of_bool stat.trusted ] ;
              td [ pcdata @@ string_of_float stat.score ] ;
              td [ pcdata_t @@ string_of_state stat.state ] ;
              td [ pcdata @@ Int64.to_string stat.stat.total_sent ] ;
              td [ pcdata @@ Int64.to_string stat.stat.total_recv ] ;
              td [ pcdata @@ string_of_int stat.stat.current_inflow ] ;
              td [ pcdata @@ string_of_int stat.stat.current_outflow ] ;
        ]) stats)


let make_map map_id countries =
  Ammap3.ready "/ammap3" (fun () ->
      Js_utils.log "display...";
      let dataProvider = Ammap3.dataProvider (Ammap3.amCharts())##maps##worldLow
      in
      dataProvider##zoomLevel <- 1.0;
      dataProvider##zoomLongitude <- 10;
      dataProvider##zoomLatitude <- 52;
      let minValue = ref 1 in
      let maxValue = ref 1 in
      let areas = ref [] in
      List.iter (fun ((country_name, country_code), total) ->
          if country_name <> "" then begin
            if total > !maxValue then maxValue := total;
            if total < !minValue then minValue := total;
            areas := Ammap3.item ~customData:(string_of_int total)
                country_name country_code
                total :: !areas
          end
        ) countries;

      dataProvider##areas <- Js.array (Array.of_list !areas);

      let areasSettings = Ammap3.areasSettings () in
      areasSettings##unlistedAreasColor <- Js.string "#DDDDDD";
      areasSettings##rollOverOutlineColor <- Js.string  "#FFFFFF";
      areasSettings##rollOverColor <- Js.string  "#CC0000";
      areasSettings##balloonText <- Js.string  (t_ s_legend_runs_nodes);
      areasSettings##autoZoom <- true;

      let legend = Ammap3.legend () in
      (* legend on the left side, not ok with valueLegend
         legend##width <- 400;
         legend##backgroundAlpha <- 0.5;
         legend##backgroundColor <- Js.string "#FFFFFF";
         legend##borderColor <- Js.string"#666666";
         legend##borderAlpha <- 1.;
         legend##bottom <- 15;
         legend##left <- 15;
         legend##horizontalGap <- 10;
         legend##data <- Js.array [|
                          Ammap3.legendDataItem (* ~color:"#3366CC" *)
                            "Countries with nodes";
                        |];
      *)

      let valueLegend = Ammap3.valueLegend() in
      valueLegend##right <- 20;
      valueLegend##minValue <- Js.string (string_of_int !minValue);
      valueLegend##maxValue <- Js.string (string_of_int !maxValue);

      let map = Ammap3.create () in

      map##areasSettings <- areasSettings;
      map##dataProvider <- dataProvider;
      map##creditsPosition <- Js.string "top-right";
      map##valueLegend <- valueLegend;
      map##addLegend(legend);
      map##colorSteps <- 10;
      map##minValue <- !minValue;
      map##maxValue <- !maxValue;
      map##write (Js.string map_id);
    );
  ()

let update_map countries =
  make_map map_id
           (List.map (fun { country_name; country_code; total } ->
                (country_name, country_code), total) countries)

let map_div = div ~a:[ a_id map_id; a_class [ clg12 ] ] []

let make_panel () =
  let current_state = match Jsloc.find_arg "state" with
      | Some "running" -> "running"
      | _ -> "all"
  in
  let button ~id ~title ~state =
    a ~a:( a_id id ::
           Common.a_link ~aclass:([ btn; btn_primary ] @
                                  (if state = current_state then ["disabled"] else []))
             ~args:[ "state", state ] "network"
         )
      [ pcdata title ] ;

  in
  div [
    div ~a:[ a_class [ "network-cmd"; clgoffset4 ; clg4 ] ] [
      button ~id:connected_btn_id ~state:"running" ~title:(t_ s_connected);
      button ~id:all_btn_id ~state:"all" ~title:(t_ s_all);
    ];

    map_div;
    PeersPanel.make_clg12 ~footer:true ();
  ]
