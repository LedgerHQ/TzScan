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
open Js_utils
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Table
open Bootstrap_helpers.Color
open Bootstrap_helpers.Panel
open Common
open Js
open Lang (* s_ *)
open Tezos_types
open Text

(* cut float at 2 digits after dot *)
let simplify_float f =
  float_of_int (int_of_float (f *. 100.)) /. 100.

let home_transaction_id = "home-transactions"
let home_blocks_id = "home-heads"
let leftbox_volume_id = "leftbox-volume"
let leftbox_timestamp_id = "leftbox-timestamp"
let leftbox_baker_id = "leftbox-baker"
let leftbox_nb_transactions_id = "leftbox-nb-transactions"
let marketcap_container_id = "marketcap-container"
let marketcap_timestamp_id = "marketcap-timestamp"
let stats_container_id = "stats-container"
let state_container_id = "state-container"
let cycle_container_id = "cycle-container"
let blocks_loading_id = "home-blocks-loading"
let progress_title_id = "progress-title"

let odometer_active_baker_rate_id = "odometer-baker-rate"

let odometer_transac_id = "odometer-tr"
let odometer_orig_id = "odometer-or"
let odometer_del_id = "odometer-del"
let odometer_act_id = "odometer-act"

let fan_end_id = "fan-endors"
let fan_block_id = "fan-block"
let fan_baking_rate_id = "fan-baking-rate"
let fan_activation_rate_id = "fan-activation-rate"

let color ?(limit=0.) v =
  match float_of_string_opt v with
  | None -> []
  | Some f ->
    if f > limit then
      [ a_class [green]; a_title (Printf.sprintf "value above %.2g%%" limit) ]
    else if f < -1. *. limit then
      [ a_class [red]; a_title (Printf.sprintf "value below -%.2g%%" limit) ]
    else
      [ a_class [blue]; a_title (Printf.sprintf "value between -%.2g%% and %.2g%%" limit limit) ]

let additional_divs = ref
    (div [] : (Html_types.div_content_fun Tyxml_js.Html5.elt))

let make_stats_loading () =
  let last_block_value =
    tablex ~a:[ a_class [ btable ] ]
      [ tbody
          [ tr [
                th [ pcdata_t s_txns ] ;
                th [ pcdata_t s_volume ] ;
                th [ pcdata_t s_age ] ;
                th [ pcdata_t s_baker ]
              ] ;
            tr [
              td [ span ~a:[ a_id leftbox_nb_transactions_id ] [ Common.pcdata_ () ] ] ;
              td [ span ~a:[ a_id leftbox_volume_id ]  [ Common.pcdata_ () ] ] ;
              td [ span ~a:[ a_id leftbox_timestamp_id ] [ Common.pcdata_ () ] ] ;
              td [ span ~a:[ a_id leftbox_baker_id ; a_class ["no-overflow"] ]
                     [ Common.pcdata_ () ] ]
            ]
          ]
      ]
  in

  let progress_title =
    div ~a:[ a_class [row; clg5; cxs12]; a_id progress_title_id ] [
      div ~a:[ a_class [ "title" ]; a_id "progress-title" ] [
        cube_icon () ; space_icon () ;
        pcdata_t s_block ] ] in
  let start_block = div ~a:[a_class [clg2;cxs3]; a_id "start-block"] [] in
  let cycle_container =
    div ~a:[ a_class ([clg4; cxs6] @ [ "no-overflow" ]); a_id cycle_container_id ] [
      div ~a:[ a_class ["progress"]] [
        div ~a:[ a_id "bar"; a_class ["progress-bar"; "progress-bar-striped"];
                 a_role ["progressbar"]] [
          span ~a:[a_id "bar-span"] []]]] in
  let end_block = div ~a:[a_class [clg1;cxs3]; a_id "end-block"] [] in
  let marketcap_container =
    if Infos.api.api_config.conf_has_marketcap then
      let marketcap_info =
        div ~a:[ a_class [ row ] ] [
          div ~a:[ a_class [ clg4; csm6; cxs12 ] ] [
            pcdata (t_ s_ico_ratio ^ ": --") ];
          div ~a:[ a_class [ clg4; csm6; cxs12 ] ]
            [ pcdata (t_ s_total_supply ^ ": --") ] ;
          (* div ~a:[ a_class [ clg4; csm6; cxs12 ] ]
           *   [ pcdata_s "Circulating supply: --" ] *) ] in
      let marketcap_title =
        div ~a:[ a_class [ row ] ] [
          p ~a:[ a_class [ clg11; csm10; cxs6; "title" ] ] [
            chart_line_icon () ; space_icon () ;
            a ~a:[ a_href "https://coinmarketcap.com/currencies/tezos/" ]
              [ pcdata_t s_market_cap ];
          ];
          Common.make_loading_gif [ "loading-heading"; clg1; csm2; cxs6 ]
        ] in
      let marketcap_table =
        tablex ~a:[ a_class [ btable ] ] [
          tbody [
            tr [
              th [ pcdata_t s_price_usd ] ;
              th [ pcdata_t s_price_btc ] ;
              th [ pcdata_t s_volume_24h ] ;
              th [ pcdata_t s_change_1h ] ;
              th [ pcdata_t s_change_24h ] ;
              th [ pcdata_t s_change_7d ] ;
            ] ;
            tr [
              td [ Common.pcdata_ () ] ;
              td [ Common.pcdata_ () ] ;
              td [ Common.pcdata_ () ] ;
              td [ Common.pcdata_ () ] ;
              td [ Common.pcdata_ () ] ;
              td [ Common.pcdata_ () ]
            ]
          ]
        ]
      in
      [
        div ~a:[ a_class [ "no-overflow" ]; a_id marketcap_container_id ] [
          marketcap_title ;
          marketcap_table ;
          marketcap_info ]
      ]
    else
      []
  in
  let last_block_container =
    div ~a:[ a_class ["no-overflow" ]; a_id "last-block-container" ]
      [ progress_title; start_block ; cycle_container; end_block; last_block_value ] in
  let left_container =
    make_panel
      ~panel_class:[ "front-content" ]
      ~panel_body_content:
        (marketcap_container @ [ last_block_container ]) () in
  let left_div =
    div
      ~a:[ a_class
             [ clg6;
               clgoffset0;
               csm12;
               csmoffset0;
               cxs10;
               cxsoffset1;
               "front-box" ] ] [
      left_container ] in
  let state_div =
    div ~a:[ a_class [ row ] ] [
      span ~a:[ a_class [ clg3; csm3 ] ] [ pcdata_t s_tzscan_node_chain ] ;
      span ~a:[ a_class [ clg3; csm3 ] ] [ pcdata_t s_tzscan_node_balances ] ] in
  let state_title =
    div ~a:[ a_class [ row ] ] [
      p ~a:[ a_class [ clg11; csm11; cxs9; "title"; "hidden-xs" ] ] [
        signal_icon () ; space_icon () ;
        pcdata_t s_tzscan_tezos_nodes ];
      p ~a:[ a_class [ clg11; csm11; cxs9; "title"; "visible-xs" ] ] [
        signal_icon () ; space_icon () ;
        pcdata_t s_tzscan_tezos_nodes_xs ];
      Common.make_loading_gif [ "loading-heading"; clg1; csm1; cxs3 ]
    ] in
  let state_container =
    div ~a:[ a_class [ "no-overflow" ]; a_id state_container_id ] [
      state_title; state_div ] in
  let stats_title =
    div ~a:[ a_class [ row ] ] [
      p ~a:[ a_class [ clg11; csm11; cxs9; "title" ] ] [
        chart_pie_icon () ; space_icon () ;
        pcdata_t s_chain_stats ];
      Common.make_loading_gif [ "loading-heading"; clg1; csm1; cxs3 ]
    ] in
  let stats_container =
    div ~a:[ a_class [ "no-overflow" ]; a_id stats_container_id ] [
      stats_title ] in
  let right_container =
    make_panel
      ~panel_class:[ "front-content" ]
      ~panel_body_content:[ state_container; stats_container ] () in
  let right_div =
    div
      ~a:[ a_class
             [ clg6;
               clgoffset0;
               csm12;
               csmoffset0;
               cxs10;
               cxsoffset1;
               "front-box" ] ] [
      right_container ] in
  let ads_div = !additional_divs in

  let front_row =
    div ~a:[ a_id "front-row" ; a_class [ row ] ] [
      ads_div ; left_div ; right_div
    ] in
  front_row

(* Last blocks *)
let make_home_blocks blocks =
  let theads = tr [
      th @@ cl_icon_xs clock_icon (t_ s_age);
      th @@ cl_icon_xs cube_icon (t_ s_level);
      th ~a:[ a_class [ "hidden-xs" ] ] @@
      cl_icon_xs cookie_icon (t_ s_baker);
      th ~a:[ a_class [ "hidden-xs" ] ] @@
      cl_icon_xs (number_icon cube_icon) (t_ s_nbops);
      th @@ cl_icon_xs Tez.icon (t_ s_volume);
      th ~a:[ a_class [ "hidden-xl" ] ] @@ cl_icon Tez.icon (t_ s_fees);
    ] in
  tablex ~a:[ a_class [ "table" ] ] [
    tbody @@
    theads ::
    List.map (fun block ->
        let timestamp_str = Date.to_string block.timestamp in
        let td_timestamp = td [ ] in
        Manip.appendChild td_timestamp
          (Format_date.auto_updating_timespan timestamp_str);
        tr [
          td_timestamp ;
          td [ Common.make_link @@ string_of_int block.level ] ;
          Common.account_w_blockies
            ~aclass:["hidden-xs" ]
            ~crop_len:15
            block.baker ;
          td ~a:[ a_class [ "hidden-xs" ] ]
            [ pcdata @@ string_of_int block.nb_operations ] ;
          td [ Tez.pp_amount ~width:3 block.volume ] ;
          td ~a:[ a_class [ "hidden-xl" ] ] [
            Tez.pp_amount ~width:3 block.fees] ;
        ]
      ) blocks
  ]

let update_progress level =
  let bar = find_component "bar" in
  let cycle_position = level.lvl_cycle_position in
  let cycle = level.lvl_cycle in
  let cst = Infos.constants ~cycle in
  let blocks_per_cycle = cst.blocks_per_cycle in
  let percent = (cycle_position + 1 )* 100 / blocks_per_cycle in
  Manip.SetCss.width bar (Printf.sprintf "%d%%" percent) ;
  let bar_span = find_component "bar-span" in
  let levels_left = cst.blocks_per_cycle - level.lvl_cycle_position in
  Manip.setInnerHtml bar_span
    (Printf.sprintf "%d%% Est. %s" percent
       (Format_date.time_before_level ~cst levels_left));
  let component = find_component progress_title_id in
  let to_update =
    div ~a:[ a_class [ "title" ] ] [
      cube_icon () ; space_icon () ;
      pcdata @@
      t_subst s_subst_block_cycle
        (function
            "level" -> string_of_int level.lvl_level
          | "cycle" -> string_of_int level.lvl_cycle
          | _ -> "????")
    ]  in
  Manip.replaceChildren component [ to_update ];
  let start_block = find_component "start-block" in
  let end_block = find_component "end-block" in
  Manip.setInnerHtml start_block @@
  string_of_int (level.lvl_cycle * blocks_per_cycle + 1);
  Manip.setInnerHtml end_block @@
  string_of_int ((level.lvl_cycle + 1) * blocks_per_cycle);
  ()

let update_blocks blocks =
  let block_table = make_home_blocks blocks in
  let content_div = find_component home_blocks_id in
  begin try
      let loading_div = find_component blocks_loading_id in
      Manip.removeSelf loading_div;
    with _ -> ()
  end ;
  Manip.removeChildren content_div;
  Manip.appendChild content_div block_table

let update_state_row node_timestamps =
  let container = find_component state_container_id in
  let state_div =
    div ~a:[ a_class [ row ] ]
      (List.map (fun (kind, timestamp) ->
           div ~a:[ a_class [ clg3 ; csm2; cxs12 ] ] [
             Node_state_ui.node_state_icon timestamp ;
             pcdata kind ] ) node_timestamps)
  in
  let state_title =
    div ~a:[ a_class [ row ] ] [
      div ~a:[a_class [ clg12; csm12 ; cxs12 ] ] [
        div ~a:[ a_class [ row ] ] [
          p ~a:[ a_class [ clg12; csm12; cxs12; "title" ] ] [
            signal_icon () ; space_icon () ;
            pcdata_t s_tzscan_tezos_nodes ] ;
          div ~a:[a_class [ clg12; csm12; cxs12; ] ] [ state_div ] ] ] ;
    ] in
  Manip.removeChildren container ;
  Manip.appendChild container state_title


(* Update the volume field in the marketcap div *)
let update_leftbox_volume volume =
  let component = find_component leftbox_volume_id in
  Manip.replaceChildren component [ Tez.pp_amount volume ]

let update_leftbox_nb_transactions nb_transactions =
  let component = find_component leftbox_nb_transactions_id in
  let value = string_of_int nb_transactions in
  Manip.setInnerHtml component value

let update_leftbox_timestamp ts =
  let component = find_component leftbox_timestamp_id in
  Manip.removeChildren component;
  Manip.appendChild component (Format_date.auto_updating_timespan ts)

let update_leftbox_baker baker =
  let component = find_component leftbox_baker_id in
  Manip.replaceChildren component [Common.make_link_account baker]

let update_leftbox_marketcap price_usd price_btc volume
    change_1 change_24 change_7 last_updated (* total_supply _circu_supply *) =
  let unopt ~default = function None -> default | Some v -> v in
  let container = find_component marketcap_container_id in
  let price_usd = float_of_string price_usd in
  let volume = unopt ~default:"- " volume in
  let change_1 = unopt ~default:"- " change_1 in
  let change_24 = unopt ~default:"- " change_24 in
  let change_7 = unopt ~default:"- " change_7 in
  let ico_returns =
    Printf.sprintf "%s: x%.2f" (t_ s_ico_ratio) (price_usd /. 0.47)
  in
  let last_updated =
    (jsnew date_fromTimeValue(float_of_string last_updated *. 1000.))##toString() in
  let ts_span =
    span ~a:[ a_id marketcap_timestamp_id; a_class [ clg2; csm2; cxs6 ] ] [
      Format_date.auto_updating_timespan (Js.to_string last_updated)
    ] in
  let marketcap_info =
    div ~a:[ a_class [ row ] ] [
      div ~a:[ a_class [ clg6; csm6; cxs12 ] ]
        [ pcdata (t_ s_total_ico_supply ^ ": ") ;
          Tez.approx_amount
            (Ico_constants.total_supply_ico
               Infos.api.api_config.conf_ico) ] ;
      (* div ~a:[ a_class [ clg6; csm6; cxs12 ] ]
       *   [ pcdata_t "Circulating supply: " ;
       *     Tez.amount_float_tez circu_supply ] ; *)
      div ~a:[ a_class [ clg12; csm12; cxs12 ] ] [
        pcdata ico_returns ] ] in
  let marketcap_title =
    div ~a:[ a_class [ row ] ] [
      p ~a:[ a_class [ clg10; csm10; cxs6; "title" ] ] [
        chart_line_icon () ; space_icon () ;
        a ~a:[ a_href "https://coinmarketcap.com/currencies/tezos/" ]
          [ pcdata_t s_market_cap ] ;
      ];
      ts_span
    ] in
  let marketcap_table =
    div ~a:[ a_class [ btable_responsive ] ] [
      tablex ~a:[ a_class [ btable ] ] [
        tbody [
          tr [
            th [ pcdata_t s_price_usd ] ;
            th [ pcdata_t s_price_btc ] ;
            th [ pcdata_t s_volume_24h ] ;
            th [ pcdata_t s_change_1h ] ;
            th [ pcdata_t s_change_24h ] ;
            th [ pcdata_t s_change_7d ] ;
          ] ;
          tr [
            td [ pcdata @@ Printf.sprintf "$%.2f" price_usd ] ;
            td [ pcdata price_btc ] ;
            td [ pcdata @@ "$" ^ volume ] ;
            td ~a:(color ~limit:0.25 change_1) [ pcdata @@ change_1 ^ "%" ] ;
            td ~a:(color ~limit:1. change_24) [ pcdata @@ change_24 ^ "%" ] ;
            td ~a:(color ~limit:1. change_7) [ pcdata @@ change_7 ^ "%" ]
          ]
        ]
      ]
    ] in
  Manip.removeChildren container;
  Manip.appendChild container marketcap_title ;
  Manip.appendChild container marketcap_table ;
  Manip.appendChild container marketcap_info

let min_blocks_per_hour = 20
let min_ops_per_hour = 200
let min_volume_per_hour = 0L
let min_fees_per_hour = 0L

let update_stats ms =
  let container = find_component stats_container_id in

  let title = p ~a:[ a_class [ "title" ] ] [
      chart_pie_icon () ; space_icon () ;
      pcdata_t s_chain_stats ]
  in
  let theads =
    tr (
        (th [pcdata ""]) ::
        Array.to_list (Array.map (fun s ->
            th ~a:[ a_class [ white ]] [Jslang.pcdata_s s]) ms.ms_period))
  in
  let row to_string times title min_per_hour table =
    tr (
        th ~a:[ a_class [ white ] ]  [pcdata_t title] ::
          Array.to_list
            (Array.mapi
               (fun i nb ->
                 let result_class =
                   if times min_per_hour ms.ms_nhours.(i) > nb then
                     red else "stats-container-result"
                 in
                 td ~a:[ a_class [ result_class ] ] (to_string nb))
               table)
      )
  in
  let row64 to_string times title min_per_hour table =
    tr (
        th ~a:[ a_class [ white ] ]  [pcdata_t title] ::
          Array.to_list
            (Array.mapi
               (fun i nb ->
                 let result_class =
                   if times min_per_hour (Int64.of_int ms.ms_nhours.(i)) > nb then
                     red else "stats-container-result"
                 in
                 td ~a:[ a_class [ result_class ] ] (to_string nb))
               table)
      )
  in
  let rowi title min_per_hour table =
    row (fun i ->
        [pcdata (string_of_int i)]
      ) ( * ) title min_per_hour table
  in
  let rowxtz ?(width=5)title min_per_hour table =
    row64 (fun i ->
        [Tez.pp_amount ~width i]
      ) Int64.mul title min_per_hour table
  in
  let table =
    div ~a:[ a_class [ btable_responsive ] ] [
      tablex ~a:[ a_class [ btable ] ] [
        tbody [
          theads;
          rowi s_blocks min_blocks_per_hour  ms.ms_nblocks;
          rowi s_ops min_ops_per_hour ms.ms_nops;
          rowxtz ~width:4 s_volume min_volume_per_hour ms.ms_volume;
          rowxtz s_fees min_fees_per_hour ms.ms_fees;
        ]
      ] ] in
  Manip.removeChildren container;
  Manip.appendChildren container [title; table] ;
  ()

let update_fan h24 =
  let end_rate = int_of_float h24.h24_end_rate in
  let blocks_rate = int_of_float h24.h24_block_0_rate in
  let baking_rate = int_of_float h24.h24_baking_rate in
  let exec_end =
    Printf.sprintf "var bar = document.getElementById('%s').ldBar; bar.set(%d)"
      fan_end_id
      end_rate in
  let exec_blocks =
    Printf.sprintf "var bar = document.getElementById('%s').ldBar; bar.set(%d)"
      fan_block_id
      blocks_rate in
  let exec_baking_rate =
    Printf.sprintf "var bar = document.getElementById('%s').ldBar; bar.set(%d)"
      fan_baking_rate_id
      baking_rate in
  ignore (Js.Unsafe.eval_string exec_end) ;
  ignore (Js.Unsafe.eval_string exec_blocks) ;
  ignore (Js.Unsafe.eval_string exec_baking_rate)


let init_fan () =
  let fan_end = Printf.sprintf "var bar=new ldBar(\".%s\",{});" fan_end_id in
  let fan_bl = Printf.sprintf "var bar=new ldBar(\".%s\",{});" fan_block_id in
  let fan_br = Printf.sprintf "var bar=new ldBar(\".%s\",{});" fan_baking_rate_id in
  ignore (Js.Unsafe.eval_string fan_end) ;
  ignore (Js.Unsafe.eval_string fan_bl) ;
  ignore (Js.Unsafe.eval_string fan_br)

let make_fan percent fan_init_class =
  let set_fan bar =
    let barjs = Tyxml_js.To_dom.of_element bar in
    barjs##setAttribute(Js.string "data-value",
                        Js.string @@ string_of_int percent) ;
    barjs##setAttribute(Js.string "data-preset",
                        Js.string "circle") in
  let pb =
    div ~a:[ a_id fan_init_class ;
             a_class [ fan_init_class; "ldBar"; "label-center"; "auto" ] ] [] in
  ignore (set_fan pb);
  pb

let update_odometer h24 =
  let od1 = find_component odometer_transac_id in
  let od2 = find_component odometer_orig_id in
  let od3 = find_component odometer_del_id in
  let od4 = find_component odometer_act_id in
  let od5 = find_component odometer_active_baker_rate_id in
  Manip.setInnerHtml od1 @@ string_of_int h24.h24_transactions ;
  Manip.setInnerHtml od2 @@ string_of_int h24.h24_originations ;
  Manip.setInnerHtml od3 @@ string_of_int h24.h24_delegations ;
  Manip.setInnerHtml od4 @@ string_of_int h24.h24_activations ;
  Manip.setInnerHtml od5 @@ string_of_int h24.h24_active_baker

let init_odometer () =
  let od1 = Tyxml_js.To_dom.of_element @@ find_component odometer_transac_id in
  let od2 = Tyxml_js.To_dom.of_element @@ find_component odometer_orig_id in
  let od3 = Tyxml_js.To_dom.of_element @@ find_component odometer_del_id in
  let od4 = Tyxml_js.To_dom.of_element @@ find_component odometer_act_id in
  let od5 = Tyxml_js.To_dom.of_element @@ find_component odometer_active_baker_rate_id in
  ignore @@ Odometer.odometer od1 ;
  ignore @@ Odometer.odometer od2 ;
  ignore @@ Odometer.odometer od3 ;
  ignore @@ Odometer.odometer od4 ;
  ignore @@ Odometer.odometer od5

let update_h24 h24 =
  update_odometer h24 ;
  update_fan h24

let make_page () =
  (* Market Cap *)
  let stats_div =
    div [ make_stats_loading () ] in

  (* Blocks  *)
  let block_div =
    div ~a:[ a_id home_blocks_id ;
             a_class [ "blocks-div"; csm12 ] ] [] in

  (* Latests Blocks/Transactions *)
  div ~a:[ a_class [ row; "summary" ] ] [
    stats_div ;
    div ~a:[ a_class [ clg6; cxs12 ] ] [
      div ~a:[ a_class [ row ] ] [
        div ~a:[ a_class [ clg11; "section-title" ] ]
          [ cubes_icon () ; space_icon () ;
            pcdata_t s_blocks ; Glossary_doc.(help HBlock);
            span [a ~a:(Common.a_link ~aclass:[ "paginate"; "ntm" ] "/blocks")
                    [ pcdata_t s_view_all ]
                 ]
          ] ;
        Common.make_home_loading_gif blocks_loading_id [ cxs12 ];
        block_div;
      ]
    ] ;
    div ~a:[ a_class [ clg6; cxs12 ] ] [
      div ~a:[ a_class [ row ] ] [

        div ~a:[ a_class [ cxs12; "section-title" ] ]
          [ chart_line_icon () ; space_icon () ;
            pcdata_t s_last_24h ;
          ] ;

        div ~a:[ a_class [ cxs6; "stat-item" ] ] [
          div [ h4 [ pcdata_t s_endorsement_rate ] ] ;
          make_fan 0 fan_end_id ] ;
        div ~a:[ a_class [ cxs6; "stat-item" ] ] [
          div [ h4 [ pcdata_t s_block_prio_0_baked ] ] ;
          make_fan 0 fan_block_id ] ;

        div ~a:[ a_class [ cmd4 ; cxs8; "stat-item" ] ] [
          a ~a:(Common.a_link "transactions") [
            div [ h4 [ pcdata_t s_transactions] ] ;
            div ~a:[ a_id odometer_transac_id ;
                     a_class [ "odometer"; "odometer-theme-train-station" ] ] [
              pcdata "0"] ;
          ]
        ] ;
        div ~a:[ a_class [ cmd2 ; cxs4; "stat-item" ] ] [
          a ~a:(Common.a_link "originations") [
            div [ h4 [ pcdata_t s_originations] ] ;
            div ~a:[ a_id odometer_orig_id ;
                     a_class [ "odometer"; "odometer-theme-train-station" ] ] [
              pcdata "0"] ;
          ]
        ] ;
        div ~a:[ a_class [ cmd4; cxs8; "stat-item" ] ] [
          a ~a:(Common.a_link "delegations") [
            div [ h4 [ pcdata_t s_delegations ] ] ;
            div ~a:[ a_id odometer_del_id ;
                     a_class [ "odometer"; "odometer-theme-train-station" ] ] [
              pcdata "0"] ;
          ]
        ] ;
        div ~a:[ a_class [ cmd2; cxs4 ; "stat-item" ] ] [
          a ~a:(Common.a_link "activations") [
            div [ h4 [ pcdata_t s_activations ] ] ;
            div ~a:[ a_id odometer_act_id ;
                     a_class [ "odometer"; "odometer-theme-train-station" ] ] [
              pcdata "0"] ;
          ]
        ] ;
        div ~a:[ a_class [ cxs12; "section-title" ] ]
          [ chart_line_icon () ; space_icon () ;
            pcdata_t s_last_snapshot ] ;

        div ~a:[ a_class [ cxs6; "stat-item" ] ] [
          div [ h4 [ pcdata_t s_staking_ratio ] ] ;
          make_fan 0 fan_baking_rate_id
        ] ;

        div ~a:[ a_class [ cxs6; "stat-item" ] ] [
          a ~a:(Common.a_link "rolls-distribution") [
            div [ h4 [ pcdata_t s_roll_owners ] ] ;
            div ~a:[ a_id odometer_active_baker_rate_id ;
                     a_class [ "odometer"; "odometer-theme-train-station" ] ] [
              pcdata "0"] ;
          ]
        ] ;

      ]
    ]
  ]
