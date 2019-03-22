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
open Data_types
open Ocp_js
open Html
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Align
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Panel
open Bootstrap_helpers.Color
open Lang
open Text

let amcharts3_ready = Amcharts3.ready "/amcharts3"
let pcf = Common.txtf

let archive_links = ref []
let archive_loaded = ref false

let img_src_passed =
  "images/icon-passed.png"
let img_src_progress =
  "images/icon-play.png"
let img_src_wait =
  "images/icon-pause.png"
let img_src_ignored =
  "images/icon-ignored.png"

let load_archive_links () =
  if not !archive_loaded then
    Xhr.get "archive_links" "/proposal_links.json"
      (fun res ->
         (try
            archive_links :=
              List.map (fun (hash, name, url) -> hash, (name, url)) @@
              EzEncoding.destruct
                Json_encoding.(list (obj3 (req "hash" string) (req "name" string)
                                       (req "url" string))) res
          with exn ->
            Js_utils.log "Cannot parse proposal_links: %s" (Printexc.to_string exn));
         archive_loaded := true)

module ProposalsPanel =
  Panel.MakePageTable(struct
    let name = "proposals"
    let theads () = tr [
        th [txt_t s_voting_period];
        th [txt_t s_hash];
        th [txt_t s_source];
        th [txt_t s_archive];
        th [txt_t s_counts];
        th [txt_t s_votes];
      ]
    let page_size = 20
    let table_class = "default-table"
    let title_span = Panel.title_nb s_proposals
  end)

module PeriodProposalsPanel =
  Panel.MakePageTable(struct
    let name = "period-proposals"
    let theads () = tr [
        th [txt_t s_hash];
        th [txt_t s_source];
        th [txt_t s_archive];
        th [txt_t s_counts];
        th [txt_t s_votes];
      ]
    let page_size = 20
    let table_class = "default-table"
    let title_span _nb = span [ txt_t s_proposals ]
  end)

module BallotsPanel =
  Panel.MakePageTable(struct
    let name = "ballots"
    let theads () = tr [
        th [txt_t s_ballot];
        th [txt_t s_counts];
        th [txt_t s_votes];
        th [txt_t s_percent]
      ]
    let page_size = 20
    let table_class = "default-table"
    let title_span _nb = span [ txt_t s_ballots ]
  end)

module VotesPanel =
  Panel.MakePageTable(struct
    let name = "votes"
    let theads () = tr [
        th [txt_t s_voting_period];
        th [txt_t s_source];
        th [txt_t s_votes];
        th [txt_t s_ballot];
        th [txt_t s_operation];
      ]
    let page_size = 20
    let table_class = "default-table"
    let title_span = Panel.title_nb s_votes
  end)

let pie_proposals_id = "proposals-pie-div"
let pie_ballots_id = "ballots-pie-div"
let gauge_quorum_id = "quorum-gauge-div"
let gauge_supermajority_id = "supermajority-gauge-div"
let progress_title_id2 = "proposal-progress-title-2"
let start_level_id = "proposal-start-level"
let end_level_id = "proposal-end-level"
let bar_span_id = "proposal-bar-span"
let progress_bar_id = "proposal-bar"
let control_left_id = "proposal-control-left-div"
let control_right_id = "proposal-control-right-div"
let proposal_main_id = "proposal-main-div"
let period_proposals_panel_id = "period-proposals-panel"
let proposals_panel_id = "proposals-panel"
let ballots_panel_id = "ballots-panel"
let ballots_quorum_id = "ballots-quorum"

let steps_id = "steps"
let steps_one_id = "steps-one"
let steps_two_id = "steps-two"
let steps_three_id = "steps-three"
let steps_four_id = "steps-four"

(* Makers *)

let make_controls ?(left=true) ?(right=true) ?period () =
  let previous_args, next_args = match period with
    | None -> [], []
    | Some period -> ["period", string_of_int (period - 1)],
                     ["period", string_of_int (period + 1)] in
  let control_left =
    if left then
      a ~a:((a_title "Previous voting period") :: (
          Common.a_link
            ~aclass:["carousel-control"; "left"]
            ~args:previous_args
            "proposals")) [ glyph "chevron-left" ]
    else a [] in
  let control_right =
    if right then
      a ~a:((a_title "Next voting period") :: (
          Common.a_link
            ~aclass:["carousel-control"; "right"]
            ~args:next_args
            "proposals")) [ glyph "chevron-right" ]
    else a [] in
  control_left, control_right

let make_step step_id name descr img_src =
  div ~a:[ a_id @@ Common.make_id step_id "voting" ; a_class [ step_id ] ] [
    img ~a:[ a_id @@ Common.make_id step_id "img" ; a_class [ "steps-img" ] ]
      ~src:img_src
      ~alt:img_src () ;
    h3 ~a:[ a_id @@ Common.make_id step_id "name" ;
            a_class [ "steps-name" ] ] [ txt name ] ;
    p ~a:[ a_id @@ Common.make_id step_id "description";
           a_class [ "steps-description" ] ] [ txt descr ]
  ]

let make_timeline period =
  section ~a:[ a_id steps_id ; a_class [ "steps-section" ] ] [
    h1 ~a:[ a_class [ "steps-header" ] ] (
      match period with
      | None -> [
          txt_t s_tezos_protocol_amendment_votes ;
          br () ;
          span ~a:[ a_id progress_title_id2 ; a_class [ "steps-title-italic" ] ] []
        ]
      | Some period -> [
          txt_t s_tezos_protocol_amendment_votes ;
          br () ;
          span ~a:[ a_class [ "steps-title-italic"] ] [ txt @@ Printf.sprintf "Period %d : " period ];
          span ~a:[ a_id progress_title_id2 ; a_class [ "steps-title-italic" ] ] []
        ]
    ) ;

    div ~a:[ a_class [ "steps-timeline" ] ] [
      make_step
        steps_one_id
        (t_ s_proposal)
        (t_ s_proposal_description)
        img_src_wait ;

      make_step
        steps_two_id
        (t_ s_exploration)
        (t_ s_exploration_description)
        img_src_wait ;

      make_step
        steps_three_id
        (t_ s_testing)
        (t_ s_testing_description)
        img_src_wait ;

      make_step
        steps_four_id
        (t_ s_promotion)
        (t_ s_promotion_description)
        img_src_wait
    ] ;
    div ~a:[ a_class [ row ; "steps-legend" ] ] [
      span ~a:[ a_class [ clg2; cxs6 ] ] [
        img
          ~src:img_src_passed
          ~alt:img_src_passed () ;
        txt "Formal Period"
      ];
      span ~a:[ a_class [ clg2 ; cxs6] ] [
        img
          ~src:img_src_progress
          ~alt:img_src_progress () ;
        txt "Current Period"
      ];
      span ~a:[ a_class [ clg2 ; cxs6 ] ] [
        img
          ~src:img_src_wait
          ~alt:img_src_wait () ;
        txt "Coming Period"
      ];
      span ~a:[ a_class [ clg2 ; cxs6 ] ] [
        img
          ~src:img_src_ignored
          ~alt:img_src_ignored () ;
        txt "N/A"
      ];
    ]
  ]


let make args =
  load_archive_links ();
  let period = match List.find_opt (fun (x,_) -> x = "period") args with
    | None -> None
    | Some (_, period) -> int_of_string_opt period in
  let timeline = make_timeline period in
  let control_left, control_right = make_controls ?period () in
  let voting_view =  div [
    div ~a:[a_class [row]] [
      h2 ~a:[a_class [cxs12; text_center] ] [
      ];
      div ~a:[a_class [clg2;cxs3; text_right; "voting-level"];
              a_id start_level_id] [];
      div ~a:[a_class [clg8; cxs6]] [
        div ~a:[ a_class ["progress"; "voting-progress"]] [
          div ~a:[ a_class ["progress-bar"; "progress-bar-striped"];
                   a_role ["progressbar"];
                   a_id progress_bar_id ] [
            span ~a:[a_id bar_span_id] []]]
      ];
      div ~a:[a_class [clg2; cxs3; "voting-level"];
              a_id end_level_id] []
    ];
    div ~a:[ a_class ["navigation-container"] ] [
      div ~a:[ a_id control_left_id ] [control_left];
      div ~a:[ a_class ["navigation-inner"]; a_id proposal_main_id ] [
        div ~a:[ a_id period_proposals_panel_id; a_style "display: none" ] [
          div ~a:[ a_id pie_proposals_id ] [];
          PeriodProposalsPanel.make ~footer:true () ];
        div ~a:[ a_id ballots_panel_id; a_style "display: none" ] [
          div ~a:[ a_class [row] ] [
            div ~a:[ a_class [csm12; clg4] ] [
              div ~a:[ a_id pie_ballots_id ] []];
            div ~a:[ a_class [csm6; clg4; Bootstrap_helpers.Align.text_center] ] [
              div ~a:[ a_id gauge_quorum_id] [] ];
            div ~a:[ a_class [csm6; clg4] ] [
              div ~a:[ a_id gauge_supermajority_id ] []];
          ];
          BallotsPanel.make ~footer:true () ]
      ];
      div ~a:[ a_id control_right_id ] [control_right]
    ]
  ] in
  div [ timeline ; voting_view ]

let make_all_proposals () =
  load_archive_links ();
  ProposalsPanel.make ~footer:true ()

let make_votes () =
  load_archive_links ();
  VotesPanel.make ~footer:true ()

(* Update *)

let update_controls ?left ?right ?period () =
  let control_left, control_right = make_controls ?left ?right ?period () in
  let control_left_div = Js_utils.find_component control_left_id in
  let control_right_div = Js_utils.find_component control_right_id in
  Js_utils.Manip.replaceChildren control_left_div [control_left];
  Js_utils.Manip.replaceChildren control_right_div [control_right]

let update_timeline period_kind period_status =
  let update_step step_id step_status =
    let step = Js_utils.find_component @@ Common.make_id step_id "voting" in
    let step_name = Js_utils.find_component @@ Common.make_id step_id "name" in
    let step_descr = Js_utils.find_component @@ Common.make_id step_id "description" in
    let step_img = Js_utils.find_component @@ Common.make_id step_id "img" in
    let new_step_img =
      match step_status with
      | VPS_passed ->
        Js_utils.Manip.addClass step_name "period-vote-passed" ;
        Js_utils.Manip.addClass step_descr "description-passed" ;
        img ~a:[ a_id @@ Common.make_id step_id "img" ; a_class [ "steps-img" ] ]
          ~src:(img_src_passed)
          ~alt:(img_src_passed) ()
      | VPS_current ->
        Js_utils.Manip.addClass step_name "period-vote-progress" ;
        Js_utils.Manip.addClass step_descr "description-progress"  ;
        img ~a:[ a_id @@ Common.make_id step_id "img" ; a_class [ "steps-img" ] ]
          ~src:(img_src_progress)
          ~alt:(img_src_progress) ()
      | VPS_wait ->
        Js_utils.Manip.addClass step_name "period-vote-wait" ;
        Js_utils.Manip.addClass step_descr "description-wait" ;
        img ~a:[ a_id @@ Common.make_id step_id "img" ; a_class [ "steps-img" ] ]
          ~src:(img_src_wait)
          ~alt:(img_src_wait) ()
      | VPS_ignored ->
        Js_utils.Manip.addClass step_name "period-vote-ignored" ;
        Js_utils.Manip.addClass step_descr "description-ignored" ;
        img ~a:[ a_id @@ Common.make_id step_id "img" ; a_class [ "steps-img" ] ]
          ~src:(img_src_ignored)
          ~alt:(img_src_ignored) () in
    (* Updating icon in timeline *)
    Js_utils.Manip.removeSelf step_img ;
    Js_utils.Manip.appendChildFirst step new_step_img in
  (* First highlight the title of the selected period *)
  begin
    match period_kind with
    | NProposal ->
      Js_utils.Manip.addClass (Js_utils.find_component "steps-one-name") "period-current" ;
    | NTesting_vote ->
      Js_utils.Manip.addClass (Js_utils.find_component "steps-two-name") "period-current" ;
    | NTesting ->
      Js_utils.Manip.addClass (Js_utils.find_component "steps-three-name") "period-current" ;
    | NPromotion_vote ->
      Js_utils.Manip.addClass (Js_utils.find_component "steps-four-name") "period-current" ;
  end ;
  (* Update all periods : icon, status, etc. *)
  begin match period_status with
    | [ one ; two ; three ; four ] ->
      update_step "steps-one" one ;
      update_step "steps-two" two ;
      update_step "steps-three" three ;
      update_step "steps-four" four
    | _ ->                      (* should never happen *)
      update_step "steps-one" VPS_current ;
      update_step "steps-two" VPS_wait ;
      update_step "steps-three" VPS_wait ;
      update_step "steps-four" VPS_wait
  end


let update_progress period period_kind cycle level period_status =
  let cst = Infos.constants ~cycle in
  let start_level = period * cst.blocks_per_voting_period + 1 in
  let end_level = (period + 1) * cst.blocks_per_voting_period in
  let period_position = level - start_level + 1 in
  let est_time =
    if end_level - level = 0 then ""
    else "~ " ^ (Format_date.time_before_level ~cst (end_level - level)) ^ " left" in
  let percent = period_position * 100 / cst.blocks_per_voting_period in
  let bar = Js_utils.find_component progress_bar_id in
  Js_utils.Manip.SetCss.width bar (Printf.sprintf "%d%%" percent) ;

  let percent_span = Printf.sprintf "%d%% %s" percent est_time in
  let bar_span = Js_utils.find_component bar_span_id in
  Js_utils.Manip.setInnerHtml bar_span percent_span;
  let start_level_div = Js_utils.find_component start_level_id in
  let end_level_div = Js_utils.find_component end_level_id in
  Js_utils.Manip.setInnerHtml start_level_div (string_of_int start_level);
  Js_utils.Manip.setInnerHtml end_level_div (string_of_int end_level);
  let title_span2 = Js_utils.find_component progress_title_id2 in
  update_timeline period_kind period_status;
  Js_utils.Manip.setInnerHtml title_span2
    (Tezos_utils.pp_voting_period_kind period_kind)


(* Panels *)

let period_proposals_to_rows
    (_, _, _, used_count, used_votes, unused_count, unused_votes) props =
  match props with
  | [] ->
    [| tr [ td ~a:[ a_colspan 5 ] [txt "No proposal for this period"] ] |]
  | props ->
    Array.of_list @@
    (List.map (fun {prop_hash; prop_count; prop_votes; prop_source; _} ->
         let link = match List.assoc_opt prop_hash !archive_links with
           | Some (name, path) -> Common.make_link ~crop_len:20 ~path name
           | None -> Common.txt_ () in
         tr [
           td [ Common.make_link ~crop_len:15 ~crop_limit:lg_size ~path:"votes"
                  ~args:["proposal", prop_hash; "vote_kind", "proposal"] prop_hash];
           Common.account_w_blockies ~crop_len:25 ~crop_limit:lg_size prop_source;
           td [ link ];
           td [ pcf "%d / %d" prop_count used_count ];
           td [ pcf "%d / %d" prop_votes used_votes ]
         ]) props) @
    [tr ~a:[ a_class ["info"] ] [
        td [ txt "Undecided"]; td []; td [];
        td [ pcf "%d" unused_count ];
        td [ pcf "%d" unused_votes ]]]

let proposals_to_rows = function
  | [] ->
    [ tr [ td ~a:[ a_colspan 4 ] [txt "No proposal in the blockchain"] ] ]
  | props ->
    let _, l = List.fold_left
        (fun acc {prop_period; prop_hash; prop_count; prop_votes; prop_source; _} ->
           let link = match List.assoc_opt prop_hash !archive_links with
             | Some (name, path) -> Common.make_link ~crop_len:20 ~path name
             | None -> Common.txt_ () in
           let row =
             if prop_period = fst acc then
               tr [
                 td [];
                 td [ Common.make_link ~crop_len:15 ~crop_limit:lg_size ~path:"votes"
                        ~args:["proposal", prop_hash; "vote_kind", "proposal"] prop_hash];
                 Common.account_w_blockies ~crop_len:15 ~crop_limit:lg_size prop_source;
                 td [ link ];
                 td [ pcf "%d" prop_count ];
                 td [ pcf "%d" prop_votes ]
               ]
             else
               tr [
                 td [ pcf "%d" prop_period ];
                 td [ Common.make_link ~crop_len:15 ~crop_limit:lg_size ~path:"votes"
                        ~args:["proposal", prop_hash; "vote_kind", "proposal"] prop_hash];
                 Common.account_w_blockies ~crop_len:15 ~crop_limit:lg_size prop_source;
                 td [ link ];
                 td [ pcf "%d" prop_count ];
                 td [ pcf "%d" prop_votes ]
               ] in
           prop_period, row :: (snd acc)
        ) (0, []) props in
    List.rev l

let ballots_to_rows voting_period prop_hash
    n_yay n_nay n_pass v_yay v_nay v_pass total_count total_votes =
  let n_total = n_yay + n_nay + n_pass in
  let v_total = v_yay + v_nay + v_pass in
  let args = ["proposal",prop_hash; "vote_kind", "ballot";
              "period", string_of_int voting_period] in
  [|
    tr [
      td [ Common.make_link ~crop_len:15 ~crop_limit:lg_size ~path:"votes"
             ~args:(("ballot", "Yay") :: args) "Yay" ];
      td [ pcf "%d / %d" n_yay n_total ];
      td [ pcf "%d / %d" v_yay v_total ];
      td [ pcf "%.2f %%" @@ (float v_yay) /. (float v_total) *. 100. ]
    ];
    tr [
      td [ Common.make_link ~crop_len:15 ~crop_limit:lg_size ~path:"votes"
             ~args:(("ballot", "Nay") :: args) "Nay" ];
      td [ pcf "%d / %d" n_nay n_total ];
      td [ pcf "%d / %d" v_nay v_total ];
      td [ pcf "%.2f %%" @@ (float v_nay) /. (float v_total) *. 100. ]
    ];
    tr [
      td [ Common.make_link ~crop_len:15 ~crop_limit:lg_size ~path:"votes"
             ~args:(("ballot", "Pass") :: args) "Pass" ];
      td [ pcf "%d / %d" n_pass n_total ];
      td [ pcf "%d / %d" v_pass v_total ];
      td [ pcf "%.2f %%" @@ (float v_pass) /. (float v_total) *. 100. ]
    ];
    tr ~a:[ a_class ["info"] ] [
      td [ txt "Undecided"];
      td [ pcf "%d" (total_count - n_total) ];
      td [ pcf "%d" (total_votes - v_total) ];
      td []]
  |]

let votes_to_rows ?(empty="No votes for this proposal") nvotes = function
  | [] -> [ tr [ td ~a:[ a_colspan 5 ] [txt empty] ] ]
  | props ->
    (tr ~a:[ a_class ["info"]] [
        td [ txt "Total" ]; td []; td [ pcf "%d" nvotes ]; td []; td []] ) ::
    (List.map
       (fun {prop_period; prop_op; prop_votes; prop_source; prop_ballot; _} ->
          let prop_op = Misc.unopt "" prop_op in
          let ballot =
            Misc.unoptf (Common.txt_ ())
              (fun x -> txt @@ Tezos_utils.string_of_ballot_vote x) prop_ballot in
          tr [
            td [ pcf "%d" prop_period ];
            Common.account_w_blockies ~crop_len:15 ~crop_limit:lg_size prop_source;
            td [ pcf "%d" prop_votes ];
            td [ ballot ];
            td [ Common.make_link ~crop_len:15 prop_op ];
          ]) props)

(* Pies *)

let update_proposal_pie props =
  let data = Array.of_list @@
    List.map (fun {prop_hash; prop_votes; _} ->
        Common.crop_hash ~crop_len:15 prop_hash,
        prop_votes) props in
  let balloon =  "[[title]]<br><span style='font-size:14px'><b>[[value]] upvotes</b> ([[percents]]%)</span>" in
  amcharts3_ready (fun () ->
      let pie = Amcharts3.pie () in
      let len = Array.length data in
      let max_len = 10 in
      let legend = Amcharts3.legend () in
      let data =
        if len <= max_len then data
        else
          let data2 = Array.make max_len ("",0) in
          Array.iteri (fun i (s, n) ->
              if i < max_len then data2.(i) <- (s, n)
              else
                data2.(max_len - 1) <- ("", n + (snd data2.(max_len - 1)))) data;
          data2
      in
      pie##dataProvider <- Amcharts3.Pie.dataProvider data;
      pie##outlineColor <- Js.string "#FFFFFF";
      pie##outlineAlpha <- 0.8;
      pie##outlineThickness <- 2;
      pie##balloonText <- Js.string balloon;
      pie##depth3D <- 15;
      pie##angle <- 30;
      pie##colors <- Js.array [|Js.string "#0000FF"; Js.string "#FF0000";
                                Js.string "#14e3b4"|];
      if Dom_html.window##screen##width <= 500 then
        begin
          pie##labelsEnabled <- false;
          legend##valueText <- Js.string "[[percents]]%";
          pie##legend <- legend
        end;
      pie##export <- Amcharts3.export ();
      pie##write (Js.string pie_proposals_id);
      let pie_div = Tyxml_js.To_dom.of_div @@ Js_utils.find_component pie_proposals_id in
      pie_div##style##width <- Js.string "100%";
      pie_div##style##height <- Js.string "350px";
    )

let update_ballot_pie v_yay v_nay v_pass =
  let data = [| "Yay", v_yay; "Nay", v_nay; "Pass", v_pass |] in
  let balloon =  "[[title]]<br><span style='font-size:14px'><b>[[value]] votes</b> ([[percents]]%)</span>" in
  let pie = Amcharts3.pie () in
  let legend = Amcharts3.legend () in
  pie##dataProvider <- Amcharts3.Pie.dataProvider data;
  pie##outlineColor <- Js.string "#FFFFFF";
  pie##outlineAlpha <- 0.8;
  pie##outlineThickness <- 2;
  pie##balloonText <- Js.string balloon;
  pie##depth3D <- 15;
  pie##angle <- 30;
  pie##colors <- Js.array [|Js.string "#0000FF"; Js.string "#FF0000";
                            Js.string "#14e3b4"|];
  if Dom_html.window##screen##width <= 500 then
    begin
      pie##labelsEnabled <- false;
      legend##valueText <- Js.string "[[percents]]%";
      pie##legend <- legend
    end;
  pie##export <- Amcharts3.export ();
  pie##write (Js.string pie_ballots_id);
  let pie_div = Tyxml_js.To_dom.of_div @@ Js_utils.find_component pie_ballots_id in
  pie_div##style##width <- Js.string "100%";
  pie_div##style##height <- Js.string "400px"

(* gauges *)

class type cyl_gauge = object
  method category : Js.js_string Js.t Js.prop
  method value1 : float Js.prop
  method value2 : float Js.prop
  method value3 : float Js.prop
  method value4 : float Js.prop
end

let supermajority_gauge v_yay v_nay =
  let chart : cyl_gauge Js.t Amcharts3_bis.serial Js.t =
    Amcharts3_bis.create_chart () in
  chart##depth3D <- 100;
  chart##angle <- 10;

  let casted = v_yay + v_nay in
  let supermajority = (casted * 8) / 10 in
  let supermajority_f = (float v_yay) /. (float casted) *. 100. in
  let approved_supermajority = v_yay >= supermajority in
  let grey, red, green, black = "#cdcdcd", "#ff0000", "#00ff00", "#000000" in
  let value1, value2, value3, value4, color1, color2, color3, color4,
      alpha2l, alpha3l, alpha2f, alpha3f, cw2, cw3 =
    if approved_supermajority then
      80., 0., supermajority_f -. 80., 100. -. supermajority_f,
      green, black, green, grey, 1., 0.8, 0., 0.8, 5., 1.
    else
      supermajority_f, 80. -. supermajority_f, 0., 20., red, grey, black, grey,
      0.2, 1., 0.2, 0., 1., 5. in

  let chart_data : cyl_gauge Js.t = Js.Unsafe.obj [||] in
  chart_data##value1 <- value1;
  chart_data##value2 <- value2;
  chart_data##value3 <- value3;
  chart_data##value4 <- value4;
  chart_data##category <- Js.string (Printf.sprintf "supermajority: %.2f%%" supermajority_f);
  chart##dataProvider <- Js.array [| chart_data |];
  chart##fontSize <- 15;

  let value_axis : Amcharts3_bis.valueAxis Js.t = Js.Unsafe.obj [||] in
  value_axis##stackType <- Js.string "100%";
  value_axis##gridAlpha <- 0.;
  chart##valueAxes <- Js.array [| value_axis |];

  let graph1 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph1##_type <- Js.string "column";
  graph1##topRadius <- 1;
  graph1##columnWidth <- 1.;
  graph1##showOnAxis <- true;
  graph1##lineThickness <- 0;
  graph1##lineAlpha <- 0.8;
  graph1##lineColor <- Js.string color1;
  graph1##fillColors <- Js.array [| Js.string color1 |];
  graph1##fillAlphas <- Js.array [|0.8|];
  graph1##valueField <- Js.string "value1";
  graph1##labelText <- Js.string "votes";
  graph1##balloonText <- Js.string (Printf.sprintf "%.2f %%" supermajority_f);

  let graph2 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph2##_type <- Js.string "column";
  graph2##topRadius <- 1;
  graph2##columnWidth <- cw2;
  graph2##showOnAxis <- true;
  graph2##lineThickness <- 5;
  graph2##lineAlpha <- alpha2l;
  graph2##lineColor <- Js.string color2;
  graph2##fillColors <- Js.array [|Js.string color2|];
  graph2##fillAlphas <- Js.array [|alpha2f|];
  graph2##valueField <- Js.string "value2";
  graph2##labelText <- Js.string "80% threshold";
  graph2##showBalloon <- false;
  graph2##labelPosition <- Js.string "top";

  let graph3 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph3##_type <- Js.string "column";
  graph3##topRadius <- 1;
  graph3##columnWidth <- cw3;
  graph3##showOnAxis <- true;
  graph3##lineThickness <- 0;
  graph3##lineAlpha <- alpha3l;
  graph3##lineColor <- Js.string color3;
  graph3##fillColors <- Js.array [|Js.string color3|];
  graph3##fillAlphas <- Js.array [|alpha3f|];
  graph3##valueField <- Js.string "value3";
  graph3##showBalloon <- false;

  let graph4 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph4##_type <- Js.string "column";
  graph4##topRadius <- 1;
  graph4##columnWidth <- 1.;
  graph4##showOnAxis <- true;
  graph4##lineThickness <- 0;
  graph4##lineAlpha <- 0.2;
  graph4##lineColor <- Js.string color4;
  graph4##fillColors <- Js.array [|Js.string color4|];
  graph4##fillAlphas <- Js.array [|0.2|];
  graph4##valueField <- Js.string "value4";
  graph4##showBalloon <- false;

  chart##graphs <- Js.array [| graph1; graph2; graph3; graph4|];
  chart##categoryField <- Js.string "category";

  let category_axis : Amcharts3_bis.categoryAxis Js.t = Js.Unsafe.obj [||] in
  category_axis##axisAlpha <- 0.;
  category_axis##labelOffset <- 40;
  category_axis##gridAlpha <- 0.;
  chart##categoryAxis <- category_axis;

  chart##export <- Amcharts3_bis.export ();

  chart##write(Js.string gauge_supermajority_id);
  let content = Js_utils.find_component gauge_supermajority_id in
  Js_utils.Manip.SetCss.height content "400px";
  Js_utils.Manip.SetCss.width content "320px";
  Js_utils.Manip.SetCss.margin content "0 auto"

let quorum_gauge expected_quorum total_votes v_yay v_nay v_pass =
  let chart : cyl_gauge Js.t Amcharts3_bis.serial Js.t =
    Amcharts3_bis.create_chart () in
  chart##depth3D <- 100;
  chart##angle <- 10;

  let casted = v_yay + v_nay in
  let actual = casted + v_pass in
  let actual_quorum = (actual * 10000) / total_votes in
  let actual_quorum_f = (float actual_quorum) /. 100. in
  let expected_quorum_f = (float expected_quorum) /. 100. in
  let approved_quorum = actual_quorum >= expected_quorum in
  let grey, red, green, black = "#cdcdcd", "#ff0000", "#00ff00", "#000000" in
  let value1, value2, value3, value4, color1, color2, color3, color4,
      alpha2l, alpha3l, alpha2f, alpha3f, cw2, cw3 =
    if approved_quorum then
      expected_quorum_f, 0., actual_quorum_f -. expected_quorum_f, 100. -. actual_quorum_f,
      green, black, green, grey, 1., 0.8, 0., 0.8, 5., 1.
    else
      actual_quorum_f, expected_quorum_f -. actual_quorum_f, 0., 100. -. expected_quorum_f, red, grey, black, grey,
      0.2, 1., 0.2, 0., 1., 5. in

  let chart_data : cyl_gauge Js.t = Js.Unsafe.obj [||] in
  chart_data##value1 <- value1;
  chart_data##value2 <- value2;
  chart_data##value3 <- value3;
  chart_data##value4 <- value4;
  chart_data##category <- Js.string (Printf.sprintf "quorum: %.2f %%" actual_quorum_f);
  chart##dataProvider <- Js.array [| chart_data |];
  chart##fontSize <- 15;

  let value_axis : Amcharts3_bis.valueAxis Js.t = Js.Unsafe.obj [||] in
  value_axis##stackType <- Js.string "100%";
  value_axis##gridAlpha <- 0.;
  chart##valueAxes <- Js.array [| value_axis |];

  let graph1 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph1##_type <- Js.string "column";
  graph1##topRadius <- 1;
  graph1##columnWidth <- 1.;
  graph1##showOnAxis <- true;
  graph1##lineThickness <- 0;
  graph1##lineAlpha <- 0.8;
  graph1##lineColor <- Js.string color1;
  graph1##fillColors <- Js.array [| Js.string color1 |];
  graph1##fillAlphas <- Js.array [|0.8|];
  graph1##valueField <- Js.string "value1";
  graph1##labelText <- Js.string "votes";
  graph1##balloonText <- Js.string (Printf.sprintf "%.2f" actual_quorum_f);

  let graph2 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph2##_type <- Js.string "column";
  graph2##topRadius <- 1;
  graph2##columnWidth <- cw2;
  graph2##showOnAxis <- true;
  graph2##lineThickness <- 5;
  graph2##lineAlpha <- alpha2l;
  graph2##lineColor <- Js.string color2;
  graph2##fillColors <- Js.array [|Js.string color2|];
  graph2##fillAlphas <- Js.array [|alpha2f|];
  graph2##valueField <- Js.string "value2";
  graph2##labelText <- Js.string (Printf.sprintf "%.2f%% threshold" expected_quorum_f);
  graph2##showBalloon <- false;
  graph2##labelPosition <- Js.string "top";

  let graph3 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph3##_type <- Js.string "column";
  graph3##topRadius <- 1;
  graph3##columnWidth <- cw3;
  graph3##showOnAxis <- true;
  graph3##lineThickness <- 0;
  graph3##lineAlpha <- alpha3l;
  graph3##lineColor <- Js.string color3;
  graph3##fillColors <- Js.array [|Js.string color3|];
  graph3##fillAlphas <- Js.array [|alpha3f|];
  graph3##valueField <- Js.string "value3";
  graph3##showBalloon <- false;

  let graph4 : Amcharts3_bis.graph Js.t = Js.Unsafe.obj [||] in
  graph4##_type <- Js.string "column";
  graph4##topRadius <- 1;
  graph4##columnWidth <- 1.;
  graph4##showOnAxis <- true;
  graph4##lineThickness <- 0;
  graph4##lineAlpha <- 0.2;
  graph4##lineColor <- Js.string color4;
  graph4##fillColors <- Js.array [|Js.string color4|];
  graph4##fillAlphas <- Js.array [|0.2|];
  graph4##valueField <- Js.string "value4";
  graph4##showBalloon <- false;

  chart##graphs <- Js.array [| graph1; graph2; graph3; graph4|];
  chart##categoryField <- Js.string "category";

  let category_axis : Amcharts3_bis.categoryAxis Js.t = Js.Unsafe.obj [||] in
  category_axis##axisAlpha <- 0.;
  category_axis##labelOffset <- 40;
  category_axis##gridAlpha <- 0.;
  chart##categoryAxis <- category_axis;

  chart##export <- Amcharts3_bis.export ();

  chart##write(Js.string gauge_quorum_id);
  let content = Js_utils.find_component gauge_quorum_id in
  Js_utils.Manip.SetCss.height content "400px";
  Js_utils.Manip.SetCss.width content "320px";
  Js_utils.Manip.SetCss.margin content "0 auto"

let update_proposals total_info props =
  Js_utils.hide (Js_utils.find_component ballots_panel_id);
  if props <> [] then update_proposal_pie props;
  let (nb_proposals, _, _, _, _, _, _) = total_info in
  PeriodProposalsPanel.paginate_all
    (period_proposals_to_rows total_info props);
  PeriodProposalsPanel.update_title [ Panel.title_nb s_proposals nb_proposals ];
  Js_utils.show (Js_utils.find_component period_proposals_panel_id)

let update_all_proposals ~nrows xhr =
  ProposalsPanel.paginate_fun ~nrows proposals_to_rows xhr

let update_quorum expected_quorum total_votes v_yay v_nay v_pass =
  let casted = v_yay + v_nay in
  let actual = casted + v_pass in
  let actual_quorum = (actual * 10000) / total_votes in
  let actual_quorum_f = (float actual_quorum) /. 100. in
  let expected_quorum_f = (float expected_quorum) /. 100. in
  let supermajority = (casted * 8) / 10 in
  let supermajority_f = (float v_yay) /. (float casted) *. 100. in
  let updated_quorum = (8 * expected_quorum + 2 * actual_quorum) / 10 in
  let updated_quorum_f = (float updated_quorum) /. 100. in
  let approved_quorum = actual_quorum >= expected_quorum in
  let color_quorum = if approved_quorum then green else red in
  let approved_supermajority = v_yay >= supermajority in
  let color_supermajority = if approved_supermajority then green else red in
  let str_result =
    if approved_supermajority && approved_quorum then "Proposal passes "
    else "Proposal doesn't pass " in
  let content =
    make_panel ()
      ~panel_body_content:[
        h4 ~a:[ a_class [row] ] [
          span ~a:[ a_class [color_quorum; cxs4; cxsoffset1; text_center] ] [
            Common.txtf "( Actual quorum = %.2f %% ) " actual_quorum_f;
            entity "ge"; Common.txtf " ( Expected quorum = %.2f %% )" expected_quorum_f ];
          span ~a:[ a_class [cxs2; text_center] ] [ txt " and " ];
          span ~a:[ a_class [color_supermajority; cxs4; text_center] ] [
            Common.txtf "( Supermajority = Yay / (Yay + Nay)  = %.2f %% ) " supermajority_f;
            entity "ge"; txt " 80 %" ] ];
        h4 ~a:[ a_class [text_center; blue]] [
          txt str_result; entity "rarr";
          Common.txtf " Next quorum %.2f %%" updated_quorum_f ]
      ] in
  Js_utils.Manip.replaceChildren (Js_utils.find_component ballots_quorum_id) [content]

let update_ballots voting_period quorum (total_count, total_votes)
    (proposal_hash, n_yay, n_nay, n_pass, v_yay, v_nay, v_pass) =
  Js_utils.hide (Js_utils.find_component period_proposals_panel_id);
  if v_yay + v_nay + v_pass <> 0 then
    amcharts3_ready (fun () ->
        update_ballot_pie v_yay v_nay v_pass;
        supermajority_gauge v_yay v_nay;
        quorum_gauge quorum total_votes v_yay v_nay v_pass);
  BallotsPanel.paginate_all @@
  ballots_to_rows voting_period proposal_hash
    n_yay n_nay n_pass v_yay v_nay v_pass total_count total_votes;
  BallotsPanel.update_title [
    span [ txt "Ballots for " ];
    span [ txt @@ Common.crop_hash ~crop_len:20 proposal_hash] ];
  Js_utils.show (Js_utils.find_component ballots_panel_id)

let update_testing hash =
  let main_div = Js_utils.find_component proposal_main_id in
  let testing_content = [
    txt @@ Printf.sprintf "Testing of proposal %s currently in testing" hash
  ] in
  Js_utils.Manip.replaceChildren main_div testing_content

let update_votes ?empty ~nrows nvotes xhr hash =
  VotesPanel.paginate_fun ~nrows (votes_to_rows ?empty nvotes) xhr;
  VotesPanel.update_title [Panel.title_nb (ss_ ("Votes for " ^ hash)) nrows];

(* Account Panel *)

module AccountVotesPanel =
  Panel.MakePageTable(struct
    let name = "account-votes"
    let theads () = tr [
        th [txt_t s_operation_hash];
        th [txt_t s_voting_period];
        th [txt_t s_kind ];
        th [txt_t s_proposal_hash];
        th [txt_t s_rolls];
        th [txt_t s_ballot];
      ]
    let page_size = 20
    let table_class = "default-table"
    let title_span nb = Panel.title_nb s_votes nb
  end)

let account_votes_to_rows vts =
  match vts with
  | [] ->
    [ tr [ td ~a:[ a_colspan 5 ] [ txt "No votes for this account" ] ] ]
  | vts ->
    List.map (fun prop ->
        let ballots = match prop.prop_period_kind with
          | NTesting_vote | NPromotion_vote
            -> txt @@ Tezos_utils.string_of_ballot_vote @@
            Tezos_utils.ballot_of_int prop.prop_count
          | _ -> Common.txt_ () in
        let operation =
          Misc.unoptf (Common.txt_ ())
            (Common.make_link ~crop_len:15) prop.prop_op in
        let voting_period_str = string_of_int prop.prop_period in
        let voting_period =
          Common.make_link ~path:"proposals" ~args:["period", voting_period_str]
            voting_period_str in

        tr [
          td [ operation ];
          td [ voting_period ];
          td [ txt @@ Tezos_utils.string_of_voting_period_kind prop.prop_period_kind ];
          td [ txt @@ Common.crop_hash ~crop_len:15 prop.prop_hash ];
          td [ txt @@ string_of_int prop.prop_votes ];
          td [ ballots ] ]
      ) vts

let vote_graphs chart_id proposals ballots =
  let data =
    (List.map (fun (period, count, rolls) ->
         period, [| "proposal", count * rolls |]) proposals) @
    (List.map (fun (period, count, rolls) ->
         period, [| "ballot", count * rolls |]) ballots) in
  let data =
    List.sort (fun (period1,_) (period2,_) -> compare period1 period2) data in
  let data = Array.of_list @@
    List.map (fun (period, a) -> string_of_int period, a) data in
  amcharts3_ready (fun () ->
      let chart = Amcharts3.serialN () in
      chart##dataProvider <- Amcharts3.DataItem.dataProvider data;
      chart##marginLeft <- 10;

      let leftAxis = Amcharts3.valueAxis() in
      leftAxis##axisAlpha <- 0.;
      leftAxis##inside <- false;
      leftAxis##dashLength <- 3;
      leftAxis##title <- Js.string "Proposals";
      leftAxis##titleColor <- Js.string "#0000ff";
      leftAxis##color <- Js.string "#0000ff";
      leftAxis##position <- Js.string "left";
      chart##addValueAxis(leftAxis);

      let rightAxis = Amcharts3.valueAxis() in
      rightAxis##axisAlpha <- 0.;
      rightAxis##inside <- false;
      rightAxis##dashLength <- 3;
      rightAxis##title <- Js.string "Ballots";
      rightAxis##titleColor <- Js.string "#ff0000";
      rightAxis##color <- Js.string "#ff0000";
      rightAxis##position <- Js.string "right";
      chart##addValueAxis(rightAxis);

      let graph1 = Amcharts3.graphN "proposal" "smoothedLine" in
      graph1##lineColor <- Js.string "#ff0000";
      graph1##negativeLineColor <- Js.string "#00ffff";
      graph1##lineThickness <- 2;
      graph1##balloonText <- Js.string "[[category]]<br><b><span style='font-size:14px;'>[[price_usd]]</span></b>";
      graph1##title <- Js.string "votes proposals";
      graph1##valueAxis <- rightAxis;
      chart##addGraph( graph1 );

      let graph2 = Amcharts3.graphN "ballot" "smoothedLine" in
      graph2##lineColor <- Js.string "#0000ff";
      graph2##negativeLineColor <- Js.string "#ffff00";
      graph2##lineThickness <- 2;
      graph2##balloonText <- Js.string "[[category]]<br><b><span style='font-size:14px;'>[[price_btc]]</span></b>";
      graph2##title <- Js.string "votes ballots";
      graph2##valueAxis <- leftAxis;
      chart##addGraph( graph2 );

      chart##export <- Amcharts3.export ~divId:(Js.string "export-div") ();

      let legend = Amcharts3.legend () in
      legend##useGraphSettings <- true;
      legend##valueAlign <- Js.string "left";
      legend##valueWidth <- 200;
      chart##legend <- legend;

      let chartCursor = Amcharts3.chartCursor () in
      chartCursor##cursorAlpha <- 0;
      chartCursor##cursorPosition <- Js.string "mouse";
      chart##addChartCursor(chartCursor);

      let chartScrollbar = Amcharts3.chartScrollbar() in
      chart##addChartScrollbar(chartScrollbar);
      chart##creditsPosition <- Js.string "bottom-right";
      chart##write (Js.string chart_id);

      let div = Tyxml_js.To_dom.of_div @@ Js_utils.find_component chart_id in
      div##style##width <- Js.string "100%";
      div##style##height <- Js.string "350px"
    )

let update_account_votes ~nrows xhr =
  AccountVotesPanel.paginate_fun ~nrows account_votes_to_rows xhr
