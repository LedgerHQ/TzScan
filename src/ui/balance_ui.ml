open Tyxml_js.Html5
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Data_types
open Tezos_types
open Text
open Lang
open Bootstrap_helpers.Color
open Bootstrap_helpers.Icon
open Common

let chart_id = "balance-chart-id"

let charts_div_id hash = Common.make_id "balance-charts-div-id" hash
let prediction_div_id hash = Common.make_id "balance-prediction-div-id" hash
let snapshot_div_id hash = Common.make_id "balance-snapshot-div-id" hash

let predict_diff
      (bal_list : (int32 * balance) list)
      nb_bak
      nb_end
      current_cycle
      current_level
      (upcoming_unfrozen : balance) =
  let cst = Infos.constants ~cycle:current_cycle in
  let config = Infos.api.api_config in
  let baking_deposits =
    Int64.(mul
             (Infos.rampup ~cycle:current_cycle cst.block_security_deposit)
             (of_int nb_bak))
  and baking_rewards =
    if current_cycle < config.conf_start_reward_cycle
    then 0L
    else
      Int64.(mul cst.block_reward (of_int nb_bak))

  and end_deposits =
    Int64.(mul
             (Infos.rampup ~cycle:current_cycle cst.endorsement_security_deposit)
             (of_int nb_end))
  and end_rewards =
    if current_cycle < config.conf_start_reward_cycle
    then 0L
    else
      Int64.(mul cst.endorsement_reward (of_int nb_end))
  in

  let upcoming_loss = Int64.add baking_deposits end_deposits in
  let (++) = Int64.add in

  let b_spendable = Int64.sub upcoming_unfrozen.b_frozen upcoming_loss
  and b_frozen = Int64.(sub (upcoming_loss ++ baking_rewards ++ end_rewards) upcoming_unfrozen.b_frozen)
  and b_rewards = Int64.(sub (baking_rewards ++ end_rewards)  upcoming_unfrozen.b_rewards)
  and b_deposits = Int64.(sub upcoming_loss upcoming_unfrozen.b_deposits)
  and b_fees = (* Mean of the n<=5 previous fee diffs *)
    let mean_lvl = 5 in
    let rec __mean sum mean (bal_hist : (int32 * balance) list) =
      if mean = mean_lvl then Int64.div sum (Int64.of_int mean)
      else match bal_hist with
             [] -> 0L
           | (_,bal) :: [] ->
               Int64.div
                 (Int64.add sum bal.b_fees)
                 (Int64.of_int (mean+1))
           | (_,curr) :: ((_,prev) as hd) :: tl ->
              __mean (Int64.add sum (Int64.sub curr.b_fees prev.b_fees)) (mean+1) (hd :: tl) in

    let mean_on_a_cycle = (__mean 0L 0 bal_list)
    in
    let reported_on_the_rest_of_the_cycle =
      let remaining_blocks = cst.blocks_per_cycle - current_level mod cst.blocks_per_cycle in
      Int64.(div (mul mean_on_a_cycle (of_int remaining_blocks)) (of_int cst.blocks_per_cycle))
    in
    let current_fees = (snd @@ List.hd bal_list).b_fees
    in
    if Int64.compare (Int64.add current_fees reported_on_the_rest_of_the_cycle) Int64.zero < 0
    then Int64.neg current_fees
    else
    reported_on_the_rest_of_the_cycle
  in
  {b_spendable;b_frozen;b_rewards;b_fees;b_deposits}

let chart_style = "chart-style"

let fopt f = function
  | Some e -> f e
  | None -> ()

let currency_buttons _hash =

  let tezos_button_action _ =
    List.iter hide @@ Js_utils.Manip.by_class "bal-dollar";
    List.iter show @@  Js_utils.Manip.by_class "bal-tz";

    fopt hide @@ Js_utils.Manip.by_id "tezos-button-disact";
    fopt show @@ Js_utils.Manip.by_id "tezos-button-act";
    fopt hide @@ Js_utils.Manip.by_id "dollar-button-act";
    fopt show @@ Js_utils.Manip.by_id "dollar-button-disact";
    true
  and dollar_button_action _ =
    List.iter hide @@ Js_utils.Manip.by_class "bal-tz";
    List.iter show @@  Js_utils.Manip.by_class "bal-dollar";

    fopt hide @@ Js_utils.Manip.by_id "dollar-button-disact";
    fopt show @@ Js_utils.Manip.by_id "dollar-button-act";
    fopt hide @@ Js_utils.Manip.by_id "tezos-button-act";
    fopt show @@ Js_utils.Manip.by_id "tezos-button-disact";
    true
  in
  let tezos_button_activated =
    button
      ~a:[
        a_class ["btn"; "btn-primary"];
        a_id "tezos-button-act";
      ]
      [Tez.icon ()]

  and tezos_button_disactivated =
    button
      ~a:[
        a_class ["btn"; "btn-default"];
        a_id "tezos-button-disact";
        a_onclick tezos_button_action
      ]
      [Tez.icon ()]
  and dollar_button_activated =
    button
      ~a:[
        a_class ["btn"; "btn-primary"];
        a_id "dollar-button-act";
      ]
      [Tez.dollar ()]
  and dollar_button_disactivated =
    button
      ~a:[
        a_class ["btn"; "btn-default"];
        a_id "dollar-button-disact";
        a_onclick dollar_button_action
      ]
      [Tez.dollar ()]
  in

  hide dollar_button_activated;
  hide tezos_button_disactivated;

  (span [tezos_button_activated;tezos_button_disactivated],
   span [dollar_button_activated; dollar_button_disactivated])

let rec update_chart_div ~splitted hash rev_balances predictions =
  let splitted_button_action _ =
    if splitted then true (* Do nothing *)
    else
      begin
        update_chart_div ~splitted:true hash rev_balances predictions;
        true
      end
  and merged_button_action _ =
    if not splitted then true (* Do nothing *)
    else
      begin
        update_chart_div ~splitted:false hash rev_balances predictions;
        true
      end
  in
  let splitted_button =
    button
      ~a:[a_class ["btn"; (if splitted then "btn-primary" else "btn-default")];
          a_id "splitted-button";
          a_onclick splitted_button_action
         ]
      [Bootstrap_helpers.Icon.chart_up ();pcdata " / "; Bootstrap_helpers.Icon.chart_down ()]
  in
  let merged_button =
    button
      ~a:[a_class ["btn";(if not splitted then "btn-primary" else "btn-default")];
          a_id "merged-button";
          a_onclick merged_button_action
         ]
      [Bootstrap_helpers.Icon.chart_up ();pcdata "+"; Bootstrap_helpers.Icon.chart_down ()]
  in
  let container = find_component @@ charts_div_id hash in
  let chart_buttons =
       div ~a:[a_class [row; "bal_currency_merge_button_div"]]
         [div ~a:[a_class [clgoffset4;clg2;cmdoffset4;cmd2;csmoffset4;csm2;cxsoffset3;cxs3;"center"]]
            [span ~a:[a_class ["clear-float"; "center"]] [pcdata @@ Lang.t_ s_merged_charts];
             br () ;
             merged_button];
          div ~a:[a_class [clg2;cmd2;csm2;cxs3;"center"]]
            [span ~a:[a_class ["clear-float";"center"]] [pcdata @@ Lang.t_ s_splitted_charts];
             br () ;
             splitted_button]]
  in
  if rev_balances = [] then
    Js_utils.hide container
  else
    let data = Balance_charts_ui.balances_to_data rev_balances predictions in
    let to_update_content =
      if splitted
      then
        div ~a:[ a_class [ panel_body ] ] [
          h3 [pcdata @@ Lang.t_ s_balance_evolution];
          div ~a:[ a_class [ row ] ] [
            div ~a:[ a_id (chart_id^"-spendable"); a_class [ "balance-charts" ] ] [ ]
          ];
          div ~a:[ a_class [ row ] ] [
            div ~a:[ a_id (chart_id^"-frozen"); a_class [ "balance-charts" ] ] [ ]
          ];
          div ~a:[ a_class [ row ] ] [chart_buttons]
        ]
      else
        div ~a:[ a_class [ panel_body ] ] [
          h3 [pcdata @@ Lang.t_ s_balance_evolution];
          div ~a:[ a_class [ row ] ] [
            div ~a:[ a_id chart_id; a_class [ "balance-charts" ] ] [ ]
          ];
          div ~a:[ a_class [ row ] ] [chart_buttons];
        ]
    in
    Manip.removeChildren container ;
    Manip.appendChild container to_update_content ;
    Balance_charts_ui.make_chart
      splitted
      chart_id
      data
    ;
    let resize_div div size =
      Manip.SetCss.height div size in
    if splitted then
      let div = find_component (chart_id^"-spendable") in
      resize_div div "400px";
      let div = find_component (chart_id^"-frozen") in
      resize_div div "500px"
    else
      let div = find_component chart_id in
      resize_div div "600px"

let make_chart hash =
  div ~a:[ a_class [ row ] ] [
    div ~a:[ a_id @@ charts_div_id hash; a_class [ panel; panel_primary ] ] [
      div ~a:[ a_class [ panel_body ] ] [ ]
    ]
  ]

let update_cycle_snapshot ?price_usd hash rev_balances predictions =

  let dollar_val v =
    match price_usd with
      None -> None
    | Some change ->
      let fprice_usd = float_of_string change in
      Some (Int64.of_float (fprice_usd *. (Int64.to_float v)))
  in
  let bal_zero =
    {b_spendable = Int64.zero;
     b_frozen =  Int64.zero;
     b_rewards =  Int64.zero;
     b_fees =  Int64.zero;
     b_deposits =  Int64.zero}
  in
  let curr_cycle,prev_balance =
    match rev_balances with
      [] | _ :: [] ->
            Int32.zero, bal_zero

      | (c,_) :: (_,bal) :: _ -> c,bal
  in

  let arrow title v diff last_bal prediction classes =
    (*let prediction = Int64.add diff prediction in*)
    let arrow_style diff =
        if diff = Int64.zero then ("→ ","blue") else
          if Int64.compare diff Int64.zero < 0 then "↘ ","red"
          else "↗ ","green" in
    let arrow,color = arrow_style diff in
    let arrow_predict,color_predict = arrow_style prediction in
    let divs_abs_val =
      let vals_for_v = dollar_val v in
      let tz_div =
        h4 ~a:[a_class ["bal-tz"]] [Tez.pp_amount ~precision:3 ~icon:Tez.icon v];
      in
      match vals_for_v with
        None -> [tz_div]
      | Some dollar_val ->
        (h4 ~a:[a_class ["bal-dollar"]]
          [Tez.pp_amount ~precision:3 ~icon:Tez.dollar dollar_val]) :: [tz_div];

    and divs_diff =
      let vals_for_diff = dollar_val diff
      in
      let tz_div =
        (if diff <> Int64.zero then h4 else div)
          ~a:[a_class [color; "bal_snapshot_main_diff"; "bal-tz"]]
          [pcdata @@ arrow; Tez.pp_amount ~precision:3 ~icon:Tez.icon diff];
      in
      match vals_for_diff with
        None -> [tz_div]
      | Some dollar_val ->
        ((if diff <> Int64.zero then h4 else div)
          ~a:[a_class [color; "bal_snapshot_main_diff"; "bal-dollar"]]
          [pcdata @@ arrow; Tez.pp_amount ~precision:3 ~icon:Tez.dollar dollar_val]) :: [tz_div]
    and div_transition =
      div [pcdata ((Lang.t_ s_estimated_at_the_end_of_cycle) ^ " " ^ Int32.to_string curr_cycle)]

    and divs_prediction =
      let vals_for_pred = dollar_val (Int64.add last_bal prediction) in
      let div_tz =
        div
          ~a:[a_class ["bal-tz"]]
          [Tez.pp_amount ~precision:3 ~icon:Tez.icon @@ Int64.add last_bal prediction] in
      match vals_for_pred with
        None -> [div_tz]
      | Some dollar_val ->
        div
          ~a:[a_class ["bal-dollar"]]
          [Tez.pp_amount ~precision:3 ~icon:Tez.dollar dollar_val] ::
        [div_tz]

    and divs_diff_predictions =
      let vals_for_pred_diff = dollar_val prediction
      in
      let div_tz =
       div ~a:[a_class [color_predict;"bal-tz"]]
         [pcdata @@ arrow_predict; Tez.pp_amount ~precision:3 ~icon:Tez.icon prediction];
      in
      match vals_for_pred_diff with
        None -> [div_tz]
      | Some dollar_val ->
       (div ~a:[a_class [color_predict;"bal-dollar"]]
         [pcdata @@ arrow_predict; Tez.pp_amount ~precision:3 ~icon:Tez.dollar dollar_val]) ::
       [div_tz] in

    div ~a:[a_class (classes @ ["center";panel_body])]
      ([h3 ([pcdata title])]
    @ divs_abs_val
    @ divs_diff
    @ [div_transition]
    @ divs_prediction
    @ divs_diff_predictions) in
  let align_classes_evaluated_balance =
    [clgoffset1;cmdoffset1;csmoffset1;cxsoffset1;clg10;cmd10;csm10;cxs10]
  and align_classes_spendable =
    [clgoffset1;cmdoffset1;csmoffset0;cxsoffset0;clg2;cmd2;csm6;cxs12]
  and align_classes_frozen =
    [clg2;cmd2;csm6;cxs12]
  and align_classes_rewards =
    [clg2;cmd2;csm4;cxs12]
  and align_classes_other =
    [clg2;cmd2;csm4;cxs12] in

  let help =
    span
      ~a:[a_class [clg1;cmd1;csm1;cxs1;"bal_snapshot_main_diff"]]
      [Glossary_doc.help Glossary_doc.HBalance_Snapshot] in
  let compo_row bal diff =
    [
      arrow
        (Lang.t_ s_evaluated_balance)
        (Int64.add bal.b_spendable bal.b_frozen)
        (Int64.add diff.b_spendable diff.b_frozen)
        (Int64.add prev_balance.b_spendable prev_balance.b_frozen)
        (Int64.add predictions.b_spendable predictions.b_frozen)
        align_classes_evaluated_balance;
      help;
      arrow
        (Lang.t_ s_balance)
        bal.b_spendable
        diff.b_spendable
        prev_balance.b_spendable
        predictions.b_spendable
        align_classes_spendable
     ;
     arrow
       (Lang.t_ s_frozen)
       bal.b_frozen
       diff.b_frozen
       prev_balance.b_frozen
       predictions.b_frozen
       align_classes_frozen;
     arrow
       (Lang.t_ s_rewards)
       bal.b_rewards
       diff.b_rewards
       prev_balance.b_rewards
       predictions.b_rewards
       align_classes_rewards;
     arrow
       (Lang.t_ s_fees)
       bal.b_fees
       diff.b_fees
       prev_balance.b_fees
       predictions.b_fees
       align_classes_other;
     arrow
       (Lang.t_ s_deposits)
       bal.b_deposits
       diff.b_deposits
       prev_balance.b_deposits
       predictions.b_deposits
       align_classes_other;
   ]
  in
  let content =
    match rev_balances with
      [] ->
       compo_row
         bal_zero
         bal_zero

    | (_,bal) :: [] ->
       compo_row bal bal

    | (_,curr) :: (_,prev) :: _ ->
       compo_row
         curr
         {b_spendable = Int64.sub curr.b_spendable prev.b_spendable;
          b_frozen = Int64.sub curr.b_frozen prev.b_frozen;
          b_rewards = Int64.sub curr.b_rewards prev.b_rewards;
          b_fees = Int64.sub curr.b_fees prev.b_fees;
          b_deposits = Int64.sub curr.b_deposits prev.b_deposits;
         }

  in
  let div_content =
    div content in

  let container = find_component @@ snapshot_div_id hash in
  Manip.appendChild container div_content;

  let to_hide =
    Js_utils.Manip.by_class "bal-dollar"
  in
  List.iter hide to_hide

let update_balance_ui
    ?price_usd hash bal_hist curr_cycle curr_level bakes ends upcoming_unfrozen =
  let rev_hist = List.rev bal_hist in
  let prediction =
    predict_diff rev_hist bakes ends curr_cycle curr_level upcoming_unfrozen in

  update_chart_div ~splitted:false hash bal_hist prediction;
  update_cycle_snapshot ?price_usd hash rev_hist prediction;
  match price_usd with
    None -> fopt hide (Js_utils.Manip.by_id "currency-selector")
  | _ -> ()

let make_snapshot hash =
  let buttons_div =
    let tezos_button,dollar_button = currency_buttons hash in
      div ~a:[a_class [row]]
         [div ~a:[a_class [clg12;cmd12;csm12;cxs12;"center";"bal_snapshot_main_diff"]]
            [pcdata @@ Lang.t_ s_currency];
          div ~a:[a_class [clgoffset5;clg1;cmdoffset5;cmd1;csmoffset5;csm1;cxsoffset4;cxs2;"center"]]
            [tezos_button];
          div ~a:[a_class [clg1;cmd1;csm1;cxs2;"center"]] [dollar_button]
         ];
       in

  div ~a:[a_class [panel;panel_primary]]
   [div ~a:[ a_id @@ snapshot_div_id hash; a_class [ row ] ] [
   ];
    div ~a:[a_id "currency-selector"] [buttons_div]
   ]

let frozen itis =
  if itis
  then
    [(span ~a:[a_class [blue]; a_title @@ t_ s_frozen]
        [Bootstrap_helpers.Icon.snowflake (); pcdata " "])]
  else []

let endorsement itis =
  if itis
  then [(span ~a:[a_title @@ t_ s_endorsements]
           [Bootstrap_helpers.Icon.thumb_up (); pcdata " "])] else []
(* Yes, the logo has a translation *)
let bake itis = if itis then [(span ~a:[a_title @@ t_ s_bakes] [entity @@ t_ s_icon_bake; pcdata " "])] else []
let transaction itis =
  if itis then [(span ~a:[a_class [red]; a_title @@ t_ s_transactions] [Tez.icon (); pcdata " "])] else []

module BalancePanel =
  struct
    include
      Panel.MakePageTable(
          struct
            let title_span =
              Panel.title_nb s_balance_updates_last
                ~help:Glossary_doc.HBalance_Updates
            let table_class = "transactions-table"
            let page_size = 20
            let name = "balance_updates_metadata"
            let theads () =
              tr [
                  th ~a:[ a_class [ cxs1 ] ] @@ [span [pcdata @@ t_ s_cycle]];
                  th ~a:[ a_class [ cxs1 ] ] @@ cl_icon cube_icon (t_ s_level);
                  th ~a:[ a_class [ cxs1 ] ] @@ cl_icon clock_icon (t_ s_date);
                  th ~a:[ a_class [ cxs1 ] ] @@ cl_icon Tez.icon (t_ s_diff);
                  th ~a:[ a_class [ cxs1 ] ] @@ cl_icon exchange_icon (t_ s_type_of_update);
                  th ~a:[ a_class [ cxs1 ] ] @@ [span [pcdata @@ s_ ""]];
                ]
          end)
    let make_legend () =
      div [p ((frozen true) @ [pcdata @@ Printf.sprintf ": %s" @@ t_ s_frozen_balance_updates]);
           p ((endorsement true) @ [pcdata @@  Printf.sprintf ": %s" @@ t_ s_endorsements]);
           p ((bake true) @ [pcdata @@  Printf.sprintf ": %s" @@ t_ s_bakes]);
           p ((transaction true) @ [pcdata @@  Printf.sprintf ": %s" @@ t_ s_transactions]);]
  end

let make_balance_updates_table ?price_usd b_list =

  let extra_info bu =
    (frozen bu.bu_frozen)
    @ (endorsement (bu.bu_op_type = "Endorsement"))
    @ (bake (bu.bu_op_type = "Header"))
    @ (transaction (bu.bu_op_type = "Transaction"))
  in
  let cst = Infos.constants ~cycle:0 in
  match List.rev b_list with
  | [] -> [ tr [ td ~a:[ a_colspan 5 ] [ pcdata_s "No balance updates since 5 cycles." ]]]
  | _ ->
    List.map
      (fun
        b ->
        tr [
          td [ pcdata @@ Int32.to_string @@ Int32.(div b.bu_level @@ of_int cst.blocks_per_cycle)];
          td [ Common.make_link (Int32.to_string b.bu_level)];
          td [
              let date,time =
                Tezos_types.Date.pretty_date
                  b.bu_date in
              pcdata @@ Printf.sprintf "%s (%s)" date time];
          td (Tez.with_usd price_usd b.bu_diff) ;
          td [ pcdata (s_ b.bu_update_type) ];
          td [ div (extra_info b)]
        ]) b_list
