open Data_types
open Text

let amcharts3_ready = Amcharts3.ready "/amcharts3"

let to_tez f = Int64.to_float @@ Int64.div f Tezos_constants.Constants.tez_units

let evaluated_label = "evaluated"
let predict_eval_label = "predict_evaluated"

let spendable_label = "spendable"
let predict_spendable_label = "predict_spendable"

let frozen_label = "frozen"
let predict_frozen_label = "predict_frozen"

let reward_label = "reward"
let predict_reward_label = "predict_reward"

let fees_label = "fees"
let predict_fees_label = "predict_fees"

let deposits_label = "deposit"
let predict_deposits_label = "predict_deposit"

let balances_to_data data predict =
  let rec update_data = function
    | [] -> []

    |  (cy,bal) :: [] ->

       (Lang.t_ s_now,
        [| evaluated_label, to_tez (Int64.add bal.b_spendable bal.b_frozen);
           spendable_label, to_tez bal.b_spendable;
           frozen_label, to_tez bal.b_frozen;
           reward_label, to_tez bal.b_rewards;
           fees_label, to_tez bal.b_fees;
           deposits_label,  to_tez bal.b_deposits;
           predict_eval_label,  to_tez (Int64.add predict.b_spendable predict.b_frozen);
           predict_spendable_label, to_tez predict.b_spendable;
           predict_frozen_label, to_tez predict.b_frozen;
           predict_reward_label, to_tez predict.b_rewards;
           predict_fees_label,  to_tez predict.b_fees;
           predict_deposits_label,  to_tez predict.b_deposits;
        |]) ::
          [(Int32.to_string cy),
           [| predict_eval_label,
                (to_tez
                   (Int64.(add
                             (add bal.b_frozen predict.b_frozen)
                             (add bal.b_spendable predict.b_spendable)
                          )
                   )
                );
              predict_spendable_label, to_tez (Int64.add bal.b_spendable predict.b_spendable);
              predict_frozen_label,  to_tez (Int64.add bal.b_frozen predict.b_frozen);
              predict_reward_label,  to_tez (Int64.add bal.b_rewards predict.b_rewards);
              predict_fees_label,  to_tez (Int64.add bal.b_fees predict.b_fees);
              predict_deposits_label,  to_tez (Int64.add bal.b_deposits predict.b_deposits);|]]

    |  (prev_c,prev_bal)
       :: (cy,bal)
       :: [] ->
       ((Int32.to_string prev_c),
        [|evaluated_label,  to_tez (Int64.add prev_bal.b_spendable prev_bal.b_frozen);
          spendable_label, to_tez prev_bal.b_spendable;
          frozen_label,  to_tez prev_bal.b_frozen;
          reward_label,  to_tez prev_bal.b_rewards;
          fees_label,  to_tez prev_bal.b_fees;
          deposits_label,  to_tez prev_bal.b_deposits;
|]) ::

       (Lang.t_ s_now,
        [|evaluated_label,  to_tez (Int64.add bal.b_spendable bal.b_frozen);
          spendable_label, to_tez bal.b_spendable;
          frozen_label,  to_tez bal.b_frozen;
          reward_label,  to_tez bal.b_rewards;
          fees_label,  to_tez bal.b_fees;
          deposits_label,  to_tez bal.b_deposits;
          predict_eval_label, to_tez (Int64.add bal.b_spendable bal.b_frozen);
          predict_spendable_label, to_tez bal.b_spendable;
          predict_frozen_label,  to_tez bal.b_frozen;
          predict_reward_label,  to_tez bal.b_rewards;
          predict_fees_label,  to_tez bal.b_fees;
          predict_deposits_label,  to_tez bal.b_deposits;
        |]) ::
          [(Int32.to_string cy),
           [| predict_eval_label,
              to_tez Int64.(add
                              (add bal.b_spendable predict.b_spendable)
                              (add bal.b_frozen predict.b_frozen));
              predict_spendable_label, to_tez (Int64.add bal.b_spendable predict.b_spendable);
              predict_frozen_label,  to_tez (Int64.add bal.b_frozen predict.b_frozen);
              predict_reward_label,  to_tez (Int64.add bal.b_rewards predict.b_rewards);
              predict_fees_label,  to_tez (Int64.add bal.b_fees predict.b_fees);
              predict_deposits_label,  to_tez (Int64.add bal.b_deposits predict.b_deposits);|]]

    | (cycle,bal) :: tl ->
       ((Int32.to_string cycle),
        [|evaluated_label,  to_tez (Int64.add bal.b_spendable bal.b_frozen);
          spendable_label, to_tez bal.b_spendable;
          frozen_label,  to_tez bal.b_frozen;
          reward_label,  to_tez bal.b_rewards;
          fees_label,  to_tez bal.b_fees;
          deposits_label,  to_tez bal.b_deposits|]) :: (update_data tl)
  in
  Array.of_list @@ update_data data



let make_chart splitted chart_id data =

  let graph_maker color _bullet_type ?(dotted=false) axis label title =

    let graph =
      Amcharts3.graphN label "smoothedLine" in
    graph##lineColor <- Js.string color;
    graph##bullet <- Js.string "none";
    graph##bulletSize <- 8;
    graph##bulletBorderColor <- Js.string "#FFFFFF";
    graph##bulletBorderAlpha <- 1;
    graph##bulletBorderThickness <- 2;
    graph##lineThickness <- 2;
    graph##valueAxis <- axis;
    graph##dashLength <- (if dotted then 5 else 0);
    graph##balloonText <-
      if dotted then Js.string "" else
        Js.string
          (Printf.sprintf
           "[[category]]<br><b><span style='font-size:14px;'> [[%s]] %s</span></b>"
           label
           "&#xa729"
          );
    graph##title <- Js.string title;
    graph
  in

  amcharts3_ready (fun () ->
      let chart = Amcharts3.serialN () in
      let frozen_chart = if splitted then Amcharts3.serialN () else chart in
      (* Creating the chart *)
      chart##dataProvider <- Amcharts3.DataItem.dataProvider data;
      frozen_chart##dataProvider <- Amcharts3.DataItem.dataProvider data;
      chart##marginLeft <- 10;
      frozen_chart##marginLeft <- 10;

      let update_categoryAxis categoryAxis =
        categoryAxis##parseDates <- false;
        categoryAxis##dashLength <- 3;
        categoryAxis##minorGridEnabled <- true;
        categoryAxis##minorGridAlpha <- 0.1;
        categoryAxis##title <- Js.string "Cycle"
      in

      update_categoryAxis chart##categoryAxis ;
      update_categoryAxis frozen_chart##categoryAxis;

      let leftAxis = Amcharts3.valueAxis() in
      leftAxis##axisAlpha <- 0.;
      leftAxis##inside <- false;
      leftAxis##dashLength <- 3;
      leftAxis##position <- Js.string "left";
      leftAxis##title <- Js.string @@ Lang.t_ s_balance ;
      leftAxis##includeAllValues <- true;
      chart##addValueAxis(leftAxis);

      let rightAxis = Amcharts3.valueAxis() in
      rightAxis##axisAlpha <- 0.;
      rightAxis##inside <- false;
      if not splitted then rightAxis##offset <- 35;
      rightAxis##dashLength <- 3;
      rightAxis##position <- Js.string (if splitted then "left" else "right");
      leftAxis##includeAllValues <- true;
      rightAxis##title <- Js.string @@ Lang.t_ s_frozen_balance ;

      frozen_chart##addValueAxis(rightAxis);

      (* Defining the different graphs *)
       let predict_reward_graph =
        graph_maker
          "#0033FF"
          "square"
          ~dotted:true
          rightAxis
          predict_reward_label
           @@ Lang.t_ s_reward_estimation in

      let predict_fees_graph =
        graph_maker
          "#00FF00"
          "square"
          ~dotted:true
          rightAxis
          predict_fees_label
           @@ Lang.t_ s_fees_estimation in

      let predict_deposits_graph =
        graph_maker
          "#FF00FF"
          "square"
          ~dotted:true
          rightAxis
          predict_deposits_label
           @@ Lang.t_ s_deposit_estimation in

      let predict_frozen_graph =
        graph_maker
          "#0DE8D9"
          "square"
          ~dotted:true
          rightAxis
          predict_frozen_label
           @@ Lang.t_ s_frozen_balance_estimation in

      let predict_spendable_graph =
        graph_maker
          "#D1655D"
          "round"
          ~dotted:true
          leftAxis
          predict_spendable_label
           @@ Lang.t_ s_balance_estimation
      in
      let predict_eval_graph =
        graph_maker
          "#000000"
          "round"
          ~dotted:true
          leftAxis
          predict_eval_label
           @@ Lang.t_ s_evaluated_balance
      in
      let reward_graph =
        graph_maker
          "#0033FF"
          "square"
          rightAxis
          reward_label
           @@ Lang.t_ s_rewards_frozen in

      let fees_graph =
        graph_maker
          "#00FF00"
          "square"
          rightAxis
          fees_label
           @@ Lang.t_ s_fees_frozen in

      let deposits_graph =
        graph_maker
          "#FF00FF"
          "square"
          rightAxis
          deposits_label
           @@ Lang.t_ s_deposits_frozen in

      let frozen_graph =
        graph_maker
          "#0DE8D9"
          "square"
          rightAxis
          frozen_label
           @@ Lang.t_ s_frozen_balance in

      let spendable_graph =
        graph_maker
          "#D1655D"
          "round"
          leftAxis
          spendable_label
           @@ Lang.t_ s_balance
      in

      let evaluated_graph =
        graph_maker
          "#000000"
          "round"
          leftAxis
          evaluated_label
           @@ Lang.t_ s_evaluated_balance
      in

      (* Order is important : printed after, a graph will be printed over those printed before *)
      frozen_chart##addGraph( predict_deposits_graph );
      frozen_chart##addGraph( predict_fees_graph );
      frozen_chart##addGraph( predict_reward_graph );
      frozen_chart##addGraph( predict_frozen_graph );
      chart##addGraph( predict_spendable_graph );
      chart##addGraph( predict_eval_graph );
      frozen_chart##addGraph( deposits_graph );
      frozen_chart##addGraph( fees_graph );
      frozen_chart##addGraph( reward_graph );
      frozen_chart##addGraph( frozen_graph );
      chart##addGraph( spendable_graph );
      chart##addGraph( evaluated_graph );

      (* And now the rest *)
      let legend = Amcharts3.legend () in
      legend##useGraphSettings <- true;
      legend##reversedOrder <- true;
      legend##fontSize <- 13;
      legend##valueAlign <- Js.string "left";
      legend##valueWidth <- 20;
      chart##legend <- legend;

      let legendFrozen = Amcharts3.legend () in
      if splitted then
        begin
          legendFrozen##useGraphSettings <- true;
          legendFrozen##reversedOrder <- true;
          legendFrozen##fontSize <- 13;
          legendFrozen##valueAlign <- Js.string "left";
          legendFrozen##valueWidth <- 20;
          frozen_chart##legend <- legendFrozen
        end;
      let chartCursor = Amcharts3.chartCursor () in
      chartCursor##cursorAlpha <- 0;
      chartCursor##cursorPosition <- Js.string "mouse";
      chart##addChartCursor(chartCursor);

      let chartCursorFrozen = Amcharts3.chartCursor () in
      if splitted then
        begin
          chartCursorFrozen##cursorAlpha <- 0;
          chartCursorFrozen##cursorPosition <- Js.string "mouse";
          frozen_chart##addChartCursor(chartCursorFrozen)
        end;

      let chartScrollbar = Amcharts3.chartScrollbar() in
      chart##addChartScrollbar(chartScrollbar);
      chart##creditsPosition <- Js.string "bottom-right";

      let chartScrollbarFrozen =  Amcharts3.chartScrollbar() in
      if splitted then
        begin
          frozen_chart##addChartScrollbar(chartScrollbarFrozen);
          frozen_chart##creditsPosition <- Js.string "bottom-right"
        end;

      chart##write (if splitted then Js.string (chart_id^"-spendable") else Js.string chart_id);

      if splitted then
      frozen_chart##write (Js.string (chart_id^"-frozen"))

    )
