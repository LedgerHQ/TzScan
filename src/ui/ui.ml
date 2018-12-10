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
open Lang
open Text

(* Home page *)
let home () =
  (* Home page with 10 last block *)
  let home_div = Home_ui.make_page () in
  Common.update_main_content home_div;
  Home_ui.init_fan () ;
  Home_ui.init_odometer () ;
  Common.do_and_update_every 30 Api_request.Home.request;
  Common.do_and_update_every 15 Api_request.Home.h24_stats;
  Api_request.Home.mini_stats ()

let block_hash hash args =
  let content = Block_ui.make_page hash args in
  Common.update_main_content content;
  Api_request.Block.request hash

let operations hash =
  let content = Operation_ui.make_page hash in
  Common.update_main_content content;
  Common.do_and_update_every 10
    (fun () -> Api_request.Operation.request hash (Jsloc.args ()))

let account_hook = ref (fun _hash -> ())

let account hash args =
  let content = Account_ui.make_page hash args in
  Common.update_main_content content;
  Api_request.Account.request hash (Jsloc.args ());
  !account_hook hash

let api () =
  Api_doc.make_doc ~base_url:(Common.api "make-doc") ()

let glossary () = Glossary_doc.make_doc ()

let blocks () =
  let content = Blocks_ui.make_blocks () in
  Common.update_main_content content;
  Common.do_and_update_every 60 Api_request.Blocks.request

let snapshot_blocks () =
  let content = Blocks_ui.make_snapshot_blocks () in
  Common.update_main_content content;
  Api_request.Blocks.snapshot_blocks ()

let baking_rights () =
  let content = Blocks_ui.make_baking_rights () in
  Common.update_main_content content;
  Blocks_ui.filter_handler (Api_request.Search.search_account Blocks_ui.update_filter)
    Api_request.Blocks.request_baking_rights;
  Api_request.Blocks.request_baking_rights ()

let heads () =
  let content = Blocks_ui.Heads.make () in
  Common.update_main_content content;
  Common.do_and_update_every 60 Api_request.Heads.request

let accounts () =
  let content = Accounts_ui.make_accounts () in
  Common.update_main_content content;
  Api_request.Accounts.request ()

let top_accounts () =
  let content = Top_accounts_ui.make_page () in
  Common.update_main_content content;
  Api_request.Top_accounts.request ()

let contracts () =
  let content = Accounts_ui.make_accounts ~contract:true () in
  Common.update_main_content content;
  Api_request.Accounts.request ~contract:(Some true) ()

let transactions ~refresh ~pending =
  let content = Operations_ui.Transactions.make ~pending () in
  Common.update_main_content content;
  Common.do_and_update_every
    refresh (fun () -> Api_request.Operations.Transactions.request ~pending)

let activations ~refresh ~pending =
  let content = Tyxml_js.Html5.div [
      Operations_ui.make_activation_alert () ;
      Operations_ui.Activations.make ~pending () ] in
  Common.update_main_content content;
  Common.do_and_update_every
    refresh (fun () ->
        Api_request.Operations.Activations.request ~pending;
        Api_request.Operations.update_activation_alert ())

let endorsements ~refresh ~pending =
  let content = Operations_ui.Endorsements.make ~pending () in
  Common.update_main_content content;
  Common.do_and_update_every
    refresh (fun () -> Api_request.Operations.Endorsements.request ~pending)

let delegations ~refresh ~pending =
  let content = Operations_ui.Delegations.make ~pending () in
  Common.update_main_content content;
  Common.do_and_update_every
    refresh (fun () -> Api_request.Operations.Delegations.request ~pending)

let originations ~refresh ~pending =
  let content = Operations_ui.Originations.make ~pending () in
  Common.update_main_content content;
  Common.do_and_update_every
    refresh (fun () -> Api_request.Operations.Originations.request ~pending)

let double_baking ~refresh ~pending =
  let content = Operations_ui.Double_Bakings.make ~pending () in
  Common.update_main_content content;
  Common.do_and_update_every
    refresh (fun () -> Api_request.Operations.Double_baking.request ~pending)

let double_endorsement ~refresh ~pending =
  let content = Operations_ui.Double_Endorsements.make ~pending () in
  Common.update_main_content content;
  Common.do_and_update_every
  refresh (fun () ->
	     Api_request.Operations.Double_endorsement.request ~pending)

let nonces () =
  let content = Operations_ui.Nonces.make () in
  Common.update_main_content content;
  Common.do_and_update_every 300 Api_request.Operations.Nonces.request

let inject_a_signed_operation () =
  let content = Inject_ui.Signed.make () in
  Common.update_main_content content

let network_stats () =
  let content = Network_stats_ui.make_panel () in
  Common.update_main_content content;
  Api_request.Network.request ()

let charts_bakers () =
  let content = Charts_ui.make_bakers_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_bakers ()

let charts_blocks_per_day () =
  let content = Charts_ui.make_blocks_per_day_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_blocks_per_day ()

let charts_delay_per_day () =
  let content = Charts_ui.make_delay_per_day_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_delay_per_day ()

let charts_priorities_per_day () =
  let content = Charts_ui.make_priorities_per_day_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_priorities_per_day ()

let charts_bakers_per_day () =
  let content = Charts_ui.make_bakers_per_day_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_bakers_per_day ()

let charts_operations_per_day () =
  let content = Charts_ui.make_operations_per_day_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_operations_per_day ()

let charts_operations_per_block_per_day () =
  let content = Charts_ui.make_operations_per_block_per_day_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_operations_per_block_per_day ()

let charts_fees_per_day () =
  let content = Charts_ui.make_chart_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_fees_per_day ()

let charts_volume_per_day () =
  let content = Charts_ui.make_chart_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_volume_per_day ()

let health_stats () =
  let content = Health_stats_ui.make_page () in
  Common.update_main_content content;
  Api_request.Health.request ()

let context_stats () =
  let content = Context_stats_ui.make_page () in
  Common.update_main_content content;
  Api_request.Context.request ()

let rolls_distrib () =
  let content = Rolls_distribution_ui.make_page () in
  Common.update_main_content content;
  Api_request.Rolls_distribution.request ()

let not_found hash =
  Xhr.get "Content"
    ("/pages/" ^ hash ^ ".html")
    ~error:(fun _code ->
        let content = Search.not_found hash in
        Common.update_main_content content)
    (fun res ->
       (Common.get_div_by_id "content")##innerHTML <- Js.string res
    )

let protocols () =
  let content = Protocols_ui.make () in
  Common.update_main_content content;
  Api_request.Protocols.request ()

let market_prices () =
  let content = Charts_ui.make_chart_panel () in
  Common.update_main_content content;
  Api_request.Charts.request_market_prices ()

(* try path functions *)
let try_level path =
  try
    let level = int_of_string path in
    Common.block_hash_level level
  with Failure _f -> not_found path

let dispatch path =
  Js_utils.log "Serving %S.." (String.concat "/" path);
  match path with
  (* PATHS *)
  | [] -> home ()
  | [path] ->
    begin
      match path with
      | "" -> home ()
      | "workers-status"
      | "api" -> api ()
      | "glossary" -> glossary ()
      | "blocks" -> blocks ()
      | "baking-rights" -> baking_rights ()
      | "heads" -> heads ()
      | "contracts" -> contracts ()
      | "context" -> context_stats ()
      | "accounts" -> accounts ()
      | "top" -> top_accounts ()
      | "activations" -> activations ~refresh:30 ~pending:false
      | "transactions" -> transactions ~refresh:30 ~pending:false
      | "endorsements" -> endorsements ~refresh:60 ~pending:false
      | "delegations"  -> delegations  ~refresh:60 ~pending:false
      | "originations" -> originations ~refresh:60 ~pending:false
      | "double-baking" -> double_baking ~refresh:60 ~pending:false
      | "double-endorsement" -> double_endorsement ~refresh:60 ~pending:false
      | "pending-activations" -> activations ~refresh:10 ~pending:true
      | "pending-transactions" -> transactions ~refresh:10 ~pending:true
      | "pending-endorsements" -> endorsements ~refresh:10 ~pending:true
      | "pending-delegations"  -> delegations  ~refresh:10 ~pending:true
      | "pending-originations" -> originations ~refresh:10 ~pending:true
      | "pending-double-baking" -> double_baking ~refresh:10 ~pending:true
      | "pending-double-endorsement" -> double_endorsement ~refresh:10 ~pending:true
      | "nonces" -> nonces ()
      | "inject-signed-operation" -> inject_a_signed_operation ()
      | "network" -> network_stats ()
      | "charts_bakers" -> charts_bakers ()
      | "blocksperday" -> charts_blocks_per_day ()
      | "delayperday" -> charts_delay_per_day ()
      | "prioperday" -> charts_priorities_per_day ()
      | "bakersperday" -> charts_bakers_per_day ()
      | "opsperday" -> charts_operations_per_day ()
      | "opsperblock" -> charts_operations_per_block_per_day ()
      | "feesperday" -> charts_fees_per_day ()
      | "volumeperday" -> charts_volume_per_day ()
      | "health" -> health_stats ()
      | "rolls-distribution" -> rolls_distrib ()
      | "snapshot-blocks" -> snapshot_blocks ()
      | "protocols" -> protocols ()
      | "market_prices" -> market_prices ()
      | _ ->
        (* Specific hashes *)
        match String.get path 0 with
        | 'o' | 'O' -> operations path
        | 'b' | 'B' -> block_hash path (Jsloc.args ())
        | _ ->
          if String.length path = 36 then
            match String.sub path 0 2 with
            | "tz" | "TZ" | "KT" -> account path (Jsloc.args ())
            | _ -> try_level path
          else
            try_level path
    end
  | _ -> not_found (Jsloc.path_string ())

let () =
  Common.link_dispatcher := dispatch


let switch_button () =
  let network = Infos.api.api_config.conf_network in
  let button = Js_utils.find_component (network ^ "-button") in
  Js_utils.Manip.addClass button "btn-primary"


let donation_keys = [
  "KT1Ko8AWfxqEhqvhZqdnuAeBm5XTW3eFti2b" ;
]

let setup_page () =
  Dom_html.window##document##title <- Js.string (t_ s_tzscan_title);
  begin
    let logo_img = Filename.concat "/images" Infos.www.www_logo in
    (Common.get_img_by_id "logo")##src <- Js.string logo_img;
  end;
  (* begin translations *)
  begin
    let span = Js_utils.find_component "search-go" in
    Js_utils.Manip.replaceChildren span [ pcdata_t s_go ]
  end;
  begin
    let input = Js_utils.find_component "search" in
    Js.Opt.case
      (Dom_html.CoerceTo.input
         (Js_utils.Manip.get_elt "input" input))
      (fun () -> assert false)
      (fun input ->
         input##placeholder <- Js.string (t_ s_search_by_address))
  end;
  (* end translations *)
  begin
    let span = Js_utils.find_component "network-buttons" in
    List.iter (fun (network, link) ->
        let ele = a ~a:[ a_class ["btn"; "btn-default"; "btn-network"];
                         a_id (network ^ "-button");
                         a_href link] [pcdata network]
        in
        Js_utils.Manip.appendChild span ele ;
      ) Infos.www.www_networks;
  end;
  Xhr.get "footer" Infos.www.www_footer (fun res ->
      (Common.get_div_by_id "footer")##innerHTML <- Js.string res;
      (* Donation tz1 *)
      let donation_id = "donation" in
      let container = Js_utils.find_component donation_id in
      let tz1 =
        match Common.shuffle donation_keys with
        | [] -> "tz1gE3rqbA6aKooeoWHw3KTwQ622FZHA2CWc"
        | hd :: _ -> hd in
      let open Tyxml_js.Html5 in
      let to_add =
        span [
          pcdata "Support us : " ;
          a ~a:[ a_href tz1 ] [ pcdata tz1 ]
        ] in
      Js_utils.Manip.appendChild container to_add ;
    );
  ()

let run () =
  Common.dispatch ( Jsloc.path () )

let runner = ref run

let () =
  Common.register_redraw setup_page;
  Common.register_redraw switch_button;
  Common.register_redraw (Search.search_handler Api_request.Search.request);
  Common.register_redraw (fun () -> !runner ());
  Common.register_redraw Menu.create;
  ()

(*
   If `!Common.api_host` is set, it means that the API_HOST was hardcoded
  in the sources, and should be used, unless another API server is specified
  in the URL argument. In this case, we only download `info.json` for its
  language part.

   If !Common.api_host is not set, we need to download `info.json` before
  any prior call to the API node.
*)

let init () =
  Onload.add (fun () ->
      Www_request.info (function
          | None ->
            Js_utils.log "Fatal error: could not download 'info.json'";
          | Some info ->
            Js_utils.log "'info.json' loaded";
            Infos.www.www_currency_name <- info.www_currency_name;
            Infos.www.www_currency_short <- info.www_currency_short;
            Infos.www.www_currency_symbol <- info.www_currency_symbol;
            Infos.www.www_languages <- info.www_languages ;
            Infos.www.www_footer <- info.www_footer ;
            Infos.www.www_logo <- info.www_logo ;
            Infos.www.www_networks <- info.www_networks ;
            begin
              match Infos.www.www_auth with
              | Some _ -> ()
              | None ->
                Infos.www.www_auth <- info.www_auth ;
            end;
            begin
              match !Common.api_host with
              | Some _ -> ()
              | None ->
                Infos.www.www_apis <- info.www_apis ;
                Common.set_api_node () ;
            end;
            Api_request.Server.info Common.initial_redraw
        );
    )
