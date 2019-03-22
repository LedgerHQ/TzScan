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

open Ocp_js.Html
open Js_utils
open Bootstrap_helpers.Menu
open Data_types
open StringCompat


let txt s = txt (Lang.s_ s)
let link2 ?(disabled=false) classes lnk s  =
  Link2(classes, Common.a_link lnk, txt s, disabled)
let x__________x = Separator []


type dropdown = {
  dd_score : int ;
  dd_name : string ;
  mutable dd_items : dropdown_item list ;
}

and dropdown_item = {
  ddi_score : int ;
  ddi_disabled : bool ;
  ddi_link : string ;
  ddi_name : string ;
}

let menu_list = ref []
let menu_map = ref StringMap.empty

let dd_compare dd1 dd2 = compare dd1.dd_score dd2.dd_score
let ddi_compare ddi1 ddi2 = compare ddi1.ddi_score ddi2.ddi_score

let add_item dd ?(score=10) ?(disabled=false) ddi_link ddi_name =
  let ddi = {
    ddi_score = score;
    ddi_name ;
    ddi_link ;
    ddi_disabled = disabled ;
  } in
  dd.dd_items <- dd.dd_items @ [ddi]

let add_separator dd =
  add_item dd "" ""

let add_dropdown ?(score=10) dd_name =
  let dd = {
    dd_score = score ;
    dd_name ;
    dd_items = [];
  } in
  menu_list := !menu_list @ [ dd ]  ;
  menu_map := StringMap.add dd.dd_name dd !menu_map;
  dd

let create () =
  let menu_list = List.stable_sort dd_compare !menu_list in
  (* compare by score *)

  let current_lang = match Jslang.get() with
    | None -> "en"
    | Some lang -> lang
  in
  log "current_lang = %S" current_lang;
  let menu =
    [
      link2 ["active"] "/" "Home";
    ] @
    List.map (fun dd ->
        Dropdown ([], [txt dd.dd_name],
                  List.map (fun ddi ->
                      if ddi.ddi_name = "" then
                        x__________x
                      else
                      if ddi.ddi_disabled then
                        link2
                          ~disabled:true
                          ["disabled"]
                          ddi.ddi_link
                          ddi.ddi_name
                      else
                        link2 []
                          ddi.ddi_link
                          ddi.ddi_name
                    ) (List.stable_sort ddi_compare dd.dd_items),
                  false)
      ) menu_list
    @
    [
      Dropdown([],  [span ~a:[ a_class ["glyphicon"; "glyphicon-cog"] ] []; txt " "],
               ((Header ([], "Language")) ::
                (List.map (fun (name, lang) ->
                     let args = List.filter (fun (v,_) -> v <> "lang") (Jsloc.args ()) in
                     let args = ("lang",lang):: args in
                     Link([], Common.link ~args (Jsloc.path_string ()),
                          txt name, lang = current_lang)
                   ) Infos.www.www_languages)) @
               ((Header ([], "Theme")) ::
                (List.map (fun (name, theme) ->
                     Action([], (fun () ->
                         Common.change_theme theme;
                         Cookie.set "theme" theme),
                          txt name))
                    Infos.www.www_themes)),
               false
              )
    ]
  in

  match Manip.by_id "top-menu" with
  | None -> ()
  | Some menu_ul ->
    let menu = List.map bootstrap_menu menu in
    Manip.replaceChildren menu_ul menu;
    ()

let menu_blocks =
  let dd = add_dropdown "Blocks" in
  add_item dd "blocks" "Blocks";
  add_item dd "heads"  "Blocks (Alt. Branches)";
  add_item dd "baking-rights"  "Bakings rights";
  add_item dd "snapshot-blocks"  "Snapshots";
  dd

let menu_operations =
  let dd = add_dropdown "Operations" in
  add_item dd "/transactions"  "Transactions";
  add_item dd "/endorsements"  "Endorsements";
  add_item dd "/delegations"  "Delegations";
  add_item dd "/originations"  "Originations";
  add_item dd "/activations"  "Activations";
  add_separator dd;
  add_item dd "/double-baking"  "Double Baking Evidence";
  add_item dd "/double-endorsement"  "Double Endorsement Evidence";
  add_separator dd;
  add_item dd "/nonces"  "Nonces";
  add_separator dd;
  add_item dd "/inject-signed-operation"  "Inject a Signed Operation";
  dd

let menu_accounts =
  let dd = add_dropdown "Accounts" in
  add_item dd "/accounts"  "All Accounts";
  add_item dd "/contracts"  "All Contracts";
  add_item dd "/top"  "Top accounts";
  add_separator dd;
  add_item dd ~disabled:true "/known-contracts" "Known Contracts";
  dd

let menu_protocols =
  let dd = add_dropdown "Protocols" in
  add_item dd "/protocols" "All Protocols";
  add_item dd "/proposals"  "Voting Periods";
  add_item dd "/all-proposals"  "All Proposals";
  dd

let menu_stats =
  let dd = add_dropdown "Stats" in
  add_item dd "/rolls-distribution"  "Rolls Distribution";
  add_item dd "/context"  "Key numbers";
  add_item dd "/network"  "Network";
  add_item dd "/health"  "Health";
  dd

let menu_charts =
  let dd = add_dropdown "Charts" in
  add_item dd "/charts_bakers"  "Bakers";
  add_item dd "/blocksperday"  "Blocks Per Day";
  add_item dd "/delayperday"  "Delay Between Blocks";
  add_item dd "/prioperday"  "Priorities Per Day";
  add_item dd "/bakersperday"  "Bakers Per Day";
  add_item dd "/opsperday"  "Operations Per Day";
  add_item dd "/opsperblock"  "Operations Per Block";
  add_item dd "/feesperday"  "Fees Per Day";
  add_item dd "/volumeperday"  "Volume Per Day";
  add_item dd "/market_prices" "Market Prices";
  dd

let menu_misc =
  let dd = add_dropdown ~score:100 "Misc" in
  add_item dd "/faq" "FAQ";
  add_item dd "/news" "News";
  add_item dd "/about" "About Us";
  add_item dd "/api" "API";
  add_item dd "/glossary" "Glossary";
  add_item dd "/apps" "Apps";
  dd
