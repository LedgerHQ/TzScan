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
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Data_types

let spf = Printf.sprintf
let current_focus = ref (-1)

let rem x y =
  let result = x mod y in
  if result >= 0 then result
  else result + y


let is_long_enough_for_completion hash =
  let len = String.length hash in
  len >= 3 &&
  match hash.[0], hash.[1], hash.[2] with
  | 'K', 'T', '1'
  | 't', 'z', ('1'..'4')
    -> len >= 4
  | _ -> true

let search_handler ?onclick ?(input_id="search") ?(button_id="search-go")
    ?(list_id="search_list") request () =
  let search_go = find_component button_id in
  let search_input = find_component input_id in
  match Manip.parent search_input with
  | None -> failwith "no parent"
  | Some div_search ->
    let search_list =
      div ~a:[ a_id list_id; a_class ["autocomplete-items"]]
        [] in
    Manip.appendChild div_search search_list;

    Manip.Ev.onclick search_go (match onclick with
        | None ->
          (fun e ->
             Dom.preventDefault e;
             let search_value = Manip.value search_input |>
                                String.trim in
             Dom_html.window##location##pathname <- Js.string search_value ;
             true)
        | Some onclick -> onclick
      );

    Manip.Ev.oninput search_input (fun _e ->
        show search_list;
        let search_value = Manip.value search_input in
        if is_long_enough_for_completion search_value then
          request (Url.urlencode search_value)
        else (
          Manip.removeChildren search_list;
          Manip.setInnerHtml search_go "Go");
        true
      );

    Manip.Ev.onkeydown search_input (fun e ->
        let children = Manip.children search_list in
        let length = List.length children in
        match length with
        | 0 -> true
        | n ->
          let child = (To_dom.of_div (List.nth children (rem !current_focus n))) in
          match e##keyCode with
          | 13 -> Dom.preventDefault e;
            child##click(); true
          | _ -> child##classList##remove(Js.string "autocomplete-active");
            if e##keyCode = 40 then incr current_focus
            else if e##keyCode = 38 then decr current_focus;
            (To_dom.of_div (List.nth children (rem !current_focus n)))
            ##classList##add(Js.string "autocomplete-active");
            true
      );

    Manip.Ev.onclick (Window.body window) (fun e ->
        match Js.Opt.to_option e##target with
        | None -> true
        | Some target -> if target##id <> Js.string input_id then hide search_list; true
      )

let update_search ?more_click ?(input_id="search") ?(button_id="search-go")
    ?(list_id="search_list") nb_search hash results =
  let hash = Url.urldecode hash in
  let search_list = find_component list_id in
  let clear_list () = Manip.removeChildren search_list in
  let search_input = find_component input_id in
  clear_list ();
  let search_go = find_component button_id in
  let more_click =
    Misc.unopt (fun _ -> (To_dom.of_button search_go)##click()) more_click in
  match nb_search with
  | 0 -> Manip.setInnerHtml search_go "Go"
  | _->
    if nb_search <> -1 then Manip.setInnerHtml search_go (spf "Go (%i)" nb_search);
    let length = String.length hash in
    let hash_lowercase = String.lowercase_ascii hash in
    List.iteri (fun i (user, kind) ->
        let onclick _ =
          (To_dom.of_input search_input)##value <- Js.string user.tz;
          clear_list () ;
          more_click user;
          true in
        let target, postname = match kind, user.alias with
          | "alias", Some alias -> alias, spf " / %s" user.tz
          | "account", Some alias -> user.tz, spf " / %s" alias
          | _ -> user.tz, "" in
        let length_i = String.length target in
        let text_i_before, text_i_strong, text_i_after =
          if length_i >= length &&
             String.equal hash_lowercase
               (String.lowercase_ascii (String.sub target 0 length))
          then
            "",
            String.sub target 0 length,
            String.sub target length (length_i-length)
          else
            String.sub target 0 3,
            String.sub target 3 length,
            String.sub target (3+length) (length_i - length - 3)
        in
        let input_id = spf "input-%i" i in
        let input_i =
          input ~a:[a_value target; a_id input_id; a_input_type `Hidden;] () in
        let div_i = div ~a:[a_onclick onclick]
            [txt text_i_before;
             strong [txt text_i_strong];
             txt text_i_after;
             txt postname;
             input_i] in
        Manip.appendChild search_list div_i;
      )
      results


 let hash_type hash = match String.get hash 0 with
  | 'o' | 'O' -> "operation "
  | 'b' | 'B' -> "block "
  | 't' | 'T' -> "account "
  | _ -> "path "

let not_found hash =
  div ~a:[ a_class [ "404-div"; row ] ] [
    div ~a:[ a_class [ clg12; cxs12 ] ] [
      div ~a:[ a_id @@ hash;
               a_class [ panel; panel_primary ]] [
        div ~a:[ a_class [ panel_heading ] ] [
          h3 ~a:[ a_class [ panel_title ] ] [ txt "404" ]
        ] ;
        div ~a:[ a_class [ panel_body ] ]
          [ txt (spf "%s %s not found" (hash_type hash) hash) ]
      ] ;
    ]
  ]
