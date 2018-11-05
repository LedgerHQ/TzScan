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
open Bootstrap_helpers.Icon
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Common (* redefines Xhr for EzAPI urls *)

(* a [request] is [url, handler] for Xhr.get *)
type request = EzAPI.url * (string -> unit)

(** Pagination utils **)

let set_url_arg arg value =
  match Url.url_of_string (Js.to_string Dom_html.window##location##href) with
  | None -> ()
  | Some url ->
    let args = Url.Current.arguments in
    let replaced = ref false in
    let args = List.map (fun (k, v) ->
        if k = arg then begin
          replaced := true;
          k, value
        end
        else k, v
      ) args in
    let args = if !replaced then args else (arg, value) :: args in
    let url = match url with
      | Url.Http hu -> Url.Http { hu with Url.hu_arguments = args }
      | Url.Https hu -> Url.Https { hu with Url.hu_arguments = args }
      | Url.File fu -> Url.File { fu with Url.fu_arguments = args }
    in
    Dom_html.window##history##replaceState(
      Js.Opt.empty,
      Js.string "",
      Js.some (Js.string (Url.string_of_url url)))

let set_page_in_browser_url ~show_in_url page =
  if show_in_url then
    set_url_arg "p" (string_of_int page)

let pagination_xhr_with_timestamp
    url ?(show_in_url=true) page decode update update_timestamp =
  (url : EzAPI.url), (fun res ->
    let data = decode res in
    update data;
    update_timestamp data ;
    set_page_in_browser_url ~show_in_url page;
  )

let pagination_xhr_with_timestamp_and_level
    url ?(show_in_url=true) page decode update update_timestamp update_level =
  (url : EzAPI.url), (fun res ->
    let data = decode res in
    update data;
    update_timestamp data ;
    update_level data;
    set_page_in_browser_url ~show_in_url page;
  )

let pagination_xhr url ?(show_in_url=true) page decode update =
  (url : EzAPI.url), (fun res ->
      let data = decode res in
      update data;
      set_page_in_browser_url ~show_in_url page;
  )

let make_onclick ?update_timestamp ?update_level
    url ~show_in_url number page decode update total =
  let has_question = String.contains url '?' in
  let url =
    EzAPI.URL
      (Printf.sprintf "%s%cnumber=%d&p=%d"
         url (if has_question then '&' else '?')
         number page) in
  let url, cb = match update_timestamp, update_level with
    | Some update_ts, Some update_lvl ->
      pagination_xhr_with_timestamp_and_level
        ~show_in_url
        url
        page
        decode
        (update page total)
        update_ts update_lvl
    | _ ->
      pagination_xhr ~show_in_url url page decode (update page total)
  in
  a_onclick (fun _ -> Xhr.get "Pagination" url cb; true)


let paginate container_id
    ?update_timestamp ?update_level ~show_in_url
    url number page decode update total =
  let pagine_id = Common.paginate_id container_id in
  let prev_id = Common.prev_id container_id in
  let next_id = Common.next_id container_id in
  let nb_pages = (total - 1) / number + 1 in
  if nb_pages <= 1
  then span ~a:[a_class ["hidden"]] []
  else
    let pages = Common.page_range page nb_pages in
    let mk_onclick page =
      make_onclick ?update_timestamp ?update_level ~show_in_url
        url number page decode update total in
    let items = List.map (function
        | Some p, true ->
          li ~a:[a_class ["active"]]
            [a [ pcdata @@ string_of_int (p+1) ]]
        | Some p, false ->
          li [a ~a:[mk_onclick p; a_class ["hidden-xs"; "hidden-sm"]]
                [ pcdata @@ string_of_int (p+1) ]]
        | None, _ ->
          li ~a:[a_class ["disabled"; "hidden-xs"; "hidden-sm"]]
            [a [ pcdata "..." ]]
      ) pages in
    let prev =
      if page = 0 then
        li ~a:[a_class ["disabled"]] [a [pcdata "«"]]
      else
        li [a ~a:[ a_id prev_id; mk_onclick (page-1) ] [pcdata "«"]]
    in
    let next =
      if page + 1 = nb_pages then
        li ~a:[a_class ["disabled"]] [a [pcdata "»"]]
      else
        li [a ~a:[ a_id next_id; mk_onclick (page+1); ] [pcdata "»"]]
    in
    ul ~a:[a_class ["pagination"; "pagination-sm"]; a_id pagine_id] ( prev :: items @ [next] )


let make_pagination_header ?(classes = []) container_id title
    ?update_timestamp ?update_level  ~show_in_url
    url number page decode update total help =
  if total = 0 then
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ panel_title ] ]
        (if title <> "" then [
            div [ span [ pcdata title; help ] ]
          ] else [
           div [ span ~a:[ a_class [ "help-no-title" ] ] [ help ] ]
         ])
    ]
  else
    div ~a:[ a_class [ panel_heading ] ] [
      div ~a:[ a_class [ panel_title ] ] [
        div ~a:[ a_class [ row ] ]
          (if title <> "" then [
              div ~a:[ a_class [ clg6; cxs6 ] ] [ span [ pcdata title; help ]
                                          ] ;
              div ~a:[ a_class ([ clg6; cxs6 ] @ classes) ] [
                paginate container_id
                  ?update_timestamp ?update_level
                   ~show_in_url
                  url number page decode update total
              ]
            ] else [
             div ~a:[ a_class [ clg6 ] ] [ ] ;
             div ~a:[ a_class ([ clg6 ] @ classes) ] [
               span ~a: [ a_class [ "help-no-title" ] ] [ help ];
               paginate container_id
                 ?update_timestamp ?update_level
                  ~show_in_url
                 url number page decode update total
             ]
           ])
      ]
    ]

let desactivate_next_arrow container_id =
  let paginate_elt = find_component @@ Common.paginate_id container_id in
  let next_arrow_elt = find_component @@ Common.next_id container_id in
  let desactived_next_arrow = double_right_icon () in
  Manip.replaceChild paginate_elt desactived_next_arrow next_arrow_elt

let pagination
  (EzAPI.URL url) page total number container_id to_update_title content
  decode update
  ?(update_timestamp = None) ?(update_level = None)
  ?(show_in_url=true)
  ops help =
  let len = List.length ops in
  let container = find_component container_id in
  let to_update_heading =
    if len = 0 && page = 0 then
      div ~a:[ a_class [ panel_heading ] ] [
        div ~a:[ a_class [ panel_title ] ]
          (if to_update_title <> "" then [
              div [ span [ pcdata to_update_title; help ] ]
            ] else [
             div [ span ~a:[ a_class [ "help-no-title" ] ] [ help ] ]
           ])
      ]
    else
      make_pagination_header
        ~classes:[ "paginate" ]
        container_id
        to_update_title
        ?update_timestamp
        ?update_level
        ~show_in_url
        url
        number
        page
        decode
        update
        total
        help
  in
  Manip.removeChildren container ;
  Manip.appendChild container to_update_heading ;
  Manip.appendChild container content

let update_op_timestamp update_timestamp data =
  List.iter (fun op ->
      Common.timestamp
        op.op_block_hash
        (update_timestamp op))
    data

let update_op_level update_level data =
  List.iter (fun op ->
      Common.level
        op.op_block_hash
        (update_level op))
    data

let pagination_ops ~update_timestamp ~update_level =
  pagination
    ~update_timestamp:(Some (update_op_timestamp update_timestamp))
    ~update_level:(Some (update_op_level update_level))

let pagination_xhr_ops params page decode update update_timestamp =
  pagination_xhr_with_timestamp params page decode update
    (update_op_timestamp update_timestamp)

let pagination_xhr_txs params page decode update update_timestamp update_level =
  pagination_xhr_with_timestamp_and_level params page decode update
    (update_op_timestamp update_timestamp)
    (update_op_level update_level)
