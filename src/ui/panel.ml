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

open Tyxml_js
open Html5
open Js_utils
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Panel
open Bootstrap_helpers.Table
open Bootstrap_helpers.Input
open Bootstrap_helpers.Button
open Bootstrap_helpers.Icon
open Text
open Lang

type theads = Html_types.tr Tyxml_js.Html5.elt


module PageInput = struct

  let template id =
    Printf.sprintf "<div id=\"%s\" class=\"%s\"></div>" id
      (String.concat "\" \"" [])

  let get_page id page page_number =
    let page_input = find_component @@ Common.make_id "input" id in
    let curr =
      try int_of_string (Manip.value page_input) with _ -> page in
    match page_number with
      None ->  if curr < 1 then page else curr
    | Some page_number ->
      if curr < 1 || curr > page_number then page else curr

  let onclick_accel id plus page page_number _  =
    let curr = get_page id page page_number in
    let page_input = find_component @@ Common.make_id "input" id in
    let value =
      if plus then
        match page_number with
          None -> curr + 10
        | Some page_number ->
          if curr + 10 > (page_number - 1) then page_number
          else curr + 10
      else
      if curr < 11 then 1
      else curr - 10
    in
    let page_input = Tyxml_js.To_dom.of_input page_input in
    if plus then
      page_input##value <- Js.string (string_of_int value)
    else page_input##value <- Js.string (string_of_int value);
    true

  let mk_accel_button id plus page page_number =
    if plus then
      button ~a:[ a_class [ btn; btn_default ];
                  a_button_type `Button;
                  a_onclick (onclick_accel id plus page page_number); ] [
        pcdata "+10" ]
    else
      button ~a:[ a_class [ btn; btn_default ];
                  a_button_type `Button;
                  a_onclick (onclick_accel id plus page page_number); ] [
        pcdata "-10" ]

  let onclick_goto id page page_number goto _ =
    let page = get_page id page page_number in
    goto (page - 1);
    true

  let add_onreturn id page page_number goto =
    let page_input = find_component @@ Common.make_id "input" id in
    Manip.Ev.onreturn page_input (fun _ ->
        let page = get_page id page page_number in
        goto (page - 1))

  let mk_popover id page page_number goto =
    div ~a: [ a_class [ input_group; input_group_sm ] ] [
      span ~a:[ a_class [ input_group_btn ]; ] [
        mk_accel_button id false page page_number;
      ];
      Html5.input ~a:[ a_input_type `Text; a_class [ form_control; "page-input" ];
                 a_placeholder "Go to"; a_id @@ Common.make_id "input" id;
               ] ();
      span ~a:[ a_class [ input_group_btn ] ] [
        mk_accel_button id true page page_number;
        button ~a:[ a_class [ btn; btn_default ];
                    a_button_type `Button;
                    a_onclick (onclick_goto id page page_number goto);
                  ]
          [ right_icon () ]
      ]
    ]


  let init_inputs ids page page_number goto =
    ignore (Js.Unsafe.eval_string
              "jQuery('[data-toggle=\"popover\"]').popover();");
    List.iter (fun id ->
        let j_obj =
          Printf.sprintf "jQuery(\'#%s\')" (Common.make_id "link" id) in
        let elt = Js.Unsafe.eval_string j_obj  in
        Js.Unsafe.meth_call elt "on"
          [| Js.Unsafe.inject (Js.string "inserted.bs.popover");
             Js.Unsafe.inject (fun _ ->
                 let elt = find_component id in
                 Manip.removeChildren elt;
                 Manip.appendChild elt (mk_popover id page page_number goto);
                 add_onreturn id page page_number goto;
                 Js._true);
          |]) ids



  let link_with_input ?(prefix="") =
    let create_id = let cpt = ref 0 in fun () ->
        incr cpt;
        Common.make_id (prefix ^ "goto-page") (string_of_int !cpt) in
    fun value ->
      let id = create_id () in
      a ~a: [ Bootstrap_helpers.Attributes.a_data_toggle "popover";
              Bootstrap_helpers.Attributes.a_data_placement `Top;
              Bootstrap_helpers.Attributes.a_data_html true;
              Bootstrap_helpers.Attributes.a_data_content (template id);
              a_id @@ Common.make_id "link" id;
            ]
        [ pcdata value ],
      id

end


(* Creates a range : [0 ; p-accel; p-1; p; p+1; p+accel; last_page]
   If accel < 2 then : [0; p-1; p; p+1; last_page]
*)
let mk_range ?(accel=10) page page_total =
  let last_page = page_total - 1 in
  let last_pages =
    if page >= last_page then []
    else if accel < 2 || page + accel >= last_page then [ last_page ]
    else [ page + accel; last_page ] in
  let next_page =
    if page + 1 < last_page then page + 1 :: last_pages else last_pages in
  let prev_page =
    if page - 1 < 0 then page :: next_page
    else (page - 1) :: page :: next_page in
  let prev_minus_accel =
    if page <= accel || accel < 2 then prev_page else
      (page - accel) :: prev_page in
  match prev_minus_accel with
  | hd :: _ -> if hd > 0 then 0 :: prev_minus_accel else prev_minus_accel
  | [] -> assert false

(* If use_sep = true, adds (None, false) between two pages that are not
   contiguous.
   Act as `List.map mk_page range` otherwise.
 *)
let mk_pages ?(use_sep = false) mk_page range =
  let rec mk rem =
    match rem with
    | i :: j :: rem ->
      if j - i <> 1 then
        if use_sep then mk_page i :: (None, false) :: mk (j :: rem)
        else mk_page i :: mk (j :: rem)
      else mk_page i :: mk (j :: rem)
    | i :: [] -> mk_page i :: []
    | [] -> []
  in
  mk range


(* `page_range page nb_pages` is a variant of `Common.page_range`, BUT
page_total here is the total number of pages, whereas
`Common.page_range page last_page` has the last page number for
argument. Here, `page` is between `0` and `nb_pages-1`. *)
let large_page_range page page_total =
  let make_page i = Some i, i = page in
  match page_total with
  | Some page_total ->
    if page_total <= 5
    then Misc.list_init page_total make_page
    else mk_pages ~use_sep:true make_page (mk_range ~accel:0 page page_total)
  | None ->
     List.map make_page
              (match page with
               | 0 -> [0;1;2;10]
               | 1 -> [0;1;2;11]
               | 2 -> [0;1;2;12]
               | _ ->
                  if page > 10 then
                    [0;page-10;page;page+10]
                  else
                    [0;page-1;page;page+10]) @ [None, false]

let find_url_arg arg =
  Misc.list_find_opt (fun (k, _v) -> k = arg) Url.Current.arguments

let set_page_in_browser_url ?(urlarg="p") page =
  if urlarg <> "" then
    Common.set_url_arg ~default:"1" urlarg (string_of_int page)

let set_size_in_browser_url ?(urlarg="r") page =
  if urlarg <> "" then
    Common.set_url_arg ~default:"20" urlarg (string_of_int page)

let theads columns =
  tr (List.map (fun (spans, width) ->
          if width < 0 then begin
              th spans
            end
          else
            th ~a:[ a_class [
                        Printf.sprintf "col-lg-%d" width ] ]
               spans
        ) columns)

let theads_of_strings columns () =
  tr (List.map (fun (name, width) ->
          if width < 0 then begin
              if id_ name = "->" then
                th ~a:[ a_class [ "arrow" ] ] [ pcdata "" ]
              else
                th [ pcdata_t name ]
            end
          else
            th ~a:[ a_class [
                        Printf.sprintf "col-lg-%d" width ] ]
               [ pcdata_t name ] ;
        ) columns)

let title_nb ?help s nb =
  let s = Lang.t_ s in
  let ele =
           (if nb < 0 then
              [ pcdata s ]
            else
              [ pcdata (s^" ");
                span ~a:[a_class ["badge"]] [ pcdata @@ string_of_int nb ] ]
           )
  in
  match help with
  | None -> span ele
  | Some h -> span (ele @ [ Glossary_doc.help h ])

let find_page ?(urlarg="p") current_page =
  if urlarg = "" then current_page
  else
    match find_url_arg urlarg with
    | None -> current_page
    | Some (_, p_str) ->
      try
        int_of_string p_str
      with _ -> 1

let find_size ?(urlarg="r") current_size =
  if urlarg = "" then current_size
  else
    match find_url_arg urlarg with
    | None -> current_size
    | Some (_, p_str) ->
      try
        int_of_string p_str
      with _ -> 20

(* Simple table, no pagination *)
module Make(M: sig

                     val name : string
                     val title_span :  [> Html_types.span ] Tyxml_js.Html5.elt
                     val columns : (text * int) list
                     val table_class : string

                   end) = struct

(* let s_name = ss_ M.name  *)

  let theads = theads_of_strings M.columns

  let title_id = Printf.sprintf "%s-title" M.name
  let loading_id = Printf.sprintf "%s-loading" M.name
  let table_id = Printf.sprintf "%s-table" M.name

  let make_common () =
    make_panel
      ~panel_title_content:(
        div [
          div ~a:[ a_class [ row ] ] [
            div ~a:[  a_id title_id; a_class [ clg4; csm6; cxs6 ] ]  [ M.title_span  ] ;
            div ~a:[  a_id loading_id ]
              [ Common.make_loading_gif [ "loading-heading"; clg8; csm6; cxs6 ] ]
          ]
        ])
      ~panel_body_content:[
        div ~a: [ a_id table_id ] [
          tablex ~a:[ a_class [ btable; M.table_class ] ] [
            tbody
              (theads() :: [
                  tr [
                    td [
                      Lang.pcdata_t s_fetching_data ] ] ] )]
        ]
      ] ()

  let make ?(panel_class = []) () =
    div ~a:[ a_class panel_class ] [
      make_common ()
    ]

  let make_clg12 () =
    make ~panel_class:[ clg12 ] ()

  let display rows =
    let div = find_component loading_id in
    Manip.removeChildren div;
    let container = find_component table_id in
    let table =
      tablex ~a:[ a_class [ btable; "blocks-table" ] ] [
               tbody (theads() :: rows) ]

    in
    Manip.removeChildren container ;
    Manip.appendChild container table;
    ()

end

type 'data content =
  | Loading of string (* div_id *)
  | Content of string (* name *) * text * text * 'data (* rows *)

(* Generic functor, not exported, used two other functors
`MakePageTable` and `MakePageNoTable`. *)
module MakePageGen(M: sig

                    type data

                    val name : string
                    val title_span : int ->
                                     [> Html_types.span ] Tyxml_js.Html5.elt
                    val page_size : int

                    val content_maker :
                      data content ->
                      [> `Div | `Table ] Tyxml_js.Html5.elt

                    val page_range : int -> int option ->
                      (int option * bool) list

              end) = struct

  let s_name = ss_ M.name
  let s_no_name = ss_ ("No " ^ M.name)

  let title_id = Printf.sprintf "%s-title" M.name
  let loading_id = Printf.sprintf "%s-loading" M.name
  let table_id = Printf.sprintf "%s-table" M.name
  let footer_id = Printf.sprintf "%s-footer" M.name

  let current_page = ref 1
  let current_size = ref M.page_size
  let min_size, max_size = 5, 50

  let make_common ?(footer=false) ?(suf_id = "") ~before ~after () =
    let panel_footer_content =
      if not footer then None
      else
        Some [ div [
            div ~a:[ a_class [ row ] ] [
              div ~a:[ a_class [clg4; cxs5] ] [];
              div ~a:[ a_id (footer_id ^ suf_id) ; a_class [ clg8; cxs7; "paginate"] ]
                [] ] ] ] in
      make_panel
        ~panel_title_content:(
          div [
            div ~a:[ a_class [ row ] ] [
              div ~a:[  a_id (title_id ^ suf_id) ; a_class [ clg4; cxs5 ] ]
                [ M.title_span (-1)  ] ;
              div ~a:[ a_id (loading_id ^ suf_id) ; a_class [ clg8; cxs7; "paginate"] ]
                [Common.make_loading_gif [ "loading-heading" ] ]
            ]
          ])
        ~panel_body_content:
          ([ div
               (before @ [M.content_maker (Loading (table_id ^ suf_id))] @ after) ])
        ?panel_footer_content ()

  let make ?(footer=false) ?(suf_id = "") ?(panel_class = []) ?(before=[]) ?(after=[]) () =
    div ~a:[ a_class panel_class ] [
      make_common ~footer ~suf_id ~before ~after ()
    ]

  let make_clg12 ?(footer=false) ?(suf_id = "") ?(before=[]) ?(after=[]) () =
    make ~footer ~suf_id ~panel_class:[ clg12 ] ~before ~after ()

  let make_clg8 ?(suf_id = "") ?(before=[]) ?(after=[]) () =
    div ~a:[ a_class [ clg8 ] ] [
          div ~a:[ a_class [ panel; panel_primary ]] [
                div ~a:[ a_class [ panel_heading ] ] [
                      div [
                            h3 ~a:[ a_id title_id;
                                    a_class [ clg8; panel_title ] ]
                               [M.title_span (-1) ];
                            div ~a:[ a_id loading_id ]
                                [Common.span_loading_gif [ "loading-heading";
                                                           clg4]]
                    ]
                    ] ;
                div ~a:[ a_class [ panel_body ] ]
                  [ div
                      (before @ [M.content_maker (Loading (table_id ^ suf_id))] @ after) ]
              ]
        ]

  let paginate ?(page_sizer=true) ?(suf_id = "")
      ?urlarg_page ?urlarg_size ?nrows ?(title_span=M.title_span) updater =
    let tdiv = find_component (title_id ^ suf_id) in
    Manip.removeChildren tdiv;
    let tspan = title_span (match nrows with
        | None -> -1
        | Some nrows -> nrows) in
    Manip.appendChild tdiv tspan;
    let nb_pages page_size =
      match nrows with
      | None -> None
      | Some nb -> Some (max 1 ( (nb-1) / page_size + 1))
    in
    (* page is between 0 and nb_pages-1 *)
    let rec update_panel page page_size (rows: M.data) =
      let nb_pages = nb_pages page_size in
      let pages = M.page_range page nb_pages in
      let seps = ref [] in
      let mk_onclick_page page =
        a_onclick (fun _ -> do_update page page_size; true) in
      let mk_onclick_size new_page_size id =
        (fun _ ->
            let b = find_component id in
            let elt = Tyxml_js.To_dom.of_select b in
            elt##innerHTML <-
              Js.string (Printf.sprintf "%s: %d"
                           (t_ s_rows) page_size);
            (* Remains on the "current page", i.e. with the same first operations *)
            let new_page = page * page_size / new_page_size in
            do_update new_page new_page_size) in
      let make_paginate ?(prefix="") () =
        match nb_pages with
        | Some (0|1) ->
          span ~a:[a_class ["hidden"]] []
        | _ ->
          let items = List.map (function
              | Some p, true ->
                li ~a:[a_class ["active"]]
                  [a [ pcdata @@ string_of_int (p+1) ]]
              | Some p, false ->
                li [a ~a:[mk_onclick_page p; a_class ["hidden-xs"; "hidden-sm"]]
                      [ pcdata @@ string_of_int (p+1) ]]
              | None, _ ->
                let link, id = PageInput.link_with_input ~prefix "..." in
                seps := id :: !seps;
                li ~a:[a_class ["hidden-xs"; "hidden-sm"; ]]
                  [ link ]
            ) pages in
          let prev =
            if page = 0 then
              li ~a:[a_class ["disabled"]] [a [pcdata "«"]]
            else
              li [a ~a:[mk_onclick_page (page-1)] [pcdata "«"]]
          in
          let next =
            match nb_pages with
            | Some p when p = page+1 ->
               li ~a:[a_class ["disabled"]] [a [pcdata "»"]]
            | _ ->
               li [a ~a:[mk_onclick_page (page+1)] [pcdata "»"]]
          in
          let mobile_select =
            let link, id = PageInput.link_with_input ~prefix "..." in
            seps := id :: !seps;
            li ~a:[a_class ["hidden-lg"; "hidden-xl"; ]]
              [ link ] in
          ul ~a:[a_class ["pagination"; "pagination-sm"]]
            ( prev :: items @ [mobile_select; next] )
      in
      let make_page_sizer () =
        if not page_sizer then span ~a:[a_class ["hidden"]] []
        else
          let open Bootstrap_helpers.Menu in
          let sizes = [ min_size; 10; 20; max_size ] in
          let id = Common.make_id M.name "size-selector" in

          let select = List.map (fun p ->
              Action ([], mk_onclick_size p id, pcdata (string_of_int p)))
              sizes in
          bootstrap_dropdown_button
            ~btn_class:[ btn_default; btn_sm; ]
            ~ctn_class:["hidden-xs"; "page-size"]
            id
            [ pcdata (Printf.sprintf "%s: %d" (t_ s_rows) page_size) ]
            select
      in

      let span_paging_up_top = make_paginate ~prefix:"top-" () in
      let span_paging_up_bot = make_paginate ~prefix:"bot-" () in
      let span_sizing = make_page_sizer () in
      current_page := page+1;
      current_size := page_size;
      set_page_in_browser_url ?urlarg:urlarg_page (page+1);
      set_size_in_browser_url ?urlarg:urlarg_size (!current_size);
      let loading_div = find_component (loading_id ^ suf_id) in
      Manip.removeChildren loading_div;
      Manip.appendChildren loading_div [span_sizing; span_paging_up_top];

      let container = find_component (table_id ^ suf_id) in
      let table = M.content_maker (Content (M.name, s_name, s_no_name, rows)) in
      Manip.removeChildren container ;
      Manip.appendChild container table;
      begin match Manip.by_id (footer_id ^ suf_id) with
      | Some footer ->
        Manip.removeChildren footer ;
        Manip.appendChild footer span_paging_up_bot
      | _ -> () end;
      PageInput.init_inputs !seps !current_page nb_pages
        (fun p -> do_update p page_size)


    and do_update page page_size =
      updater page page_size (update_panel page page_size)
    in
    let r = find_size ?urlarg:urlarg_size !current_size in
    let r = max min_size (min r max_size) in
    let p = find_page ?urlarg:urlarg_page !current_page - 1 in
    let p = match nb_pages r with
      | Some nb_pages -> max 0 (min p (nb_pages-1))
      | None -> p
    in
    do_update p r

  let paginate_fun ?page_sizer ?suf_id
      ?urlarg_page ?urlarg_size to_trs ?nrows ?title_span xhr =
    paginate ?page_sizer ?suf_id ?urlarg_page ?urlarg_size ?nrows ?title_span
      (fun page page_size cont ->
        xhr page page_size (fun v ->
             cont (to_trs v)));
    ()

end


let table_maker table_class theads
                (data : Html_types.tr Tyxml_js.Html5.elt list content) =
  match data with
  | Loading table_id ->
     div ~a: [ a_class [ btable_responsive ]; a_id table_id] [
       tablex ~a:[ a_class [ btable; table_class ] ] [
         tbody
           ( (theads()) :: [
                 tr [ td [ Lang.pcdata_t s_fetching_data ] ] ] )] ]
  | Content (_name, _s_name, s_no_name, rows) -> match rows with
    | [] -> div ~a:[ a_class [ clg12; cxs12 ] ] [ pcdata_t s_no_name ]
    | _ ->
      tablex ~a:[ a_class [ btable; table_class; btable_striped ] ] [
        tbody ( (theads()) :: rows)]


module MakePageTable(M: sig

                         val name : string
                         val title_span : int ->
                                          [> Html_types.span ] Tyxml_js.Html5.elt
                         val page_size : int

                         val theads : unit -> theads
                         val table_class : string

                     end) = struct

  include MakePageGen(struct
                    let content_maker = table_maker M.table_class
                                                    M.theads
                    include M
                    type data =
                      Html_types.tr Tyxml_js.Html5.elt list

                    let page_range = large_page_range
                  end)

  let paginate_all ?page_sizer ?suf_id ?urlarg_page ?urlarg_size rows =
    let nrows = Array.length rows in
    let updater page page_size cont =
      let pos = page * page_size in
      let next_pos = min (pos + page_size) nrows in
      cont (Array.to_list (Array.sub rows pos (next_pos-pos)))
    in
    paginate ?page_sizer ?suf_id ?urlarg_page ?urlarg_size ~nrows updater;
    ()

end


let div_maker data =
  match data with
  | Loading table_id ->
     div ~a:[ a_id table_id ] [Lang.pcdata_t s_fetching_data ]
  | Content (_name, _s_name, s_no_name, rows) ->
     match rows with
     | [] -> div ~a:[ a_class [ clg12; cxs12 ] ] [ pcdata_t s_no_name ]
     | _ -> div (List.map
                   (fun row ->
                     div ~a:[ a_class [ clg12; cxs12; "no-overflow" ]]
                          [row])
                   rows)

let _short_page_range page nb_pages =
  let make_page i = Some i, i = page in
  match nb_pages with
  | None ->
     if page = 0 then
       [make_page 0; make_page 1; None,false]
     else
       [make_page 0; make_page page; None,false]
  | Some nb_pages ->
     match page with
     | 0 ->
        [make_page 0; make_page 1; make_page (nb_pages-1)]
     | _ ->
        if page < nb_pages-1 then
          [make_page 0; make_page page; make_page (nb_pages-1)]
        else
          [make_page 0; make_page (page-1); make_page (nb_pages-1)]

module MakePageNoTable
         (M: sig

              val name : string
              val title_span : int ->
                               [> Html_types.span ]
                               Tyxml_js.Html5.elt
              val page_size : int

            end) = struct

  include MakePageGen(struct
                       include M
                       type data = Html_types.div Tyxml_js.Html5.elt list
                       let content_maker = div_maker
                       let page_range = large_page_range
                     end)


  let paginate_all ?page_sizer ?suf_id ?urlarg_page ?urlarg_size rows =
    let nrows = Array.length rows in
    let updater page page_size cont =
      let pos = page * page_size in
      let next_pos = min (pos + page_size) nrows in
      cont (Array.to_list (Array.sub rows pos (next_pos-pos)))
    in
    paginate ?page_sizer ?suf_id ?urlarg_page ?urlarg_size ~nrows updater;
    ()

end

let select_rows page page_size l =
  List.rev @@ snd @@
  List.fold_left (fun (i, acc) x ->
      (i + 1, if i >= page * page_size && i < (page + 1) * page_size then x :: acc
       else acc)) (0,[]) l

module MakePageTableList(
    M: sig
      type data
      val name : string
      val title_span : int -> [> Html_types.span ] Tyxml_js.Html5.elt
      val page_size : int
      val theads : unit -> theads
      val table_class : string

    end) = struct

  let s_name = ss_ M.name
  let s_no_name = ss_ ("No " ^ M.name)

  let title_id = Printf.sprintf "%s-title" M.name
  let loading_id = Printf.sprintf "%s-loading" M.name
  let table_id = Printf.sprintf "%s-table" M.name
  let footer_id = Printf.sprintf "%s-footer" M.name

  let current_page = ref 1
  let current_size = ref M.page_size
  let min_size, max_size = 5, 50
  let datas : M.data list ref = ref []

  let make_paginate ?(prefix="") page page_size seps f =
    let mk_onclick_page page =
      a_onclick (fun _ -> f page page_size; true) in
    let nb_pages = max 1 ( ((List.length !datas - 1) / page_size + 1)) in
    let pages = large_page_range page (Some nb_pages) in
    if nb_pages <= 1 then span ~a:[a_class ["hidden"]] []
    else (
      let items = List.map (function
          | Some p, true ->
            li ~a:[a_class ["active"]] [a [ pcdata @@ string_of_int (p+1) ]]
          | Some p, false ->
            li [a ~a:[mk_onclick_page p; a_class ["hidden-xs"; "hidden-sm"]]
                  [ pcdata @@ string_of_int (p+1) ]]
          | None, _ ->
            let link, id = PageInput.link_with_input ~prefix "..." in
            seps := id :: !seps;
            li ~a:[a_class ["hidden-xs"; "hidden-sm"; ]] [ link ]
        ) pages in
      let prev =
        if page = 0 then li ~a:[a_class ["disabled"]] [a [pcdata "«"]]
        else li [a ~a:[mk_onclick_page (page-1)] [pcdata "«"]] in
      let next =
        if nb_pages = page + 1 then li ~a:[a_class ["disabled"]] [a [pcdata "»"]]
        else li [a ~a:[mk_onclick_page (page+1)] [pcdata "»"]] in
      let mobile_select =
        let link, id = PageInput.link_with_input ~prefix "..." in
        seps := id :: !seps;
        li ~a:[a_class ["hidden-lg"; "hidden-xl"; ]] [ link ] in
      ul ~a:[a_class ["pagination"; "pagination-sm"]]
        ( prev :: items @ [mobile_select; next] ))

  let make_page_sizer page_sizer page page_size f =
    let mk_onclick_size new_page_size id _ =
      let b = find_component id in
      let elt = Tyxml_js.To_dom.of_select b in
      elt##innerHTML <- Js.string (Printf.sprintf "%s: %d" (t_ s_rows) page_size);
      let new_page = page * page_size / new_page_size in
      f new_page new_page_size in
    if not page_sizer then span ~a:[a_class ["hidden"]] []
    else
      let open Bootstrap_helpers.Menu in
      let sizes = [ min_size; 10; 20; max_size ] in
      let id = Common.make_id M.name "size-selector" in
      let select = List.map (fun p ->
          Action ([], mk_onclick_size p id, pcdata (string_of_int p)))
          sizes in
      bootstrap_dropdown_button
        ~btn_class:[ btn_default; btn_sm; ]
        ~ctn_class:["hidden-xs"; "page-size"]
        id
        [ pcdata (Printf.sprintf "%s: %d" (t_ s_rows) page_size) ]
        select

  let make
      ?(page_sizer=true) ?(suf_id="") ?urlarg_page ?urlarg_size
      ?(footer=false) ?(panel_class=[]) ?(before=[]) ?(after=[])
      to_trs l =
    datas := l;
    let nb_pages page_size = max 1 ( ((List.length !datas - 1) / page_size + 1)) in
    let rec update_panel page page_size =
      let nb_pages = nb_pages page_size in
      let seps = ref [] in
      let span_paging_up_top =
        make_paginate ~prefix:"top-" page page_size seps update_panel in
      let span_paging_up_bot =
        make_paginate ~prefix:"bot-" page page_size seps update_panel in
      let span_sizing = make_page_sizer page_sizer page page_size update_panel in
      current_page := page+1;
      current_size := page_size;
      set_page_in_browser_url ?urlarg:urlarg_page (page+1);
      set_size_in_browser_url ?urlarg:urlarg_size (!current_size);
      let loading_div = find_component (loading_id ^ suf_id) in
      Manip.removeChildren loading_div;
      Manip.appendChildren loading_div [span_sizing; span_paging_up_top];
      let rows = select_rows page page_size !datas in
      let rows = to_trs rows in
      let container = find_component (table_id ^ suf_id) in
      let table = table_maker M.table_class M.theads
          (Content (M.name, s_name, s_no_name, rows)) in
      Manip.removeChildren container ;
      Manip.appendChild container table;
      begin match Manip.by_id (footer_id ^ suf_id) with
      | Some footer ->
        Manip.removeChildren footer ;
        Manip.appendChild footer span_paging_up_bot
      | _ -> () end;
      PageInput.init_inputs !seps !current_page (Some nb_pages)
        (fun p -> update_panel p page_size) in

    let r = find_size ?urlarg:urlarg_size !current_size in
    let r = max min_size (min r max_size) in
    let p = find_page ?urlarg:urlarg_page !current_page - 1 in
    let p = max 0 (min p (nb_pages r - 1)) in
    let seps = ref [] in
    let span_paging_up_top =
      make_paginate ~prefix:"top-" p r seps update_panel in
    let span_paging_up_bot =
      make_paginate ~prefix:"bot-" p r seps update_panel in
    let span_sizing = make_page_sizer page_sizer p r update_panel in
    let rows = select_rows p r !datas in
    let rows = to_trs rows in
    let table = table_maker M.table_class M.theads
        (Content (M.name, s_name, s_no_name, rows)) in
    (* PageInput.init_inputs !seps !current_page (Some (nb_pages r))
     *     (fun p -> update_panel p r); *)
    let main_panel =
      let panel_footer_content =
        if not footer then None
        else
          Some [ div [
              div ~a:[ a_class [ row ] ] [
                div ~a:[ a_class [clg4; cxs5] ] [];
                div ~a:[ a_id (footer_id ^ suf_id) ; a_class [ clg8; cxs7; "paginate"] ]
                  [span_paging_up_bot] ] ] ] in
      make_panel
        ~panel_title_content:(
          div [
            div ~a:[ a_class [ row ] ] [
              div ~a:[  a_id (title_id ^ suf_id) ; a_class [ clg4; cxs5 ] ]
                [ M.title_span (List.length !datas)  ] ;
              div ~a:[ a_id (loading_id ^ suf_id) ; a_class [ clg8; cxs7; "paginate"] ]
                [span_sizing; span_paging_up_top]
            ]
          ])
        ~panel_body_content:
          ([ div
               (before @ [
                   div ~a: [ a_class [ btable_responsive ];
                             a_id (table_id ^ suf_id) ] [ table ] ] @ after) ])
        ?panel_footer_content ()
    in
    div ~a:[ a_class panel_class ] [ main_panel ]

end
