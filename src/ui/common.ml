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

open Lang
open StringCompat
open Data_types
open Tezos_types
open Tyxml_js.Html5
open Js
open Js_utils
open Bootstrap_helpers.Table
open Bootstrap_helpers.Form
open EzAPI.TYPES
open Text

let html_escaped s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len -1 do
    match s.[i] with
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

(* This function is set in Main, it updates the content of the page
   depending on the path given. It is used for internal links. *)
let link_dispatcher = ref (fun (_path : string list) -> ())

let pages = Hashtbl.create 113

(* Every time we dispatch to a new page, we update this counter. It is
   used in timers to prevent timers from one page to run on another
   page after the user moved between them.  *)
let current_page = ref 0
let get_current_page () = !current_page
let dispatch path =
  try
    (Hashtbl.find pages path) ()
  with Not_found ->
    incr current_page;
    !link_dispatcher path


let add_page path updater =
  Hashtbl.add pages path updater

let redrawers = ref []
let initial_redraw_done = ref false
let register_redraw r =
  redrawers := !redrawers @ [r]
let redraw () =
  List.iter (fun f -> try f () with _ -> ()) !redrawers
let initial_redraw () =
  initial_redraw_done := true;
  redraw ()
let redraw () =
  if !initial_redraw_done then begin
    redraw ()
  end



let () =
  Random.self_init ();
  Lang.init redraw



let api_host = ref None

let base_of_host host = BASE (Jsloc.proto() ^ host)
let set_api_host host = api_host := Some (base_of_host host)

let set_api_node () =
  match !api_host with
  | Some _ -> ()
  | None ->
    let host =
      match Jsloc.find_arg "api" with
      | None ->
        let hosts = Infos.www.www_apis in
        hosts.( Random.int (Array.length hosts))

      | Some api_host ->
        match api_host with
        | ""
        | "x" -> "api-x.tzscan.io"
        | "1" | "2" | "3"
        | "4" | "5" | "6" -> Printf.sprintf "api%s.tzscan.io" api_host
        | _ -> api_host
    in
    set_api_host host

(* If API_HOST is set, it should always be used, unless another API
   server is specified in the URL arguments. *)
let () =
  if TzscanConfig.api_host <> "" then begin
    let host =
      Printf.sprintf "%s%s" TzscanConfig.api_host
        (if TzscanConfig.api_port = "" then ""
         else ":" ^ TzscanConfig.api_port )
    in
    Infos.www.www_apis <- [| host |];
    set_api_node ()
  end


let api s = match !api_host with
  | None ->
    Js_utils.log "Error: calling uninitialized API from %S" s;
    assert false
  | Some api_host -> api_host

module OptionsStorage = JsStorage.MakeLocal(struct
                             type t = string StringMap.t
                             let name = "options"
                           end)

let options = match OptionsStorage.get () with
  | None -> StringMap.empty
  | Some list -> list

let options =
  let modified = ref false in
  let options = ref options in
  List.iter (fun (arg, value) ->
      if String.length arg > 0 &&
         match arg.[0] with
         'A'..'Z' -> true
         | _ -> false
      then begin
        modified := true;
        options := StringMap.add arg value !options
      end
    ) (Jsloc.args ());
  let options = !options in
  if !modified then OptionsStorage.set options;
  options




let small_panel_number = 5
let big_panel_number = 20

type focus =
  | Focused
  | Blurred_since of float

let window_focused = ref Focused

let stop_update_delay = 3. *. 60. *. 1000. (* 3 minutes *)

let () =
  Dom_html.window##onblur <-
    Dom_html.handler (fun _ ->
        window_focused := Blurred_since (jsnew date_now ())##valueOf();
        Js._true);
  Dom_html.window##onfocus <-
    Dom_html.handler (fun _ ->
        window_focused := Focused;
        Js._true)

(* ****************** *)
let loading () = span [ pcdata_t s_loading ]
let bullshit_s = "--"
let bullshit_d = ~-1
let pcdata_ () = pcdata "--"

(* The two functions below can probably be used in operation_ui to
   refactor some code *)
let mk_row_lbl clg lbl =
  div ~a:[ a_class [ clg; "lbl" ] ] [ Jslang.pcdata_s lbl ]

let mk_row_val clg v =
  div ~a:[ a_class [ clg; "value" ] ] v

let make_id prefix hash = Printf.sprintf "%s-%s" prefix hash

let summary_id hash = make_id "summary" hash
let timestamp_id hash = make_id "ts" hash
let volume_id hash = make_id "volume" hash
let block_uncles_id hash = make_id "block_uncles" hash

let paginate_id container_id = make_id "paginate" container_id
let prev_id container_id = make_id "prev" container_id
let next_id container_id = make_id "next" container_id

let confirmation_blocks_id = "confirmation-blocks"

(* *********** *)

let safe_value s = if s = "" then bullshit_s else s

let responsive_title icon title =
  [
    span ~a:[ a_class [ "visible-xs-inline"; "visible-sm-inline" ] ] [
      icon ()
    ];
    span ~a:[ a_class [ "hidden-xs"; "hidden-sm" ] ] [
      pcdata title
    ]
  ]

let responsive_title_xs icon title =
  [
    span ~a:[ a_class [ "visible-xs-inline" ] ] [
      icon ()
    ];
    span ~a:[ a_class [ "hidden-xs" ] ] [
      pcdata title
    ]
  ]

let responsive_title_fun icon title value =
  [
    span ~a:[ a_class [ "visible-xs-inline"; "visible-sm-inline" ] ] [
      icon ()
    ];
    span ~a:[ a_class [ "hidden-xs"; "hidden-sm" ] ] [
      title value
    ]
  ]

let responsive_column_title title abbrev =
  [
    span ~a:[ a_class [ "visible-xs-inline"; "visible-sm-inline" ] ] [
      pcdata abbrev
    ];
    span ~a:[ a_class [ "hidden-xs"; "hidden-sm" ] ] [
      pcdata title
    ]
  ]

let make_home_loading_gif id classes =
  div ~a:[ a_id id; a_class classes ] [
    img
      ~alt:"loading"
      ~src:(uri_of_string "/images/white_loading.gif") ()
  ]

let make_loading_gif classes =
  div ~a:[ a_class classes ] [
    img
      ~alt:"loading"
      ~src:(uri_of_string "/images/loading.gif") ()
  ]

let span_loading_gif classes =
  span ~a:[ a_class classes ] [
    img
      ~alt:"loading"
      ~src:(uri_of_string "/images/loading.gif") ()
  ]

let link ?(args=[]) path =
  match args with
  | [] -> path
  | _ ->
    let args =
      String.concat "&"
        (List.map (fun (key, value) -> key ^ "=" ^ value) args)
    in
    if String.contains path '?' then
      Printf.sprintf "%s&%s" path args
    else
      Printf.sprintf "%s?%s" path args

let initial_args =
  List.fold_left (fun args name ->
      match Jsloc.find_arg name with
      | None -> args
      | Some value -> (name, value) :: args)
    [] ["lang"; "node"; "api"; "test"]

(* Used to add a lang=fr argument if needed *)
let a_link ?(args=[]) ?(aclass=[]) path =
  if OcpString.starts_with ~prefix:"http" path then
    (* external link *)
    [a_href (link ~args path)]
  else
    let args = initial_args @ args in
    if String.contains path '?' then
      (* internal link with internal args *)
      match aclass with
      | [] -> [ a_href (link ~args path) ]
      | _ -> [ a_href (link ~args path) ; a_class aclass ]
    else
      match aclass with
      | [] ->
        [a_onclick (fun _ ->
             Dom_html.window##history##pushState(
               Js.some (Js.string path),
               Js.string "",
               Js.some (Js.string path));
             Jsloc.set_args args;
             dispatch (OcpString.split_simplify path '/');
             let nav_bar = find_component "mainNavBar" in
             Manip.removeClass nav_bar "in";
             false);
         a_href (link ~args path) ]
      | _ ->
        [a_onclick (fun _ ->
             Dom_html.window##history##pushState(
               Js.some (Js.string path),
               Js.string "",
               Js.some (Js.string path));
             Jsloc.set_args args;
             dispatch (OcpString.split_simplify path '/');
             let nav_bar = find_component "mainNavBar" in
             Manip.removeClass nav_bar "in";
             false);
         a_href (link ~args path);
         a_class aclass ]

let () =
  Lwt_js_events.(async (fun () ->
      onpopstates (fun _popStateEvent _something ->
          dispatch (Jsloc.path ());
          Lwt.return ()
        )))

let crop_hash ?crop_len hash =
  match crop_len with
  | Some crop_len ->
    let len = String.length hash in
    if len < crop_len then hash
    else
      String.sub hash 0 crop_len ^ "..."
  | None -> hash

let make_link ?crop_len ?(args=[]) ?(aclass=[]) ?path content =
  let path = match path with
    | None -> content
    | Some path -> path
  in
  a ~a:( a_link ~args ~aclass path ) [ pcdata @@ crop_hash ?crop_len content ]

let make_link_account ?crop_len ?args account =
  make_link ?args
    (match account.alias with
     | None -> crop_hash ?crop_len account.tz
     | Some alias -> alias)
    ~path:account.tz

let make_link_level ?args hash level =
  let level_str = string_of_int level in
  make_link hash ?args ~path:level_str

let legend () =
  div ~a:[ a_class [ "legend" ] ] [
    div [
      div ~a:[ a_class [ "slot-green"; "slot-legend" ] ] [ ] ;
      span [ pcdata "Current Block" ] ;
    ] ;
    div [
      div ~a:[ a_class [ "slot-legend" ] ] [ ] ;
      span[ pcdata "No Endorsement" ] ;
    ];
    div [
      div ~a:[ a_class [ "slot-red"; "slot-legend" ] ] [ ] ;
      span [ pcdata "Another Block" ] ;
    ];
    div [
      div ~a:[ a_class [ "slot-double"; "slot-legend" ] ] [ ] ;
      span[ pcdata "Double Endorsement" ] ;
    ];
    div [
      div ~a:[ a_class [ "slot-gray"; "slot-legend" ] ] [ ] ;
      span[ pcdata "Pending Endorsement" ] ;
    ];
  ]

let is_double_endorsement i operations =
  let check =
    List.find_all (fun (_, _, e) ->
        List.exists (fun e -> e = i) e.endorse_slot) operations in
  match check with
  | _ :: [] -> false
  | _ -> true

let make_slot i = div [ pcdata @@ string_of_int i ]

let make_slots nb =
  Array.to_list @@
  Array.mapi (fun i _ ->
      let id = Printf.sprintf "slot-%d" i in
      td ~a:[ a_class [ "slot" ]; a_id id ] [ make_slot i ] )
    (Array.make nb (td []))

let make_8_endorsements_row = function
  | a :: b :: c :: d :: e :: f :: g :: h :: tl ->
    tr [ a ; b ; c ; d ; e ; f ; g ; h ], tl
  | _ -> assert false

let rec make_8_endorsements_rows slots =
  if List.length slots >= 8 then
    let row, tl = make_8_endorsements_row slots in
    row :: (make_8_endorsements_rows tl)
  else
    [ tr slots ]

let make_endorsements_slots slots =
  let rows = make_8_endorsements_rows slots in
  tablex ~a:[ a_class [ btable; btable_bordered ] ]
    [ tbody rows ]

let update_endorsements_slots bhash slots operations =
  List.mapi (fun i row ->
      begin try
          let (op_hash, op_block_hash, e) =
            List.find
              (fun (_, _, e) ->
                List.exists (fun e -> i = e) e.endorse_slot
              ) operations in
          let row =
            td ~a:[ a_class ["slot"] ]
              [ a ~a:( a_link op_hash ) [ make_slot i ] ] in
          if is_double_endorsement i operations then
            Js_utils.Manip.addClass row "slot-double"
          else if op_block_hash = Utils.pending_block_hash
                  && e.endorse_block_hash = bhash then
            Js_utils.Manip.addClass row "slot-gray"
          else if e.endorse_block_hash = bhash then
            Js_utils.Manip.addClass row "slot-green"
          else
            Js_utils.Manip.addClass row "slot-red" ;
          row
        with Not_found -> row
      end) slots


let replace_div_by_id id list =
  match Manip.by_id id with
  | None ->
    Js_utils.log "replace_div_by_id %S: div not found" id
  | Some div ->
    Manip.replaceChildren div list

let update_main_content div =
  replace_div_by_id "content" [div];
     (* Hack for scrollbar *)
  ignore (Js.Unsafe.eval_string "jQuery('.scrollbar-macosx').scrollbar();")

let auto_refresh = ref (match Jsloc.find_arg "refresh" with
    | Some ( "0" | "false" | "n" | "N" ) -> false
    | None | Some _ -> true)

let do_and_update_every time_s f =
  let current_page = get_current_page () in
  f ();
  let cb () =
    if !auto_refresh && current_page = get_current_page () then
      match !window_focused with
      | Focused -> f ()
      | Blurred_since last ->
        if (jsnew date_now ())##valueOf() -. last <= stop_update_delay
        then f ()
        else ()
  in
  Dom_html.window##setInterval(
    Js.wrap_callback cb,
    float_of_int time_s *. 1000.)
  |> ignore
  (* let unfocused = ref false in
   * let enter_loop () =
   *   f ();
   *   Dom_html.window##setInterval(Js.wrap_callback f,
   *                                float_of_int time_s *. 1000.)
   * in
   * let interval_id = ref (enter_loop ()) in
   * Dom_html.window##onblur <-
   *   Dom_html.handler (fun _ ->
   *       print_endline("blured");
   *       Dom_html.window##clearInterval(!interval_id);
   *       unfocused := true;
   *       Js._true);
   * Dom_html.window##onfocus <-
   *   Dom_html.handler (fun _ ->
   *       print_endline("focused");
   *       if !unfocused then begin
   *         unfocused := true;
   *         interval_id := enter_loop ()
   *       end;
   *       Js._true) *)

let get_fitness raw_fitness =
  match String.split_on_char ' ' raw_fitness with
  | [ _version ; fitness ] ->
     (* TODO: do something with version ? *)
     int_of_float @@ float_of_string @@ "0x" ^ fitness
  | _ -> 0



module Xhr = EzXhr

let request_xhr ?error s = EzXhr.get1 ?error (api "request_xhr") s

let timestamp hash update =
  if hash <> Utils.pending_block_hash && hash <> Utils.orphan_block_hash then
    request_xhr Service.V1.timestamp "Common.timestamp" update hash

let volume hash update =
  if hash <> Utils.pending_block_hash && hash <> Utils.orphan_block_hash then
    request_xhr Service.V1.volume "Common.volume" update hash

let level hash update =
  if hash <> Utils.pending_block_hash && hash <> Utils.orphan_block_hash then
    request_xhr Service.V1.level "Common.level"
      (fun level -> update level.lvl_level) hash

let block_hash_level level =
  request_xhr Service.V1.block_hash_level "Common.block_hash_level"
    (fun hash -> dispatch [hash]) level

let balance hash update =
  request_xhr Service.V1.node_account "Account.details" update hash


(* Pagination utilities *)

let set_url_arg ?(default = "1") arg value =
  let args = Jsloc.args () in
  let replaced = ref false in
  let args = List.fold_right (fun (k, v) newargs ->
                 if k = arg then begin
                     replaced := true;
                     if value = default then
                       newargs
                     else
                       (k, value) :: newargs
                   end
                 else (k, v) :: newargs
               ) args [] in
  let args = if !replaced || value = default then args else
      (arg, value) :: args in
  Jsloc.set_args args


let page_range page nb_pages =
  let pages =
    if nb_pages <= 5
    then Misc.list_init nb_pages (fun i -> Some i, i = page)
    else if page <= 2
    then Misc.list_init 3 (fun i -> Some i, i = page)
    else if page >= nb_pages - 3
    then Misc.list_init 3 (fun i ->
        let p = i + nb_pages - 3 in
        Some p, p = page
      )
    else [ Some page, true ]
  in
  match pages, List.rev pages with
  | [], _ -> pages
  | (Some first, _) :: _, (Some last, _) :: _ ->
    let prefix =
      if first <> 0 then [ Some 0, false; None, false ]
      else [] in
    let suffix =
      if last <> nb_pages - 1 then [ None, false; Some (nb_pages - 1), false ]
      else [] in
    prefix @ pages @ suffix
  | _ -> assert false

let make_fetching () =  Lang.pcdata_t s_fetching_data

let cl_title = responsive_column_title
let cl_icon = responsive_title
let cl_icon_xs = responsive_title_xs

let get_ele_by_id coerce id =
  match Js.Opt.to_option
          (Dom_html.window##document##getElementById (Js.string id))
  with
  | None -> failwith "get_ele_by_id: id not found"
  | Some ele ->
     match Js.Opt.to_option @@ coerce ele with
     | None -> failwith "get_ele_by_id: id not a div"
     | Some div -> div

let get_div_by_id = get_ele_by_id Dom_html.CoerceTo.div
let get_img_by_id = get_ele_by_id Dom_html.CoerceTo.img

let account_w_blockies ?(scale=2) ?(aclass=[]) ?(before=[]) ?(after=[])
    ?crop_len ?args account =
    td ~a:[a_class ("account-w-blockies" :: aclass)]
      (before @ [ Base58Blockies.create ~scale account.tz;
        make_link_account ?crop_len ?args account ] @ after)

let account_w_blockies_no_link ?(scale=2) ?(tdaclass=[]) ?(txtaclass=[])hash =
    td ~a:[a_class ("account-w-blockies" :: tdaclass)]
      [ Base58Blockies.create ~scale hash;
        span ~a:[ a_class txtaclass] [ pcdata hash ] ]

let get_transactions ops =
  List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous _ -> acc
      | Sourced sop ->
         begin match sop with
         | Consensus _ | Amendment _ | Dictator _ -> acc
         | Manager (_, src, list) ->
            List.fold_left (fun acc mop ->
                match mop with
                | Transaction tr ->
                   (op.op_hash, op.op_block_hash, src, tr) :: acc
                | Origination _ | Reveal _ | Delegation _ -> acc
              ) acc list
         end
    ) [] ops

let get_delegations ops =
  List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous _ -> acc
      | Sourced sop ->
         begin match sop with
         | Consensus _ | Amendment _ | Dictator _ -> acc
         | Manager (_, src, list) ->
            List.fold_left (fun acc mop ->
                match mop with
                | Delegation del ->
                   (op.op_hash, op.op_block_hash, src, del) :: acc
                | Origination _ | Reveal _ | Transaction _ -> acc
              ) acc list
         end
    ) [] ops

let get_originations ops =
  List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous _ -> acc
      | Sourced sop ->
         begin match sop with
         | Consensus _ | Amendment _ | Dictator _ -> acc
         | Manager (_, src, list) ->
            List.fold_left (fun acc mop ->
                match mop with
                | Origination ori ->
                   (op.op_hash, op.op_block_hash, src, ori) :: acc
                | Reveal _ | Transaction _ | Delegation _ -> acc
              ) acc list
         end
    ) [] ops

let get_endorsement_op_type op = match op with
  | Anonymous _ -> None
  | Sourced sop ->
     begin match sop with
     | Consensus Endorsement endorse -> Some endorse
     | _ -> None
     end

let get_endorsements ops =
  List.rev @@ List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous _ -> acc
      | Sourced sop ->
         begin match sop with
         | Consensus Endorsement endorse ->
            (op.op_hash, op.op_block_hash, endorse) :: acc
         | _ -> acc
         end
    ) [] ops

let get_activations ops =
  List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous list ->
         List.fold_left (fun acc aop ->
             match aop with
             | Activation act ->
                (op.op_hash, op.op_block_hash, act) :: acc
             | _ -> acc)
           acc list
      | Sourced _ -> acc
    ) [] ops

let get_double_baking_evidence ops =
  List.rev @@ List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous list ->
         List.fold_left (fun acc aop ->
             match aop with
             | Double_baking_evidence dbe ->
                (op.op_hash, op.op_block_hash, dbe) :: acc
             | _ -> acc)
           acc list
      | Sourced _ -> acc
    ) [] ops

let get_double_endorsements_evidence ops =
  List.rev @@ List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous list ->
         List.fold_left (fun acc aop ->
             match aop with
             | Double_endorsement_evidence dee ->
                (op.op_hash, op.op_block_hash, dee) :: acc
             | _ -> acc)
           acc list
      | Sourced _ -> acc
    ) [] ops

let get_seed_nonce_revelations ops =
  List.fold_left (fun acc op ->
      match op.op_type with
      | Anonymous list ->
         List.fold_left (fun acc aop ->
             match aop with
             | Seed_nonce_revelation nonce ->
                (op.op_hash, op.op_block_hash, nonce) :: acc
             | _ -> acc)
           acc list
      | Sourced _ -> acc
    ) [] ops


let failed_class failed =
  if failed then [ "danger" ] else []

let shuffle l =
  List.map (fun v -> Random.int 1_000_000, v) l
  |> List.sort (fun (i, _) (i', _) -> compare i i')
  |> List.map snd

let choose_name account = match account.alias with
  | None -> account.tz
  | Some alias -> alias

let local_aliases = Hashtbl.create 11

let pcdata_account account =
  pcdata (match account.alias with
      | Some alias -> alias
      | None ->
        try
          Hashtbl.find local_aliases account.tz
        with Not_found -> account.tz
    )

let hash_to_name ?alias tz = {tz; alias}

let time_diff timestamp =
  let now = jsnew date_now () in
  let timestamp_f = date##parse (Js.string timestamp) in
  timestamp_f -. date##parse(now##toString())

let make_options ?(title="Cycle:") id_container arr update =
  let container = find_component id_container in
  let n = Array.length arr in
  let options =
    Array.to_list @@ Array.mapi (fun i s ->
        if i=0 then option ~a:[ a_selected () ] (pcdata s)
        else option (pcdata s)) arr in
  let select_elt =
    select ~a:[ a_class [form_control] ] options in
  Manip.Ev.onchange_select select_elt (fun _e ->
      let select_eltjs = Tyxml_js.To_dom.of_select select_elt in
      let opt = select_eltjs##options##item(select_eltjs##selectedIndex) in
      let selection =
        Js.Opt.case opt
          (fun () -> "")
          (fun opt ->
             try Js.to_string opt##value
             with _ -> "") in
      update selection;
      true);
  let content = form ~a:[ a_class [ form_inline ] ] [
      div ~a:[ a_class [ form_group ] ] [
        label [ pcdata title; Bootstrap_helpers.Icon.space_icon () ] ;
        select_elt ] ] in
  Manip.removeChildren container ;
  Manip.appendChild container content ;
  if n <> 0 then update arr.(0)

let update_from_attr ?(attr="data-value") f elt =
  let elt2 = Manip.get_elt attr elt in
  match Js.Opt.to_option elt2##getAttribute(Js.string attr) with
  | None -> Manip.replaceChildren elt (f "")
  | Some value -> Manip.replaceChildren elt (f (Js.to_string value))

let update_by_class ?(attr="data-value") cl f =
  let l = Manip.by_class cl in
  List.iter (update_from_attr ~attr f) l

let download_button elt xhr =
  button ~a:[ a_button_type `Submit; a_onclick (fun _e ->
      xhr (fun s ->
          Dom_html.window##location##href <- Js.string ("/download/" ^ s));
      true) ] elt

let compute_fees manager_ops =
  List.fold_left (fun acc op ->
      match op with
      | Transaction tr -> Int64.add tr.tr_fee acc
      | Origination ori -> Int64.add ori.or_fee acc
      | Delegation del -> Int64.add del.del_fee acc
      | Reveal rvl -> Int64.add rvl.rvl_fee acc) 0L manager_ops


let img_path logo =
  if String.length logo >= 7 && String.sub logo 0 7 = "http://" then logo
  else Printf.sprintf "/images/%s" logo
