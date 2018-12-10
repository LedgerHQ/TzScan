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
open Bootstrap_helpers.Grid
open Bootstrap_helpers.Icon

let find_url_arg arg =
  Misc.list_find_opt (fun (k, _v) -> k = arg) Url.Current.arguments

let make_doc ?base_url () =
  match Manip.by_id "api-doc" with
  | Some _ -> ()
  | None ->

    let documentation =
      (*       Api_documentation.files @ *)
      List.map (fun section ->
          EzAPI.section_name section,
          EzAPI.md_of_services ~section ?base_url
            (Api_info.files @ Service_doc.doc)
        ) (Service.V2.sections @
           (match find_url_arg "doc" with
            | None -> []
            | Some _ -> Service.V2.other_sections))
    in
    let left = div ~a:[ a_class [ clg9 ] ] (
        List.map
          (fun (file, content) ->
             log "%s" (Filename.remove_extension file);
             let div_section = div ~a:[ a_id (Filename.remove_extension file) ] [] in
             Manip.setInnerHtml div_section (Omd.(to_html @@ of_string content) ^ "<hr/>\n");
             div_section) documentation ) in
    let right =
      div ~a:[ a_class [ clg3 ] ] [
        ul ~a:[ a_class [ "list-group" ] ] @@
        List.map (fun (file, _ ) ->
            let section_name = Filename.remove_extension file in
            li ~a:[ a_class [ "list-group-item" ] ] [
              a ~a:[ a_href ("#" ^ section_name) ] [
                pcdata (String.capitalize_ascii section_name)
              ]
            ])
          documentation ;
        div ~a:[ a_class [ "app-version" ]] [
          div [ pcdata "Javascript version" ];
          div [ clock_icon () ; space_icon () ; pcdata TzscanConfig.en_date ];
          div [ code_branch_icon () ; space_icon () ;
                pcdata @@ Printf.sprintf "Commit: %s" TzscanConfig.commit];
        ];
        div ~a:[ a_class [ "app-version" ]; a_id "api-version" ] [
          div [ pcdata "API Server version" ];
          div [ clock_icon () ; space_icon () ;
                pcdata Infos.api.api_versions.server_build ];
          div [ code_branch_icon () ; space_icon () ;
                pcdata (Printf.sprintf "Commit: %s"
                          Infos.api.api_versions.server_commit) ]

        ]
      ]
    in
    let container =
      div ~a:[ a_id "api-doc"; a_class [ row ] ] [ left; right ] in
    Common.update_main_content container;

    (* Add a button Show/Hide and hide every <pre> longer than 2 lines *)
    begin
      let nbuttons = ref 0 in
      match Manip.by_id "api-doc" with
      | None -> assert false
      | Some doc ->
        let columns = Manip.children doc in
        let sections = Manip.children (List.hd columns) in
        List.iter (fun section ->
            let divs = Manip.children section in
            match Js.Opt.to_option (Dom_html.CoerceTo.div
                                      (Manip.get_elt "div" section)) with
            | None -> ()
            | Some section_div ->
              List.iter (fun ele ->
                  try
                    match Js.Opt.to_option (Dom_html.CoerceTo.pre
                                              (Manip.get_elt "pre" ele)) with
                    | None -> ()
                    | Some pre ->
                      let html = Js.to_string pre##innerHTML in
                      let nlines = ref 0 in
                      for i = 0 to String.length html - 1 do
                        if html.[i] = '\n' then incr nlines;
                      done;
                      if !nlines > 2 then
                        let id = string_of_int !nbuttons in
                        incr nbuttons;
                        let but = button ~a:[ a_id id ]
                            [pcdata "Show/Hide"] in
                        let div = div [ but ] in
                        let div = Tyxml_js.Html5.toelt div in
                        Dom.appendChild section_div div;
                        Dom.replaceChild section_div div pre;
                        Dom.appendChild div pre;
                        let b = find_component id in
                        let state = ref true in
                        let handler _ =
                          state := not !state;
                          pre##style##display <-
                            Js.string
                              (if !state then "none" else "");
                          Js._true
                        in
                        (Tyxml_js.To_dom.of_button b)##onclick <-
                          Dom_html.handler handler;
                        pre##style##display <- Js.string "none";
                  with _exn ->
                    (* Js_utils.log "exn: %s" (Printexc.to_string exn) *)
                    ()
                ) divs
          ) sections;
    end;
    ()
