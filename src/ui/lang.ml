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

open StringCompat

(* For now, translations are hardcoded in the javascript.
   However, we have two dynamic mechanisms:
   * the client will download "lang-LG.json" to download translations
       for the "LG" language.
   * the client will use the "lang_url" argument to try to download
       another language file
   In both case, the successful download of a translation file triggers
    a complete redraw of the interface, hopefully with the new translations
    in use.
*)

module Encoding = struct
  open Json_encoding

  let translations =
    obj2
      (req "lang" string)
      (req "translations" (list (list string)))
end

let set_lang name =
  Jslang.set ~set:`Cookie name

let () =
  match Jsloc.lang () with
  | Some lang -> set_lang lang
  | _ -> ()

let init redraw =

  let download_lang_url lang_file =
    Xhr.get "lang" lang_file
      (fun res ->
         (try
            let lang, translations =
              EzEncoding.destruct Encoding.translations res
            in
            let set = ref StringSet.empty in
            let translations =
              List.fold_left (fun translations pair ->
                  match pair with
                  | s :: tr ->
                    if StringSet.mem s !set then
                      Js_utils.log "duplicate translation: %S" s;
                    set := StringSet.add s !set;
                    let tr = match tr with
                        [] -> s
                      | tr :: _ -> tr
                    in
                    (s, tr) :: translations
                  | [] -> translations) [] translations
            in
            StringSet.iter (fun s ->
                if not (StringSet.mem s !set) then
                  Js_utils.log "No transaction for %S" s
              ) (Jslang.string_ids());
            Jslang.add_translations lang translations;
            set_lang lang;
            redraw ()
          with exn ->
            Js_utils.log "Cannot parse lang translations: %s" (Printexc.to_string exn)
         );
      )
  in
  begin
    match Jslang.get () with
    | None -> ()
    | Some lang ->
      download_lang_url (Printf.sprintf "lang-%s.json" lang)
  end;
  begin
    match Jsloc.find_arg "lang_url" with
    | None -> ()
    | Some lang_url ->
      download_lang_url lang_url
  end



type text = Jslang.string_id
let t_ = Jslang.t_
let s_ = Jslang.s_

let pcdata_t = Jslang.pcdata_t
let pcdata_s = Jslang.pcdata_s
let ss_ = Jslang.ss_
let id_ = Jslang.id_
