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

module Services = struct

  let info : www_server_info EzAPI.service0 =
    EzAPI.service
      ~params:[]
      ~name:"info"
      ~output:Api_encoding.WWW.www_server_info
      EzAPI.Path.(root // "info.json" )

end

let host =
  match Jsloc.url() with
  | Url.Http hu ->
    Printf.sprintf "http://%s:%d" hu.Url.hu_host hu.Url.hu_port
  | Url.Https hu ->
    Printf.sprintf "https://%s:%d" hu.Url.hu_host hu.Url.hu_port
  | _ -> "https://tzscan.io"

let info f =
  EzXhr.get0 (EzAPI.TYPES.BASE host) Services.info "www.info"
    ~error:(fun status content ->
        let content =
          match content with
          | None -> "network error"
          | Some content -> content
        in
        Js_utils.log "/info.json: error %d: %s\n%!" status content;
        f None
      )
    (fun info -> f (Some info)) ()
