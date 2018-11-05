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

open Json_encoding
type address = {url: string; port: int}

let debug fmt = Utils.debug !Debug_constants.debug_config fmt

let address_encoding =
  conv
    (fun {url; port} -> (url, port, None))
    (fun (url, port, _pref) -> {url; port})
    (obj3
       (req "url" string)
       (req "port" int)
       (opt "pref" string))


let all_addresses = ref []
let api_addresses = ref [||]

type config =
  {
    name : string ;
    crawler : address  list ;
    api : address list ;
    data_path : string option ;
  }

let config_encoding =
  conv
    (fun
      { name ; crawler ; api ; data_path }
      -> ( name , crawler , api , data_path , None , None )
    )
    (fun
      ( name , crawler , api , data_path,
        _max_default_blocks , _max_default_operations )
      -> { name ; crawler ; api ; data_path }
    )
    ( obj6
      (req "name" string)
      (req "crawler" (list address_encoding))
      (req "api" (list address_encoding))
      (opt "data" string)
      (opt "max_default_blocks" int)
      (opt "max_default_operations" int)
    )

let configs_encoding = list config_encoding

let configs_example = [
  { name = "betanet-main";
    crawler = [ { url = "http://localhost"; port = 18732 } ];
    api = [
      { url = "http://another-host"; port = 18732 };
    ];
    data_path = None;
  }
]

let () =
  Printexc.register_printer (fun exn ->
      match exn with
      | Json_encoding.Cannot_destruct (path, exn) ->
        let s = Printf.sprintf "Cannot destruct JSON (%s, %s)"
            (Json_query.json_pointer_of_path path)
            (Printexc.to_string exn)
        in
        Some s
      | _ -> None)

let error msg =
  Printf.eprintf "Fatal error: %s\n%!" msg;
  Printf.eprintf "Configuration file should look like:\n%s\n%!"
    (EzEncoding.construct ~compact:false configs_encoding configs_example) ;
  exit 1

let load_json_config encoding filename =
  try
    let ic = open_in filename in
    let json = Ezjsonm.from_channel ic in
    close_in ic ;
    destruct encoding json
  with
  | exn ->
    Printf.eprintf "Fatal error while reading %S:\n  %s\n%!"
      filename (Printexc.to_string exn);
    exit 2

let parse_config file =
  let file = match file with
    | None -> error "No config file specified"
    | Some f when not (Sys.file_exists f) ->
      error "Config file does not exist"
    | Some f -> f in
  load_json_config configs_encoding file

let load_config_api file =
  let crawler = "main" in
  let wanted_name = Printf.sprintf "%s-%s" TzscanConfig.database crawler in
  Printf.eprintf "Looking for config %S\n%!" wanted_name;
  let config =
    List.find_opt (fun c -> c.name = wanted_name)
      (parse_config file) in
  match config with
  | None -> error "No config file specified"
  | Some c ->
    debug "[Config] Running network : %S\n%!" c.name ;
    let crawler_addresses =
      List.map (fun {url;port} ->
          ("crawler", Printf.sprintf "%s:%d" url port)) c.crawler
    in
    let addresses =
      let addresses =
        List.map (fun {url;port} ->
            ("balance", Printf.sprintf "%s:%d" url port)) c.api
      in
      all_addresses := addresses @ crawler_addresses;
      match addresses with
      | [] -> crawler_addresses
      | _ -> addresses
    in
    api_addresses := Array.of_list addresses;
    begin
      match !api_addresses with
      | [||] -> error "No node addresses for balance queries"
      | _ -> ()
    end

let get_config_crawler ~crawler file =
  let wanted_name = Printf.sprintf "%s-%s" TzscanConfig.database crawler in
  Printf.eprintf "Looking for config %S\n%!" wanted_name;
  let config =
    List.find_opt (fun c -> c.name = wanted_name)
      (parse_config (Some file)) in
  match config with
  | None -> error "No config file specified"
  | Some c ->
    debug "[Config] Running network : %S\n url %s\n%!"
      c.name
      (String.concat " "
         (List.map (fun { url ; port; _} ->
              Printf.sprintf "%s:%d" url  port) c.crawler)) ;
    { Data_types.nodes =
        List.map (fun {url; port; _} ->
            { Data_types.address = url; port }) c.crawler;
      data_path = c.data_path ;
    }

let get_addresses () = !all_addresses
let get_api_address () =
  let api_addresses = !api_addresses in
  snd api_addresses.(Random.int (Array.length api_addresses))

(*
let get_address_pref ?fpref () =
  let rec aux = function
    | [] -> error ()
    | [ {url; port; pref} ] ->
      if fpref <> None && fpref <> pref then
        debug "[Config] Node preference not reached";
      Printf.sprintf "%s:%d" url port
    | {url; port; pref} :: _  when pref = fpref -> Printf.sprintf "%s:%d" url port
    | _ :: q -> aux q
  in aux !addresses
*)
