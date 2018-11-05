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

let (>>=) = Dbr.Monad.(>>=)

(* Default arguments values *)
let api_port = if TzscanConfig.api_port = "" then
                 8080
               else
                 int_of_string TzscanConfig.api_port
let api_port = ref api_port
let root_port = ref 8000
let root = ref None
let default = ref (Some "index.html")
let node_config = ref None
let api_config = ref None

let alias_latency = ref 300.

(* Command-line arguments *)
let speclist = [
    "-p", Arg.Set_int api_port, "API server port";
    "--api-port", Arg.Set_int api_port, "api-server port";
    "--root-port", Arg.Set_int root_port, "file-server port";
    "--root", Arg.String (fun s -> root := Some s),
    "ROOT Serve files from ROOT for file-server";
    "--default", Arg.String (fun s -> default := Some s),
    "FILE Use FILE as default for file-server";
    "--save-api-config", Arg.String Infos.save_api_config,
    "File Save default API configuration into FILE";
    "--api-config", Arg.String (fun s -> api_config := Some s),
    "File Use FILE for API configuration";
    "--node-config", Arg.String (fun s -> node_config := Some s),
    "File Use FILE for api-node requests";
    "--no-default", Arg.Unit (fun () -> default := None),
    " No default for file-server (return 404)";
    "--time-alias", Arg.Set_float alias_latency,
    "Latency for alias table update";
]

(* Main *)
let server services =
  Printexc.record_backtrace true;
  Arg.parse speclist (fun str ->
      Printf.eprintf "Fatal error: unexpected argument %S\n%!" str;
      raise (Arg.Bad str)) "Tezos Explorer web API server" ;
  Config.load_config_api !node_config;
  begin
    match !api_config with
    | None ->
      Printf.eprintf
        "Fatal error: you must provide a configuration with --api-config\n%!";
      exit 2
    | Some filename ->
      let api_config = Config.load_json_config
          Api_encoding.V1.Server.api_server_config filename
      in
      Infos.api.api_config <- api_config
  end;
  let servers = [ !api_port, EzAPIServer.API services ] in
  let servers = match !root with
    | None -> servers
    | Some root ->
       (!root_port, EzAPIServer.Root (root, !default)) :: servers
  in
  Lwt_main.run (
      Stats.init () >>= fun () ->
      Printf.eprintf "Starting servers on ports [%s]\n%!"
                     (String.concat ","
                        (List.map (fun (port,_) ->
                             string_of_int port) servers));
      let update_table () =
        Lwt.bind (Dbr.all_aliases ())
          (fun l ->
             Alias.reset ();
             List.iter (fun (hash, alias) -> Alias.change_alias hash alias) l;
             Lwt.return_unit)
      in
      let rec update_loop () =
        Printf.printf "update of alias table\n%!";
        update_table () >>= fun () ->
        Lwt_unix.sleep !alias_latency >>= fun () ->
        update_loop () in
      Lwt_list.iter_p (fun x -> x)
        [EzAPIServer.server servers; update_loop ()]
    )
