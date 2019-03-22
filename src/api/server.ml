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
let csv_lifespan = ref (24. *. 3600.)
(* sofar fixed at one day, need to change handler to put it variable *)
let local_services_file = ref None

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
    "--verbose-db", Arg.Set Reader.verbose_mode,
    " Verbosity mode for DB requests";
    "--local-services", Arg.String (fun s -> local_services_file := Some s),
    "Choose a local services.json instead of the one on the web server";
    "--csv-lifespan", Arg.Float (fun f -> csv_lifespan := f *. 3600.),
    "Lifespan of csv"
  ]

let services_encoding = ref None

let encoding_hook encoding =
  services_encoding := Some encoding

let update_aliases () =
  Lwt.return @@
  match !services_encoding with
  | None -> Printf.printf "services encoding not loaded\n%!"
  | Some services_encoding ->
    match !local_services_file with
    | None ->
      begin
        match Config.get_services_filename () with
        | None ->
          Printf.eprintf "No services, no aliases to update.\n%!" ;
        | Some url ->
          try
            EzCurl.get "" (EzAPI.TYPES.URL url)
              ~error:(fun i s ->
                  let msg = match s with None -> "" | Some s -> s in
                  Printf.eprintf "Downloading services from %S... \
                                  [ FAIL ] [ %d ] [ %S ]\n%!" url i msg)
              (fun content ->
                 Printf.eprintf "Downloading services from %S... [ OK ]\n%!" url ;
                 let json = Ezjsonm.from_string content in
                 Alias.change_aliases_from_json services_encoding json)
          with Curl.CurlException (_code, i, s) ->
            Printf.eprintf "[Service request] [%d] %s Curl exception: %s\n%!" i url s
      end
    | Some filename ->
      try
        Printf.eprintf "Reading services from %S\n%!" filename ;
        let ic = open_in filename in
        begin
          try
            let json = Ezjsonm.from_channel ic in
            Alias.change_aliases_from_json services_encoding json
          with exn ->
            Printf.eprintf "Fatal error while destructing %S:\n %s\n%!"
              filename (Printexc.to_string exn)
        end;
        close_in ic
      with exn ->
        Printf.eprintf "Fatal error while reading %S:\n  %s\n%!"
          filename (Printexc.to_string exn)

let clear_csv () =
  match Config.get_csv_dir () with
  | Some csv_dir when (String.length csv_dir) > 10 ->
    begin
      try
        let a = Sys.readdir csv_dir in
        Printf.eprintf "Cleaning CSV directory at %S\n%!" csv_dir ;
        Array.iter (fun s ->
            Sys.remove @@ Printf.sprintf "%s%s" csv_dir s) a
      with Sys_error _ ->
        Printf.eprintf "Directory to store csv files not found at %S.\
                        \nMaybe you should try `mkdir %s` first."
          csv_dir csv_dir ;
        exit 1
    end
  | _ -> ()

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
      let rec update_loop () =
        Printf.printf "Updating alias table [in progress]...\n%!";
        update_aliases () >>= fun () ->
        Printf.printf "Updating alias table [done]...\n%!";
        Lwt_unix.sleep !alias_latency >>= fun () ->
        update_loop () in
      let rec csv_gc () =
        clear_csv ();
        Lwt_unix.sleep !csv_lifespan >>= fun () ->
        csv_gc () in
      Lwt_list.iter_p (fun x -> x)
        [EzAPIServer.server servers; update_loop (); csv_gc ()]
    )
