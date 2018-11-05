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
open Data_types

let count = ref false
let start_level_count = ref (-1)
let end_level_count = ref (-1)

let alternative_heads_flag = ref true
let pending_operations_flag = ref false

let quit = ref false
let sleep = 3

let config_file = ref ""
let new_head = ref None

let until_mode = ref false

let catchup_step = 500

let debug fmt = Utils.debug !Debug_constants.debug_crawler fmt

let crawler = ref "main"

let () =
  Random.self_init ()

let get_crawler () =
  Config.get_config_crawler ~crawler:!crawler !config_file

let rec choose_node config =
  if !quit then exit 1 ;
  match config.nodes with
  | [] -> (* There is no node available  *)
    (* We need to re-read the configuration, maybe new nodes appeared. *)
    debug
      "[Crawler] No node available. Re-reading the config file %s.\n%!"
      !config_file ;
    Unix.sleep sleep ;
    choose_node @@ get_crawler ()
  | _ ->
    List.nth config.nodes @@
    Random.int @@
    List.length config.nodes

let get_data_path config =
  if !quit then exit 1 ; config.data_path

(* Command-line arguments *)
let speclist = ref [
  "--no-alternative-heads",
  Arg.Clear alternative_heads_flag,
  " Do not pull for alt-heads";
  "--pending-operations",
  Arg.Set pending_operations_flag,
  " Pull for pending operations (expensive)";
  "--until",
  Arg.String (fun block_hash ->
      until_mode := true ;
      new_head := Some block_hash),
  " Run a crawler where the given block hash." ;
  "--debug-request",
  Arg.Bool (fun b -> Debug_constants.debug_request := b),
  " Enable/Disable logging for node requests." ;
  "--debug-crawler",
  Arg.Bool (fun b -> Debug_constants.debug_crawler := b),
  " Enable/Disable logging in the crawler." ;
  "--debug-db-writer",
  Arg.Bool (fun b -> Debug_constants.debug_writer := b),
  " Enable/Disable logging in the db writer." ;
  "--debug-db-reader",
  Arg.Bool (fun b -> Debug_constants.debug_reader := b),
  " Enable/Disable logging in the db reader." ;
  "--debug-config",
  Arg.Bool (fun b -> Debug_constants.debug_config := b),
  " Enable/Disable logging in the config module." ;
  "--offline",
  Arg.Set Cache.offline,
  " Run offline (without a node, using a cache)" ;
  "--cache-dir",
  Arg.String (fun s -> Cache.cache_dir := Some s),
  "DIR Use cache in DIR" ;
  "--count",  Arg.Set count, "Run the crawler with counters";
  "--count-level",
  Arg.(Tuple [Set_int start_level_count; Set_int end_level_count]),
  "Update counters down to start level and up to end level (2 \
   arguments, -1 for the second to go up the head)";
]

let ( crawlers :
        ((unit -> unit) * (config -> unit) * int * string)
          StringMap.t ref
    ) = ref StringMap.empty

let run () =
  (* Catch Ctrl-C and quit properly *)
  Sys.(set_signal sigint (Signal_handle (fun _ -> quit := true))) ;
  Arg.parse !speclist (fun cfg -> config_file := cfg) "Tezos Explorer crawler" ;
  if not @@ Sys.file_exists !config_file then
    Config.error "Config file does not exist";
  let init, main_loop, sleep_loop, msg_loop =
    StringMap.find !crawler !crawlers in
  Printf.eprintf "[Crawler] Starting a crawler for %s\n%!" msg_loop;
  init ();
  while true do
    if not !quit then begin
      main_loop @@ get_crawler () ;
      Unix.sleep sleep_loop
    end else begin
      Printf.eprintf "[Crawler] Exiting properly\n%!" ;
      exit 1
    end
  done
