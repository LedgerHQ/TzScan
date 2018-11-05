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

(* if TEZOS_CRAWLER_CACHE is set, every request will save its content in
   the cache, and a cache hit will be used instead of requesting a node. *)
let cache_dir =
  try
    let cache_dir = Sys.getenv "TEZOS_CRAWLER_CACHE" in
    if not (Sys.file_exists cache_dir && Sys.is_directory cache_dir) then begin
      Printf.eprintf
        "Error: TEZOS_CRAWLER_CACHE is not an existing directory.\n%!";
      exit 2
    end;
    Some cache_dir
  with Not_found -> None

(* if TEZOS_CRAWLER_OFFLINE is set, the crawler will only use the
   data from the cache. *)
let offline =
  try ignore (Sys.getenv "TEZOS_CRAWLER_OFFLINE"); true
  with Not_found -> false

let cache_dir = ref cache_dir
let offline = ref offline

type cache_entry = {
  key_file_content : string;
  key_file_info : string;
}

let get_cache_entry ~cache_dir ~key =
    let hash = Digest.to_hex (Digest.string key) in
    let cache_dir = Filename.concat cache_dir (String.sub hash 0 2) in
    if not (Sys.file_exists cache_dir) then Unix.mkdir cache_dir 0o755;
    let cache_dir = Filename.concat cache_dir (String.sub hash 2 2) in
    if not (Sys.file_exists cache_dir) then Unix.mkdir cache_dir 0o755;
    let key_file = Filename.concat cache_dir hash in
    let key_file_content = key_file ^ ".data" in
    let key_file_info = key_file ^ ".info" in
    {
      key_file_content;
      key_file_info;
    }

let read_from_cache ~debug ~key ce =
  let content = FileString.read_file ce.key_file_content in
  Printf.eprintf "(%skey %s\n  found in cache %s)\n%!"
    debug key ce.key_file_content;
  content

let with_cache ?(debug="") ~cachable ~key ~request =
  match !cache_dir with
  | None -> request ()
  | Some cache_dir ->
    let ce = get_cache_entry ~cache_dir ~key in
    let in_cache =
      if cachable || !offline then
        if Sys.file_exists ce.key_file_content then
          try
            Some (read_from_cache ~debug ~key ce)
          with exn ->
            Printf.eprintf "Error while accessing cache file %S:\n%s%!"
              ce.key_file_content (Printexc.to_string exn);
            exit 2
        else
        if !offline then begin
          Printf.eprintf "Error while accessing cache file %S in offline mode:\n%!"
            ce.key_file_content;
          exit 2
        end else
          None
      else None
    in
    match in_cache with
    | Some content -> content
    | None ->
        let content = request () in
        let key_file_temp = ce.key_file_content ^ ".temp" in

        FileString.write_file ce.key_file_info (key ^ "\n");
        FileString.write_file key_file_temp content;
        Unix.rename key_file_temp ce.key_file_content;
        Printf.eprintf "[cache] saved %s into %s\n%!" key ce.key_file_content;
        
        content

let get ~debug ~cache_dir ~key =
  let ce = get_cache_entry ~cache_dir ~key in
  if Sys.file_exists ce.key_file_content then
    read_from_cache ~debug ~key ce
  else raise Not_found

let (>>=) = Lwt.(>>=)

let with_cache_lwt ?(debug="") ~cachable ~key ~request =
  match !cache_dir with
  | None -> request ()
  | Some cache_dir ->
    let ce = get_cache_entry ~cache_dir ~key in
    let in_cache =
      if cachable || !offline then
        if Sys.file_exists ce.key_file_content then
          try
            Some (read_from_cache ~debug ~key ce)
          with exn ->
            Printf.eprintf "Error while accessing cache file %S:\n%s%!"
              ce.key_file_content (Printexc.to_string exn);
            exit 2
        else
        if !offline then begin
          Printf.eprintf "Error while accessing cache file %S in offline mode:\n%!"
            ce.key_file_content;
          exit 2
        end else
          None
      else None
    in
    match in_cache with
    | Some content -> Lwt.return content
    | None ->
      request () >>= fun content ->
      let key_file_temp = ce.key_file_content ^ ".temp" in

      FileString.write_file ce.key_file_info (key ^ "\n");
      FileString.write_file key_file_temp content;
      Unix.rename key_file_temp ce.key_file_content;

      Lwt.return content
