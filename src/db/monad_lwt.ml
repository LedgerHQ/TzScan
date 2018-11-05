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

include Lwt

type in_channel = Lwt_io.input_channel
type out_channel = Lwt_io.output_channel


let output_char = Lwt_io.write_char

let output_string = Lwt_io.write
let flush = Lwt_io.flush
let input_char = Lwt_io.read_char
let really_input = Lwt_io.read_into_exactly
let close_in = Lwt_io.close

type 'a pool = 'a Lwt_pool.t
let pool_create = Lwt_pool.create
let pool_use = Lwt_pool.use


let open_connection sockaddr =
  let sock = Lwt_unix.socket
      (Unix.domain_of_sockaddr sockaddr) Lwt_unix.SOCK_STREAM 0 in
  Lwt.catch
    (fun () ->
       Lwt_unix.connect sock sockaddr >>=
       (fun () ->
          Lwt_unix.set_close_on_exec sock;
          return (Lwt_io.of_fd ~mode:Lwt_io.input sock,
                  Lwt_io.of_fd ~mode:Lwt_io.output sock)
       )
    )
  (fun exn -> Lwt_unix.close sock >>= fun () -> fail exn)

let output_binary_int oc n =
  output_char oc (Char.chr (n lsr 24)) >>= fun () ->
  output_char oc (Char.chr ((n lsr 16) land 255)) >>= fun () ->
  output_char oc (Char.chr ((n lsr 8) land 255)) >>= fun () ->
  output_char oc (Char.chr (n land 255))

let input_binary_int ic =
  input_char ic >>= fun a ->
  input_char ic >>= fun b ->
  input_char ic >>= fun c ->
  input_char ic >>= fun d ->
  return ((Char.code a lsl 24)
      lor (Char.code b lsl 16)
      lor (Char.code c lsl 8)
      lor (Char.code d))
