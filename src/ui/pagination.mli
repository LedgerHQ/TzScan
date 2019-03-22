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

open Ocp_js

type request = EzAPI.url * (string -> unit)

val pagination :
           EzAPI.url ->
           int ->
           int ->
           int ->
           string ->
           string Html.wrap Html.wrap Html.wrap ->
           'a elt ->
           (string -> 'b) ->
           (int -> int -> 'b -> unit) ->
           ?update_timestamp:('b -> unit) option ->
           ?update_level:('b -> unit) option ->
           ?show_in_url:bool ->
           'c list ->
           [< Html_types.span_content_fun > `PCDATA ] elt ->
           unit

 val pagination_ops :
   update_timestamp:(Data_types.operation -> string -> unit) ->
   update_level:(Data_types.operation -> int -> unit) ->
   EzAPI.url ->
   int ->
   int ->
   int ->
   string ->
   string Html.wrap Html.wrap Html.wrap ->
   'a elt ->
   (string -> Data_types.operation list) ->
   (int -> int -> Data_types.operation list -> unit) ->
   ?show_in_url:bool ->
   'b list ->
   [< Html_types.span_content_fun > `PCDATA ] elt ->
   unit

 val pagination_xhr :
   EzAPI.url ->
   ?show_in_url:bool ->
   int ->
   (string -> 'a) ->
   ('a -> unit) -> request

 val pagination_xhr_ops :
   EzAPI.url ->
   int ->
   (string -> Data_types.operation list) ->
   (Data_types.operation list -> 'a) ->
   (Data_types.operation -> string -> unit) ->
   request

 val pagination_xhr_txs :
   EzAPI.url ->
   int ->
   (string -> Data_types.operation list) ->
   (Data_types.operation list -> 'a) ->
   (Data_types.operation -> string -> unit) ->
   (Data_types.operation -> int -> unit) ->
   request
