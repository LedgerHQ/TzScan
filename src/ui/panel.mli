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

type theads = Html_types.tr Tyxml_js.Html5.elt

val theads : ([< Html_types.th_content_fun ] Tyxml_js.Html5.elt list
              * int) list -> theads
val theads_of_strings : (Lang.text * int) list -> unit -> theads

val title_nb :
  ?help:Glossary_doc.helpers ->
  Lang.text ->
  int -> [> Html_types.span ] Tyxml_js.Html5.elt


(* Panel without pagination *)
module Make(M: sig

                     val name : string
                     val title_span :  [> Html_types.span ] Tyxml_js.Html5.elt
                     val columns : (Lang.text * int) list
                     val table_class : string

                   end) : sig

  val make :
    ?panel_class:string list -> unit -> [> Html_types.div ] Tyxml_js.Html5.elt

  val make_clg12 : unit -> [> Html_types.div ] Tyxml_js.Html5.elt

  val display :
    Html_types.tr Tyxml_js.Html5.elt list -> unit

end





(* Panel with pagination *)
module MakePageTable(M: sig

                    (* name of kind of values displayed in the table: account *)
                    val name : string

                    (* [title_span nb] when nb = -1 means not known,
                    otherwise, the number of items *)
                    val title_span : int ->
                                     [> Html_types.span ] Tyxml_js.Html5.elt
                    (* The names of the columns *)
                    val theads : unit -> theads

                    val table_class : string
                    val page_size : int

                  end) : sig

  val make :
    ?footer:bool ->
    ?suf_id:string ->
    ?panel_class:string list ->
    ?before:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    ?after:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    unit -> [> Html_types.div ] Tyxml_js.Html5.elt
  val make_clg12 :
    ?footer:bool ->
    ?suf_id:string ->
    ?before:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    ?after:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    unit -> [> Html_types.div ] Tyxml_js.Html5.elt
  val make_clg8 :
    ?suf_id:string ->
    ?before:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    ?after:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    unit -> [> Html_types.div ] Tyxml_js.Html5.elt


  (* [update nb updater] nb = total number of items,
      and [updater page page_size cont] should download the content
      of page [page] of size [page_size], calling [cont] with the
      rows to be displayed in the table. *)
  val paginate :
    ?page_sizer:bool ->
    ?suf_id:string ->
    ?urlarg_page:string ->                          (* "p" by default in the URL *)
    ?urlarg_size:string ->                          (* "r" by default in the URL *)
    ?nrows:int ->                                  (* total number of items *)
    ?title_span:(int -> Html_types.span Tyxml_js.Html5.elt) ->
     (int ->                                        (* page to query *)
     int ->                                            (* page size *)
     (Html_types.tr Tyxml_js.Html5.elt list -> unit) -> (* callback *)
     unit) ->
    unit

  val paginate_all :
    ?page_sizer:bool ->
    ?suf_id:string ->
    ?urlarg_page:string ->                          (* "p" by default in the URL *)
    ?urlarg_size:string ->                          (* "r" by default in the URL *)
    Html_types.tr Tyxml_js.Html5.elt array ->                   (* all rows *)
    unit

  val paginate_fun :
    ?page_sizer:bool ->
    ?suf_id:string ->
    ?urlarg_page:string ->                          (* "p" by default in the URL *)
    ?urlarg_size:string ->                          (* "r" by default in the URL *)
    ('data -> Html_types.tr Tyxml_js.Html5.elt list) -> (* conversion to tr *)
    ?nrows:int ->                                  (* total number of items *)
    ?title_span:(int -> Html_types.span Tyxml_js.Html5.elt) ->
    (int ->                                        (* page to query *)
     int ->                                            (* page size *)
     ('data -> unit) ->                                 (* callback *)
     unit) ->
    unit

end




(* Panel with pagination *)
module MakePageNoTable(M: sig

                    (* name of kind of values displayed in the table: account *)
                    val name : string

                    (* [title_span nb] when nb = -1 means not known,
                    otherwise, the number of items *)
                    val title_span : int ->
                                     [> Html_types.span ] Tyxml_js.Html5.elt
                    val page_size : int

                  end) : sig

  val make :
    ?footer:bool ->
    ?suf_id:string ->
    ?panel_class:string list ->
    ?before:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    ?after:[ `Div | `Table ] Tyxml_js.Html5.elt list ->
    unit -> [> Html_types.div ] Tyxml_js.Html5.elt
  (* val make :
   *   ?suf_id:string ->
   *   ?panel_class:string list -> unit -> [> Html_types.div ] Tyxml_js.Html5.elt *)


  (* [update nb updater] nb = total number of items,
      and [updater page page_size cont] should download the content
      of page [page] of size [page_size], calling [cont] with the
      rows to be displayed in the table. *)
  val paginate :
    ?page_sizer:bool ->
    ?suf_id:string ->
    ?urlarg_page:string ->                          (* "p" by default in the URL *)
    ?urlarg_size:string ->                          (* "r" by default in the URL *)
    ?nrows:int ->                                  (* total number of items *)
    ?title_span:(int ->  Html_types.span Tyxml_js.Html5.elt) ->
   (int ->                                        (* page to query *)
     int ->                                            (* page size *)
     (Html_types.div Tyxml_js.Html5.elt list -> unit) -> (* callback *)
     unit) ->
    unit

  val paginate_all :
    ?page_sizer:bool ->
    ?suf_id:string ->
    ?urlarg_page:string ->                          (* "p" by default in the URL *)
    ?urlarg_size:string ->                          (* "r" by default in the URL *)
    Html_types.div Tyxml_js.Html5.elt array ->                   (* all rows *)
    unit

  val paginate_fun :
    ?page_sizer:bool ->
    ?suf_id:string ->
    ?urlarg_page:string ->                          (* "p" by default in the URL *)
    ?urlarg_size:string ->                          (* "r" by default in the URL *)
    ('data -> Html_types.div Tyxml_js.Html5.elt list) -> (* conversion to tr *)
    ?nrows:int ->                                  (* total number of items *)
    ?title_span:(int ->  Html_types.span Tyxml_js.Html5.elt) ->
    (int ->                                        (* page to query *)
     int ->                                            (* page size *)
     ('data -> unit) ->                                 (* callback *)
     unit) ->
    unit

end
