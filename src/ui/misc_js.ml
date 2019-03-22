
open Ocp_js

module IntMap = Map.Make(struct type t = int let compare = compare end)
module UpdateOnFocus = struct

  open Js
  let current_page = ref 0
  let get_current_page () = !current_page
  let incr_page () = incr current_page

  let auto_refresh = ref (match Jsloc.find_arg "refresh" with
      | Some ( "0" | "false" | "n" | "N" ) -> false
      | None | Some _ -> true)

  type focus =
    | Focused
    | Blurred_since of float

  let window_focused = ref Focused

  let stop_update_delay = 365. *. 24. *. 60. *. 60. *. 1000. (* 365 days *)

  let () =
    Dom_html.window##onblur <-
      Dom_html.handler (fun _ ->
          window_focused := Blurred_since (jsnew date_now ())##valueOf();
          Js._true);
    Dom_html.window##onfocus <-
      Dom_html.handler (fun _ ->
          window_focused := Focused;
          Js._true)

  let timer_id = ref 0
  let timers = ref IntMap.empty

  let clear_timer n timer =
    timers := IntMap.remove n !timers;
    Dom_html.window##clearInterval(timer);
    ()

  let update_every ?(always=false) time_s f =
    incr timer_id;
    let id = !timer_id in
    f ();
    (* save the current_page and the timer, so that we can remove the timer
       when we go to another page.*)
    let state = ref None in
    let cb () =
      if
        match !state with
        | None -> true (* always *)
        | Some (current_page, timer) ->
          if current_page = get_current_page() then
            true
          else begin
            clear_timer id timer;
            state := None;
            false
          end
      then
        if !auto_refresh then
          match !window_focused with
          | Focused -> f ()
          | Blurred_since last ->
            if (jsnew date_now ())##valueOf() -. last <= stop_update_delay
            then f ()
            else ()
    in
    Dom_html.window##setInterval(
        Js.wrap_callback cb,
        float_of_int time_s *. 1000.)
    |> fun timer ->
    if not always then begin
      state := Some (get_current_page(), timer);
      timers := IntMap.add id timer !timers
    end

  let clear_timers () =
    IntMap.iter clear_timer !timers

end

let input = Html.input

let find_input input_id =
  let input = Js_utils.find_component input_id in
  To_dom.of_input input

let get_input_value input_id =
  Js.to_string (find_input input_id)##value

let set_input_value input_id s =
  (find_input input_id)##value <- Js.string s
