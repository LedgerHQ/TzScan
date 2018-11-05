(* Uses JS library blockies https://github.com/ethereum/blockies *)

open Js_utils

let alphabet =
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let b58_charcodes =
  let l = String.length alphabet in
  let h = Hashtbl.create l in
  for i = 0 to l - 1 do
    Hashtbl.add h alphabet.[i] i
  done;
  h

let b58_code c =
  try
    Hashtbl.find b58_charcodes c
  with Not_found -> failwith "Not base58"

let base58_to_int s =
  let open Big_int in
  let x = ref (big_int_of_int 0) in
  let pos = ref (big_int_of_int 1) in
  let l = String.length s in
  for i = l - 1 downto 0 do
    let b58c = b58_code s.[i] in
    x := add_big_int !x (mult_int_big_int b58c !pos) ;
    pos := mult_int_big_int 58 !pos;
  done;
  !x

let hex_alpha = "0123456789abcdef"

let big_int_to_hex b =
  let open Big_int in
  let l = ref [] in
  let b = ref b in
  let b16 = big_int_of_int 16 in
  while gt_big_int !b zero_big_int do
    let q, r = quomod_big_int !b b16 in
    l := hex_alpha.[int_of_big_int r] :: !l;
    b := q;
  done;
  let n = List.length !l in
  let s = Bytes.create n in
  List.iteri (Bytes.set s) !l;
  let s = Bytes.to_string s in
  if n mod 2 = 0 then s else "0" ^ s

let base58_to_hex s =
  s |> base58_to_int |> big_int_to_hex

(* let tz1_to_hex s =
 *   Format.eprintf "tz1 : %s @." s;
 *   let h = base58_to_hex s in
 *   Format.eprintf "h : %s @." h;
 *   String.sub h
 *     6 (\* remove 3 bytes prefix *\)
 *     (String.length h - 8 - 6) (\* remove 4 bytes checksum *\) *)

let create ?(size=8) ?(scale=8) tz1 =
  let h = (base58_to_hex tz1) in
  let o = Js.Unsafe.(obj [|
      (* seed used to generate icon data, default: random *)
      "seed", Js.string h |> inject;
      (* "color", "#dfe"; to manually specify the icon color, default: random *)
      (* "bgcolor", "#aaa"; choose a different background color, default: random *)
      (* width/height of the icon in blocks, default: 8 *)
      "size", inject size;
      (* width/height of each block in pixels, default: 4 *)
      "scale", inject scale;
      (* each pixel has a 13% chance of being of a third color. default:
         random. Set to -1 to disable it. These "spots" create structures that
         look like eyes, mouths and noses. *)
      "spotcolor", Js.string "#000" |> inject;
    |]
    ) in
  let icon =
    Js.Unsafe.(meth_call (variable "blockies")) "create" [|o|]
    |> Tyxml_js.Of_dom.of_canvas
  in
  Manip.SetCss.borderRadius icon "3px";
  icon


let find_component id =
  match Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)

let update ?id ?className ?(left=false) tz1 =
  let elts =
    match id, className with
    | None, None -> []
    | Some id, _ -> [find_component id]
    | _, Some cl -> Manip.by_class cl
  in
  List.iter (fun sp ->
      let icon = create tz1 in
      Manip.removeChildren sp;
      Manip.appendChild sp icon;
      Manip.SetCss.height icon "100%";
      Manip.SetCss.width icon "100%";
      if left then
        Manip.SetCss.borderRadius icon "3px 0 0 3px"
    ) elts
