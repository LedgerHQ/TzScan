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

(* Don't fix warnings in imported modules from Tezos *)
[@@@warning "-32-9-60"]

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then
    Some (String.sub s x (n - x))
  else
    None

let apply_option ~f = function
  | None -> None
  | Some x -> f x

let map_option ~f = function
  | None -> None
  | Some x -> Some (f x)

let has_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  n >= x && String.sub s 0 x = prefix

let common_prefix s1 s2 =
  let last = min (String.length s1) (String.length s2) in
  let rec loop i =
    if last <= i then last
    else if s1.[i] = s2.[i] then
      loop (i+1)
    else
      i in
  loop 0

let may_cons xs x = match x with None -> xs | Some x -> x :: xs

let filter_map f l =
  List.rev @@ List.fold_left (fun acc x -> may_cons acc (f x)) [] l

module Base58 = struct
  open Lwt.Infix

  let base = 58
  let zbase = Z.of_int base

  let log2 x = log x /. log 2.
  let log2_base = log2 (float_of_int base)


  module Alphabet = struct

    type t = { encode: string ; decode: string }

    let make alphabet =
      if String.length alphabet <> base then
        invalid_arg "Base58: invalid alphabet (length)" ;
      let str = Bytes.make 256 '\255' in
      for i = 0 to String.length alphabet - 1 do
        let char = int_of_char alphabet.[i] in
        if Bytes.get str char <> '\255' then
          Format.kasprintf invalid_arg
            "Base58: invalid alphabet (dup '%c' %d %d)"
            (char_of_int char) (int_of_char @@ Bytes.get str char) i ;
        Bytes.set str char (char_of_int i) ;
      done ;
      { encode = alphabet ; decode = Bytes.to_string str }

    let bitcoin =
      make "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    let ripple =
      make "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"
    let flickr =
      make "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

    let default = bitcoin

    let all_in_alphabet alphabet string =
      let ok = Array.make 256 false in
      String.iter (fun x -> ok.(Char.code x) <- true) alphabet.encode ;
      let res = ref true in
      for i = 0 to (String.length string) - 1 do
        res := !res && ok.(Char.code string.[i])
      done;
      !res

    let pp ppf { encode } = Format.fprintf ppf "%s" encode

  end
  open Alphabet

  let count_trailing_char s c =
    let len = String.length s in
    let rec loop i =
      if i < 0 then len
      else if String.get s i <> c then (len-i-1)
      else loop (i-1) in
    loop (len-1)

  let count_leading_char s c =
    let len = String.length s in
    let rec loop i =
      if i = len then len
      else if String.get s i <> c then i
      else loop (i+1) in
    loop 0

  let of_char ?(alphabet=Alphabet.default) x =
    let pos = String.get alphabet.decode (int_of_char x) in
    if pos = '\255' then failwith (Printf.sprintf "Invalid data %c" x);
    int_of_char pos

  let to_char ?(alphabet=Alphabet.default) x =
    alphabet.encode.[x]

  let raw_encode ?(alphabet=Alphabet.default) s =
    let len = String.length s in
    let s = String.init len (fun i -> String.get s (len - i - 1)) in
    let zero = alphabet.encode.[0] in
    let zeros = count_trailing_char s '\000' in
    let res_len = (len * 8 + 4) / 5 in
    let res = Bytes.make res_len '\000' in
    let s = Z.of_bits s in
    let rec loop s =
      if s = Z.zero then 0 else
        let s, r = Z.div_rem s zbase in
        let i = loop s in
        Bytes.set res i (to_char ~alphabet (Z.to_int r)) ;
        i + 1 in
    let i = loop s in
    let res = Bytes.sub_string res 0 i in
    String.make zeros zero ^ res

  let raw_decode ?(alphabet=Alphabet.default) s =
    let zero = alphabet.encode.[0] in
    let zeros = count_leading_char s zero in
    let len = String.length s in
    let rec loop res i =
      if i = len then res else
        let x = Z.of_int (of_char ~alphabet (String.get s i)) in
        let res = Z.(add x (mul res zbase)) in
        loop res (i+1)
    in
    let res = Z.to_bits @@ loop Z.zero zeros in
    let res_tzeros = count_trailing_char res '\000' in
    let len = String.length res - res_tzeros in
    String.make zeros '\000' ^
    String.init len (fun i -> String.get res (len - i - 1))

  let checksum s =
    let hash =
      Nocrypto.Hash.digest `SHA256 @@
      Nocrypto.Hash.digest `SHA256 @@
      Cstruct.of_string s in
    let res = Bytes.make 4 '\000' in
    Cstruct.blit_to_bytes hash 0 res 0 4 ;
    Bytes.to_string res

  (* Append a 4-bytes cryptographic checksum before encoding string s *)
  let safe_encode ?alphabet s =
    raw_encode ?alphabet (s ^ checksum s)

  let safe_decode ?alphabet s =
    let s = raw_decode ?alphabet s in
    let len = String.length s in
    let msg = String.sub s 0 (len-4)
    and msg_hash = String.sub s (len-4) 4 in
    if msg_hash <> checksum msg then
      invalid_arg "safe_decode" ;
    msg

  type data = ..

  type 'a encoding = {
    prefix: string ;
    length: int ;
    encoded_prefix: string ;
    encoded_length: int ;
    to_raw: 'a -> string ;
    of_raw: string -> 'a option ;
    wrap: 'a -> data ;
  }

  let simple_decode ?alphabet { prefix ; of_raw } s =
    safe_decode ?alphabet s |>
    remove_prefix ~prefix |>
    apply_option ~f:of_raw

  let simple_encode ?alphabet { prefix ; to_raw } d =
    safe_encode ?alphabet (prefix ^ to_raw d)

  type registred_encoding = Encoding : 'a encoding -> registred_encoding

  module MakeEncodings(E: sig
      val encodings: registred_encoding list
    end) = struct

    let encodings = ref E.encodings

    let check_ambiguous_prefix prefix encodings =
      List.iter
        (fun (Encoding { encoded_prefix = s }) ->
           if remove_prefix ~prefix:s prefix <> None ||
              remove_prefix ~prefix s <> None then
             Format.ksprintf invalid_arg
               "Base58.register_encoding: duplicate prefix: %S, %S." s prefix)
        encodings

    let make_encoded_prefix prefix len =
      let zeros = safe_encode (prefix ^ String.make len '\000')
      and ones = safe_encode (prefix ^ String.make len '\255') in
      let len = String.length zeros in
      if String.length ones <> len then
        Format.ksprintf invalid_arg
          "Base58.registred_encoding: variable length encoding." ;
      let rec loop i =
        if i = len then len
        else if zeros.[i] = ones.[i] then loop (i+1)
        else i in
      let len = loop 0 in
      if len = 0 then
        invalid_arg
          "Base58.register_encoding: not a unique prefix." ;
      String.sub zeros 0 len, String.length zeros

    let register_encoding ~prefix ~length ~to_raw ~of_raw ~wrap =
      let to_raw x =
        let s = to_raw x in assert (String.length s = length) ; s in
      let of_raw s = assert (String.length s = length) ; of_raw s in
      let encoded_prefix, encoded_length = make_encoded_prefix prefix length in
      check_ambiguous_prefix encoded_prefix !encodings ;
      let encoding =
        { prefix ; length ; encoded_prefix ; encoded_length ;
          to_raw ; of_raw ; wrap } in
      encodings := Encoding encoding :: !encodings ;
      encoding

    let check_encoded_prefix enc p l =
      if enc.encoded_prefix <> p then
        Format.kasprintf failwith
          "Unexpected prefix %s (expected %s)"
          p enc.encoded_prefix ;
      if enc.encoded_length <> l then
        Format.kasprintf failwith
          "Unexpected encoded length %d for %s (expected %d)"
          l p enc.encoded_length

    let decode ?alphabet s =
      try
        let rec find s = function
          | [] -> None
          | Encoding { prefix ; of_raw ; wrap } :: encodings ->
            match remove_prefix ~prefix s with
            | None -> find s encodings
            | Some msg -> of_raw msg |> map_option ~f:wrap in
        let s = safe_decode ?alphabet s in
        find s !encodings
      with Invalid_argument _ -> None

  end
  open Alphabet
  type 'a resolver =
      Resolver : {
        encoding: 'h encoding ;
        resolver: 'a -> string -> 'h list Lwt.t ;
      } -> 'a resolver

  module MakeResolvers(R: sig
      type context
      val encodings: registred_encoding list ref
    end) = struct

    let resolvers = ref []

    let register_resolver
        (type a)
        (encoding : a encoding)
        (resolver : R.context -> string -> a list Lwt.t) =
      resolvers := Resolver { encoding ; resolver } :: !resolvers

    let partial_decode ?(alphabet=Alphabet.default) request len =
      let zero = alphabet.encode.[0] in
      let last = alphabet.encode.[base-1] in
      let n = String.length request in
      let min = raw_decode ~alphabet (request ^ String.make (len - n) zero) in
      let max = raw_decode ~alphabet (request ^ String.make (len - n) last) in
      let prefix_len = common_prefix min max in
      String.sub min 0 prefix_len

    let complete ?alphabet context request =
      let rec find s = function
        | [] -> Lwt.return_nil
        | Resolver { encoding ; resolver } :: resolvers ->
          if not (has_prefix ~prefix:encoding.encoded_prefix s) then
            find s resolvers
          else
            let prefix =
              partial_decode ?alphabet request encoding.encoded_length in
            let len = String.length prefix in
            let ignored = String.length encoding.prefix in
            let msg =
              if len <= ignored then ""
              else begin
                assert (String.sub prefix 0 ignored  = encoding.prefix) ;
                String.sub prefix ignored (len - ignored)
              end in
            resolver context msg >|= fun msgs ->
            filter_map
              (fun msg ->
                 let res = simple_encode encoding ?alphabet msg in
                 remove_prefix ~prefix:request res |>
                 map_option ~f:(fun _ -> res))
              msgs in
      find request !resolvers

  end

  include MakeEncodings(struct let encodings = [] end)
  include MakeResolvers(struct
      type context = unit
      let encodings = encodings
    end)

  let register_resolver enc f = register_resolver enc (fun () s -> f s)
  let complete ?alphabet s = complete ?alphabet () s

  module Make(C: sig type context end) = struct
    include MakeEncodings(struct let encodings = !encodings end)
    include MakeResolvers(struct
        type context = C.context
        let encodings = encodings
      end)
  end

  module Prefix = struct

    (* 32 *)
    let block_hash = "\001\052" (* B(51) *)
    let operation_hash = "\005\116" (* o(51) *)
    let operation_list_hash = "\133\233" (* Lo(52) *)
    let operation_list_list_hash = "\029\159\109" (* LLo(53) *)
    let protocol_hash = "\002\170" (* P(51) *)

    (* 20 *)
    let ed25519_public_key_hash = "\006\161\159" (* tz1(36) *)

    (* 16 *)
    let cryptobox_public_key_hash = "\153\103" (* id(30) *)

    (* 32 *)
    let ed25519_public_key = "\013\015\037\217" (* edpk(54) *)

    (* 64 *)
    let ed25519_secret_key = "\043\246\078\007" (* edsk(98) *)
    let ed25519_signature = "\009\245\205\134\018" (* edsig(99) *)

    (* 4 *)
    let net_id = "\087\082\000" (* Net(15) *)

  end

end

module MBytes = Bytes

module Hex_encode = struct
  let gen_encode length get s =
    let n = length s in
    let result = Bytes.create (n*2) in
    for i = 0 to n-1 do
      Bytes.blit_string (Printf.sprintf "%02x" (get s i)) 0 result (2*i) 2;
    done;
    Bytes.unsafe_to_string result

  (* let hex_of_bytes = gen_encode MBytes.length MBytes.get_uint8 *)
  let hex_encode = gen_encode String.length (fun s i -> int_of_char s.[i])

  (* From OCaml's stdlib. See [Digest.from_hex], and [hex_decode], [bytes_of_hex]
     below for examples. *)
  let gen_decode create set h =
    let n = String.length h in
    if n mod 2 <> 0 then invalid_arg ("hex_decode: " ^ h);
    let digit c =
      match c with
      | '0'..'9' -> int_of_char c - int_of_char '0'
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | _c -> invalid_arg ("hex_decode: " ^ h)
    in
    let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
    let result = create (n / 2) in
    for i = 0 to n/2 - 1 do
      set result i (byte (2 * i));
    done;
    result

  let hex_decode s =
    gen_decode Bytes.create (fun s i c -> Bytes.set s i (char_of_int c)) s |>
    Bytes.unsafe_to_string

  (* let bytes_of_hex s =
   *   gen_decode MBytes.create MBytes.set_int8 s *)

end

module type Name = sig
  val name: string
  val title:  string
  val size: int option
end

module type PrefixedName = sig
  include Name
  val b58check_prefix: string
end

(*-- Type specific Hash builder ---------------------------------------------*)

module Make_minimal_Blake2B (K : Name) = struct

  type t = Sodium.Generichash.hash

  include K

  let size =
    match K.size with
    | None -> 32
    | Some x -> x

  let of_string s =
    if String.length s <> size then
      None
    else
      Some (Sodium.Generichash.Bytes.to_hash (Bytes.of_string s))
  let of_string_exn s =
    match of_string s with
    | None ->
        let msg =
          Printf.sprintf "%s.of_string: wrong string size (%d)"
            K.name (String.length s) in
        raise (Invalid_argument msg)
    | Some h -> h
  let to_string s = Bytes.to_string (Sodium.Generichash.Bytes.of_hash s)

  let of_hex s = of_string (Hex_encode.hex_decode s)
  let of_hex_exn s = of_string_exn (Hex_encode.hex_decode s)
  let to_hex s = Hex_encode.hex_encode (to_string s)

  let compare = Sodium.Generichash.compare
  let equal x y = compare x y = 0

  (* let of_bytes b =
   *   if MBytes.length b <> size then
   *     None
   *   else
   *     Some (Sodium.Generichash.Bigbytes.to_hash b)
   * let of_bytes_exn b =
   *   match of_bytes b with
   *   | None ->
   *       let msg =
   *         Printf.sprintf "%s.of_bytes: wrong string size (%d)"
   *           K.name (MBytes.length b) in
   *       raise (Invalid_argument msg)
   *   | Some h -> h *)
  let to_bytes = Sodium.Generichash.Bigbytes.of_hash

  (* let read src off = of_bytes_exn @@ MBytes.sub src off size *)
  (* let write dst off h = MBytes.blit (to_bytes h) 0 dst off size *)

  let hash_bytes l =
    let open Sodium.Generichash in
    let state = init ~size () in
    List.iter (Bigbytes.update state) l ;
    final state

  let hash_string l =
    let open Sodium.Generichash in
    let state = init ~size () in
    List.iter
      (fun s ->
         Sodium.Generichash.Bytes.update state (BytesLabels.unsafe_of_string s))
      l ;
    final state

  (* let fold_read f buf off len init =
   *   let last = off + len * size in
   *   if last > MBytes.length buf then
   *     invalid_arg "Hash.read_set: invalid size.";
   *   let rec loop acc off =
   *     if off >= last then
   *       acc
   *     else
   *       let hash = read buf off in
   *       loop (f hash acc) (off + size)
   *   in
   *   loop init off *)

  let path_length = 6
  let to_path key l =
    let key = to_hex key in
    String.sub key 0 2 :: String.sub key 2 2 ::
    String.sub key 4 2 :: String.sub key 6 2 ::
    String.sub key 8 2 :: String.sub key 10 (size * 2 - 10) :: l
  let of_path path =
    let path = String.concat "" path in
    of_hex path
  let of_path_exn path =
    let path = String.concat "" path in
    of_hex_exn path

  let prefix_path p =
    let p = Hex_encode.hex_encode p in
    let len = String.length p in
    let p1 = if len >= 2 then String.sub p 0 2 else ""
    and p2 = if len >= 4 then String.sub p 2 2 else ""
    and p3 = if len >= 6 then String.sub p 4 2 else ""
    and p4 = if len >= 8 then String.sub p 6 2 else ""
    and p5 = if len >= 10 then String.sub p 8 2 else ""
    and p6 = if len > 10 then String.sub p 10 (len - 10) else "" in
    [ p1 ; p2 ; p3 ; p4 ; p5 ; p6 ]

  module Table = struct
    include Hashtbl.Make(struct
        type nonrec t = t
        let hash s =
          Int64.to_int
            (EndianString.BigEndian.get_int64
               (Bytes.unsafe_to_string (Sodium.Generichash.Bytes.of_hash s))
               0)
        let equal = equal
      end)
  end

end

module Make_Blake2B (R : sig
    val register_encoding:
      prefix: string ->
      length:int ->
      to_raw: ('a -> string) ->
      of_raw: (string -> 'a option) ->
      wrap: ('a -> Base58.data) ->
      'a Base58.encoding
  end) (K : PrefixedName) = struct

  include Make_minimal_Blake2B(K)

  (* Serializers *)

  type Base58.data += Hash of t

  let b58check_encoding =
    R.register_encoding
      ~prefix: K.b58check_prefix
      ~length:size
      ~wrap: (fun s -> Hash s)
      ~of_raw:(fun h -> of_string h) ~to_raw:to_string

  let of_b58check_opt s =
    Base58.simple_decode b58check_encoding s
  let of_b58check_exn s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected hash (%s)" K.name
  let to_b58check s = Base58.simple_encode b58check_encoding s

  let to_short_b58check s =
    String.sub (to_b58check s) 0 (10 + 2 * String.length K.b58check_prefix)

  let pp ppf t =
    Format.pp_print_string ppf (to_b58check t)

  let pp_short ppf t =
    Format.pp_print_string ppf (to_short_b58check t)

  module Set = struct
    include Set.Make(struct type nonrec t = t let compare = compare end)
    exception Found of elt
    let random_elt s =
      let n = Random.int (cardinal s) in
      try
        ignore
          (fold (fun x i -> if i = n then raise (Found x) ; i+1) s 0 : int) ;
        assert false
      with Found x -> x
  end

  let random_set_elt = Set.random_elt

  module Map = struct
    include Map.Make(struct type nonrec t = t let compare = compare end)
  end

end

module Prefix = struct

  (* 20 *)
  let contract_hash = "\002\090\121" (* TZ(36) *)

  (* 32 *)
  let nonce_hash = "\069\220\169" (* nce(53) *)
  let script_expr_hash = "\013\044\064\027" (* expr(54) *)
  let random_state_hash = "\076\064\204" (* rng(53): never used... *)

end

module Operation_hash =
  Make_Blake2B (Base58) (struct
    let name = "Operation_hash"
    let title = "A Tezos operation ID"
    let b58check_prefix = Base58.Prefix.operation_hash
    let size = None
  end)

module Contract_hash = Make_Blake2B(Base58)(struct
    let name = "Contract_hash"
    let title = "A contract ID"
    let b58check_prefix = Prefix.contract_hash
    let size = Some 20
  end)

module Public_key_hash = Make_Blake2B(Base58)(struct
    let name = "Ed25519.Public_key_hash"
    let title = "An Ed25519 public key ID"
    let b58check_prefix = Base58.Prefix.ed25519_public_key_hash
    let size = Some 20
  end)


(*************  *)
module Ed25519 = struct

  module Public_key = struct

    type t = Sodium.Sign.public_key
    let compare = Sodium.Sign.compare_public_keys
    let (=) xs ys = compare xs ys = 0
    let (<>) xs ys = compare xs ys <> 0
    let (<) xs ys = compare xs ys < 0
    let (<=) xs ys = compare xs ys <= 0
    let (>=) xs ys = compare xs ys >= 0
    let (>) xs ys = compare xs ys > 0
    let max x y = if x >= y then x else y
    let min x y = if x <= y then x else y

    type Base58.data +=
      | Public_key of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_public_key
        ~length:Sodium.Sign.public_key_size
        ~to_raw:(fun x -> Bytes.to_string (Sodium.Sign.Bytes.of_public_key x))
        ~of_raw:(fun x ->
            try Some (Sodium.Sign.Bytes.to_public_key (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Public_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 public key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = Sodium.Sign.Bytes.to_public_key s

    let () =
      Base58.check_encoded_prefix b58check_encoding "edpk" 54

    let hash v =
      Public_key_hash.hash_bytes
        [ Sodium.Sign.Bigbytes.of_public_key v ]
  end
end
(* **********  *)



let originated_TZ1 op_hash =
  let data =
    (Bytes.to_string
       (Sodium.Generichash.Bytes.of_hash @@
        Operation_hash.of_b58check_exn op_hash)) ^ "\000\000\000\000" in
  Contract_hash.to_b58check @@ Contract_hash.hash_string [ data ]

let pk_to_tz1 (edpk : string ) =
  let data = Ed25519.Public_key.hash (Ed25519.Public_key.of_b58check_exn edpk) in
  Public_key_hash.to_b58check data

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36 ;
  Base58.check_encoded_prefix Contract_hash.b58check_encoding "KT1" 36
