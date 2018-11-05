let (>>=) = Lwt.bind

let spf = Format.sprintf
let pf = Format.printf
let epf = Format.eprintf
let spfl ?(sep="; ") l =
  spf "[%s]" @@ String.concat sep @@ List.map (spf "%s") l

let init_list len f =
  let rec aux = function
    | 0 -> [f 0]
    | n -> (f (n)) :: (aux (n-1)) in
  List.rev (aux (len-1))

let array_map3 f a b c =
  Array.mapi (fun i x -> f x b.(i) c.(i)) a

let print_json_value v =
  let s = Ezjsonm.to_string @@ Ezjsonm.wrap v in
  String.sub s 1 (String.length s - 2)

let make_tab depth = if depth<1 then "" else String.make depth '\t'

let rec print_level ?(depth=0) level json =
  match (level, json) with
  | (_, `Null) | (_, `Bool _) | (_, `Float _) | (_, `String _) |
    (0, _) -> print_json_value json
  | (lv, `A a) -> let sep = ",\n" ^ (make_tab depth) in
    spf "%s[%s]" sep @@ String.concat sep @@ List.map (print_level ~depth:(depth+1) (lv-1)) a
  | (lv, `O o) -> spf "{%s}" @@ String.concat (",\n" ^ (make_tab depth)) @@
    List.map (fun (k,v) -> spf "%s: %s" k @@ print_level ~depth:(depth+1) (lv-1) v) o

let print_json ?outfile ?(level=0) json =
  let fmt = match outfile with
    | None -> Format.std_formatter
    | Some filename ->
      let oc = open_out filename in
      Format.formatter_of_out_channel oc in
  let s = print_level level json in
  Format.fprintf fmt "%s" s


(* Services *)

let filter_version version =
  List.fold_left (fun acc (path, params) ->
      if Str.string_match (Str.regexp ("/" ^ version)) path 0 then
        (Str.replace_first (Str.regexp ("/" ^ version)) "" path , params) :: acc
      else
        acc
    ) []

let get_services ?(version="v1") () =
  let services = List.map (fun (_id, _name, path, _registered, params) ->
      path, List.map (fun (value, _name, _descr, _type) -> value) params) @@
    EzAPI.services_doc_map EzAPI.service_to_readable in
  filter_version version services

let services_to_json services =
  Ezjsonm.dict (
    List.map (fun (path,params) -> path, Ezjsonm.list Ezjsonm.string params)
      services)

let print_services ?outfile services =
  print_json ?outfile @@ services_to_json services

let load_services infile =
  let json = Ezjsonm.from_channel (open_in infile) in
  let d = Ezjsonm.get_dict json in
  List.map (fun (k,l) -> k, Ezjsonm.get_list Ezjsonm.get_string l) d


module Service = struct
  type t = string * string list
  let compare (path1, pars1) (path2, pars2) =
    let r = String.compare path1 path2 in
    if r = 0 then
      Misc.compare_lengths pars1 pars2
    else
      r
end

module SS = Set.Make(Service)

let compare_services s1 s2 =
  let s1 = SS.of_list s1 in
  let s2 = SS.of_list s2 in
  let s1ms2 = SS.diff s1 s2 in
  let s2ms1 = SS.diff s2 s1 in
  let error = ref "" in
  if (not @@ SS.is_empty s1ms2) || (not @@ SS.is_empty s2ms1) then
    begin
      error := "@[<v 0> v1 - v2 services:@;";
      SS.iter (fun (path, params) ->
          error:= spf "%s@[<h 2>\t%s %s@]@;" !error path (spfl params)) s1ms2;
      error := spf "%s@]@[<v 0> v2 - v1 services:@;" !error;
      SS.iter (fun (path, params) ->
          error := spf "%s@[\t%s %s@]@;" !error path (spfl params)) s2ms1;
      error := !error ^ "@]";
    end;
  !error

let test_services s1 s2 () =
  Format.kasprintf failwith "%s" (compare_services s1 s2)



(* Request *)

type response = {
  name: string;
  mutable status: int;
  mutable raw_content: string;
  mutable content: Ezjsonm.value;
  mutable timing: float}
let empty name = {
  name; status = 200;
  raw_content = "";
  content = Ezjsonm.unit ();
  timing = Unix.gettimeofday ()}


let mvar = Lwt_mvar.create 1

let cohttp_get msg url ?error ?(headers=[]) f =
  let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) headers in
  Cohttp_lwt_unix.Client.get ~headers
    (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  if code = 200 then begin
    Cohttp_lwt.Body.to_string body >>= fun body ->
    Lwt_mvar.take mvar >>= fun irequest ->
    Lwt_mvar.put mvar (irequest + 1) >>= fun () ->
    Lwt_io.printf "%d: %s" irequest msg >>= fun () ->
    Lwt.return @@ f body
  end else
    match error with
    | None -> Lwt.return ()
    | Some error ->
        Lwt.return @@ error code

let sprint_param (name,value) = spf "%s=%s" name value
let sprint_params = function
  | [] -> ""
  | params -> spf "?%s" @@ String.concat "&" (List.map sprint_param params)

let request
    ?(url="http://api.tzscan.io/") ?(version="v1") ?(params=[]) name =
  let full_name = spf "%s%s" name (sprint_params params) in
  let r = empty full_name in
  cohttp_get
    (spf "request.get %s%s%s\r%!" url version full_name)
    (spf "%s%s%s" url version full_name)
    (fun s -> r.raw_content <- s;
      r.timing <- Unix.gettimeofday () -. r.timing )
    ~error:(fun s -> r.status <- s;
             r.timing <- Unix.gettimeofday () -. r.timing) >>= fun () ->
  if r.raw_content <> "" then
    r.content <- Ezjsonm.value @@ Ezjsonm.from_string r.raw_content;
  Lwt.return r

let timing_table = ref []

let request_timing
    ?(url="http://api.tzscan.io/") ?(version="v1") ?(params=[]) name =
  let full_name = spf "%s%s" name (sprint_params params) in
  let r = empty full_name in
  cohttp_get
    (spf "request.get %s%s%s\r%!" url version full_name)
    (spf "%s%s%s" url version full_name)
    (fun _ ->
       r.timing <- Unix.gettimeofday () -. r.timing;
       timing_table := r :: !timing_table
    )
    ~error:(fun s -> r.status <- s;
             r.timing <- Unix.gettimeofday () -. r.timing;
             timing_table := r :: !timing_table)

let response_to_json r =
  Ezjsonm.dict [
    "name", Ezjsonm.string r.name;
    "status", Ezjsonm.int r.status;
    "content", r.content;
    "timing", Ezjsonm.float r.timing]

let json_to_response json =
  let d = Ezjsonm.get_dict json in
  let name = Ezjsonm.get_string @@ snd @@ List.find (fun (k,_) -> k = "name") d in
  let r = empty name in
  List.iter (fun (k,v) -> match k with
      | "status" -> r.status <- Ezjsonm.get_int v
      | "content" -> r.content <- v;
      | "timing" -> r.timing <- Ezjsonm.get_float v
      | _ -> ()) d;
  r


(* Compare JSON *)

let error_buffer = ref ""
let check error prn expected given msg =
  if given <> expected then
    error := spf "%s@[%s@, v1: %s@, v2: %s@]\n" !error msg (prn expected) (prn given)


let rec compare_json ?(field="") err json1 json2 =
  match (json1,json2) with
  | `Null, `Null -> ()
  | `Bool b1, `Bool b2 ->
    check err string_of_bool b1 b2
      (spf "boolean values for %s are different" field)
  | `Float f1, `Float f2 ->
    check err string_of_float f1 f2
      (spf "float values for %s are different" field)
  | `String s1, `String s2 ->
    check err (fun s -> s) s1 s2
      (spf "string values for %s are different" field)
  | `A l1, `A l2 ->
    check err string_of_int (List.length l1) (List.length l2)
      (spf "arrays for %s have different length"  field);
    if List.length l1 = List.length l2 then
      let i = ref 0 in
      List.iter2 (fun v1 v2 ->
          let field = spf "%s.(%d)" field !i in
          incr i;
          compare_json ~field err v1 v2) l1 l2
  | `O l1, `O l2 ->
    check err string_of_int (List.length l1) (List.length l2)
      (spf "arrays for %s have different length"  field);
    if List.length l1 = List.length l2 then
      let l1 = List.sort (fun (k1,_) (k2,_) -> String.compare k1 k2) l1 in
      let l2 = List.sort (fun (k1,_) (k2,_) -> String.compare k1 k2) l2 in
      let i = ref 0 in
      List.iter2 (fun (k1,v1) (k2,v2) ->
          check err (fun x -> x) k1 k2
            (spf "record keys are different for %s.(%d)" field !i);
          incr i;
          if k1 = k2 then
            let field = spf "%s/%s" field k1 in
            compare_json ~field err v1 v2) l1 l2
  | _, _ ->
    check err string_of_int 0 1
    (spf "value for %s have different types" field)

let test_response js1 js2 () =
  error_buffer := "";
  compare_json error_buffer js1 js2;
  if !error_buffer <> "" then Format.kasprintf failwith "\n%s" !error_buffer



(* Entries *)

let get_blocks ?(url="http://api.tzscan.io/") ?(version="v1") ?(params=[]) () =
  request ~url ~version ~params "/blocks" >>= fun r ->
  Lwt.return @@ List.map
    (fun x -> Ezjsonm.get_string (Ezjsonm.find x ["hash"]))
    (Ezjsonm.get_list (fun x -> x) r.content)

let get_accounts ?(url="http://api.tzscan.io/") ?(version="v1") ?(params=[]) () =
  request ~url ~version ~params "/accounts" >>= fun r ->
  Lwt.return @@ List.map
    (fun x -> Ezjsonm.get_string (Ezjsonm.find x ["hash"]))
    (Ezjsonm.get_list (fun x -> x) r.content)

let get_operations ?(url="http://api.tzscan.io/") ?(version="v1") ?(params=[]) () =
  request ~url ~version ~params "/operations" >>= fun r ->
  Lwt.return @@ List.map
    (fun x -> Ezjsonm.get_string (Ezjsonm.find x ["hash"]))
    (Ezjsonm.get_list (fun x -> x) r.content)

let get_last_level ?(url="http://api.tzscan.io/") ?(version="v1") () =
  request ~url ~version "/head" >>= fun r ->
  Lwt.return @@ Ezjsonm.get_int @@ Ezjsonm.find r.content ["level"]

let random_hash ?(start="") ?(len=51) () =
  start ^
  (String.init (len - String.length start)
     (fun _ -> let i= Random.int 52 in
       char_of_int (if i<26 then i+97 else i+39)))

let get_entries ?(url="http://api.tzscan.io/") ?(version="v1") ?(params=[]) () =
  get_blocks ~url ~version ~params () >>= fun blocks ->
  get_accounts ~url ~version ~params () >>= fun accounts ->
  get_operations ~url ~version ~params () >>= fun operations ->
  get_accounts ~url ~version ~params:(("contract","true")::params) () >>= fun contracts ->
  get_last_level ~url ~version () >>= fun last_level ->
  let levels = init_list 10 (fun _ -> Random.int last_level |> string_of_int) in
  let cycles = init_list 10 (fun _ -> Random.int (last_level/128) |> string_of_int) in
  let periods = ["minutes"; "hours"] in
  let nodes = ["balance"; "state"] in
  let fake_block = random_hash ~start:"B" () in
  let fake_account = random_hash ~start:"tz1" ~len:36 () in
  let fake_operation = random_hash ~start:"o" () in
  let fake_contract = random_hash ~start:"TZ1" ~len:36 () in
  let fake_level = string_of_int @@ last_level + 100 in
  let fake_cycle = string_of_int @@ last_level/128 + 10 in
  let fake_period = "century" in
  let fake_node = "fake_node" in
  Lwt.return
    ["blocks", fake_block :: blocks;
     "accounts", fake_account :: accounts;
     "operations", fake_operation :: operations;
     "contracts", fake_contract :: contracts;
     "levels", fake_level :: levels;
     "cycles", fake_cycle :: cycles;
     "periods", fake_period :: periods;
     "nodes", fake_node :: nodes]

let entries_to_json entries =
  Ezjsonm.dict @@
  List.map (fun (name, l) -> name, Ezjsonm.list Ezjsonm.string l) entries

let print_entries ?outfile entries =
  print_json ?outfile @@ entries_to_json entries

let load_entries infile =
  let json = Ezjsonm.from_channel (open_in infile) in
  let d = Ezjsonm.get_dict json in
  List.map (fun (k,l) -> k, Ezjsonm.get_list Ezjsonm.get_string l) d





(* Request List *)

let arg_list = ["blocks", "<block_hash>";
                "accounts", "<account_hash>";
                "contracts", "<contract_hash>";
                "operations", "<op_hash>";
                "any", "<hash>";
                "periods", "<period>";
                "levels", "<level>";
                "cycles", "<cycle>";
                "nodes", "<node>"]

let type_choice = [
  "seed";
  "faucet";
  "transaction";
  "origination";
  "delegation";
  "endorsment";
  "proposal";
  "ballot";
  "reveal"]
let p_choice = ["5"; "10"; "20"]
let number_choice = ["2";"5";"10"]
let state_choice = ["running"]
let operations_choice = ["true";"false"]
let contract_choice = ["true";"false"]
let status_choice = ["pending"]
let choice_list = [
  "type", type_choice;
  "p", p_choice;
  "number", number_choice;
  "state", state_choice;
  "operations", operations_choice;
  "contract", contract_choice;
  "status", status_choice]

let add_none l = None :: (List.map (fun x -> Some x) l)

let rec params_list = function
  | [] -> [[]]
  | param :: params ->
    let choices = List.assoc param choice_list in
    List.concat @@
    List.map (fun e ->
        List.map (fun c -> match c with
            | None -> e
            | Some c -> (param, c) :: e) (add_none choices))
      (params_list params)

let service_requests service entries =
  Lwt.bind entries
    (fun entries ->
       let l = match Misc.list_find_opt (fun (_, sarg) ->
           Str.string_match (Str.regexp (".*" ^ sarg)) (fst service) 0) arg_list with
       | None -> [fst service]
       | Some (s, sarg) ->
         let l_entries =
           if s <> "any" then List.assoc s entries
           else
             List.concat [
               List.assoc "blocks" entries;
               List.assoc "accounts" entries;
               List.assoc "contracts" entries;
               List.assoc "operations" entries] in
         List.map
           (fun e -> Str.replace_first (Str.regexp sarg) e (fst service))
           l_entries in
       let plist = params_list (snd service) in
       Lwt.return @@ List.concat @@
       List.map (fun e -> List.map (fun p -> e, p ) plist) l)

let get_responses_list ?(url="http://api.tzscan.io/") ?(version="v1") entries services =
  Lwt_list.map_s
    (fun service ->
       service_requests service entries >>= fun l ->
       Lwt_list.map_s (fun (name, params) ->
           request ~url ~version ~params name) l)
    services
  >>= fun l -> Lwt.return @@ List.concat l

let responses_to_json responses =
  Ezjsonm.list (fun x -> x) (List.map response_to_json responses)

let load_responses infile =
  let json = Ezjsonm.from_channel (open_in infile) in
  Ezjsonm.get_list json_to_response json

module SSet = Set.Make(String)

let inter_services f r1 r2 =
  let s1 = SSet.of_list @@ List.map (fun r -> r.name) r1 in
  let s2 = SSet.of_list @@ List.map (fun r -> r.name) r2 in
  let names = SSet.elements @@ SSet.inter s1 s2 in
  List.map (fun n ->
      let r11 = List.find (fun r -> r.name = n) r1 in
      let r22 = List.find (fun r -> r.name = n) r2 in
      n, f r11, f r22
    ) names

let test_list_services r1 r2 () =
  let s1 = SSet.of_list @@ List.map (fun r -> r.name) r1 in
  let s2 = SSet.of_list @@ List.map (fun r -> r.name) r2 in
  let s1ms2 = SSet.elements @@ SSet.diff s1 s2 in
  let s2ms1 = SSet.elements @@ SSet.diff s2 s1 in
  if (s1ms2 <> []) || (s2ms1 <> []) then
    Format.kasprintf failwith "\nv1 - v2: %s\nv2 - v1: %s\n"
      (spfl s1ms2) (spfl s2ms1)



(* Timings *)

type timing_stat = {
  request_name: string;
  nrequest: int;
  percent: float;
  average: float;
  median: float;
  std_dev: float
}
let empty_timing_stat ?(nrequest=0) request_name = {
  request_name;
  nrequest;
  percent = 0.;
  average = 0.;
  median = 0.;
  std_dev = 0.}

let average l f = match l with
  | [] -> 0.
  | l -> (List.fold_left (fun acc x -> acc +. (f x)) 0. l) /.
         (float @@ List.length l)

let median l f = match l with
  | [] -> 0.
  | l ->
    let l = List.sort (fun x1 x2 -> compare (f x1) (f x2)) l in
    let n = List.length l in
    if n mod 2 = 0 then
      (f (List.nth l (n/2 - 1)) +.
       f (List.nth l (n/2))) /. 2.
    else
      f (List.nth l (n/2))

let std_dev l f = match l with
  | [] -> 0.
  | l -> let av = average l f in
    sqrt @@
    (List.fold_left (fun acc x -> ((f x) -. av) ** 2. +. acc) 0. l)
    /. (float @@ List.length l)

let remove_params s =
  try
    String.sub s 0 (String.index s '?')
  with _ -> s

let short_request s =
  let s = remove_params s in
  let i = String.rindex s '/' in
  if i = 0 then s else String.sub s 0 i

let init_response_short r = empty @@ short_request r.name

let rec mem_response r = function
  | [] -> false
  | h :: t -> r.name = h.name || mem_response r t

let rec mem_timing_stat r = function
  | [] -> false
  | h :: t -> (short_request r.name) = h.request_name || mem_timing_stat r t

let average_timing = function (* responses with same short name already *)
  | [] -> empty_timing_stat "empty"
  | (h :: _t) as responses ->
    let nrequest = List.length responses in
    let request_name = short_request h.name in
    match List.filter (fun r -> r.status = 200) responses with
    | [] -> empty_timing_stat ~nrequest request_name
    | rs ->
      let get_timing = fun r -> r.timing in
      let count = List.length rs in
      let average = average rs get_timing in
      let median = median rs get_timing in
      let percent = (float count) /. (float nrequest) in
      let std_dev = std_dev rs get_timing in
      {request_name; nrequest; percent; average; median; std_dev}

let average_timings responses =
  List.fold_left (fun acc r ->
      if mem_timing_stat r acc then acc else
        let short_name = short_request r.name in
        (average_timing @@
         List.filter (fun r2 -> short_request r2.name = short_name) responses) :: acc)
    [] responses

(* let compare_timing r1 r2 () =
 *   if r1.status <> r2.status then
 *     Format.kasprintf failwith "@[different status for %s@, v1: %d@, v2: %d@]\n"
 *       r1.name r1.status r2.status;
 *   if r2.timing > 3. *. r1.timing then
 *     Format.kasprintf failwith "@[v2 much slower than v1 for %s@, v1: %f@, v2: %f@]\n"
 *       r1.name r1.timing r2.timing *)

let timing_stat_to_json tstat =
  Ezjsonm.dict [
    "name", Ezjsonm.string tstat.request_name;
    "n", Ezjsonm.int tstat.nrequest;
    "percent", Ezjsonm.float tstat.percent;
    "average", Ezjsonm.float tstat.average;
    "median", Ezjsonm.float tstat.median;
    "std_dev", Ezjsonm.float tstat.std_dev
  ]

let timing_stats_to_json l =
  Ezjsonm.list (fun x-> x) (List.map timing_stat_to_json l)

(* Argument handling *)
let set_opt_string r = Arg.String (fun s -> r := Some s)
let array_string a = Arg.Tuple
    (init_list (Array.length a) (fun i -> Arg.String (fun s -> a.(i) <- s)))


let () =
  let action = ref "nothing" in
  let version = ref None in
  let url = ref None in
  let outfile = ref None in
  let fentries1 = ref None in
  let fservices1 = ref None in
  let fresponses1 = ref None in
  let request_name = ref None in
  let nrequest = ref 1 in
  let version2 = [|""; ""|] in
  let fservices2 = [|""; ""|] in
  let fresponses2 = [|""; ""|] in
  let speclist = [
    ("-a", Arg.Set_string action, "action to do");
    ("-v", set_opt_string version, "optional versions of the API");
    ("-u", set_opt_string url, "optional api url");
    ("-o", set_opt_string outfile, "optional output file");
    ("-e", set_opt_string fentries1, "optional entries");
    ("-s", set_opt_string fservices1, "optional services");
    ("-rs", set_opt_string fresponses1, "optional responses");
    ("-r", set_opt_string request_name, "API request");
    ("-n", Arg.Set_int nrequest, "number of request");
    ("-v2", array_string version2, "2 versions inputs");
    ("-s2", array_string fservices2, "2 services inputs");
    ("-rs2", array_string fresponses2, "response files to compare")
  ] in
  let usage_msg = "Produce list of function for a API version" in
  Arg.parse speclist (fun _s -> ()) usage_msg;

  (* utils *)
  let is_version v = v <> "" && String.get v 0 = 'v' in
  let make_request0 () = match !request_name with
    | None -> epf "No path for request name\n%!"; Lwt.return [empty "error"]
    | Some name ->
      Lwt_list.map_s
        (fun _ -> request ?url:!url ?version:!version name)
        (init_list !nrequest (fun x -> x))
        >>= fun l -> Lwt_unix.sleep 0.1 >>= fun () -> Lwt.return l
  in
  let make_timing0 () = match !request_name with
    | None -> epf "No path for request name\n%!"; Lwt.return []
    | Some name ->
      Lwt_list.iter_p
        (fun _ -> request_timing ?url:!url ?version:!version name)
        (init_list !nrequest (fun x -> x))
      >>= fun () -> Lwt_unix.sleep 0.1 >>= fun () -> Lwt.return !timing_table
  in
  let make_entries0 () = get_entries ?url:!url ?version:!version () in
  let make_entries1 () = match !fentries1 with
    | None -> make_entries0 ()
    | Some infile -> Lwt.return (load_entries infile) in
  let make_services0 () = get_services ?version:!version () in
  let make_services1 () = match !fservices1 with
    | None -> make_services0 ()
    | Some infile -> load_services infile in
  let make_services2 v s = match (is_version v), s <> "" with
    | false, false -> epf "no version or service file provided"; []
    | false, _ -> load_services s
    | _, _ -> get_services ~version:v () in
  let make_responses1 () = match !fresponses1 with
    | None -> get_responses_list ?url:!url ?version:!version
                (make_entries1 ()) (make_services1 ())
    | Some infile -> Lwt.return (load_responses infile) in
  let make_responses2 v s r = match (is_version v), s <> "", r <> "" with
    | false, false, false -> epf "no version or service or response file provided";
      Lwt.return []
    | false, false, _ -> Lwt.return (load_responses r)
    | _, _, _ ->
      get_responses_list ?url:!url ~version:v (make_entries1 ()) (make_services2 v s) in

  (* print json *)
  let json, level = Lwt_main.run
      (match !action with
       | "request" -> make_request0 ()
         >>= fun lr -> Lwt.return (responses_to_json lr, 2)
       | "timing" -> make_timing0 ()
         >>= fun lr -> Lwt.return (timing_stat_to_json (average_timing lr), 2)
       | "services" -> Lwt.return (services_to_json @@ make_services0 (), 1)
       | "entries" -> make_entries0 ()
         >>= fun le -> Lwt.return (entries_to_json le, 2)
       | "requests" -> make_responses1 ()
         >>= fun lr -> Lwt.return (responses_to_json lr, 2)
       | "timings" -> make_responses1 ()
         >>= fun lr -> Lwt.return ((timing_stats_to_json (average_timings lr)), 1)
       | _ ->
         Lwt_io.eprintf "Action not recognized!" >>= fun () ->
         Lwt.return (Ezjsonm.dict [], 0)) in
  if json <> Ezjsonm.dict [] then (
    print_newline ();
    print_json ~level ?outfile:!outfile json);

  (* compare stuff *)
  match !action with
  | "compare_services" ->
    let s2 = Array.map2 make_services2 version2 fservices2 in
    Alcotest.run ~argv:[|""|] "Test services" [
      "test_services", ["test_services", `Quick, (test_services s2.(0) s2.(1))]]
  | "compare" ->
    Lwt_main.run (
      let r2 = array_map3 make_responses2 version2 fservices2 fresponses2 in
      r2.(0) >>= fun r20 -> r2.(1) >>= fun r21 ->
      let js = inter_services (fun r -> r.content) r20 r21 in
      Lwt.return @@ Alcotest.run ~argv:[|""|] "Test version1" [
        "test_services",["test_services", `Quick, (test_list_services r20 r21)];
        "test_request", List.map
          (fun (name, js1, js2) -> name, `Quick, test_response js1 js2) js
    ])
  | _ -> ()
