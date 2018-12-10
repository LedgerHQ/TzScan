(* wget 'http://tz.api2.tzscan.io/chains/main/blocks/"the_block"/context/raw/json/contracts/index?depth=4' *)
open StringCompat

module TYPES = struct

  type tez = int64

  type frozen_balance = {
    deposits : int64;
    rewards : int64;
    fees : int64 option;
  }

  type contract = {
    balance : tez option;
    big_map : string list option;
    counter : int64 option;
    delegated : (string * bool) list option;
    frozen_balance : (int * frozen_balance) list option;
    manager : string option;
    spendable : bool option;
    delegatable : bool option;
    roll_list : int option;
    change : tez option;
    delegate : string option;
    delegate_desactivation : int option;
    inactive_delegate : bool option;
    paid_bytes : int64 option;
    used_bytes : int64 option;
    storage : Json_repr.ezjsonm option;
    code : Json_repr.ezjsonm option;
  }

  type context_contracts = (string * contract) list



  type contract_info = {
    addr : string;
    contract: contract;

    mutable current_balance : int64;
    mutable total_balance : int64;
    (* balance  frozen_balance.{deposits  fees  rewards} *)

    mutable total_frozen_balance : int64;
    mutable total_frozen_deposits : int64;
    mutable total_frozen_rewards : int64;
    mutable total_frozen_fees : int64;
    mutable total_staking_balance : int64;
    mutable total_delegated : int64;
    mutable total_paid_bytes : int64;
    mutable total_used_bytes : int64;
  }

  type _ change_kind =
    | Balance : tez change_kind
    | Counter : int64 change_kind
    | Frozen_balance : int -> frozen_balance change_kind
    | Delegated : string -> bool change_kind
    | Roll_list : int change_kind
    | Change : tez change_kind
    | Delegate : string change_kind
    | Delegate_desactivation : int change_kind
    | Inactive_delegate : bool change_kind
    | Paid_bytes : int64 change_kind
    | Used_bytes : int64 change_kind

  type _ change =
    | Update : 'a change_kind * 'a option * 'a option -> unit change

(*
      delegated : (string * bool) list option;
*)

  type diff_snapshots = {
    added_contracts : (string * contract) list;
    removed_contracts : string list;
    changed_contracts : (string * unit change list) list;
  }

  type snapshot = {
    map : contract_info StringMap.t;
    array : contract_info array;
  }

end

open TYPES

module JSON : sig

  (* Parse a string in JSON format containing a Tezos context and
     return the list of contracts. *)

  val decode_context_contracts : content:string -> context_contracts

  val decode_diff_snapshots : content:string -> TYPES.diff_snapshots
  val encode_diff_snapshots : TYPES.diff_snapshots -> string

  val contract_encoding : contract Json_encoding.encoding
  val context_contracts_encoding : context_contracts Json_encoding.encoding
  val diff_snapshots_encoding : diff_snapshots Json_encoding.encoding

end = struct

  open Json_encoding
  open EzEncoding

  let tez_encoding = int64

  let frozen_balance_encoding =
    obj3
      (req "deposits" tez_encoding)
      (req "rewards" tez_encoding)
      (opt "fees" tez_encoding)

  let frozen_balance_encoding =
    conv
      (fun
        { deposits; rewards; fees }
        ->
          ( deposits, rewards, fees )
      )
      (fun
        ( deposits, rewards, fees )
        ->
          { deposits; rewards; fees }
      )
      frozen_balance_encoding

  let contract_encoding =
    merge_objs
      (obj10
         (opt "balance" tez_encoding)
         (opt "big_map" (list string))
         (opt "counter" int64)
         (opt "delegated" (list (tup2 string bool)))
         (opt "frozen_balance" (list (tup2 int frozen_balance_encoding)))
         (opt "manager" string)
         (opt "spendable" bool)
         (opt "delegatable" bool)
         (opt "roll_list" int)
         (opt "change" tez_encoding)
      )
      (obj7
         (opt "delegate" string)
         (opt "delegate_desactivation" int)
         (opt "inactive_delegate" bool)
         (opt "paid_bytes" int64)
         (opt "used_bytes" int64)
         (opt "storage" any_ezjson_value)
         (opt "code" any_ezjson_value)
      )

  let contract_encoding =
    conv
      (fun
        {
          balance;  big_map;  counter;  delegated;
          frozen_balance;  manager;  spendable;  delegatable;  roll_list;
          change;
          delegate;  delegate_desactivation;  inactive_delegate;
          paid_bytes;  used_bytes;
          storage;  code }
        ->
          (balance, big_map, counter, delegated,
           frozen_balance, manager, spendable, delegatable, roll_list, change),
          (delegate, delegate_desactivation, inactive_delegate,
           paid_bytes, used_bytes,
           storage, code)
      )
      (fun
        (
          (balance, big_map, counter, delegated,
           frozen_balance, manager, spendable, delegatable, roll_list, change),
          (delegate, delegate_desactivation, inactive_delegate,
           paid_bytes, used_bytes,
           storage, code)
        ) ->
        {
          balance;  big_map;  counter;  delegated;
          frozen_balance;  manager;  spendable;  delegatable;  roll_list;
          change;
          delegate;  delegate_desactivation;  inactive_delegate;
          paid_bytes;  used_bytes;
          storage;  code }
      )
      contract_encoding


  let context_contracts_encoding = list (tup2 string contract_encoding)

  let update convert name before after =
    let change = function
      | None -> None
      | Some n -> Some (convert n)
    in
    (name, change before, change after, None)

  let int64_change = update Int64.to_string
  let int_change = update string_of_int
  let bool_change = update string_of_bool
  let string_change = update (fun (s : string) -> s)

  let change convert kind before after =
    let change = function
      | None -> None
      | Some n -> Some (convert n)
    in
    Update (kind, change before, change after)

  let change_int64 = change Int64.of_string
  let change_int = change int_of_string
  let change_bool = change bool_of_string
  let change_string = change (fun s -> s)

  let changed_encoding =
    conv
      (function
        | Update (Balance, a, b) -> int64_change "balance" a b
        | Update (Counter, a, b) -> int64_change "counter" a b
        | Update (Roll_list, a, b) -> int_change "roll_list" a b
        | Update (Change, a, b) -> int64_change "change" a b
        | Update (Delegate, a, b) -> string_change "delegate" a b
        | Update (Delegate_desactivation, a, b) ->
          int_change "delegate_desactivation" a b
        | Update (Inactive_delegate, a, b) -> bool_change "inactive_delegate" a b
        | Update (Paid_bytes, a, b) -> int64_change "paid_bytes" a b
        | Update (Used_bytes, a, b) -> int64_change "used_bytes" a b
        | Update (Frozen_balance cycle, a, b) ->
          "frozen_balance", None, None, Some (cycle,
                                              (a : frozen_balance option),
                                              (b : frozen_balance option)
                                             )
        | Update (Delegated addr, a, b) ->
          bool_change addr a b
      )
      (fun (name, a, b, maybe_frozen) ->
         match name with
         | "balance" -> change_int64 Balance a b
         | "frozen_balance" ->
           begin
             match maybe_frozen with
             | None -> assert false
             | Some (cycle, a,b) -> Update (Frozen_balance cycle, a, b)
           end
         | "counter" -> change_int64 Counter a b
         | "roll_list" -> change_int Roll_list a b
         | "change" -> change_int64 Change a b
         | "delegate" -> change_string Delegate a b
         | "delegate_desactivation" -> change_int Delegate_desactivation a b
         | "inactive_delegate" -> change_bool Inactive_delegate a b
         | "paid_bytes" -> change_int64 Paid_bytes a b
         | "used_bytes" -> change_int64 Used_bytes a b
         | addr -> change_bool (Delegated addr) a b
      )
      (obj4
         (req "change" string)
         (opt "before" string)
         (opt "after" string)
         (opt "frozen" (tup3 int
                          (option frozen_balance_encoding)
                          (option frozen_balance_encoding)))
      )

  let diff_snapshots_encoding =
    conv
      (fun
        {
          added_contracts   ;
          removed_contracts ;
          changed_contracts ;
        }
        ->
          ( added_contracts ,
            removed_contracts ,
            changed_contracts )
      )
      (fun
        ( added_contracts ,
          removed_contracts ,
          changed_contracts )
        ->
          {
            added_contracts   ;
            removed_contracts ;
            changed_contracts ;
          }
      )
      (obj3
         (req "added_contracts" (list (tup2 string contract_encoding)))
         (req "removed_contracts" (list string))
         (req "changed_contracts" (list (tup2 string (list changed_encoding))))
      )

  let decode_context_contracts ~content =
    EzEncoding.destruct context_contracts_encoding content

  let decode_diff_snapshots ~content =
    EzEncoding.destruct diff_snapshots_encoding content
  let encode_diff_snapshots diff =
    EzEncoding.construct diff_snapshots_encoding diff

end

module Printer : sig

  val string_of_tez : int64 -> string
  val string_of_contract : contract -> string

end = struct

  let b = Buffer.create 10000

  let one_tez = 1000000L
  let string_of_tez v =
    let tez = Int64.div v one_tez in
    let mutez = Int64.sub v (Int64.mul tez one_tez) in
    Printf.sprintf "%Ld.%06Ld" tez mutez

  let string_of_contract c =
    Buffer.clear b;
    Printf.bprintf b "{\n";
    let opt name to_string field =
      match field with
      | None -> ()
      | Some v ->
        Printf.bprintf b "  %s = %s\n" name (to_string v)
    in
    opt "balance" string_of_tez c.balance;
    opt "counter" Int64.to_string c.counter;
    opt "delegated"
      (fun list ->
         let b = Buffer.create 1000 in
         Printf.bprintf b "\n";
         List.iter (fun (delegated, bool) ->
             Printf.bprintf b "    %s -> %b\n" delegated bool
           ) list;
         Buffer.contents b
      ) c.delegated;
    opt "frozen_balance"
      (fun list ->
         let b = Buffer.create 1000 in
         Printf.bprintf b "\n";
         List.iter (fun (cycle, fb) ->
             Printf.bprintf b "    %d -> { deposits: %s ; rewards : %s ; fees : %s }\n"
               cycle
               (string_of_tez fb.deposits)
               (string_of_tez fb.rewards)
               (match fb.fees with
                  None -> "0"
                | Some t -> string_of_tez t)
           ) list;
         Buffer.contents b
      ) c.frozen_balance;
    opt "manager" (fun s -> s) c.manager; (* oups, can be an edpk *)
    opt "spendable" string_of_bool c.spendable;
    opt "delegatable" string_of_bool c.delegatable;
    opt "roll_list" string_of_int c.roll_list;
    opt "change" string_of_tez c.change;
    opt "delegate" (fun s -> s) c.delegate;
    opt "delegate_desactivation" string_of_int c.delegate_desactivation;
    opt "inactive_delegate" string_of_bool c.inactive_delegate;
    opt "paid_bytes" Int64.to_string c.paid_bytes;
    opt "used_bytes" Int64.to_string c.used_bytes;
    opt "storage" (fun _s -> "true") c.storage;
    opt "code" (fun _s -> "true") c.code;

    Printf.bprintf b "}\n";
    Buffer.contents b

end

let (++) = Int64.add

module Snapshot : sig

  val compute : context_contracts -> snapshot

end = struct

  let compute v =

    let list = ref [] in
    let map =
      let map = ref StringMap.empty in
      List.iter (fun (addr, contract) ->
          let current_balance =
            match contract.balance with
            | None -> 0L
            | Some balance -> balance
          in
          let info = { addr ;
                       contract ;
                       current_balance ;
                       total_balance = 0L ;
                       total_frozen_balance = 0L ;
                       total_frozen_deposits = 0L ;
                       total_frozen_rewards = 0L ;
                       total_frozen_fees = 0L ;
                       total_staking_balance = 0L ;
                       total_delegated = 0L ;
                       total_paid_bytes = 0L ;
                       total_used_bytes = 0L ;
                    } in
          map := StringMap.add addr info !map;
          list := info :: !list;

          (* compute total_balance *)
          begin  match contract.frozen_balance with
            | None -> ()
            | Some frozen_balance ->
              List.iter (fun (_cycle, fb) ->
                  info.total_frozen_deposits <-
                    info.total_frozen_deposits ++ fb.deposits;
                  info.total_frozen_rewards <-
                    info.total_frozen_rewards ++ fb.rewards;
                  match fb.fees with
                  | None -> ()
                  | Some fees ->
                    info.total_frozen_fees <-
                      info.total_frozen_fees ++ fees
                ) frozen_balance;
          end;
          info.total_frozen_balance <-
            info.total_frozen_deposits ++
            info.total_frozen_rewards ++
            info.total_frozen_fees;
          info.total_balance <- info.total_frozen_balance ++ current_balance;

          begin match contract.paid_bytes with
            | None -> ()
            | Some size -> info.total_paid_bytes <- size
          end;

          begin match contract.used_bytes with
            | None -> ()
           | Some size -> info.total_used_bytes <- size
          end;

          begin
            match contract.delegated with
            | None -> ()
            | Some list ->
              info.total_delegated <-
                Int64.of_int (List.length list)
          end;

          begin
            match contract.delegate with
            | None -> ()
            | Some delegate_addr ->
              if delegate_addr = addr then
                info.total_staking_balance <-
                  current_balance
                  ++ info.total_frozen_deposits
                  ++ info.total_frozen_fees;
          end
        ) v;
      !map
    in

    (* Update skaking balances *)
    StringMap.iter (fun _ info ->
        match info.contract.delegated with
        | None -> ()
        | Some list ->
          List.iter (fun (addr, bool) ->
              if bool then
                let balance = try
                    (StringMap.find addr map).current_balance
                  with Not_found -> 0L
                in
                info.total_staking_balance <-
                  info.total_staking_balance ++ balance
            ) list
      ) map;

    let array = Array.of_list !list in

    { map ; array }

end


module Diff : sig

  val snapshots : snapshot -> snapshot -> diff_snapshots

end = struct

  let contracts { contract = c1 ; _ } { contract = c2 ; _ } =
    let changes = ref [] in
    let maybe_update kind f1 f2 =
      if f1 <> f2 then
        changes := Update (kind, f1, f2) :: !changes
    in
    maybe_update Balance c1.balance c2.balance;
    maybe_update Counter c1.counter c2.counter;
    maybe_update Roll_list c1.roll_list c2.roll_list;
    maybe_update Change c1.change c2.change;
    maybe_update Delegate c1.delegate c2.delegate;
    maybe_update Delegate_desactivation c1.delegate_desactivation
      c2.delegate_desactivation;
    maybe_update Inactive_delegate c1.inactive_delegate c2.inactive_delegate;
    maybe_update Paid_bytes c1.paid_bytes c2.paid_bytes;
    maybe_update Used_bytes c1.used_bytes c2.used_bytes;

    (* delegated *)
    let map_of_delegated delegated =
      let map = ref StringMap.empty in
      begin match delegated with
        | None -> ()
        | Some list ->
          List.iter (fun (addr, bool) ->
              map := StringMap.add addr bool !map
            ) list
      end;
      !map
    in
    let delegated1 = map_of_delegated c1.delegated in
    let delegated2 = map_of_delegated c2.delegated in
    StringMap.iter (fun addr bool1 ->
        try
          let bool2 = StringMap.find addr delegated2 in
          if bool1 <> bool2 then
            maybe_update (Delegated addr) (Some bool1) (Some bool2)
        with Not_found ->
          maybe_update (Delegated addr) (Some bool1) None
      ) delegated1;
    StringMap.iter (fun addr bool2 ->
        if not (StringMap.mem addr delegated1) then
          maybe_update (Delegated addr) None (Some bool2)
      ) delegated2;

    (* frozen balance *)
    let map_of_frozen_balance frozen_balance =
      let map = ref IntMap.empty in
      begin match frozen_balance with
        | None -> ()
        | Some list ->
          List.iter (fun (cycle, frozen_balance) ->
              map := IntMap.add cycle frozen_balance !map
            ) list
      end;
      !map
    in
    let frozen_balance1 = map_of_frozen_balance c1.frozen_balance in
   let frozen_balance2 = map_of_frozen_balance c2.frozen_balance in
    IntMap.iter (fun cycle bool1 ->
        try
          let bool2 = IntMap.find cycle frozen_balance2 in
          if bool1 <> bool2 then
            maybe_update (Frozen_balance cycle) (Some bool1) (Some bool2)
        with Not_found ->
          maybe_update (Frozen_balance cycle) (Some bool1) None
      ) frozen_balance1;
    IntMap.iter (fun cycle bool2 ->
        if not (IntMap.mem cycle frozen_balance1) then
          maybe_update (Frozen_balance cycle) None (Some bool2)
      ) frozen_balance2;

    !changes

  let snapshots s1 s2 =

    let added_contracts = ref [] in
    let removed_contracts = ref [] in
    let changed_contracts = ref [] in
    StringMap.iter (fun addr c1 ->
        try
          let c2 = StringMap.find addr s2.map in
          if c1 <> c2 then
            changed_contracts := (addr, contracts c1 c2) :: !changed_contracts
        with Not_found ->
          removed_contracts := addr :: !removed_contracts
      ) s1.map;
    StringMap.iter (fun addr c2 ->
        if not (StringMap.mem addr s1.map) then
          added_contracts := (addr, c2.contract) :: !added_contracts
      ) s2.map;
    {
      added_contracts = !added_contracts ;
      removed_contracts = !removed_contracts ;
      changed_contracts = !changed_contracts ;
    }


end






module Compute : sig

  val total_addresses : snapshot -> int (* number of addresses *)
  val total_keys : snapshot -> int (* number of addresses from keys *)
  val total_contracts : snapshot -> int (* number of contracts *)
  val total_accounts : snapshot -> int (* number of accounts *)
  val total_roll_owners : snapshot -> int
  val total_rolls : snapshot -> int
  val total_deleguees : snapshot -> int
  val total_multi_deleguees : snapshot -> int (* more than 1 delegated *)
  val total_delegated : snapshot -> int
  val total_self_delegates : snapshot -> int

  val total_current_balances : snapshot -> int64
  val total_frozen_balances : snapshot -> int64
  val total_frozen_deposits : snapshot -> int64
  val total_frozen_rewards : snapshot -> int64
  val total_frozen_fees : snapshot -> int64
  val total_staking_balances : snapshot -> int64
  val total_paid_bytes : snapshot -> int64
  val total_used_bytes : snapshot -> int64

  type top = snapshot -> max:int -> contract_info array

  val top_current_balance : top
  val top_total_balance : top
  val top_total_frozen_deposits : top
  val top_total_frozen_fees : top
  val top_total_frozen_rewards : top
  val top_total_frozen_balance : top
  val top_total_staking_balance : top
  val top_total_delegated : top
  val top_total_paid_bytes : top
  val top_total_used_bytes : top

end = struct

  type top = snapshot -> max:int -> contract_info array

  let top proj snapshot ~max =
    let array = snapshot.array in
    Array.sort (fun x1 x2 -> compare (proj x2) (proj x1)) array;
    let n = min max (Array.length array) in
    Array.sub array 0 n

  let top_current_balance = top (fun info -> info.current_balance)
  let top_total_balance = top (fun info -> info.total_balance)
  let top_total_frozen_balance = top (fun info -> info.total_frozen_balance)
  let top_total_frozen_deposits = top (fun info -> info.total_frozen_deposits)
  let top_total_frozen_rewards = top (fun info -> info.total_frozen_rewards)
  let top_total_frozen_fees = top (fun info -> info.total_frozen_fees)
  let top_total_staking_balance = top (fun info -> info.total_staking_balance)
  let top_total_delegated =
    top (fun info -> info.total_delegated)
  let top_total_paid_bytes = top (fun info -> info.total_paid_bytes)
  let top_total_used_bytes = top (fun info -> info.total_used_bytes)

  let total_addresses snapshot = Array.length snapshot.array

  let sum proj snapshot =
    let array = snapshot.array in
    let sum = ref 0L in
    for i = 0 to Array.length array - 1 do
      sum := !sum ++ proj array.(i)
    done;
    !sum

  let sum_int proj snapshot = Int64.to_int (sum proj snapshot)

  let total_current_balances = sum (fun info -> info.current_balance)
  let total_frozen_balances = sum (fun info -> info.total_frozen_balance)
  let total_frozen_deposits = sum (fun info -> info.total_frozen_deposits)
  let total_frozen_rewards = sum (fun info -> info.total_frozen_rewards)
  let total_frozen_fees = sum (fun info -> info.total_frozen_fees)
  let total_staking_balances = sum (fun info -> info.total_staking_balance)
  let total_paid_bytes = sum (fun info -> info.total_paid_bytes)
  let total_used_bytes = sum (fun info -> info.total_used_bytes)
  let total_delegated =
    sum_int (fun info ->
        info.total_delegated)
  let total_accounts =
    sum_int (function
          { contract = { manager = None ; _ } ; _ } -> 0L
        | _ -> 1L)
  let total_contracts =
    sum_int (function
          { contract = { manager = None ; _ } ; _ } -> 1L
        | _ -> 0L)
  let total_keys =
    sum_int (function { addr ; _ } ->
        if addr.[0] = 'K' then 0L else  1L)
  let total_deleguees =
    sum_int (function { total_delegated ; _ } ->
        if total_delegated > 0L then 1L else  0L)
  let total_multi_deleguees =
    sum_int (function { total_delegated ; _ } ->
        if total_delegated > 1L then 1L else  0L)
  let total_self_delegates =
    sum_int (function
          { addr ; contract = { delegate = Some addr2  ; _ } ; _ } ->
          if addr = addr2 then 1L else 0L
        | _ -> 0L )
  let total_roll_owners =
    sum_int (function
          { addr ; contract = { delegate = Some addr2  ; _ } ;
            total_staking_balance ; _ } ->
          if addr = addr2 &&
             total_staking_balance >= 10_000_000_000L then 1L else 0L
        | _ -> 0L)
  let total_rolls =
    sum_int (function
          { addr ; contract = { delegate = Some addr2  ; _ } ;
            total_staking_balance ; _ } ->
          if addr = addr2 then
            Int64.div total_staking_balance 10_000_000_000L
          else
            0L
        | _ -> 0L)

end



module Main = struct
  open TYPES
  let test_db account str val_node val_db =
    if val_node <> val_db
    then 
      Printf.printf "[%s] Node says %s has a balance of %s while DB says its balance is %s\n%!"
        str
        account
        (Int64.to_string val_node)
        (Int64.to_string val_db)
  let add_opt a b =
    match a,b with
      Some a, Some b -> Int64.add a b
    | Some a, _ |_, Some a -> a
    | _,_ -> Int64.zero
               
  let file = "block"
  let () =
    let open Data_types in
    let content = FileString.read_file file in
    let v = JSON.decode_context_contracts ~content  in
    let url_api = {address="localhost";port=8080} in
    List.iter
      (fun (account,contract) ->
         let testdb = test_db account in 
         let saved_balance = Request.db_balance url_api account in
         let bal = match contract.balance with None -> Int64.zero | Some t -> t in
         testdb "Spendable" bal saved_balance.b_spendable;

         let frozen = 
           let rec frozens acc =
             function
             | [] -> acc
             | (_,frozen) :: tl -> 
               frozens ({TYPES.deposits = Int64.add acc.TYPES.deposits frozen.TYPES.deposits;
                         TYPES.rewards = Int64.add acc.TYPES.rewards frozen.TYPES.rewards;
                         TYPES.fees = Some (add_opt acc.TYPES.fees frozen.TYPES.fees)}) tl
           in
           let frozen_bal =
             match contract.frozen_balance with
               None -> []
             | Some l -> l in
           
           frozens ({TYPES.deposits = Int64.zero; TYPES.rewards = Int64.zero; TYPES.fees = None}) frozen_bal
         in
         testdb "Deposits" frozen.TYPES.deposits saved_balance.b_deposits;
         testdb "Rewards" frozen.TYPES.rewards saved_balance.b_rewards;
         testdb
           "Fees"
           (add_opt frozen.TYPES.fees None)
           saved_balance.b_fees;
         testdb
           "Frozen"
           (Int64.add frozen.TYPES.deposits (add_opt (Some frozen.TYPES.rewards) frozen.TYPES.fees))
           saved_balance.b_frozen
      )
      v

end
