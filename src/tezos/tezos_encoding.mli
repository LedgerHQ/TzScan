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

open Tezos_types

val z_encoding : Z.t Json_encoding.encoding

module Micheline : sig
  val encode : script_expr_t -> string
  val decode : string -> script_expr_t
  val script_expr_encoding : script_expr_t Json_encoding.encoding
  val script_encoding : (script_expr_t * script_expr_t) Json_encoding.encoding
  val script_str_encoding : (string * string) Json_encoding.encoding
end

module Encoding : sig
  module Baking_rights : sig
    val encoding : baking_rights list Json_encoding.encoding
  end
  module Endorsing_rights : sig
    val encoding : endorsing_rights list Json_encoding.encoding
  end
  module Level : sig
    val encoding : node_level Json_encoding.encoding
  end
  module Time : sig
    val encoding : string Json_encoding.encoding
  end
  module Contracts : sig
    val encoding : string list Json_encoding.encoding
  end
  module Network : sig
    val encoding : Tezos_types.network_stats list Json_encoding.encoding
    val versions : Tezos_types.network_version Json_encoding.encoding
    val data_stats : Tezos_types.data_stats Json_encoding.encoding
  end
  module Rolls : sig
    val rolls_encoding : (int * int * Tezos_types.rolls) Json_encoding.encoding
    val deleguees : string list Json_encoding.encoding
    val delegation_balances : (string * int64) list Json_encoding.encoding
  end
  module Predecessor : sig
    val encoding : Tezos_types.block_hash Json_encoding.encoding
  end
  module Alternative_blocks : sig
    val encoding : Tezos_types.block_hash list list Json_encoding.encoding
  end
  (* module Pending_operation : sig
   *   val encoding :
   *     Tezos_types.pending_operation_unparsed Json_encoding.encoding
   * end *)

  module Block_header : sig
    val encoding : Tezos_types.block_header Json_encoding.encoding
  end

  module Block : sig
    val encoding : Tezos_types.node_block Json_encoding.encoding
    val block_metadata_encoding : Tezos_types.block_metadata Json_encoding.encoding
    val encoding_fallback : Tezos_types.block_metadata -> Tezos_types.node_block Json_encoding.encoding
    val genesis_encoding : Tezos_types.node_block Json_encoding.encoding
  end
  module Operation : sig
    val encoding : node_operation list list Json_encoding.encoding
    val pending_operation :
      Tezos_types.pending_operation Json_encoding.encoding
  end
  module Delegate : sig
    val encoding :
      delegate_details Json_encoding.encoding
    val delegated_contracts_encoding :
      (string list) Json_encoding.encoding
  end
  module Injection : sig
    val injection_encoding : (string * string * bool) Json_encoding.encoding
    val result_encoding : Tezos_types.injection_result Json_encoding.encoding
  end
  module Balance_updates : sig
    val balance_encoding :
      Tezos_types.balance_updates Json_encoding.encoding
    val encoding :
      Tezos_types.balance_updates list Json_encoding.encoding
  end
  module Voting_period_repr : sig
    val kind_encoding : Tezos_types.voting_period_kind Json_encoding.encoding
  end
  module Votes : sig
    val voting_rolls_encodings : Tezos_types.voting_rolls list Json_encoding.encoding
  end
end

val constants : Tezos_types.constants Json_encoding.encoding
val conn_metadata_encoding :
  Tezos_types.conn_metadata Json_encoding.encoding
