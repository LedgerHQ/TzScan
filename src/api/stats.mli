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

(* before starting the server, may take a while *)
val init : unit -> unit Dbr.Monad.t

(* from time to time ,should not be too long, just to keep up with the head *)
val update : ?by:int -> unit -> unit Dbr.Monad.t

type stats = {
    stats_level : int;
    stats_time0 : float;
    stats_time1 : float;
    stats_name_per_day : string array;
    stats_nblocks_per_day : int array; (* ndays entries *)
    stats_nbakers_per_day : int array; (* ndays entries *)
    stats_priorities_per_day : float array;
    stats_nops_per_day : int array;
    stats_nops_per_block_per_day : int array;
    stats_fees_per_day : int64 array;
    stats_volume_per_day : int64 array;
  }

(* no computation, returns the last result of init/update *)
val stats : unit -> stats

val clients : bool -> EzAPI.ip_info array

type period = T_MINS | T_HOURS | T_DAYS
val timing_stats : period -> Data_types.timing_stats

val mini_stats : unit -> Data_types.mini_stats
