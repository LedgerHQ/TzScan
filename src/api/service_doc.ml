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

let doc = [

    "block",
{|
Returns information about the block with hash `block_hash`. If the `operations`
parameter is `true`, the field `operations` will be filled with a list of
operations.
|}
  ;

    "version", {|
Returns the current version of the API server, when it was built and on
which commit. Useful to add in bug reports.
|}
  ;

    "head", {|
Returns information about the block at the current head of the blockchain.
|}
;
  "heads", {|
Returns information about all the blocks at the head of alternative chains.
The current head is supposed to be the one with the highest fitness.
|}
;
  "genesis", {|
Returns the genesis block, i.e. the block at level 0.
|}
;
  "blocks", {|
Returns all the blocks in the current chain. Parameters `p` and `number` are
used for pagination (`p` is the number of the page, of size `number`).
|}
;

  "block_next", {|
Returns the list of hashes of the blocks after the block with hash `block_hash`.
|}
;

  "block_prev", {|
Returns information about the block before the block with hash `block_hash`.
The request will fail on the genesis block.
|}
;

  "mini_stats", {|
Returns statistics on the last hours and days of the Network: number of
blocks, operations, volume (kXTZ), fees (XTZ cents)
|};


  ]









;;
