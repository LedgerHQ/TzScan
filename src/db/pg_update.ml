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

let downgrade_1_to_0 = [
  {|DROP SCHEMA alpha CASCADE;|};
  {|DROP SCHAME tezos CASCADE;|}
]

module Constants = struct
  let last_endorsements = 100L
  let last_transactions = 1000L
end

let update_0_to_1 dbh version =
  EzPG.upgrade ~verbose:true ~dbh ~version ~downgrade:downgrade_1_to_0
        [
          (*
   -- ---
-- Globals
-- ---

-- SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
-- SET FOREIGN_KEY_CHECKS=0;

-- TODO, Schema!
           *)
          {|CREATE SCHEMA tezos|};
          {|CREATE SCHEMA alpha|};
          {|ALTER ROLE SESSION_USER SET search_path TO alpha,tezos,db,public|};

          (*
-- The agnostic part --
-- ---           --- --
           *)

          {|SET search_path TO tezos,db,public|};


          {|CREATE DOMAIN hash varchar|};
          {|CREATE DOMAIN protocol_hash hash|};
          {|CREATE DOMAIN user_hash hash|};
          {|CREATE DOMAIN block_hash hash|};
          {|CREATE DOMAIN operation_hash hash|};

          (*
-- ---
-- Table 'protocol'
-- Known protocols.
-- ---
           *)

          {|CREATE TABLE protocol (
           hash protocol_hash PRIMARY KEY,
           name VARCHAR NOT NULL
           )|};

          (*
COMMENT ON TABLE protocol IS 'Known protocols.';
COMMENT ON COLUMN protocol.hash IS 'Hash of the protocol.';
COMMENT ON COLUMN protocol.name IS 'Optional name of the protocol.';


-- ---            --- --
-- THE ALPHA PROTOCOL --
-- ---            --- --
           *)
          {|SET search_path TO alpha,tezos,db, public|};

          (*
-- ---
-- Table 'block'
-- parsed block, alpha protocol
-- ---
           *)

          {|CREATE TABLE tezos_user (
           id bigserial,
           hash user_hash PRIMARY KEY,
           contract boolean NOT NULL,
           edpk user_hash
           )|};


          {|CREATE TABLE block (
           hash block_hash PRIMARY KEY,
           predecessor block_hash NOT NULL REFERENCES block (hash) DEFERRABLE INITIALLY DEFERRED,
           fitness hash NOT NULL,
           baker user_hash NOT NULL REFERENCES tezos_user(hash),
           timestamp timestamp NOT NULL,

           protocol protocol_hash REFERENCES protocol (hash) NOT NULL,
           test_protocol protocol_hash REFERENCES protocol (hash) NOT NULL,

           network hash NOT NULL,
           test_network hash NOT NULL,
           test_network_expiration VARCHAR NOT NULL,

           level bigint NOT NULL,
           level_position bigint NOT NULL,
           priority float NOT NULL, -- float from upstream, don't touch it

           cycle bigint NOT NULL,
           cycle_position bigint NOT NULL,

           voting_period bigint NOT NULL,
           voting_period_position bigint NOT NULL,

           commited_nonce_hash hash NOT NULL,
           pow_nonce hash NOT NULL,

           distance_level bigint NOT NULL,
           operation_count bigint NOT NULL,
           validation_pass bigint NOT NULL,
           proto bigint NOT NULL,
           data VARCHAR NOT NULL,
           signature VARCHAR NOT NULL
           )|};

          (*
  volume float NOT NULL,
  fees float NOT NULL



COMMENT ON COLUMN block.hash IS 'Hash of the block.';
COMMENT ON COLUMN block.level IS 'Level of the block.';
COMMENT ON COLUMN block.priority IS 'Miner id who signed the block';
COMMENT ON COLUMN block.cycle IS 'current cycle id';
COMMENT ON COLUMN block.cycle_position IS 'current position in the cycle';
COMMENT ON COLUMN block.voting_period IS 'current voting period';
COMMENT ON COLUMN block.voting_period_position IS 'current position in the voting period';
COMMENT ON COLUMN block.commited_nonce_hash IS 'Hash of the random number committed to by the baker';
COMMENT ON COLUMN block.pow_nonce IS 'PoW nonce of the block';
COMMENT ON COLUMN block.distance_level IS 'Distance to the main chain (0 if in main chain)';
           *)

          (*
-- Indexing on the table
           *)

          {|CREATE INDEX block_time ON block (timestamp)|};
          {|CREATE INDEX block_predecessor ON block (predecessor)|};

          (*
-- ---
-- View 'block_main'
-- The alpha block chain, restricted to the current main chain
-- ---
           *)

          {|CREATE VIEW block_main AS
           SELECT b.hash, predecessor, fitness, baker, timestamp,
              protocol, test_protocol,
       network, test_network, test_network_expiration,
       level, level_position, priority,
       cycle, cycle_position,
       voting_period, voting_period_position,
       commited_nonce_hash, pow_nonce,
           validation_pass, proto, data, signature,
       operation_count,
       p.name AS protocol_name, pt.name AS test_protocol_name
           FROM block AS b
           LEFT JOIN protocol AS p ON b.protocol = p.hash
           LEFT JOIN protocol AS pt ON b.test_protocol = pt.hash
           WHERE distance_level = 0|};

          (*
-- Indexing on that view
-- some of those are probably useless, and clearly redundant with those above
           *)

          {|CREATE UNIQUE INDEX block_main_level ON block (level)
           WHERE distance_level = 0|};

          (*
-- This should work when the bug on block level 1 is fixed * issue on gitlab *
--          {|CREATE UNIQUE INDEX block_main_cycle ON block (cycle,cycle_position)
--   WHERE distance_level = 0 AND protocol = 'ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK'|};
--          {|CREATE UNIQUE INDEX block_main_protocol ON block (protocol,level_position)
--   WHERE distance_level = 0 AND protocol = 'ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK'|};
           *)

          {|CREATE INDEX block_main_baker ON block (baker)
           WHERE distance_level = 0|};

          (*
-- ---
-- Table 'operation'
-- An alpha operation, as seen by the network shell.
-- ---
           *)

          {|CREATE TYPE operation_type AS ENUM (
           'seed_nonce_revelation',
           'faucet',
           'transaction',
           'origination',
           'delegation',
           'endorsement',
           'proposal',
           'ballot',
           'activate',
           'activate_testnet'
           )|};

          {|CREATE TABLE operation (
           hash operation_hash PRIMARY KEY,
           op_type VARCHAR NOT NULL,
           timestamp timestamp NOT NULL
           )|};

          {|CREATE TABLE block_operation (
           block_hash block_hash NOT NULL REFERENCES block (hash),
           operation_hash operation_hash NOT NULL REFERENCES operation(hash)
           )|};

          {|CREATE INDEX block_operation_operation ON block_operation (operation_hash)|};
          {|CREATE INDEX block_operation_block ON block_operation (block_hash)|};

          {|CREATE VIEW operation_main AS
           SELECT o.hash, o.op_type, bo.block_hash, b.timestamp
           FROM operation AS o
           INNER JOIN block_operation AS bo ON o.hash = bo.operation_hash
           INNER JOIN block_main as b ON bo.block_hash = b.hash|};

          {|CREATE VIEW operation_pending AS
           SELECT operation.hash, operation.op_type
           FROM operation LEFT JOIN block_operation
           ON operation.hash = block_operation.operation_hash
           WHERE block_operation.operation_hash IS NULL|};

          {|CREATE TABLE seed_nonce_revelation (
           hash operation_hash NOT NULL REFERENCES operation(hash),
           level bigint NOT NULL,
           nonce VARCHAR NOT NULL,
           PRIMARY KEY (hash, level)
           )|};

          {|CREATE INDEX seed_nonce_revelation_hash ON seed_nonce_revelation (hash)|};

          {|CREATE TABLE faucet (
           hash operation_hash NOT NULL REFERENCES operation(hash),
           pkh user_hash NOT NULL REFERENCES tezos_user(hash),
           nonce bytea NOT NULL,
           PRIMARY KEY (hash, nonce)
           )|};

          {|CREATE INDEX faucet_hash ON faucet (hash)|};


          {|CREATE TABLE transaction (
           hash operation_hash PRIMARY KEY REFERENCES operation(hash),
           source user_hash NOT NULL REFERENCES tezos_user(hash),
           destination user_hash NOT NULL REFERENCES tezos_user(hash),
           fee bigint NOT NULL,
           counter bigint NOT NULL,
           amount bigint NOT NULL,
           parameters bytea
           )|};

          {|CREATE VIEW operation_main_transaction AS
           SELECT * FROM operation_main INNER JOIN transaction USING(hash)|};

          (*
-- COMMENT ON TABLE alpha_transaction IS 'a transaction in the alpha protocol';
-- COMMENT ON COLUMN alpha_transaction.hash IS 'hash of the related operation';
-- COMMENT ON COLUMN alpha_transaction.amount IS 'amount of the transaction';
-- COMMENT ON COLUMN alpha_transaction.destination IS 'destination contract';
-- COMMENT ON COLUMN alpha_transaction.parameters IS 'input data passed if any';
           *)

          {|CREATE TABLE origination (
           hash operation_hash PRIMARY KEY REFERENCES operation(hash),
           source user_hash NOT NULL REFERENCES tezos_user(hash),
           tz1 user_hash NOT NULL REFERENCES tezos_user(hash),
           fee bigint NOT NULL,
           counter bigint NOT NULL,
           manager user_hash NOT NULL REFERENCES tezos_user(hash),
           delegate user_hash REFERENCES tezos_user(hash),
           script_code bytea,
           script_storage_type varchar,
           spendable boolean NOT NULL,
           delegatable boolean NOT NULL,
           balance bigint NOT NULL
           )|};

          (*
-- COMMENT ON TABLE alpha_origination IS 'origination of an account / contract in the seed protocol';
-- COMMENT ON COLUMN alpha_origination.hash IS 'hash of the related operation';
           *)

          {|CREATE TABLE delegation (
           hash operation_hash PRIMARY KEY REFERENCES operation(hash),
           source hash NOT NULL,
           pubkey hash,
           fee bigint NOT NULL,
           counter bigint NOT NULL,
           delegate hash
           )|};

          {|CREATE VIEW operation_main_delegation AS
           SELECT * FROM operation_main INNER JOIN delegation USING(hash)|};


          (*
-- COMMENT ON COLUMN alpha_delegation.hash IS 'hash of the related operations';
-- COMMENT ON COLUMN alpha_delegation.delegate IS 'delegate pubkey hash';
           *)

          {|CREATE TABLE endorsement (
           hash operation_hash PRIMARY KEY REFERENCES operation(hash),
           source hash NOT NULL,
           edpk hash,
           block_hash hash NOT NULL,
           slot bigint[] NOT NULL
           )|};

          {|CREATE VIEW operation_main_endorsement AS
           SELECT hash, op_type, operation_main.block_hash, timestamp,
  	   source, endorsement.block_hash AS e_block_hash, slot
           FROM operation_main INNER JOIN endorsement USING(hash)|};

          {|CREATE TABLE proposal (
           hash operation_hash PRIMARY KEY REFERENCES operation(hash),
           source hash NOT NULL,
           voting_period int NOT NULL,
           proposals text[] NOT NULL
           )|};

          {|CREATE VIEW operation_main_proposal AS
           SELECT * FROM operation_main INNER JOIN proposal USING(hash)|};

          {|CREATE TYPE ballot_vote AS ENUM ('Yay', 'Nay', 'Pass')|};

          {|CREATE TABLE ballot (
           hash operation_hash PRIMARY KEY REFERENCES operation(hash),
           source hash NOT NULL,
           voting_period int NOT NULL,
           proposal hash NOT NULL,
           ballot text NOT NULL
           )|};

          {|CREATE VIEW operation_main_ballot AS
           SELECT * FROM operation_main INNER JOIN ballot USING(hash)|};


          (* -- Market Cap *)
          {|CREATE TABLE marketcap (
           currency_id VARCHAR NOT NULL,
           name VARCHAR NOT NULL,
           symbol VARCHAR NOT NULL,
           rank VARCHAR NOT NULL,
           price_usd VARCHAR NOT NULL,
           price_btc VARCHAR NOT NULL,
           volume_usd_24 VARCHAR,
           market_cap_usd VARCHAR,
           available_supply VARCHAR,
           total_supply VARCHAR,
           max_supply VARCHAR,
           percent_change_1 VARCHAR,
           percent_change_24 VARCHAR,
           percent_change_7 VARCHAR,
           last_updated bigint PRIMARY KEY
           )|};

          {|CREATE TABLE peers (
           id bigserial,
           peer_id VARCHAR PRIMARY KEY,
           country_name VARCHAR NOT NULL,
           country_code VARCHAR NOT NULL,
           point_id VARCHAR,
           trusted boolean NOT NULL,
           score float NOT NULL,
           state VARCHAR NOT NULL,
           total_sent bigint NOT NULL,
           total_received bigint NOT NULL,
           current_inflow bigint NOT NULL,
           current_outflow bigint NOT NULL,
           last_failed_connection_point VARCHAR,
           last_failed_connection_date VARCHAR,
           last_rejected_connection_point VARCHAR,
           last_rejected_connection_date VARCHAR,
           last_established_connection_point VARCHAR,
           last_established_connection_date VARCHAR,
           last_disconnection_point VARCHAR,
           last_disconnection_date VARCHAR,
           last_seen_point VARCHAR,
           last_seen_date VARCHAR,
           last_miss_point VARCHAR,
           last_miss_date VARCHAR
           )|};

          (*
-- COMMENT ON TABLE alpha_account IS 'an account in the seed protocol';
-- COMMENT ON COLUMN alpha_account.hash IS 'hash handle of the account';
-- COMMENT ON COLUMN alpha_account.manager IS 'manager of the contract';
-- COMMENT ON COLUMN alpha_account.delegate IS 'delegate of the contract';
-- COMMENT ON COLUMN alpha_account.spendable IS 'whether the funds are spendable';
-- COMMENT ON COLUMN alpha_account.delegatable IS 'whether the delegate may be changed';
-- COMMENT ON COLUMN alpha_account.balance IS 'balance in tez';
           *)

          {|CREATE VIEW block_main_with_operations AS
           SELECT b.hash, predecessor, fitness, baker, b.timestamp,
              protocol, test_protocol,
       network, test_network, test_network_expiration,
       level, level_position, priority,
       cycle, cycle_position,
       voting_period, voting_period_position,
       commited_nonce_hash, pow_nonce,
           validation_pass, proto, data, signature,
       protocol_name, test_protocol_name,
       operation_count,
       o.hash as ohash, o.op_type
       FROM block_main AS b
       LEFT JOIN operation_main AS o ON b.hash = o.block_hash|};

        ];
  ()

let downgrade_2_to_1 = []

let update_1_to_2 _dbh _version = ()

let downgrade_3_to_2 = [
  {| ALTER TABLE block DROP COLUMN volume;|};
  {| ALTER TABLE block DROP COLUMN fees;|}
]

let update_2_to_3 dbh version =
  EzPG.upgrade ~version ~dbh ~downgrade:downgrade_3_to_2 [
    {| ALTER TABLE block ADD COLUMN volume float NOT NULL DEFAULT 0. |};
    {| ALTER TABLE block ADD COLUMN fees float NOT NULL DEFAULT 0. |};
    {| CREATE TEMPORARY TABLE vol_block (
       hash block_hash PRIMARY KEY, volume float NOT NULL) |};
    {| INSERT INTO vol_block (
       SELECT bo.block_hash, SUM(COALESCE(t.amount, 0.) + COALESCE(ori.balance, 0.)) AS vol FROM
       block_operation AS bo LEFT JOIN transaction AS t ON t.hash = bo.operation_hash LEFT JOIN origination AS ori ON ori.hash =
       bo.operation_hash GROUP BY bo.block_hash) |};
    {| UPDATE block AS bl SET volume = vb.volume FROM vol_block AS vb WHERE vb.hash = bl.hash
    |};
    {| CREATE TEMPORARY TABLE fee_block (hash block_hash PRIMARY KEY, fees float NOT NULL)
    |};
    {| INSERT INTO fee_block (SELECT bo.block_hash, SUM(COALESCE(t.fee, 0) + COALESCE(ori.fee, 0) + COALESCE(d.fee, 0)) AS vol FROM
       block_operation AS bo LEFT JOIN transaction AS t ON t.hash = bo.operation_hash LEFT JOIN origination AS ori ON ori.hash =
       bo.operation_hash LEFT JOIN delegation AS d ON d.hash = bo.operation_hash GROUP BY bo.block_hash)
    |};
    {| UPDATE block AS bl SET fees = fb.fees FROM fee_block AS fb WHERE fb.hash = bl.hash
    |}
  ]

let downgrade_4_to_3 = []

let update_3_to_4 _dbh _version = ()

let downgrade_5_to_4 = [
  {|DROP TABLE switch|}
]

let update_4_to_5 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_5_to_4 [
    {|
        CREATE TABLE switch (
          cycle bigint PRIMARY KEY,
          switch_count bigint NOT NULL,
          longest_alt_chain bigint NOT NULL)
        |} ;
    {|
           INSERT INTO switch (cycle, switch_count, longest_alt_chain) (SELECT distinct cycle, 0, 0 FROM block)
          |} ;
  ]

let downgrade_6_to_5 = [
  {|DROP TABLE pending_priorites;|};
  {|DROP DOMAIN user_hashes;|}
]

let update_5_to_6 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_6_to_5 [
    {| CREATE DOMAIN user_hashes varchar[] |};
    {|
        CREATE TABLE pending_priorities (
          level bigint PRIMARY KEY,
          bakers user_hashes NOT NULL)
  |};
  ]

let list_update_6_to_7 = [
  "CREATE INDEX endorsement_block ON endorsement (block_hash)";
  (* Views to see the db as json elements *)

  "CREATE SCHEMA json";

  (* Protocols *)
  "CREATE VIEW json.protocol AS \
   SELECT protocol.hash, to_jsonb(protocol) as protocol FROM tezos.protocol";

  (* Blocks *)
  (* Json descriptors *)
  "CREATE TYPE json.operation_summary_json AS ( \
   hash operation_hash, branch VARCHAR, data VARCHAR \
   )";

  "CREATE TYPE json.block_json AS ( \
   hash block_hash, \
   predecessor_hash block_hash, \
   fitness hash, \
   timestamp VARCHAR, \
   \
   validation_pass bigint, \
   operations jsonb, \
   \
   protocol jsonb, \
   test_protocol jsonb, \
   network hash, \
   test_network hash, \
   test_network_expiration VARCHAR, \
   \
   baker user_hash, \
   nb_operations bigint, \
   priority float, \
   level bigint, \
   distance_level bigint, \
   \
   commited_nonce_hash hash, \
   pow_nonce hash, \
   proto bigint, \
   \
   data VARCHAR, \
   signature VARCHAR, \
   volume float, \
   fees float \
   )";


  (* Blocks without operations *)
  "CREATE VIEW json.block_noop AS \
   SELECT b.hash, b.level, b.distance_level, b.timestamp, \
   to_jsonb(row( \
   b.hash,b.predecessor,b.fitness,to_char(b.timestamp,'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'), \
   b.validation_pass, to_jsonb(ARRAY[to_jsonb(ARRAY[]::json.operation_summary_json[])]), \
   p.protocol, pt.protocol, b.network, b.test_network, b.test_network_expiration, \
   b.baker, b.operation_count, b.priority, b.level, b.distance_level, \
   b.commited_nonce_hash, b.pow_nonce, b.proto, \
   b.data, b.signature, b.volume, b.fees \
   )::json.block_json) \
   AS block \
   FROM alpha.block AS b \
   INNER JOIN json.protocol AS p ON b.protocol = p.hash \
   INNER JOIN json.protocol AS pt ON b.test_protocol = pt.hash";

  "CREATE VIEW json.block_noop_main AS \
   SELECT level,block FROM json.block_noop WHERE distance_level = 0";
  "CREATE VIEW json.block_noop_heads AS \
   SELECT level,timestamp,block FROM json.block_noop WHERE distance_level != 0";

  (* Blocks with operations *)
  "CREATE VIEW json.block_operations_summary AS \
   SELECT bo.block_hash as hash, \
   to_jsonb(ARRAY[array_agg(row(bo.operation_hash,'','')::json.operation_summary_json \
   ORDER BY o.timestamp DESC)]) as operations \
   FROM alpha.block_operation AS bo \
   INNER JOIN alpha.operation AS o ON bo.operation_hash = o.hash \
   GROUP BY bo.block_hash";

  "CREATE VIEW json.block_op AS \
   SELECT b.hash, b.level, b.distance_level, b.timestamp, \
   to_jsonb(row( \
   b.hash,b.predecessor,b.fitness,to_char(b.timestamp,'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"'), \
   b.validation_pass, COALESCE(bo.operations, \
   to_jsonb(ARRAY[ARRAY[]]::json.operation_summary_json[][])), \
   p.protocol, pt.protocol, b.network, b.test_network, b.test_network_expiration, \
   b.baker, b.operation_count, b.priority, b.level, b.distance_level, \
   b.commited_nonce_hash, b.pow_nonce, b.proto, \
   b.data, b.signature, b.volume, b.fees \
   )::json.block_json) \
   AS block \
   FROM alpha.block AS b \
   INNER JOIN json.protocol AS p ON b.protocol = p.hash \
   INNER JOIN json.protocol AS pt ON b.test_protocol = pt.hash \
   LEFT JOIN json.block_operations_summary AS bo ON b.hash = bo.hash";

  "CREATE VIEW json.block_op_main AS \
   SELECT level,block FROM json.block_op WHERE distance_level = 0 ORDER BY level DESC";
  "CREATE VIEW json.block_op_heads AS \
   SELECT timestamp,block FROM json.block_op WHERE distance_level != 0 ORDER BY timestamp DESC";

  (* Operations *)
  (* Transaction *)
  "CREATE TYPE json.transaction_json AS ( \
   source user_hash, \
   destination user_hash, \
   fee bigint, \
   counter bigint, \
   amount bigint, \
   parameters bytea \
   )";
  "CREATE VIEW json.transaction AS \
   SELECT hash, to_jsonb(row( \
   source, destination, fee, counter, amount, parameters \
   )::json.transaction_json) as type \
   FROM alpha.transaction";

  (* Endorsement *)
  "CREATE TYPE json.endorsement_json AS ( \
   src user_hash, \
   block block_hash, \
   slot bigint[] \
   )";
  "CREATE VIEW json.endorsement AS \
   SELECT hash, block_hash, slot, to_jsonb(row( \
   source, block_hash, slot \
   )::json.endorsement_json) AS type \
   FROM alpha.endorsement";

  (* Delegation *)
  "CREATE TYPE json.delegation_json AS ( \
   src user_hash, \
   pubkey hash, \
   fee bigint, \
   counter bigint, \
   delegate user_hash \
   )";
  "CREATE VIEW json.delegation AS \
   SELECT hash, source, delegate, to_jsonb(row( \
   source,pubkey,fee,counter,delegate)::json.delegation_json) AS type \
   FROM alpha.delegation";

  (* Origination *)
  "CREATE TYPE json.origination_json AS ( \
   source user_hash, \
   destination user_hash, \
   fee bigint, \
   counter bigint, \
   amount bigint, \
   parameters bytea \
   )";
  "CREATE VIEW json.origination AS \
   SELECT hash, to_jsonb(row( \
   source, tz1, fee, counter, balance, script_code \
   )::json.origination_json) AS type \
   FROM alpha.origination";

  (* Nonce *)
  "CREATE TYPE json.nonce_json AS ( \
   level bigint, \
   nonce VARCHAR \
   )";
  "CREATE VIEW json.nonce AS \
   SELECT hash, to_jsonb(array_agg(row( \
   level,nonce)::json.nonce_json ORDER BY level DESC)) AS type \
   FROM alpha.seed_nonce_revelation \
   GROUP BY hash";

  (* Faucet *)
  "CREATE TYPE json.faucet_json AS ( \
   id user_hash, \
   nonce VARCHAR \
   )";
  "CREATE VIEW json.faucet AS \
   SELECT hash, to_jsonb(array_agg(row(pkh,encode(nonce,'escape'))::json.faucet_json)) AS type \
   FROM alpha.faucet \
   GROUP BY hash";

  (* Proposal TODO *)
  (* Ballot TODO *)


  (* Operations *)
  "CREATE TYPE json.operation_json AS ( \
   hash operation_hash, \
   block_hash block_hash, \
   network_hash hash, \
   type jsonb \
   )";

  (* All operations *)
  "CREATE VIEW json.operation_block AS \
   SELECT o.*, \
   b.hash AS block_hash, \
   b.network AS network_hash, \
   b.distance_level, b.level \
   FROM alpha.operation as o \
   INNER JOIN block_operation as bo ON o.hash = bo.operation_hash \
   INNER JOIN block as b ON (bo.block_hash = b.hash)";

  "CREATE VIEW json.operation_block_pending AS \
   SELECT o.*, \
   'prevalidation'::block_hash AS block_hash, \
   'prevalidation'::hash AS network_hash \
   FROM alpha.operation as o \
   LEFT JOIN block_operation as bo ON o.hash = bo.operation_hash \
   LEFT JOIN block as b ON (bo.block_hash = b.hash) \
   WHERE b.hash IS NULL";

  (* Operation type to operation *)
  "CREATE FUNCTION json.make_operation_in_block (ohash operation_hash, jtype jsonb) \
   RETURNS TABLE \
   ( hash operation_hash, op_type VARCHAR, \"timestamp\" timestamp, \
   block_hash block_hash, network_hash hash, \
   distance_level bigint, level bigint, \
   operation jsonb) \
   AS $$ \
   SELECT \
   o.hash, o.op_type, o.timestamp, \
   b.hash AS block_hash, b.network AS network_hash, b.distance_level, b.level, \
   to_jsonb(row( \
   o.hash, b.hash, b.network, jtype \
   )::json.operation_json) AS operation \
   FROM alpha.operation AS o \
   INNER JOIN alpha.block_operation AS bo ON bo.operation_hash = o.hash \
   INNER JOIN alpha.block AS b ON bo.block_hash = b.hash \
   WHERE o.hash = ohash \
   $$ \
   LANGUAGE SQL \
   STABLE \
   STRICT";

  "CREATE FUNCTION json.make_operation_pending (ohash operation_hash, jtype jsonb) \
   RETURNS TABLE \
   ( hash operation_hash, op_type VARCHAR, \"timestamp\" timestamp, \
   operation jsonb) \
   AS $$ \
   SELECT \
   o.hash, o.op_type, o.timestamp, \
   to_jsonb(row( \
   o.hash,'prevalidation','prevalidation',jtype \
   )::json.operation_json) AS operation \
   FROM alpha.operation AS o \
   LEFT JOIN alpha.block_operation AS bo ON bo.operation_hash = o.hash \
   WHERE o.hash = ohash AND bo.operation_hash IS NULL \
   $$ \
   LANGUAGE SQL \
   STABLE \
   STRICT";



  "CREATE VIEW json.operation AS \
   SELECT o.hash, o.op_type, o.timestamp, o.block_hash,o.distance_level,o.level, \
   to_jsonb(row(o.hash,o.block_hash,o.network_hash, \
   COALESCE(transaction.type,endorsement.type,delegation.type,origination.type,nonce.type,faucet.type) \
   )::json.operation_json) AS operation  \
   FROM json.operation_block as o \
   LEFT JOIN json.transaction USING (hash) \
   LEFT JOIN json.endorsement USING (hash) \
   LEFT JOIN json.delegation USING (hash) \
   LEFT JOIN json.origination USING (hash) \
   LEFT JOIN json.nonce USING (hash) \
   LEFT JOIN json.faucet USING (hash)";

  "CREATE VIEW json.operation_pending AS \
   SELECT o.hash, o.op_type, o.timestamp, o.block_hash, \
   NULL::bigint AS distance_level, NULL::bigint AS level, \
   to_jsonb(row(o.hash,o.block_hash,o.network_hash, \
   COALESCE(transaction.type,endorsement.type,delegation.type,origination.type,nonce.type,faucet.type) \
   )::json.operation_json) AS operation  \
   FROM json.operation_block_pending as o \
   LEFT JOIN json.transaction USING (hash) \
   LEFT JOIN json.endorsement USING (hash) \
   LEFT JOIN json.delegation USING (hash) \
   LEFT JOIN json.origination USING (hash) \
   LEFT JOIN json.nonce USING (hash) \
   LEFT JOIN json.faucet USING (hash)";

  "CREATE VIEW json.operation_all AS \
   (SELECT * FROM json.operation_pending) UNION ALL (SELECT * FROM json.operation)";


  (* Nonces revelations for a cycle *)
  (* Nonces in a single operation *)
  "CREATE VIEW json.nonces_single_op AS \
   SELECT hash AS operation_hash, array_agg(level ORDER BY level ASC) AS levels \
   FROM alpha.seed_nonce_revelation \
   GROUP BY hash";
  (* Nonces declared in a cycle *)
  "CREATE VIEW json.cycle_nonces AS \
   SELECT b.cycle, \
   jsonb_build_object('nonces',to_jsonb(array_agg(nonces_single_op))) AS nonces \
   FROM alpha.block as b \
   INNER JOIN alpha.block_operation as bo ON b.hash = bo.block_hash \
   INNER JOIN alpha.operation as o ON bo.operation_hash = o.hash \
   INNER JOIN json.nonces_single_op ON o.hash = nonces_single_op.operation_hash \
   WHERE distance_level = 0 \
   GROUP BY b.cycle";

  (* Successor of a block *)
  (* I'd like to say, I strongly disagree with this *)
  "CREATE VIEW json.block_successor AS \
   SELECT predecessor AS hash, to_jsonb(ARRAY[hash::VARCHAR]) AS successor \
   FROM alpha.block \
   WHERE distance_level = 0";

  (* Block level *)
  "CREATE TYPE json.level_json AS ( \
   level bigint, \
   level_position bigint, \
   cycle bigint, \
   cycle_position bigint, \
   voting_period bigint, \
   voting_period_position bigint \
   )";
  "CREATE VIEW json.block_level AS \
   SELECT hash, to_jsonb(row( \
   level, level_position,cycle,cycle_position,voting_period,voting_period_position \
   )::json.level_json) as level \
   FROM alpha.block";

  (* Operations from account *)
  "CREATE VIEW json.faucet_account AS \
   SELECT hash, pkh AS account FROM alpha.faucet";
  "CREATE VIEW json.transaction_account AS \
   (SELECT hash,source AS account FROM alpha.transaction) UNION DISTINCT \
   (SELECT hash,destination AS account FROM alpha.transaction)";
  "CREATE VIEW json.origination_account AS \
   SELECT hash, source AS account FROM alpha.origination";
  "CREATE VIEW json.delegation_account AS \
   (SELECT hash, source AS account FROM alpha.delegation) UNION DISTINCT \
   (SELECT hash, delegate AS account FROM alpha.delegation)";
  "CREATE VIEW json.endorsement_account AS \
   SELECT hash, source AS account FROM alpha.endorsement";
  "CREATE VIEW json.proposal_account AS \
   SELECT hash, source AS account FROM alpha.proposal";
  "CREATE VIEW json.ballot_account AS \
   SELECT hash, source AS account FROM alpha.ballot";

  (* Bakings *)
  "CREATE VIEW json.bakings_raw AS \
   SELECT hash AS block_hash, baker AS baker_hash, distance_level, level, priority \
   FROM alpha.block";

  "CREATE VIEW json.baking AS \
   SELECT baker_hash AS baker, level, to_jsonb(bakings_raw) AS baking FROM json.bakings_raw";

  (* Endorsements *)
  "CREATE VIEW json.block_endorsement AS \
   SELECT b.hash, b.level, b.distance_level, e.slot, json.operation \
   FROM alpha.block AS b \
   INNER JOIN json.endorsement AS e ON e.block_hash = b.hash, \
   LATERAL json.make_operation_in_block (e.hash,e.type) AS json";

  (* Mini-stats *)
  "CREATE TYPE json.mini_stat_single_json AS ( \
   period VARCHAR, \
   nhours bigint, \
   nblocks bigint, \
   nops bigint, \
   fees float, \
   volume float \
   )";
  "CREATE TYPE json.mini_stat_json AS ( \
   period VARCHAR[], \
   nhours bigint[], \
   nblocks bigint[], \
   nops bigint[], \
   fees float[], \
   volume float[] \
   )";

  "CREATE VIEW json.mini_stat_raw AS \
   SELECT DISTINCT ON (nsecs) \
   CAST((EXTRACT (EPOCH FROM age(now(),timestamp AT TIME ZONE 'GMT'))) AS bigint) AS nsecs, \
   count(hash) OVER timing AS nblocks, \
   sum(operation_count) OVER timing AS nops, \
   sum(fees) OVER timing AS fees, \
   sum(volume) OVER timing AS volume \
   FROM alpha.block \
   WHERE distance_level = 0 \
   WINDOW timing AS (ORDER BY level DESC) \
   ORDER BY nsecs ASC,timestamp ASC";

  "CREATE FUNCTION json.get_mini_stat(VARCHAR,bigint) RETURNS SETOF json.mini_stat_single_json AS $$ \
   SELECT $1 AS period, $2 AS nhours, \
   ms.nblocks,ms.nops::bigint, \
   ms.fees, floor(ms.volume/100000) AS volume \
   FROM \
   ((SELECT 0 AS nsecs, 0 AS nblocks, 0 AS nops, 0 AS fees, 0 AS volume) \
   UNION ALL \
   (SELECT * FROM json.mini_stat_raw)) AS ms \
   WHERE ms.nsecs <= $2 * 3600 \
   ORDER BY nhours DESC, nblocks DESC LIMIT 1 \
   $$ LANGUAGE SQL";

  "CREATE VIEW json.mini_stat_with_hours AS \
   SELECT ms.* \
   FROM unnest(ARRAY['1 hour','6 hours','12 hours', '1 day', '7 days', '1 month'], \
   ARRAY[1,6,12,24,168,720]) AS hours (period,nhours), \
   LATERAL json.get_mini_stat(hours.period,hours.nhours) AS ms \
   ORDER BY hours.nhours ASC";

  "CREATE VIEW json.mini_stat AS \
   SELECT to_jsonb(row(array_agg(period),array_agg(nhours), \
   array_agg(nblocks),array_agg(nops), \
   array_agg(fees),array_agg(volume))::json.mini_stat_json) AS stat \
   FROM json.mini_stat_with_hours";

  (* Accounts *)
  "CREATE TYPE json.account_json AS ( \
   hash user_hash, \
   manager user_hash, \
   spendable bool, \
   delegatable bool \
   )";

  "CREATE VIEW json.account AS \
   SELECT \
   u.id, u.hash, u.contract, to_jsonb(row( \
   u.hash, COALESCE(ori.manager,u.hash), \
   COALESCE(ori.spendable,false),COALESCE(ori.delegatable,false) \
   )::json.account_json) AS account, \
   (ori.manager IS NOT NULL) AS originated \
   FROM alpha.tezos_user AS u \
   LEFT OUTER JOIN alpha.origination AS ori ON u.hash = ori.tz1" ]

let downgrade_7_to_6 = [
  {|DROP SCHEMA json CASCADE:|};
  {|DROP INDEX endorsement_block;|}
]

let update_6_to_7 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_7_to_6 list_update_6_to_7

let downgrade_8_to_7 = [
  {|DROP VIEW json.level_rights|};
  {|DROP TYPE json.level_rights_json;|};
  {|DROP TABLE level_rights;|};
  {|CREATE TABLE pending_priorities (
    level bigint PRIMARY KEY,
    bakers user_hashes NOT NULL);|}
]

let update_7_to_8 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_8_to_7 [
    {| DROP TABLE pending_priorities |};
    {|
       CREATE TABLE level_rights (
         level bigint PRIMARY KEY,
         bakers user_hashes NOT NULL,
         endorsers user_hashes NOT NULL
       )
    |};
    {|
       CREATE TYPE json.level_rights_json AS (
         level bigint,
         bakers user_hashes,
         endorsers user_hashes)
    |};
    {|
        CREATE VIEW json.level_rights AS
          SELECT level, bakers, endorsers, to_jsonb(row(
          level, bakers, endorsers)::json.level_rights_json)
          FROM alpha.level_rights
    |}
  ]

let downgrade_9_to_8 = [
  {|DROP TABLE header;|};
  {|DROP TABLE double_baking_evidence;|}
]

let update_8_to_9 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_9_to_8 [
    {| CREATE TABLE header (
       id bigserial PRIMARY KEY,
       level bigint NOT NULL,
       proto bigint NOT NULL,
       predecessor block_hash NOT NULL REFERENCES block (hash) DEFERRABLE INITIALLY DEFERRED,
       timestamp timestamp NOT NULL,
       validation_pass bigint NOT NULL,
       operations_hash hash NOT NULL,
       fitness hash NOT NULL,
       context VARCHAR NOT NULL,
       priority float NOT NULL, -- float from upstream, don't touch it
       commited_nonce_hash hash NOT NULL,
       pow_nonce hash NOT NULL,
       signature VARCHAR NOT NULL)
    |};

    {| CREATE TABLE double_baking_evidence (
       hash operation_hash PRIMARY KEY REFERENCES operation(hash),
       header1 int REFERENCES header(id),
       header2 int REFERENCES header(id))
    |};
  ]

let downgrade_10_to_9 = [
  {|DROP TABLE double_endorsement_evidence;|}
]

let update_9_to_10 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_10_to_9 [
    {| CREATE TABLE double_endorsement_evidence (
       hash operation_hash PRIMARY KEY REFERENCES operation(hash),
       block_hash1 block_hash NOT NULL,
       level1 bigint NOT NULL,
       slots1 int[] NOT NULL,
       block_hash2 block_hash NOT NULL,
       level2 bigint NOT NULL,
       slots2 int[] NOT NULL)
    |};
  ]

let downgrade_11_to_10 = [
  {|DROP TABLE reveal;|};
  {|DROP VIEW operation_main_reveal;|};
  {|DROP TYPE json.reveal_json;|};
  {|DROP VIEW json.reveal|};
  {|DROP VIEW json.reveal_account|}
]

let update_10_to_11 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_11_to_10 [
    {|CREATE TABLE reveal (
          hash operation_hash PRIMARY KEY REFERENCES operation(hash),
          source hash NOT NULL,
          fee bigint NOT NULL,
          counter bigint NOT NULL,
          pubkey hash
          )
    |};

    {|CREATE VIEW operation_main_reveal AS
      SELECT * FROM operation_main INNER JOIN reveal USING(hash)
    |};
    {|CREATE TYPE json.reveal_json AS (
       src user_hash,
       fee bigint,
       counter bigint,
       pubkey hash
      )
    |};
    {|CREATE VIEW json.reveal AS
       SELECT hash, source, to_jsonb(row(
       source,fee,counter,pubkey)::json.reveal_json) AS type
       FROM alpha.reveal
    |};
    {| CREATE VIEW json.reveal_account AS
       (SELECT hash, source AS account FROM alpha.reveal) UNION DISTINCT
       (SELECT hash, pubkey AS account FROM alpha.reveal);
    |};

  ]

let downgrade_12_to_11 = [
  {|DROP TABLE snapshot_rolls;|};
  {|DROP TABLE snapshot_owner;|};
  {|DROP TABLE snapshot_deleguees_balance;|};
  {|DROP TABLE snapshot_deleguees;|}
]

let update_11_to_12 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_12_to_11 [
    {|CREATE TABLE snapshot_rolls (
      id bigserial NOT NULL UNIQUE,
      cycle bigint PRIMARY KEY,
      index int NOT NULL,
      rolls_count int NOT NULL
      );
    |};

    {|CREATE UNIQUE INDEX snapshot_rolls_id_index ON snapshot_rolls (id) ;|};
    {|CREATE UNIQUE INDEX snapshot_rolls_cycle_index ON snapshot_rolls (cycle) ;|};

    {|CREATE TABLE snapshot_owner (
      id bigint REFERENCES snapshot_rolls(id) ON DELETE CASCADE NOT NULL,
      tz1 user_hash NOT NULL,
      count int NOT NULL,
      change bigint NOT NULL
      );|};

    {|CREATE INDEX snapshot_owner_id_index ON snapshot_owner (id) ;|};
    {|CREATE INDEX snapshot_owner_tz1_index ON snapshot_owner (tz1) ;|};

    {|CREATE TABLE snapshot_deleguees_balance (
      id bigint REFERENCES snapshot_rolls(id) ON DELETE CASCADE NOT NULL,
      tz1 user_hash NOT NULL,
      balance bigint NOT NULL
      );|};

    {|CREATE INDEX snapshot_deleguees_balance_id_index ON snapshot_deleguees_balance (id) ;|};
    {|CREATE INDEX snapshot_deleguees_balance_tz1_index ON snapshot_deleguees_balance (tz1) ;|};

    {|CREATE TABLE snapshot_deleguees (
      id bigint REFERENCES snapshot_rolls(id) ON DELETE CASCADE NOT NULL,
      tz1 user_hash NOT NULL,
      deleguee user_hash NOT NULL
      );|};

    {|CREATE INDEX snapshot_deleguees_id_index ON snapshot_deleguees (id) ;|};
    {|CREATE INDEX snapshot_deleguees_tz1_index ON snapshot_deleguees (tz1) ;|};
  ]

let downgrade_13_to_12 = [
  {|DROP TABLE activation;|};
  {|DROP INDEX activation_hahs_index;|};
  {|DROP INDEX activation_pkh_index;|}
]

let update_12_to_13 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_13_to_12 [
    {|CREATE TABLE activation (
      hash operation_hash NOT NULL REFERENCES operation(hash),
      pkh hash NOT NULL,
      secret VARCHAR NOT NULL,
      PRIMARY KEY (hash, pkh)
      )|};
    {|CREATE UNIQUE INDEX activation_hahs_index ON activation (hash) ;|};
    {|CREATE UNIQUE INDEX activation_pkh_index ON activation (pkh) ;|}
  ]

let downgrade_14_to_13 = [
  {|DROP AGGREGATE _array_agg;|}
]

let update_13_to_14 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_14_to_13 [
    {|CREATE AGGREGATE _array_agg(anyarray) (SFUNC = array_cat, STYPE = anyarray);|}
  ]

let downgrade_15_to_14 = [
  {|CREATE VIEW operation_main AS
    SELECT o.hash, o.op_type, bo.block_hash, b.timestamp
    FROM operation AS o
    INNER JOIN block_operation AS bo ON o.hash = bo.operation_hash
    INNER JOIN block_main as b ON bo.block_hash = b.hash|};
  {|CREATE VIEW operation_pending AS
    SELECT operation.hash, operation.op_type
    FROM operation LEFT JOIN block_operation
    ON operation.hash = block_operation.operation_hash
    WHERE block_operation.operation_hash IS NULL|};
  {|CREATE VIEW operation_main_transaction AS
    SELECT * FROM operation_main INNER JOIN transaction USING(hash)|};
  {|CREATE VIEW operation_main_delegation AS
    SELECT * FROM operation_main INNER JOIN delegation USING(hash)|};
  {|CREATE VIEW operation_main_endorsement AS
    SELECT hash, op_type, operation_main.block_hash, timestamp,
    source, endorsement.block_hash AS e_block_hash, slot
    FROM operation_main INNER JOIN endorsement USING(hash)|};
  {|CREATE VIEW operation_main_proposal AS
    SELECT * FROM operation_main INNER JOIN proposal USING(hash)|};
  {|CREATE VIEW operation_main_ballot AS
    SELECT * FROM operation_main INNER JOIN ballot USING(hash)|};
  {|CREATE VIEW operation_main_reveal AS
    SELECT * FROM operation_main INNER JOIN reveal USING(hash)|};
  {|CREATE VIEW block_main_with_operations AS
    SELECT b.hash, predecessor, fitness, baker, b.timestamp,
    protocol, test_protocol,
    network, test_network, test_network_expiration,
    level, level_position, priority,
    cycle, cycle_position,
    voting_period, voting_period_position,
    commited_nonce_hash, pow_nonce,
    validation_pass, proto, data, signature,
    protocol_name, test_protocol_name,
    operation_count,
    o.hash as ohash, o.op_type
    FROM block_main AS b
    LEFT JOIN operation_main AS o ON b.hash = o.block_hash|};
  {|ALTER TABLE operation DROP COLUMN op_anon_type text[];|};
  {|ALTER TABLE operation DROP COLUMN op_manager_type text[];|};
  {|ALTER TABLE endorsement DROP COLUMN block_level bigint;|};
  {|ALTER TABLE transaction ADD PRIMARY KEY hash REFERENCES operation(hash);|};
  {|DROP INDEX transaction_hash_index;|};
  {|ALTER TABLE delegation ADD PRIMARY KEY hash REFERENCES operation(hash);|};
  {|DROP INDEX delegation_hash_index;|};
  {|ALTER TABLE origination ADD PRIMARY KEY hash REFERENCES operation(hash);|};
  {|DROP INDEX origination_hash_index;|};
  {|ALTER TABLE reveal ADD PRIMARY KEY hash REFERENCES operation(hash);|};
  {|DROP INDEX reveal_hash_index;|};
  {|ALTER TABLE transaction DROP COLUMN gas_limit;|};
  {|ALTER TABLE transaction DROP COLUMN storage_limit;|};
  {|ALTER TABLE origination DROP COLUMN gas_limit;|};
  {|ALTER TABLE origination DROP COLUMN storage_limit;|};
  {|ALTER TABLE delegation DROP COLUMN gas_limit;|};
  {|ALTER TABLE delegation DROP COLUMN storage_limit;|};
  {|ALTER TABLE reveal DROP COLUMN gas_limit;|};
  {|ALTER TABLE reveal DROP COLUMN storage_limit;|};
]

let update_14_to_15 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_15_to_14 [
    {|DROP VIEW operation_pending;|} ;
    {|DROP VIEW operation_main_transaction;|};
    {|DROP VIEW operation_main_delegation;|};
    {|DROP VIEW operation_main_endorsement;|};
    {|DROP VIEW operation_main_proposal;|};
    {|DROP VIEW operation_main_ballot;|};
    {|DROP VIEW block_main_with_operations;|};
    {|DROP VIEW operation_main_reveal;|};
    {|DROP VIEW operation_main;|};
    {|ALTER TABLE operation ADD COLUMN op_anon_type text[];|};
    {|ALTER TABLE operation ADD COLUMN op_manager_type text[];|};
    {|ALTER TABLE endorsement ADD COLUMN block_level bigint;|};
    {|ALTER TABLE transaction DROP CONSTRAINT transaction_pkey|};
    {|CREATE INDEX transaction_hash_index ON transaction (hash) ;|};
    {|ALTER TABLE delegation DROP CONSTRAINT delegation_pkey|};
    {|CREATE INDEX delegation_hash_index ON delegation (hash) ;|};
    {|ALTER TABLE origination DROP CONSTRAINT origination_pkey|};
    {|CREATE INDEX origination_hash_index ON origination (hash) ;|};
    {|ALTER TABLE reveal DROP CONSTRAINT reveal_pkey|};
    {|CREATE INDEX reveal_hash_index ON reveal (hash) ;|};
    {|ALTER TABLE transaction ADD COLUMN gas_limit bigint;|};
    {|ALTER TABLE transaction ADD COLUMN storage_limit bigint;|};
    {|ALTER TABLE origination ADD COLUMN gas_limit bigint;|};
    {|ALTER TABLE origination ADD COLUMN storage_limit bigint;|};
    {|ALTER TABLE delegation ADD COLUMN gas_limit bigint;|};
    {|ALTER TABLE delegation ADD COLUMN storage_limit bigint;|};
    {|ALTER TABLE reveal ADD COLUMN gas_limit bigint;|};
    {|ALTER TABLE reveal ADD COLUMN storage_limit bigint;|};
  ]

let downgrade_16_to_15 = [
  {|ALTER TABLE level_rights DROP COLUMN bakers_priority;|} ;
]

let update_15_to_16 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_16_to_15 [
    {|ALTER TABLE level_rights ADD COLUMN bakers_priority int[] NOT NULL;|} ;
  ]

let downgrade_17_to_16 = [
  {|ALTER TABLE transaction DROP COLUMN failed;|} ;
  {|ALTER TABLE origination DROP COLUMN failed;|} ;
  {|ALTER TABLE delegation DROP COLUMN failed;|} ;
  {|ALTER TABLE reveal DROP COLUMN failed;|} ;
]

let update_16_to_17 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_17_to_16 [
    {|ALTER TABLE transaction ADD COLUMN failed boolean NOT NULL;|} ;
    {|ALTER TABLE origination ADD COLUMN failed boolean NOT NULL;|} ;
    {|ALTER TABLE delegation ADD COLUMN failed boolean NOT NULL;|} ;
    {|ALTER TABLE reveal ADD COLUMN failed boolean NOT NULL;|} ;
  ]

let downgrade_18_to_17 = [
  {|DROP INDEX transaction_index_source;|};
  {|DROP INDEX transaction_index_destination;|};
  {|DROP INDEX endorsement_index_source;|};
  {|DROP INDEX endorsement_index_block_level;|};
  {|DROP INDEX block_index_distance_level;|};
  {|DROP INDEX block_index_level;|};
  {|DROP INDEX block_index_baker;|};
  {|DROP INDEX tezos_index_contract;|};
  {|DROP INDEX reveal_index_source;|};
  {|DROP INDEX origination_index_source;|};
  {|DROP INDEX origination_index_tz1;|};
  {|DROP INDEX origination_index_delegate;|};
  {|DROP INDEX origination_index_manager;|};
  {|DROP INDEX delegation_index_source;|};
  {|DROP INDEX delegation_index_delegate;|};
]

let update_17_to_18 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_18_to_17 [
    {|CREATE INDEX transaction_index_source ON transaction (source);|};
    {|CREATE INDEX transaction_index_destination ON transaction (destination);|};
    {|CREATE INDEX endorsement_index_source ON endorsement (source);|};
    {|CREATE INDEX endorsement_index_block_level ON endorsement (block_level);|};
    {|CREATE INDEX block_index_distance_level ON block (distance_level);|};
    {|CREATE INDEX block_index_level ON block (level);|};
    {|CREATE INDEX block_index_baker ON block (baker);|};
    {|CREATE INDEX tezos_index_contract ON tezos_user (contract);|};
    {|CREATE INDEX reveal_index_source ON reveal (source);|};
    {|CREATE INDEX origination_index_source ON origination (source);|};
    {|CREATE INDEX origination_index_tz1 ON origination (tz1);|};
    {|CREATE INDEX origination_index_delegate ON origination (delegate);|};
    {|CREATE INDEX origination_index_manager ON origination (manager);|};
    {|CREATE INDEX delegation_index_source ON delegation (source);|};
    {|CREATE INDEX delegation_index_delegate ON delegation (delegate);|};
  ]

let downgrade_19_to_18 = [
  {|DROP TABLE activation_balance;|}
]

let update_18_to_19 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_19_to_18 [
    {|CREATE TABLE activation_balance (
      hash operation_hash NOT NULL REFERENCES operation(hash) PRIMARY KEY,
      pkh hash NOT NULL,
      balance bigint NOT NULL
      )|};

    {|CREATE UNIQUE INDEX activation_balance_hahs_index ON activation_balance (hash) ;|};
    {|CREATE UNIQUE INDEX activation_balance_pkh_index ON activation_balance (pkh) ;|}
  ]

let downgrade_20_to_19 = [
  {|ALTER TABLE level_rights DROP COLUMN cycle;|}
]

let update_19_to_20 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_20_to_19 [
    {|ALTER TABLE level_rights ADD COLUMN cycle bigint NOT NULL DEFAULT 0;|} ;
    {|UPDATE level_rights AS lr SET cycle = b.cycle
      FROM block AS b WHERE lr.level = b.level AND distance_level = 0|};
    {|ALTER TABLE level_rights ALTER COLUMN cycle DROP DEFAULT;|} ;
  ]

let downgrade_21_to_20 = [
   {|DROP TABLE crawler_activity;|}
]

let update_20_to_21 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_21_to_20 [
    {|CREATE TABLE crawler_activity (
      name varchar NOT NULL PRIMARY KEY,
      timestamp double precision NOT NULL,
      delay int NOT NULL
      )|};

    {|CREATE UNIQUE INDEX crawler_activity_name_index ON crawler_activity (name) ;|};
  ]

let downgrade_22_to_21 = [
  {|ALTER TABLE level_rights DROP COLUMN slots; |}
]

let update_21_to_22 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_22_to_21 [
    {|ALTER TABLE level_rights ADD COLUMN slots int[] NOT NULL DEFAULT '{}' ; |} ;
    {|DELETE FROM level_rights ; |} ;
    {|ALTER TABLE level_rights ALTER COLUMN cycle DROP DEFAULT;|} ;
  ]

let downgrade_23_to_22 = [
  {| DROP INDEX user_alias_index |};
  {| DROP TABLE user_alias |};
  {| DROP INDEX tezos_user_index_alias |};
  {| ALTER TABLE tezos_user DROP COLUMN alias |};
]

let update_22_to_23 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_23_to_22 [
    {|ALTER TABLE tezos_user ADD COLUMN alias varchar(42);|};
    {|CREATE UNIQUE INDEX tezos_user_index_alias ON tezos_user (alias) ;|};
    {|CREATE TABLE user_alias (
      tz varchar NOT NULL PRIMARY KEY,
      alias varchar(42) NOT NULL
      );|};
    {|CREATE UNIQUE INDEX user_alias_index ON user_alias (tz) ;|};
  ]

let downgrade_24_to_23 = [
    {|ALTER TABLE transaction DROP COLUMN internal|};
    {|ALTER TABLE origination DROP COLUMN internal|};
    {|ALTER TABLE delegation DROP COLUMN internal|};
    {|ALTER TABLE reveal DROP COLUMN internal|};
  ]

let update_23_to_24 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_24_to_23 [
    {|ALTER TABLE transaction ADD COLUMN internal bool NOT NULL DEFAULT FALSE;|};
    {|ALTER TABLE origination ADD COLUMN internal bool NOT NULL DEFAULT FALSE;|};
    {|ALTER TABLE delegation ADD COLUMN internal bool NOT NULL DEFAULT FALSE;|};
    {|ALTER TABLE reveal ADD COLUMN internal bool NOT NULL DEFAULT FALSE;|};
    {|ALTER TABLE transaction ALTER COLUMN internal DROP DEFAULT;|};
    {|ALTER TABLE origination ALTER COLUMN internal DROP DEFAULT;|};
    {|ALTER TABLE delegation ALTER COLUMN internal DROP DEFAULT;|};
    {|ALTER TABLE reveal ALTER COLUMN internal DROP DEFAULT;|};
  ]

let downgrade_25_to_24 = [
  {| CREATE TABLE snapshot_deleguees_balance (
      id bigint REFERENCES snapshot_rolls(id) ON DELETE CASCADE NOT NULL,
      tz1 user_hash NOT NULL,
      balance bigint NOT NULL
      ); |};
  {| ALTER TABLE snapshot_deleguees DROP COLUMN balance |} ;
  {| ALTER TABLE snapshot_owner DROP COLUMN delegated_balance |} ;
  {| ALTER TABLE snapshot_owner RENAME COLUMN  staking_balance TO change; |} ;
  {| DROP TABLE authorized |};
]

let update_24_to_25 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_25_to_24 [
    {|CREATE TABLE authorized (
      hash hash NOT NULL PRIMARY KEY
    )|};
    {|DELETE FROM snapshot_rolls CASCADE|} ;
    {|ALTER TABLE snapshot_owner RENAME COLUMN change TO staking_balance ; |} ;
    {|ALTER TABLE snapshot_owner ADD COLUMN delegated_balance bigint NOT NULL; |} ;

    {|ALTER TABLE snapshot_deleguees ADD COLUMN balance bigint NOT NULL ; |} ;
    {|ALTER TABLE snapshot_deleguees_balance RENAME TO snapshot_deleguees_balanc_deprecated ; |}
  ]


let downgrade_26_to_25 = [
  {| DROP TABLE user_sessions |};
  {| DROP TABLE user_bookmarks |};
  {| DROP TABLE user_accounts |};
]

let update_25_to_26 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_26_to_25 [
      {|
        CREATE TABLE user_accounts(
          id SERIAL PRIMARY KEY NOT NULL,
	  emailhash VARCHAR(100) UNIQUE NOT NULL,
	  pseudo VARCHAR(100) UNIQUE NOT NULL,
	  pwhash BYTEA NOT NULL
       )|};
      {|
        CREATE TABLE user_sessions(
          slogin VARCHAR(100) REFERENCES user_accounts (emailhash) NOT NULL,
	  scookie VARCHAR(30) PRIMARY KEY NOT NULL,
	  slast FLOAT NOT NULL
       )|};
      {|
        CREATE TABLE user_bookmarks(
          emailhash VARCHAR(100) REFERENCES user_accounts (emailhash) NOT NULL,
	  hash user_hash REFERENCES tezos_user (hash) NOT NULL,
          alias VARCHAR(20) NOT NULL,
	  PRIMARY KEY (emailhash,hash)
       )|}
    ]

let downgrade_27_to_26 = [
  {|ALTER TABLE double_baking_evidence DROP COLUMN accused; |} ;
  {|ALTER TABLE double_baking_evidence DROP COLUMN denouncer; |} ;
  {|ALTER TABLE double_baking_evidence DROP COLUMN lost_deposit; |} ;
  {|ALTER TABLE double_baking_evidence DROP COLUMN lost_rewards; |} ;
  {|ALTER TABLE double_baking_evidence DROP COLUMN lost_fees; |} ;
  {|ALTER TABLE double_baking_evidence DROP COLUMN gain_rewards; |} ;
]

let update_26_to_27 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_27_to_26 [
    {|ALTER TABLE double_baking_evidence ADD COLUMN accused varchar ; |} ;
    {|ALTER TABLE double_baking_evidence ADD COLUMN denouncer varchar ; |} ;
    {|ALTER TABLE double_baking_evidence ADD COLUMN lost_deposit bigint ; |} ;
    {|ALTER TABLE double_baking_evidence ADD COLUMN lost_rewards bigint ; |} ;
    {|ALTER TABLE double_baking_evidence ADD COLUMN lost_fees bigint  ; |} ;
    {|ALTER TABLE double_baking_evidence ADD COLUMN gain_rewards bigint ; |} ;

  ]

let downgrade_28_to_27 = [
  {|DROP INDEX IF EXISTS block_timestamp_index;|};
  {|DROP INDEX IF EXISTS operation_timestamp_index|} ]

let update_27_to_28 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_28_to_27 [
    {|CREATE INDEX block_timestamp_index ON block (timestamp); |} ;
    {|CREATE INDEX operation_timestamp_index ON operation (timestamp); |} ;
  ]

let downgrade_29_to_28 =
  list_update_6_to_7 @ [
    {|CREATE VIEW block_main AS
      SELECT b.hash, predecessor, fitness, baker, timestamp,
      protocol, test_protocol,
      network, test_network, test_network_expiration,
      level, level_position, priority,
      cycle, cycle_position,
      voting_period, voting_period_position,
      commited_nonce_hash, pow_nonce,
      validation_pass, proto, data, signature,
      operation_count,
      p.name AS protocol_name, pt.name AS test_protocol_name
      FROM block AS b
      LEFT JOIN protocol AS p ON b.protocol = p.hash
      LEFT JOIN protocol AS pt ON b.test_protocol = pt.hash
      WHERE distance_level = 0;|};
    {|ALTER TABLE block ALTER COLUMN fees TYPE float;|};
    {|ALTER TABLE block ALTER COLUMN volume TYPE float;|};
    {|ALTER TABLE block ALTER COLUMN priority TYPE float;|};
    {|ALTER TABLE header ALTER COLUMN priority TYPE float;|};
    {|ALTER TABLE block ALTER COLUMN fees SET DEFAULT 0.;|};
    {|ALTER TABLE block ALTER COLUMN volume SET DEFAULT 0.;|};
    {|ALTER TABLE block ALTER COLUMN priority SET DEFAULT 0.;|};
    {|ALTER TABLE header ALTER COLUMN priority SET DEFAULT 0.;|};
    {|DROP FUNCTION IF EXISTS bake_time;|};
    {|DROP FUNCTION IF EXISTS bk_rewards;|};
    {|DROP FUNCTION IF EXISTS end_rewards;|};
    {|DROP FUNCTION IF EXISTS deposits;|};

  ]

let update_28_to_29 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_29_to_28 [
    {|DROP SCHEMA json CASCADE;|};
    {|DROP VIEW block_main;|};
    {|ALTER TABLE block ALTER COLUMN fees TYPE bigint;|};
    {|ALTER TABLE block ALTER COLUMN volume TYPE bigint;|};
    {|ALTER TABLE block ALTER COLUMN priority TYPE integer;|};
    {|ALTER TABLE header ALTER COLUMN priority TYPE integer;|};
    {|ALTER TABLE block ALTER COLUMN fees SET DEFAULT 0;|};
    {|ALTER TABLE block ALTER COLUMN volume SET DEFAULT 0;|};
    {|ALTER TABLE block ALTER COLUMN priority SET DEFAULT 0;|};
    {|ALTER TABLE header ALTER COLUMN priority SET DEFAULT 0;|};
    {|CREATE OR REPLACE FUNCTION bake_time(timediff interval, priority int,
      itv_be_blocks interval, itv_be_prio interval)
      RETURNS FLOAT AS $$
      BEGIN
      RETURN
      EXTRACT(MILLISECONDS FROM
      CASE WHEN priority < 2 THEN
      (timediff - priority * itv_be_blocks)/1000.
      ELSE
      (timediff - (priority - 1) * itv_be_prio - itv_be_blocks)/1000.
      END);
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION bk_rewards(base_reward bigint, cycle bigint, cond bool)
      RETURNS BIGINT AS $$
      BEGIN
      RETURN CASE WHEN cycle > 6 AND cond THEN base_reward ELSE 0 END;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION end_rewards(base_reward bigint, cycle bigint,
      nslot int, priority int, cond bool)
      RETURNS BIGINT AS $$
      BEGIN
      RETURN CASE WHEN cycle > 6 and cond THEN
      nslot * (base_reward / ( 1 + priority ))
      ELSE 0 END;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION deposits(base_deposit bigint, cycle bigint,
      cycle_threshold bigint, nslot int, cond bool)
      RETURNS BIGINT AS $$
      BEGIN
      RETURN CASE WHEN cycle < cycle_threshold AND cond THEN
      nslot * cycle * base_deposit / cycle_threshold
      WHEN cond THEN base_deposit ELSE 0 END;
      END;
      $$ LANGUAGE plpgsql;|};
  ]

let downgrade_30_to_29 = [
  {|ALTER TABLE origination DROP COLUMN burn_tez; |} ;
  {|ALTER TABLE transaction DROP COLUMN burn_tez; |}
]

let update_29_to_30 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_30_to_29 [
    {|ALTER TABLE origination ADD COLUMN burn_tez bigint NOT NULL DEFAULT 257000; |} ;
    {|ALTER TABLE transaction ADD COLUMN burn_tez bigint NOT NULL DEFAULT 0; |} ;
  ]

let downgrade_31_to_30 =
    [
    {| ALTER TABLE user_sessions_deprecated RENAME TO user_sessions ; |} ;
    {| ALTER TABLE user_bookmarks_deprecated RENAME TO user_bookmarks ; |} ;
    {| ALTER TABLE user_accounts_deprecated RENAME TO user_accounts ; |} ;
  ]

let update_30_to_31 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_31_to_30
  [
    {| ALTER TABLE user_sessions RENAME TO user_sessions_deprecated ; |} ;
    {| ALTER TABLE user_bookmarks RENAME TO user_bookmarks_deprecated ; |} ;
    {| ALTER TABLE user_accounts RENAME TO user_accounts_deprecated ; |} ;
  ]

let downgrade_32_to_31 = [
  {|DROP TABLE endorsement_all;|};
  {|DROP TABLE endorsement_last;|};
  {|DROP TABLE transaction_all;|};
  {|DROP TABLE transaction_last;|}
]

let update_31_to_32 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_32_to_31 [
    {|CREATE TABLE endorsement_all (
      hash varchar NOT NULL,
      source varchar NOT NULL,
      block_hash varchar NOT NULL,
      block_level bigint NOT NULL,
      priority int NOT NULL,
      slots int[] NOT NULL,
      op_block_hash varchar NOT NULL,
      op_level bigint NOT NULL,
      op_cycle bigint NOT NULL,
      distance_level int NOT NULL DEFAULT -1,
      network varchar NOT NULL,
      timestamp timestamp NOT NULL,
      PRIMARY KEY (hash, op_block_hash)); |};
    {|CREATE TABLE endorsement_last (
      id bigserial NOT NULL,
      hash varchar NOT NULL,
      source varchar NOT NULL,
      block_hash varchar NOT NULL,
      block_level bigint NOT NULL,
      priority int NOT NULL,
      slots int[] NOT NULL,
      op_block_hash varchar NOT NULL,
      op_level bigint NOT NULL,
      op_cycle bigint NOT NULL,
      distance_level int NOT NULL DEFAULT -1,
      network varchar NOT NULL,
      timestamp timestamp NOT NULL,
      PRIMARY KEY (hash, op_block_hash)); |};
    {|CREATE INDEX block_level_endorsement_all_index ON endorsement_all (block_level) ;|};
    {|CREATE INDEX op_level_endorsement_all_index ON endorsement_all (op_level) ;|};
    {|CREATE INDEX op_cycle_endorsement_all_index ON endorsement_all (op_cycle) ;|};
    {|CREATE INDEX source_endorsement_all_index ON endorsement_all (source) ;|};
    {|CREATE INDEX op_block_hash_endorsement_all_index ON endorsement_all (op_block_hash) ;|};
    {|CREATE INDEX distance_level_endorsement_all_index ON endorsement_all (distance_level) ;|};
    {|CREATE INDEX timestamp_endorsement_all_index ON endorsement_all (timestamp) ;|};
    {|CREATE INDEX block_level_endorsement_last_index ON endorsement_last (block_level) ;|};
    {|CREATE INDEX op_level_endorsement_last_index ON endorsement_last (op_level) ;|};
    {|CREATE INDEX op_cycle_endorsement_last_index ON endorsement_last (op_cycle) ;|};
    {|CREATE INDEX source_endorsement_last_index ON endorsement_last (source) ;|};
    {|CREATE INDEX op_block_hash_endorsement_last_index ON endorsement_last (op_block_hash) ;|};
    {|CREATE INDEX distance_level_endorsement_last_index ON endorsement_last (distance_level) ;|};
    {|CREATE INDEX timestamp_endorsement_last_index ON endorsement_last (timestamp) ;|};
    {|CREATE TABLE transaction_all (
      hash varchar NOT NULL,
      source varchar NOT NULL,
      destination varchar NOT NULL,
      fee bigint NOT NULL DEFAULT 0,
      counter bigint NOT NULL,
      amount bigint NOT NULL DEFAULT 0,
      parameters bytea,
      gas_limit bigint,
      storage_limit bigint,
      failed boolean NOT NULL,
      internal boolean NOT NULL,
      timestamp_op timestamp NOT NULL,
      burn_tez bigint NOT NULL DEFAULT 0,
      op_level bigint,
      op_block_hash varchar,
      distance_level int,
      network varchar,
      timestamp_block timestamp );|};
    {|CREATE TABLE transaction_last (
      id bigserial NOT NULL,
      hash varchar NOT NULL,
      source varchar NOT NULL,
      destination varchar NOT NULL,
      fee bigint NOT NULL DEFAULT 0,
      counter bigint NOT NULL,
      amount bigint NOT NULL DEFAULT 0,
      parameters bytea DEFAULT '',
      gas_limit bigint,
      storage_limit bigint,
      failed boolean NOT NULL,
      internal boolean NOT NULL,
      timestamp_op timestamp NOT NULL,
      burn_tez bigint NOT NULL DEFAULT 0,
      op_level bigint,
      op_block_hash varchar,
      distance_level int,
      network varchar,
      timestamp_block timestamp );|};
    {|CREATE INDEX source_transaction_all_index ON transaction_all (source) ;|};
    {|CREATE INDEX destination_transaction_all_index ON transaction_all (destination) ;|};
    {|CREATE INDEX op_level_transaction_all_index ON transaction_all (op_level) ;|};
    {|CREATE INDEX op_block_hash_transaction_all_index ON transaction_all (op_block_hash) ;|};
    {|CREATE INDEX distance_level_transaction_all_index ON transaction_all (distance_level) ;|};
    {|CREATE INDEX timestamp_transaction_all_index ON transaction_all (timestamp_op) ;|};
    {|CREATE INDEX source_transaction_last_index ON transaction_last (source) ;|};
    {|CREATE INDEX destination_transaction_last_index ON transaction_last (destination) ;|};
    {|CREATE INDEX op_level_transaction_last_index ON transaction_last (op_level) ;|};
    {|CREATE INDEX op_block_hash_transaction_last_index ON transaction_last (op_block_hash) ;|};
    {|CREATE INDEX distance_level_transaction_last_index ON transaction_last (distance_level) ;|};
    {|CREATE INDEX timestamp_transaction_last_index ON transaction_last (timestamp_op) ;|};
  ]

let downgrade_33_to_32 = [
  {|DROP TABLE cycle_count;|};
  {|DROP TABLE cycle_count_baker;|};
  {|DROP TABLE operation_count_user;|};
  {|DROP TABLE count_info;|};
  {|DROP FUNCTION incr_agg_pro;|};
  {|DROP FUNCTION fagg_prio;|};
  {|DROP AGGREGATE aff_prio;|};
  {|DROP FUNCTION array_lin;|};
  {|DROP FUNCTION array_sum;|};
  {|DROP FUNCTION array_wavg;|};
  {|DROP FUNCTION end_rewards_array;|};
  {|DROP FUNCTION init_array_prio;|}
]

let update_32_to_33 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_33_to_32 [
    {|CREATE TABLE cycle_count (
      cycle bigint PRIMARY KEY,
      nb_transaction bigint NOT NULL DEFAULT 0,
      nb_delegation bigint NOT NULL DEFAULT 0,
      nb_origination bigint NOT NULL DEFAULT 0,
      nb_activation bigint NOT NULL DEFAULT 0,
      nb_reveal bigint NOT NULL DEFAULT 0,
      nb_dbe bigint NOT NULL DEFAULT 0,
      nb_dee bigint NOT NULL DEFAULT 0,
      nb_nonce bigint NOT NULL DEFAULT 0,
      nb_endorsement bigint[] NOT NULL DEFAULT '{}',
      nb_endorsement_op bigint NOT NULL DEFAULT 0,
      nb_prio bigint[] NOT NULL DEFAULT '{}');|};
    {|CREATE INDEX cycle_count_index ON cycle_count (cycle) ;|};
    {|CREATE TABLE cycle_count_baker (
      cycle bigint NOT NULL,
      tz varchar NOT NULL,
      nb_baking bigint[] NOT NULL DEFAULT '{}',
      nb_miss_baking bigint NOT NULL DEFAULT 0,
      nb_alt_baking bigint NOT NULL DEFAULT 0,
      nb_endorsement bigint[] NOT NULL DEFAULT '{}',
      nb_miss_endorsement bigint NOT NULL DEFAULT 0,
      nb_alt_endorsement bigint NOT NULL DEFAULT 0,
      fees bigint NOT NULL DEFAULT 0,
      time float NOT NULL DEFAULT 0.,
      PRIMARY KEY (cycle, tz));|};
    {|CREATE INDEX cycle_count_baker_index ON cycle_count_baker (cycle) ;|};
    {|CREATE INDEX cycle_count_tz_index ON cycle_count_baker (tz) ;|};
    {|CREATE TABLE operation_count_user (
      tz varchar PRIMARY KEY,
      nb_transaction_src bigint NOT NULL DEFAULT 0,
      nb_transaction_dst bigint NOT NULL DEFAULT 0,
      nb_delegation_src bigint NOT NULL DEFAULT 0,
      nb_delegation_dlg bigint NOT NULL DEFAULT 0,
      nb_origination_src bigint NOT NULL DEFAULT 0,
      nb_origination_man bigint NOT NULL DEFAULT 0,
      nb_origination_tz1 bigint NOT NULL DEFAULT 0,
      nb_origination_dlg bigint NOT NULL DEFAULT 0,
      nb_activation bigint NOT NULL DEFAULT 0,
      nb_reveal bigint NOT NULL DEFAULT 0,
      nb_dbe_bk bigint NOT NULL DEFAULT 0,
      nb_dbe_acc bigint NOT NULL DEFAULT 0,
      nb_nonce bigint NOT NULL DEFAULT 0,
      nb_endorsement bigint NOT NULL DEFAULT 0);|};
    {|CREATE INDEX operation_count_user_index ON operation_count_user (tz) ;|};
    {|CREATE OR REPLACE FUNCTION incr_agg_prio(array_prio bigint[], prio int,
      nslot int, cond bool)
      RETURNS BIGINT[] AS $$
      BEGIN
      IF prio >= 0 AND cond THEN
      FOR i IN 1 .. prio LOOP array_prio[i] := coalesce(array_prio[i], 0); END LOOP;
      array_prio[prio+1] := COALESCE(array_prio[prio+1],0::bigint) + COALESCE(nslot::bigint, 0::bigint);
      END IF;
      RETURN COALESCE(array_prio, '{}');
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION fagg_prio(array_prio bigint[])
      RETURNS BIGINT[] AS $$
      BEGIN
      RETURN COALESCE(ARRAY_REPLACE(array_prio, NULL, 0::bigint), '{}');
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE AGGREGATE agg_prio(prio int, nslot int, cond bool) (
      INIT_COND='{}',
      STYPE=BIGINT[],
      SFUNC=incr_agg_prio,
      FINALFUNC=fagg_prio);|};
    {|CREATE OR REPLACE FUNCTION array_lin(array1 bigint[], array2 bigint[],
      factor1 bigint, factor2 bigint)
      RETURNS BIGINT[] AS $$
      BEGIN
      FOR i IN 1 .. COALESCE(GREATEST(array_length(array1,1),array_length(array2,1))
      ,0) LOOP
      array1[i] := COALESCE(factor1, 1) * COALESCE(array1[i],0) +
      COALESCE(factor2, 1) * COALESCE(array2[i],0);
      END LOOP;
      RETURN COALESCE(array1,'{}');
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION array_sum(arrhh bigint[], start int)
      RETURNS BIGINT AS $$
      DECLARE s bigint := 0;
      BEGIN
      IF arrhh <> '{}' THEN
      FOR i IN start .. COALESCE(array_upper(arrhh,1),0) LOOP
      s := s + COALESCE(arrhh[i], 0);
      END LOOP;
      END IF;
      RETURN s;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION array_wavg(arrhh bigint[], start int)
      RETURNS double precision AS $$
      DECLARE s1 double precision := 0.;
      DECLARE s2 double precision := 0;
      BEGIN
      FOR i IN start .. COALESCE(array_length(arrhh,1),0) LOOP
      s2 := s2 + cast( arrhh[i] as double precision);
      s1 := s1 + cast((i-1) * arrhh[i] as double precision);
      END LOOP;
      IF s2 <> 0 THEN
      s1 := s1 / s2;
      ELSE
      s1 := 0;
      END IF;
      RETURN s1;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION end_rewards_array(base_reward bigint, cycle bigint,
      nb_endorsement bigint[])
      RETURNS BIGINT AS $$
      DECLARE s BIGINT := 0;
      BEGIN
      IF cycle > 6 THEN
      FOR i IN 1 .. COALESCE(array_upper(nb_endorsement, 1),0) LOOP
      s := s + coalesce(nb_endorsement[i], 0) * (base_reward / i);
      END LOOP;
      END IF;
      RETURN s;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION init_array_prio(priority int, nslot bigint)
      RETURNS BIGINT[] AS $$
      DECLARE arrhhh BIGINT[] := '{}';
      BEGIN
      FOR i IN 1 .. priority LOOP arrhhh[i] := 0; END LOOP;
      arrhhh[priority + 1] := COALESCE(nslot,0);
      RETURN arrhhh;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE TABLE count_info (
      hash varchar NOT NULL,
      level bigint NOT NULL,
      info varchar PRIMARY KEY);|};
    {|CREATE INDEX level_rights_level_index ON level_rights (level) ;|};
    {|CREATE INDEX level_rights_endorsers_index ON level_rights (endorsers) ;|};
    {|CREATE INDEX level_rights_cycle_index ON level_rights (cycle) ;|};
    {|CREATE INDEX level_rights_bakers_index ON level_rights (bakers) ;|};
  ]

let downgrade_34_to_33 = [
  {|ALTER TABLE marketcap DROP COLUMN valid;|} ]

let update_33_to_34 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_34_to_33 [
    {|ALTER TABLE marketcap ADD COLUMN valid bool NOT NULL DEFAULT true;|}
  ]

let downgrade_35_to_34 = [
  {|DROP TABLE top_balances;|};
  {|DROP TABLE top_frozen_balances;|};
  {|DROP TABLE top_frozen_deposits;|};
  {|DROP TABLE top_frozen_rewards;|};
  {|DROP TABLE top_paid_bytes;|};
  {|DROP TABLE top_staking_balances;|};
  {|DROP TABLE top_total_balances;|};
  {|DROP TABLE top_total_delegated;|};
  {|DROP TABLE top_total_delegators;|};
  {|DROP TABLE top_total_frozen_fees;|};
  {|DROP TABLE top_used_bytes;|};
  {|DROP TABLE day_context;|};
]

let update_34_to_35 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_35_to_34 [
    {|CREATE TABLE day_context (
      id bigserial PRIMARY KEY,
      day timestamp NOT NULL UNIQUE);|};
    {|CREATE INDEX day_context_index ON day_context (day)|};
    {|CREATE TABLE context_totals (
      id bigint REFERENCES day_context(id),
      hash varchar,
      period varchar,
      period_kind varchar,
      addresses int,
      keys int ,
      revealed int ,
      originated int ,
      contracts int ,
      roll_owners int ,
      rolls int ,
      delegated bigint ,
      delegators int ,
      deleguees int ,
      self_delegates int ,
      multi_deleguees int ,
      current_balances bigint ,
      full_balances bigint ,
      staking_balances bigint ,
      frozen_balances bigint ,
      frozen_deposits bigint ,
      frozen_rewards bigint ,
      frozen_fees bigint ,
      paid_bytes bigint ,
      used_bytes bigint);|};
    {|CREATE TABLE top_balances (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      balance bigint NOT NULL);|};
    {|CREATE TABLE top_frozen_balances (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      frozen_balance bigint NOT NULL);|};
    {|CREATE TABLE top_frozen_deposits (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      frozen_deposits bigint NOT NULL);|};
    {|CREATE TABLE top_frozen_rewards (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      frozen_rewards bigint NOT NULL);|};
    {|CREATE TABLE top_paid_bytes (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      paid_bytes bigint NOT NULL);|};
    {|CREATE TABLE top_staking_balances (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      staking_balance bigint NOT NULL);|};
    {|CREATE TABLE top_total_balances (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      total_balance bigint NOT NULL);|};
    {|CREATE TABLE top_total_delegated (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      total_delegated bigint NOT NULL);|};
    {|CREATE TABLE top_total_delegators (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      total_delegators bigint NOT NULL);|};
    {|CREATE TABLE top_total_frozen_fees (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      total_frozen_fees bigint NOT NULL);|};
    {|CREATE TABLE top_used_bytes (
      id bigint REFERENCES day_context(id),
      hash varchar NOT NULL,
      used_bytes bigint NOT NULL);|};
  ]

let downgrade_36_to_35 =
    [
    {| DROP TABLE balance_updates ; |} ;
    {| DROP TABLE balance_from_balance_updates ; |} ;
    ]

let update_35_to_36 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_36_to_35
      [
      {|
        CREATE TABLE balance_updates(
          id SERIAL PRIMARY KEY NOT NULL,
          hash user_hash NOT NULL,
          block_hash block_hash NOT NULL,
	  diff bigint NOT NULL,
	  date timestamp NOT NULL,
	  update_type VARCHAR NOT NULL,
          operation_type VARCHAR NOT NULL,
          internal BOOLEAN NOT NULL,
          level int NOT NULL,
          frozen BOOLEAN NOT NULL,
          burn BOOLEAN NOT NULL,
          distance_level int NOT NULL DEFAULT -1
       )|};{|
        CREATE TABLE balance_from_balance_updates(
          id SERIAL PRIMARY KEY NOT NULL,
          hash user_hash NOT NULL,
          spendable_balance bigint NOT NULL,
	  frozen bigint NOT NULL,
          rewards bigint NOT NULL,
          fees bigint NOT NULL,
          deposits bigint NOT NULL,
          cycle int NOT NULL
            )|};
      {|
       CREATE INDEX bal_account ON balance_from_balance_updates (hash);|};
      {|
       CREATE INDEX bal_cycle ON balance_from_balance_updates (cycle);|};
      {|
       CREATE INDEX bu_level ON balance_updates (level);|};
      {|
       CREATE INDEX bu_hash ON balance_updates (hash);|};
      ]

let downgrade_37_to_36 = [
  {|DROP TABLE coingecko_exchange;|}
]

let update_36_to_37 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_37_to_36 [
    {|CREATE TABLE coingecko_exchange (
      name varchar NOT NULL,
      base varchar NOT NULL,
      target varchar NOT NULL,
      timestamp varchar NOT NULL,
      volume float NOT NULL,
      conversion float NOT NULL,
      price_usd float NOT NULL);|}
  ]

let downgrade_38_to_37 = [
  {|CREATE OR REPLACE FUNCTION bk_rewards(base_reward bigint, cycle bigint,
    cond bool)
    RETURNS BIGINT AS $$
    BEGIN
    RETURN CASE WHEN cycle >= 7 AND cond THEN base_reward ELSE 0 END;
    END;
    $$ LANGUAGE plpgsql;|};
  {|CREATE OR REPLACE FUNCTION end_rewards(base_reward bigint, cycle bigint,
    nslot int, priority int, cond bool)
    RETURNS BIGINT AS $$
    BEGIN
    RETURN CASE WHEN cycle >= 7 and cond THEN
    nslot * (base_reward / ( 1 + priority ))
    ELSE 0 END;
    END;
    $$ LANGUAGE plpgsql;|};
  {|CREATE OR REPLACE FUNCTION deposits(base_deposit bigint, cycle bigint,
    cycle_threshold bigint, nslot int, cond bool)
    RETURNS BIGINT AS $$
    BEGIN
    RETURN CASE WHEN cycle < cycle_threshold AND cond THEN
    nslot * cycle * base_deposit / cycle_threshold
    WHEN cond THEN nslot * base_deposit ELSE 0 END;
    END;
    $$ LANGUAGE plpgsql;|};
  {|CREATE OR REPLACE FUNCTION end_rewards_array(base_reward bigint, cycle bigint,
    nb_endorsement bigint[])
    RETURNS BIGINT AS $$
    DECLARE s BIGINT := 0;
    BEGIN
    IF cycle >= 7 THEN
    FOR i IN 1 .. COALESCE(array_upper(nb_endorsement, 1),0) LOOP
    s := s + coalesce(nb_endorsement[i], 0) * (base_reward / i);
    END LOOP;
    END IF;
    RETURN s;
    END;
    $$ LANGUAGE plpgsql;|};
]

let update_37_to_38 dbh version =
  EzPG.upgrade ~dbh ~version ~downgrade:downgrade_38_to_37 [
    {|CREATE OR REPLACE FUNCTION bk_rewards(base_reward bigint, cycle bigint,
      start_reward_cycle bigint, cond bool)
      RETURNS BIGINT AS $$
      BEGIN
      RETURN CASE WHEN cycle >= start_reward_cycle AND cond THEN base_reward ELSE 0 END;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION end_rewards(base_reward bigint, cycle bigint,
      nslot int, priority int, start_reward_cycle bigint, cond bool)
      RETURNS BIGINT AS $$
      BEGIN
      RETURN CASE WHEN cycle >= start_reward_cycle and cond THEN
      nslot * (base_reward / ( 1 + priority ))
      ELSE 0 END;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION deposits(base_deposit bigint, cycle bigint,
      cycle_threshold bigint, nslot int, cond bool)
      RETURNS BIGINT AS $$
      BEGIN
      RETURN CASE WHEN cycle < cycle_threshold AND cond THEN
      nslot * cycle * base_deposit / cycle_threshold
      WHEN cond THEN nslot * base_deposit ELSE 0 END;
      END;
      $$ LANGUAGE plpgsql;|};
    {|CREATE OR REPLACE FUNCTION end_rewards_array(base_reward bigint, cycle bigint,
      nb_endorsement bigint[], start_reward_cycle bigint)
      RETURNS BIGINT AS $$
      DECLARE s BIGINT := 0;
      BEGIN
      IF cycle >= start_reward_cycle THEN
      FOR i IN 1 .. COALESCE(array_upper(nb_endorsement, 1),0) LOOP
      s := s + coalesce(nb_endorsement[i], 0) * (base_reward / i);
      END LOOP;
      END IF;
      RETURN s;
      END;
      $$ LANGUAGE plpgsql;|};
  ]


let upgrades = [
    0, update_0_to_1;
    1, update_1_to_2;
    2, update_2_to_3;
    3, update_3_to_4;
    4, update_4_to_5;
    5, update_5_to_6;
    6, update_6_to_7;
    7, update_7_to_8;
    8, update_8_to_9;
    9, update_9_to_10;
    10, update_10_to_11;
    11, update_11_to_12;
    12, update_12_to_13;
    13, update_13_to_14;
    14, update_14_to_15;
    15, update_15_to_16;
    16, update_16_to_17;
    17, update_17_to_18;
    18, update_18_to_19;
    19, update_19_to_20;
    20, update_20_to_21;
    21, update_21_to_22;
    22, update_22_to_23;
    23, update_23_to_24;
    24, update_24_to_25;
    25, update_25_to_26;
    26, update_26_to_27;
    27, update_27_to_28;
    28, update_28_to_29;
    29, update_29_to_30;
    30, update_30_to_31;
    31, update_31_to_32;
    32, update_32_to_33;
    33, update_33_to_34;
    34, update_34_to_35;
    35, update_35_to_36;
    36, update_36_to_37;
    37, update_37_to_38
  ]

let downgrades = [
  38, downgrade_38_to_37;
  37, downgrade_37_to_36;
  36, downgrade_36_to_35;
  35, downgrade_35_to_34;
  34, downgrade_34_to_33;
  33, downgrade_33_to_32;
  32, downgrade_32_to_31;
  31, downgrade_31_to_30;
  30, downgrade_30_to_29;
  29, downgrade_29_to_28;
  28, downgrade_28_to_27;
  27, downgrade_27_to_26;
  26, downgrade_26_to_25;
  25, downgrade_25_to_24;
  24, downgrade_24_to_23;
  23, downgrade_23_to_22;
  22, downgrade_22_to_21;
  21, downgrade_21_to_20;
  20, downgrade_20_to_19;
  19, downgrade_19_to_18;
  18, downgrade_18_to_17;
  17, downgrade_17_to_16;
  16, downgrade_16_to_15;
  15, downgrade_15_to_14;
  14, downgrade_14_to_13;
  13, downgrade_13_to_12;
  12, downgrade_12_to_11;
  11, downgrade_11_to_10;
  10, downgrade_10_to_9;
  9, downgrade_9_to_8;
  8, downgrade_8_to_7;
  7, downgrade_7_to_6;
  6, downgrade_6_to_5;
  5, downgrade_5_to_4;
  4, downgrade_4_to_3;
  3, downgrade_3_to_2;
  2, downgrade_2_to_1;
  1, downgrade_1_to_0
]
