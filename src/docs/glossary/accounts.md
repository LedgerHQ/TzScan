An Account is the state associated with an Address (Hash). As in a
bank, a Balance of tezzies is associated with an Account, and can be
used to perform operations on the blockchain. The Manager of an
Account is the creator of that Account: the Manager can be itself if
the Account was generated from a Private Key (Default Account).

An Account can have several properties:
* Delegatable: an Account is Delegatable if its voting rights can be transfered
  to another Account for the Proof-of-Stake operations
* Spendable: an Account is Spendable if it can be used directly in a
  Transaction as the source Account from which the tokens are taken

A Default Account has an Address starting with "tz1"
(lowercase). Addresses contains a self-correction code, that can be
used to detect small errors in spelling.

### Contract and Code

Code can be associated with an Account. In this case, the Account is
also called a Contract (or Smart Contract). Contracts can be used
everywhere where an Account could be used.

Contracts in Tezos are written in the Michelson language, stored in
the blockchain. Since Michelson is a pretty low-level language,
programers will usually write Contracts in Liquidity, and then compile
them to Michelson.

A Contract has an Address starting with "KT1" (uppercase).
