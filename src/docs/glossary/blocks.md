A Block is a set of Operations, sealed together in the Blockchain. A
Hash is computed for every Block. In a Blockchain, every Block
contains the Hash of the previous Block, so that any modification of a
Block would change the Hashes of all the next Blocks.

The following information is associated with every Block:
* Level: position of the Block in the blockchain. Level 0 is the first block
  in the blockchain (Genesis Block). At a given level, there must eventually
  be only one Block. Temporarily, there can be several competing Heads with
  different most recent blocks.
* Age/Date: the time at which the Block was Mined.
* Ops: Number of operations stored in the Block
* Volume: Total number of Tezzies transfered between Accounts by the Operations
  in the Block
* Hash: Hash of the Content of the Block (Uniq Identifier)
* Predecessor: Hash of the Preceeding Block
* Miner/Baker: Address of the Account that baked the Block
* Fitness: Quality of the block. The Block with highest
  Fitness is always chosen as the current Head by the Blockchain Protocol.
  The Fitness depends on who baked the block and the number of Endorsements.
* Protocol: Version of the Protocol
* Network: Network of the Blockchain (Mainet or Alphanet)

### Mining or Baking

Mining is the operation of selecting the most recent block in the Blockchain.
In Proof-of-Work Blockchains, Mining is an expensive tasks, performed with
clusters of computers. In Tezos, however, selecting the most recent block
is done using Proof-of-Stake, and usually called Baking instead of Mining.
