Balance updates represent the exhaustive list of modifications, internal or not,
that are performed by the Tezos protocol. Every transaction, bake, endorsement,
double baking evidences, etc. generate at least one balance update.

###Different kinds of balances
####Spendable balance
The actual amount of Tezos you own today.

####Frozen balance
The amount of Tezos frozen by earning fees, baking and endorsing blocks. This
amount will credited to the spendable balance after 5 cycles (unless you double
bake or double endorse)

###Different kinds of balance updates

####Contracts
The operations updating the spendable balance.

####Deposits
The amount of Tezos you freeze when you bake or endorse a block 

####Fees
The amount of Tezos frozen by operations requiring fees

####Rewards
The amount of Tezos you earn by baking or endorsing a block
