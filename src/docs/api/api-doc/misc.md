# Misc

## `/level/<block_hash>`

### Arguments

- `block_hash`: hash of a block

Sample request:
```bash
# Get level of block with hash BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc
curl http://api.tzscan.io/v1/level/BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc
```

### Result

Sample answer:

```json
{
  "level": 7220.000000,
  "level_position": 7218.000000,
  "cycle": 112.000000,
  "cycle_position": 50.000000,
  "voting_period": 7.000000,
  "voting_period_position": 50.000000
}
```
## `/block_level/<level>`

### Arguments

- `level`: level of a block

Sample request:
```bash
# Get level of block with level 42
curl http://api.tzscan.io/v1/block_level/42
```

### Result

Sample answer:

```json
{
  "hash": "BLiRRFwJVPm9DL2SX9qL8aJhYezXexjezon2HW37NZy1DfanXDN",
  "predecessor_hash": "BLn4npSEQK4CCV3KNvwazGbeZ4LGXKHehiC1KWN6dM3NCSSHpmc",
  "fitness": "00\n00000000000001ce",
  "timestamp": "2017-11-20T02:31:22Z",
  "operations": [ [] ],
  "protocol": {
    "name": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
    "hash": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  },
  "test_protocol": {
    "name": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
    "hash": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  },
  "network": "NetXj4yEEKnjaK8",
  "test_network": "PLACEHOLDER_test_network",
  "test_network_expiration": "PLACEHOLDER_TEST_NETWORK_EXPIRATION",
  "baker": "tz1ey28xfyVvtPRPN9d43Wbf1vkPs868CGXM",
  "nb_operations": 12,
  "priority": 0,
  "level": 42,
  "commited_nonce_hash": "nceVBgxbJUcc57kerKqNCtcX24NQwHAvCa18ZPVPnWwyGUUBCY7aA",
  "pow_nonce": ""
}
```

## `/timestamp/<hash>`

### Argument

- `<hash>` : Hash of a block or an operation

## `/volume/<block_hash>`

### Argument

- `block_hash`: hash of a block
