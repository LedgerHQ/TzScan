# Blocks

## `/blocks[?arg=<value>]`

### Parameters (*optional*)

- `operations` = `<bool>` (*default: `false`*): Get the operations hashes in the blocks.
- `number` = `<int>` (*default: `10`*): The number of asked blocks.
- `start_at_block` = `<block_hash>` (*default: `<head>`*): Retrieves `number` blocks from block with hash `<block_hash>`
- `end_at_block` = `block_hash` : Retrieves `number` blocks from `start_at_block` until hash `<block_hash>`
- `start_at_time` = `string` (format: `2017-09-27T11:30:48Z`)
- `end_at_time` = `string` (format: `2017-09-27T11:30:48Z`)
- `start_at_level` = `int`
- `end_at_level` = `int`

Sample requests:
```bash
# Last 10 blocks
curl http://api.tzscan.io/v1/blocks

# Last 50 blocks before `<hash>`
curl http://api.tzscan.io/v1/blocks?count=50&start_at_block=BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc
```

### Result

List of JSON blocks

Sample answer:
```json
[
  {
    "hash": "BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc",
    "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
    "operations" : [],
    "net_id": "NetXsYiJsDBLhon",
    "level": 7220.000000,
    "proto": 1.000000,
    "predecessor": "BMbydHaNhmc4rdX5MjtM3dXW25tZmwzAWUR44GQxLj58UGJ2Vgv",
    "timestamp": "2017-09-27T11:30:48Z",
    "operations_hash": "LLoaGLRPRx3Zf8kB4ACtgku8F4feeBiskeb41J1ciwfcXB3KzHKXc",
    "fitness": [ "00", "000000000001785d" ],
    "data": "00005a48baedd8dc7f4c88f72b179a719a8cfd40941d953a5d32ff84f4d6aec6f4d603c61d8372f81daa90be5a3dd5dbf85fe310f36cb44e2b4ec281a40ce178b1880261a7c4538a3c5aa2945ba60da492564b98482c11d3766f4392ce76fc8380d56df4995e2ab73e04"
  }
]
```

## `/block/<block_hash>`

### Argument

- `block_hash`: retrieve info on block with hash `<block_hash>`

### Parameter (*optional*)

 - `operations` = `<bool>` (*default: `false`*): Get the operations hashes in the blocks.


Sample request:
```bash
# Get block with hash BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc
curl http://api.tzscan.io/v1/block/BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc

# Get block with hash BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc and its operations
curl http://api.tzscan.io/v1/block/BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc
```

### Result

A single block

Sample answer:
```json
{
  "hash": "BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc",
  "protocol": "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK",
  "operations" : [],
  "net_id": "NetXsYiJsDBLhon",
  "level": 7220.000000,
  "proto": 1.000000,
  "predecessor": "BMbydHaNhmc4rdX5MjtM3dXW25tZmwzAWUR44GQxLj58UGJ2Vgv",
  "timestamp": "2017-09-27T11:30:48Z",
  "operations_hash": "LLoaGLRPRx3Zf8kB4ACtgku8F4feeBiskeb41J1ciwfcXB3KzHKXc",
  "fitness": [ "00", "000000000001785d" ],
  "data": "00005a48baedd8dc7f4c88f72b179a719a8cfd40941d953a5d32ff84f4d6aec6f4d603c61d8372f81daa90be5a3dd5dbf85fe310f36cb44e2b4ec281a40ce178b1880261a7c4538a3c5aa2945ba60da492564b98482c11d3766f4392ce76fc8380d56df4995e2ab73e04"
}
```

## Shortcuts
### `/head` same as `/block/<head_hash>`
### `/genesis` same as `/block/<genesis_hash>`
