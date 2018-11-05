# Operations


## `/operations/<block_hash>`

### Argument (*optional*)

 - `<block_hash>`: Gets the operations of the block with the corresponding hash.

### Parameters (*optional*)


- `type` = `<string>` (*default: All*): Filter by the type of operation. Accepted type is : `All`, `Transaction`, `Endorsement`, `Origination`, `Delegation`, `Proposal`, `Ballot`, `NonceRevelation`
- `number` = `<int>` (*default: `10`*): The number of asked blocks.
- `start_at_operation` = `<operation_hash>` (*default: `<head>`*): Retrieves `number` operations from operation with hash `<operation_hash>`
- `end_at_operation` = `operation_hash` : Retrieves `number` operations from `start_at_operation` until hash `<operation_hash>`
- `start_at_time` = `string` (format: `2017-09-27T11:30:48Z`)
- `end_at_time` = `string` (format: `2017-09-27T11:30:48Z`)
- `start_at_level` = `int`
- `end_at_level` = `int`

Sample request:
```bash
curl http://api.tzscan.io/v1/operations
```

### Result

List of operations in JSON

```json
[
  {
    "hash": "oouRh4AuQXMpjdagfAobPhKCXDxqKV1tN5hAWiFKKpbJ7YdC2V5",
    "block hash": "BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc",
    "network hash": "NetXsYiJsDBLhon",
    "type": {
      ...
      [attributes depending on operation type]
      ...
    }
  }
]
```

## `/operation/<operation_hash>`

Get information about the operation identified by `operation_hash`.

### Arguments

- `<operation_hash>`: hash of an operation

Sample request:
```bash
curl http://api.tzscan.io/v1/operations/oouRh4AuQXMpjdagfAobPhKCXDxqKV1tN5hAWiFKKpbJ7YdC2V5
```

### Result

Operation in JSON

```json
{
  "hash": "oouRh4AuQXMpjdagfAobPhKCXDxqKV1tN5hAWiFKKpbJ7YdC2V5",
  "block hash": "BLGbB521BVud9VFLPaXEezuDoqdDGTWvciU8rVctDUHUtZSiaHc",
  "network hash": "NetXsYiJsDBLhon",
  "type": {
    ...
    [attributes depending on operation type]
    ...
  }
}
```
