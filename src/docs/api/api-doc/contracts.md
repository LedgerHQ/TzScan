# Contracts

## `/contracts`

### Parameters (*optional*)

 - `number` = `<int>` : List `<number>` contracts.
 - `code` = `<bool>` (*default false*) : Show the code of the contracts

Sample request:
```bash
curl http://api.tzscan.io/v1/accounts
```

### Result

List of contracts in JSON

```json
[
  {
    TODO
  }
]
```

## `/contract/<contract_hash>`

Get information about the contract identified by `contract_hash`.

### Argument

- `contract_hash`: hash of an contract

### Parameter

- `operations` = `<bool>` (*default: `false`*): Retrives all operations of the given contract
- `code` = `<bool>` (*default false*) : Show the code of the contract

Sample request:
```bash
curl http://api.tzscan.io/v1/contract/TZ1dR5zkRparEAVvyjNF4xactuavFQ7sYdqy?operations=true
```

### Result

Contract in JSON

```json
{
  TODO
}
```