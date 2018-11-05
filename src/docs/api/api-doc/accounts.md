# Accounts

## `/accounts`

### Parameter (*optional*)

 - `number` = `<int>` : List `<number>` accounts.

Sample request:
```bash
curl http://api.tzscan.io/v1/accounts
```

### Result

List of accounts in JSON

```json
[
  {
    "hash": "TZ1dR5zkRparEAVvyjNF4xactuavFQ7sYdqy",
    "counter": 3,
    "manager": "tz1a7kVfHfmwW4YroFP6483eLA8vMkxrfEBP",
    "delegate": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
    "spendable": true,
    "delegatable": true,
    "balance": 10000,
    "operations": ["oouRh4AuQXMpjdagfAobPhKCXDxqKV1tN5hAWiFKKpbJ7YdC2V5"]
  }
]
```

## `/account/<account_hash>`

Get information about the account identified by `account_hash`.

### Arguments

- `account_hash`: hash of an account

### Parameter

- `operations` = `<bool>` (*default: `false`*): Retrives all operations of the given account

Sample request:
```bash
curl http://api.tzscan.io/v1/accounts/TZ1dR5zkRparEAVvyjNF4xactuavFQ7sYdqy?operations=true
```

### Result

Account in JSON

```json
{
  "hash": "TZ1dR5zkRparEAVvyjNF4xactuavFQ7sYdqy",
  "counter": 3,
  "manager": "tz1a7kVfHfmwW4YroFP6483eLA8vMkxrfEBP",
  "delegate": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
  "spendable": true,
  "delegatable": true,
  "balance": 10000,
  "operations": ["oouRh4AuQXMpjdagfAobPhKCXDxqKV1tN5hAWiFKKpbJ7YdC2V5"]
}
```