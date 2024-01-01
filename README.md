# Purescript Punycode

An implementation of [RFC3492](https://www.rfc-editor.org/rfc/rfc3492.html#section-7.1) in PureScript.

## Usage

```bash
spago install punycode
```

The API is quite simple:

```purescript
encode "my unicode string"
decode "my ascii string"
```

## Acknowledgements

This was influenced by the implementation in
[punycode.js](https://github.com/mathiasbynens/punycode.js/tree/main)
in addition to the pseudocode from the RFC.
