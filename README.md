# Purescript Punycode

An implementation of Punycode as described by
[RFC3492](https://www.rfc-editor.org/rfc/rfc3492.html) in PureScript.

In addition to encoding/decoding arbitrary punycode, it also implements `toASCII` and `toUnicode` from
[RFC3490](https://www.rfc-editor.org/rfc/rfc3490.html)
for the special cases of domain names and email addresses.

## Usage

```bash
spago install punycode
```

The API is quite simple:

```purescript
encode "my unicode string"
decode "my ascii string"

toUnicode "example.com"
toASCII "example.com"
```

All functions return an `Either Error String`, so you can handle
cases where the input fails to encode/decode. There are no
side effects.

## Acknowledgements

This was influenced by the implementation in
[punycode.js](https://github.com/mathiasbynens/punycode.js/tree/main)
in addition to the pseudocode from the RFC.
