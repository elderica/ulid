# ulid

Universally Unique Lexicographically Sortable Identifier

# API

## Struct: `ulid`

holds unsigned 128-bit integer that represents unique lexicographically sortable identifier.
The first 48 bits of 128 bits are a UNIX timestamp in milliseconds for lexicographically sorting.
The remaining 80 bits are randomness that ensure the identifier is unique.

## Function: `base32->u128`

decodes 26-character Crockford's Base32 to unsigned 128-bit integer.

## Function `u128->base32`

encodes unsigned 128-bit integer to 26-character Crockford's Base32.

## Condition: `base32-timestamp-overflow`

# Install

First, you should clone this repository to `~/common-lisp/ulid`.

Try ULID generation on your shell:

```
$ make build
$ ./ulid
```

You can copy `./ulid` binary anywhere you like.


---

Licence: BSD
