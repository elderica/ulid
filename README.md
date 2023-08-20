# ulid

Universally Unique Lexicographically Sortable Identifier

# Synoposis

```lisp
(asdf:load-system :ulid)

(format t "~a~&" (ulid:generate-now))
```

# API

## Type: `u128`

represents unsigned 128-bits integer.

## Type: `base32`

represents 26-characters string encoded with Crockford's Base32.

## Struct: `ulid`

holds unsigned 128-bit integer that represents unique lexicographically sortable identifier.
The first 48 bits of 128 bits are a UNIX timestamp in milliseconds for lexicographically sorting.
The remaining 80 bits are randomness that ensure the identifier is unique.

## Function: `ulid-timestamp`

returns timestamp part of `ulid`.

## Function: `ulid-randomness`

returns randomness part of `ulid`.

## Condition: `ulid-out-of-range`

are signaled when you try to decode invalid Base32 encoded string.

## Function: `ulid->u128`

converts `ulid` into unsigned 128-bits integer.

## Function `u128->base32`

encodes unsigned 128-bits integer to 26-characters Crockford's Base32.

## Function: `ulid->base32`

encodes `ulid` to 26-characters Crockford's Base32.

## Function: `base32->u128`

decodes 26-character Crockford's Base32 to unsigned 128-bit integer.

## Function: `u128->ulid`

converts unsigned 128-bits integer to `ulid`.

## Function: `base32->ulid`

decodes 26-characters Crockford's Base32 string to `ulid`.

## Function: `generate-now`

generates `ulid` from current timestamp and randomness.

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
