# Bencode

A Bencoding parser and serializer for [CHICKEN scheme](http://call-cc.org).

Usage:

```scheme
;; defaults to current-port if no port provided
(read-bencode [port])
(write-bencode data [port])
```

Conversion to/from scheme types:

```
Bencode     | Scheme
----------------------------------------
string      | string
integer     | number
list        | vector
dictionary  | alist in (symbol . *) form
```

Attempting to bencode a scheme type not on the above list,
or attempting to encode a non-integer number will cause
an exception.
