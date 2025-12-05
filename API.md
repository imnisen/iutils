# iutils API Documentation

This document provides a comprehensive reference for all exported functions in the iutils library.

## Table of Contents

- [Byte Operations](#byte-operations)
- [Cryptographic Functions](#cryptographic-functions)
- [String Utilities](#string-utilities)
- [List Operations](#list-operations)
- [I/O Operations](#io-operations)
- [Hash Tables](#hash-tables)
- [Time Functions](#time-functions)
- [Number Utilities](#number-utilities)
- [File Operations](#file-operations)
- [Stream Operations](#stream-operations)
- [Boolean Operations](#boolean-operations)
- [Other Utilities](#other-utilities)

## Byte Operations

Functions for working with byte arrays (unsigned-byte 8).

### `make-bytes-from-list`
```lisp
(make-bytes-from-list lst)
```
Create a byte array from a list of integers.

**Arguments:**
- `lst` - A list of integers (0-255) to convert to bytes

**Returns:** A byte array containing the values from the list

**Example:**
```lisp
(make-bytes-from-list '(72 101 108 108 111)) ; => #(72 101 108 108 111)
```

### `make-initial-zeros-bytes`
```lisp
(make-initial-zeros-bytes length)
```
Create a byte array of specified length filled with zeros.

**Arguments:**
- `length` - The number of bytes to create

**Returns:** A byte array of the specified length, initialized with zeros

**Example:**
```lisp
(make-initial-zeros-bytes 5) ; => #(0 0 0 0 0)
```

### `bytes->int`
```lisp
(bytes->int octet-vec &key (start 0) end (big-endian t) n-bits)
```
Convert a byte array to an integer.

**Arguments:**
- `octet-vec` - The byte array to convert
- `:start` - Starting index (default: 0)
- `:end` - Ending index (default: nil, meaning end of array)
- `:big-endian` - If T, interpret as big-endian (default: T)
- `:n-bits` - Number of bits to use (default: nil, auto-determined)

**Returns:** An integer representation of the bytes

**Example:**
```lisp
(bytes->int #(0 0 0 1))                    ; => 1 (big-endian)
(bytes->int #(1 0 0 0) :big-endian nil)   ; => 1 (little-endian)
```

### `int->bytes`
```lisp
(int->bytes bignum &key n-bits (big-endian t))
```
Convert an integer to a byte array.

**Arguments:**
- `bignum` - The integer to convert
- `:n-bits` - Number of bits for representation (default: nil)
- `:big-endian` - If T, produce big-endian bytes (default: T)

**Returns:** A byte array representing the integer

**Example:**
```lisp
(int->bytes 256)                    ; => #(1 0)
(int->bytes 256 :n-bits 32)         ; => #(0 0 1 0)
```

### `bytes->hexstr`
```lisp
(bytes->hexstr vector &key with-0x-prefix)
```
Convert a byte array to a hexadecimal string.

**Arguments:**
- `vector` - The byte array to convert
- `:with-0x-prefix` - If T, prepend "0x" (default: NIL)

**Returns:** A hexadecimal string representation

**Example:**
```lisp
(bytes->hexstr #(255 0 128))                    ; => "ff0080"
(bytes->hexstr #(255 0 128) :with-0x-prefix t)  ; => "0xff0080"
```

### `hexstr->bytes`
```lisp
(hexstr->bytes string &key with-0x-prefix)
```
Convert a hexadecimal string to a byte array.

**Arguments:**
- `string` - The hexadecimal string to convert
- `:with-0x-prefix` - If T, expects and strips "0x" prefix (default: NIL)

**Returns:** A byte array representing the hexadecimal values

**Example:**
```lisp
(hexstr->bytes "ff0080")                        ; => #(255 0 128)
(hexstr->bytes "0xff0080" :with-0x-prefix t)    ; => #(255 0 128)
```

### `int->hexstr`
```lisp
(int->hexstr bignum &key n-bits (big-endian t) with-0x-prefix)
```
Convert an integer directly to a hexadecimal string.

**Example:**
```lisp
(int->hexstr 255)                    ; => "ff"
(int->hexstr 255 :with-0x-prefix t)  ; => "0xff"
```

### `hexstr->int`
```lisp
(hexstr->int string &key (big-endian t) with-0x-prefix)
```
Convert a hexadecimal string directly to an integer.

**Example:**
```lisp
(hexstr->int "ff")                           ; => 255
(hexstr->int "0xff" :with-0x-prefix t)       ; => 255
```

### `bytes->str`
```lisp
(bytes->str vector)
```
Convert a byte array to a UTF-8 string. Use this for text data that may contain international characters.

**Example:**
```lisp
(bytes->str #(72 101 108 108 111))      ; => "Hello"
(bytes->str #(228 189 160 229 165 189)) ; => "你好"
```

### `str->bytes`
```lisp
(str->bytes string)
```
Convert a string to a UTF-8 encoded byte array.

**Example:**
```lisp
(str->bytes "Hello")  ; => #(72 101 108 108 111)
(str->bytes "你好")    ; => #(228 189 160 229 165 189)
```

### `bytes->str2`
```lisp
(bytes->str2 vector)
```
Convert a byte array to string using direct byte-to-character mapping. Use only for Latin-1 encoding or debugging binary data.

### `str->bytes2`
```lisp
(str->bytes2 string)
```
Convert a string to bytes using direct character-to-byte mapping. Returns character codes, NOT UTF-8 encoding.

### `conc-bytes`
```lisp
(conc-bytes &rest bytes-sequences)
```
Concatenate multiple byte arrays into a single byte array.

**Example:**
```lisp
(conc-bytes #(1 2) #(3 4) #(5)) ; => #(1 2 3 4 5)
```

### `bytes-equal`
```lisp
(bytes-equal b1 b2)
```
Check if two byte arrays are equal.

**Example:**
```lisp
(bytes-equal #(1 2 3) #(1 2 3)) ; => T
(bytes-equal #(1 2 3) #(1 2 4)) ; => NIL
```

### `bytes->bitstr`
```lisp
(bytes->bitstr bytes)
```
Convert bytes to their binary representation as strings.

**Example:**
```lisp
(bytes->bitstr #(1 16)) ; => #("00000001" "00010000")
```

### `hexstr->bitstr`
```lisp
(hexstr->bitstr hexstr)
```
Convert a hexadecimal string to binary representation strings.

**Example:**
```lisp
(hexstr->bitstr "09")   ; => #("00001001")
(hexstr->bitstr "ff80") ; => #("11111111" "10000000")
```

### `list-of-bytes->bytes`
```lisp
(list-of-bytes->bytes l)
```
Convert a list of byte arrays into a single concatenated byte array.

**Example:**
```lisp
(list-of-bytes->bytes '(#(1 2) #(3 4))) ; => #(1 2 3 4)
```

## Cryptographic Functions

Cryptographic hash functions and random number generation.

### `sha256`
```lisp
(sha256 bytes)
```
Compute SHA-256 hash of a byte array.

**Arguments:**
- `bytes` - A byte array to hash

**Returns:** A byte array containing the 32-byte SHA-256 hash

**Example:**
```lisp
(bytes->hexstr (sha256 (str->bytes "hello")))
; => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
```

### `sha256-from-str`
```lisp
(sha256-from-str str)
```
Compute SHA-256 hash of a string.

**Arguments:**
- `str` - The string to hash

**Returns:** A byte array containing the 32-byte SHA-256 hash

**Example:**
```lisp
(bytes->hexstr (sha256-from-str "hello"))
; => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
```

### `sha1`
```lisp
(sha1 bytes)
```
Compute SHA-1 hash of a byte array. Note: SHA-1 is considered weak for cryptographic purposes.

**Returns:** A byte array containing the 20-byte SHA-1 hash

**Example:**
```lisp
(bytes->hexstr (sha1 (str->bytes "hello")))
; => "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
```

### `ripemd160`
```lisp
(ripemd160 bytes)
```
Compute RIPEMD-160 hash of a byte array.

**Returns:** A byte array containing the 20-byte RIPEMD-160 hash

**Example:**
```lisp
(bytes->hexstr (ripemd160 (str->bytes "hello")))
; => "108f07b8382412612c048d07d13f814118445acd"
```

### `hash256`
```lisp
(hash256 bytes)
```
Compute double SHA-256 hash (SHA-256(SHA-256(bytes))). Used extensively in Bitcoin.

**Returns:** A byte array containing the 32-byte double SHA-256 hash

**Example:**
```lisp
(bytes->hexstr (hash256 (str->bytes "hello")))
; => "9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50"
```

### `hash160`
```lisp
(hash160 bytes)
```
Compute RIPEMD-160(SHA-256(bytes)) hash. Used for generating Bitcoin addresses.

**Returns:** A byte array containing the 20-byte hash

**Example:**
```lisp
(bytes->hexstr (hash160 (str->bytes "hello")))
; => "b6a9c8c230722b7c748331a8b450f05566dc7d0f"
```

### `random-bytes`
```lisp
(random-bytes size)
```
Generate cryptographically secure random bytes.

**Arguments:**
- `size` - Number of random bytes to generate

**Returns:** A byte array filled with random bytes

**Example:**
```lisp
(random-bytes 16)  ; => #(... 16 random bytes ...)
```

## String Utilities

Functions for string manipulation and conversion.

### `string+` / `conc-strings`
```lisp
(string+ &rest strings)
(conc-strings &rest strings)
```
Concatenate multiple strings into one. Both functions are aliases.

**Example:**
```lisp
(string+ "Hello" " " "World")    ; => "Hello World"
(conc-strings "foo" "bar" "baz") ; => "foobarbaz"
```

### `string-index`
```lisp
(string-index str i &key (charp nil))
```
Access a character in a string by index (supports negative indexing).

**Arguments:**
- `str` - The string to index into
- `i` - The index (negative values count from end)
- `:charp` - If T, return a character; if NIL, return a string (default: NIL)

**Example:**
```lisp
(string-index "hello" 0)           ; => "h"
(string-index "hello" -1)          ; => "o"
(string-index "hello" 1 :charp t)  ; => #\e
```

### `ensure-string`
```lisp
(ensure-string x)
```
Convert any value to its string representation.

**Example:**
```lisp
(ensure-string 42)        ; => "42"
(ensure-string 'symbol)   ; => "SYMBOL"
(ensure-string '(+ 1 2))  ; => "3" (evaluates lists)
```

### `string->integer`
```lisp
(string->integer a-string)
```
Convert a string to an integer. Signals an error if the string contains non-numeric characters.

**Example:**
```lisp
(string->integer "123")  ; => 123
(string->integer "-456") ; => -456
```

### `string->number`
```lisp
(string->number a-string)
```
Convert a string to any numeric type (integer, float, ratio).

**Example:**
```lisp
(string->number "123")    ; => 123
(string->number "3.14")   ; => 3.14
(string->number "1/2")    ; => 1/2
(string->number "-4.5e2") ; => -450.0
```

### `string-starts-with`
```lisp
(string-starts-with prefix s)
```
Check if a string starts with a given prefix.

**Example:**
```lisp
(string-starts-with "Hello" "Hello World") ; => T
(string-starts-with "Hi" "Hello World")    ; => NIL
```

### `string-ends-with`
```lisp
(string-ends-with suffix s)
```
Check if a string ends with a given suffix.

**Example:**
```lisp
(string-ends-with "World" "Hello World") ; => T
(string-ends-with "world" "Hello World") ; => NIL
```

### `string-contains-p`
```lisp
(string-contains-p substring string)
```
Check if a string contains a substring.

**Example:**
```lisp
(string-contains-p "ell" "Hello World") ; => T
(string-contains-p "xyz" "Hello World") ; => NIL
```

## List Operations

Comprehensive list manipulation functions are available in `lists.lisp`. Key functions include:

- `ensure-list` - Ensure value is a list
- `flatten` - Flatten nested lists
- `filter` - Filter list elements
- `shuffle` - Randomly shuffle a list
- `group` - Group list elements
- `list->hashtable` - Convert association list to hash table
- And many more...

## Other Utilities

The library also provides utilities for:

- **I/O Operations**: `readlist`, `prompt`, `break-loop`
- **Hash Tables**: `make-hashtable`, `set-hashtable`, `print-hashtable`
- **Time Functions**: `now-unix`, plus re-exported local-time functions
- **Number Utilities**: `ensure-number`, `number->string`
- **File Operations**: `read-file-to-bytes`
- **Stream Operations**: `bytes->input-stream`, `read-all-bytes`
- **Boolean Operations**: `ensure-bool`
- **YAML Support**: `read-yaml-file`
- **Snappy Compression**: `decode-snappy-bytes`, `decode-snappy-file`

For detailed documentation on these functions, please refer to their docstrings in the source files.
