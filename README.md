# Common Lisp IUtils

A comprehensive collection of utility functions for Common Lisp development, providing convenient helpers for common programming tasks.

## Features

- **List Operations**: Extended list manipulation functions beyond standard Common Lisp
- **String Utilities**: String manipulation, conversion, and formatting tools
- **Byte Arrays**: Comprehensive byte array handling and conversion utilities
- **Cryptography**: Cryptographic hash functions (SHA1, SHA256, RIPEMD160)
- **I/O Helpers**: Enhanced input/output operations
- **Time Utilities**: Time manipulation and formatting (extends local-time)
- **Data Structures**: Hash table utilities and conversions
- **Type Conversions**: Safe type conversion functions

## Installation

### Using Quicklisp

```lisp
(ql:quickload :iutils)
```

### Manual Installation

1. Clone this repository to your local-projects directory:
```bash
cd ~/quicklisp/local-projects/
git clone https://github.com/imnisen/iutils.git
```

2. Load the system:
```lisp
(asdf:load-system :iutils)
```

## Dependencies

- alexandria
- ironclad
- bit-smasher
- local-time
- drakma
- flexi-streams
- yason
- bordeaux-threads
- cl-json
- rutils
- cl-change-case
- thnappy
- cl-yaml

## Usage Examples

### List Utilities

```lisp
;; Check if list has single element
(u:singlep '(1)) ; => T
(u:singlep '(1 2)) ; => NIL

;; Get last element
(u:last1 '(1 2 3 4)) ; => 4

;; Group list into sublists
(u:group '(1 2 3 4 5 6 7) 3) ; => ((1 2 3) (4 5 6) (7))

;; Flatten nested lists
(u:flatten '((1 2) (3 (4 5)))) ; => (1 2 3 4 5)
```

### String Operations

```lisp
;; Concatenate strings
(u:string+ "Hello" " " "World") ; => "Hello World"

;; Check string prefix/suffix
(u:string-starts-with "Hello World" "Hello") ; => T
(u:string-ends-with "Hello World" "World") ; => T

;; String to number conversion
(u:string->integer "123") ; => 123
(u:string->number "123.45") ; => 123.45
```

### Byte Array Operations

```lisp
;; Convert between strings and bytes
(u:str->bytes "Hello") ; => #(72 101 108 108 111)
(u:bytes->str #(72 101 108 108 111)) ; => "Hello"

;; Convert to/from hex strings
(u:bytes->hexstr #(255 0 128)) ; => "ff0080"
(u:hexstr->bytes "ff0080") ; => #(255 0 128)

;; Integer conversions
(u:int->bytes 1000) ; => #(0 0 3 232)
(u:bytes->int #(0 0 3 232)) ; => 1000
```

### Cryptographic Functions

```lisp
;; SHA256 hash
(u:sha256 (u:str->bytes "Hello")) 
; => #(24 95 141 179 34 113 254 37 245 97 166 252 147 139 46 38 67 6 236 48 78 218 81 128 7 209 118 72 38 56 25 105)

;; Hash from string directly
(u:sha256-from-str "Hello")

;; Other hash functions
(u:sha1 bytes)
(u:ripemd160 bytes)
(u:hash256 bytes) ; double SHA256
(u:hash160 bytes) ; SHA256 then RIPEMD160
```

### Hash Table Utilities

```lisp
;; Create and manipulate hash tables
(defparameter *ht* (u:make-hashtable :a 1 :b 2 :c 3))

;; Convert to lists
(u:hashtable-key-to-list *ht*) ; => (:A :B :C)
(u:hashtable-value-to-list *ht*) ; => (1 2 3)

;; Safe hash access
(u:must-gethash :a *ht*) ; => 1
(u:must-gethash :d *ht*) ; => Error: Key not found
```

### Time Operations

```lisp
;; Get current Unix timestamp
(u:now-unix) ; => 1701234567

;; All local-time exports are also available through iutils
(u:now) ; => @2024-11-28T12:34:56.789000+00:00
(u:format-timestring nil (u:now)) ; => "2024-11-28T12:34:56.789000+00:00"
```

## API Reference

For detailed API documentation, see [API.md](API.md).

## Testing

Run the test suite:

```lisp
(asdf:test-system :iutils-test)
```

Or use the test runner script:

```bash
sbcl --script run-tests.lisp
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more details.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Author

- imnisen

## Acknowledgments

This library builds upon and integrates functionality from several excellent Common Lisp libraries. Special thanks to the authors and contributors of the dependency libraries.

