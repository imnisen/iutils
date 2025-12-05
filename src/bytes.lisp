(in-package :iutils)

;;;
;;; Bytes is alias of octets.
;;; Bytes is an array of (unsigned-byte 8)
;;;

(defun ensure-bytes (vector)
  "Ensure that a vector is a proper byte array (simple-array (unsigned-byte 8) (*)).
  
  Arguments:
    vector - A vector to convert to a byte array
  
  Returns:
    A byte array of type (simple-array (unsigned-byte 8) (*))
  
  Notes:
    - If the input is already a byte array, returns it unchanged
    - If the input is a simple-vector, coerces it to a byte array"
  (if (typep vector '(simple-array (unsigned-byte 8) (*)))
      vector
      (coerce vector '(simple-array (unsigned-byte 8) (*)))))

(defun make-bytes-from-list (lst &aux (length (length lst)))
  "Create a byte array from a list of integers.
  
  Arguments:
    lst - A list of integers (0-255) to convert to bytes
  
  Returns:
    A byte array (unsigned-byte 8) containing the values from the list
  
  Examples:
    (make-bytes-from-list '(72 101 108 108 111)) ; => #(72 101 108 108 111) ; \"Hello\" in ASCII
    (make-bytes-from-list '(0 255 128))           ; => #(0 255 128)
    (make-bytes-from-list nil)                    ; => #()"
  (make-array length :element-type '(unsigned-byte 8)
                     :initial-contents lst))

(defparameter *empty-byte* (make-bytes-from-list nil))

(defun make-initial-zeros-bytes (length)
  "Create a byte array of specified length filled with zeros.
  
  Arguments:
    length - The number of bytes to create
  
  Returns:
    A byte array (unsigned-byte 8) of the specified length, initialized with zeros
  
  Examples:
    (make-initial-zeros-bytes 5)  ; => #(0 0 0 0 0)
    (make-initial-zeros-bytes 0)  ; => #()"
  (make-array length :element-type '(unsigned-byte 8)))


(defun bytes->int (octet-vec &key (start 0) end (big-endian t) n-bits)
  "Convert a byte array to an integer.
  
  Arguments:
    octet-vec - The byte array to convert (any vector will be coerced)
  
  Keyword Arguments:
    start - Starting index in the byte array (default: 0)
    end - Ending index in the byte array (default: nil, meaning end of array)
    big-endian - If T, interpret bytes as big-endian; if NIL, as little-endian (default: T)
    n-bits - Number of bits to use for the integer (default: nil, auto-determined)
  
  Returns:
    An integer representation of the bytes
  
  Examples:
    (bytes->int #(0 0 0 1))                          ; => 1 (big-endian)
    (bytes->int #(1 0 0 0) :big-endian nil)        ; => 1 (little-endian)
    (bytes->int #(255 255) :n-bits 16)             ; => 65535
    (bytes->int #(0 1 2 3) :start 1 :end 3)        ; => 258 (only bytes 1-2)"
  (ironclad:octets-to-integer (ensure-bytes octet-vec) :start start :end end :big-endian big-endian :n-bits n-bits))

(defun int->bytes (bignum &key n-bits (big-endian t))
  "Convert an integer to a byte array.
  
  Arguments:
    bignum - The integer to convert to bytes
  
  Keyword Arguments:
    n-bits - Number of bits to use for representation (default: nil, auto-determined)
    big-endian - If T, produce big-endian bytes; if NIL, little-endian (default: T)
  
  Returns:
    A byte array representing the integer
  
  Examples:
    (int->bytes 1)                           ; => #(1)
    (int->bytes 256)                         ; => #(1 0)
    (int->bytes 256 :n-bits 32)              ; => #(0 0 1 0) (big-endian)
    (int->bytes 256 :big-endian nil)        ; => #(0 1) (little-endian)"
  (ironclad:integer-to-octets bignum :n-bits n-bits :big-endian big-endian))


(defun bytes->hexstr (vector &key with-0x-prefix)
  "Convert a byte array to a hexadecimal string.
  
  Arguments:
    vector - The byte array to convert (any vector will be coerced)
  
  Keyword Arguments:
    with-0x-prefix - If T, prepend \"0x\" to the result (default: NIL)
  
  Returns:
    A hexadecimal string representation of the bytes
  
  Examples:
    (bytes->hexstr #(255 0 128))                    ; => \"ff0080\"
    (bytes->hexstr #(255 0 128) :with-0x-prefix t)  ; => \"0xff0080\"
    (bytes->hexstr #(10 11 12))                     ; => \"0a0b0c\""
  (let ((hex-string (bit-smasher::byte-array-to-hex-string (ensure-bytes vector))))
    (if with-0x-prefix
        (conc-strings "0x" hex-string)
        hex-string)))

(defun hexstr->bytes (string &key with-0x-prefix)
  "Convert a hexadecimal string to a byte array.
  
  Arguments:
    string - The hexadecimal string to convert
  
  Keyword Arguments:
    with-0x-prefix - If T, expects and strips \"0x\" prefix (default: NIL)
  
  Returns:
    A byte array representing the hexadecimal values
  
  Examples:
    (hexstr->bytes \"ff0080\")                        ; => #(255 0 128)
    (hexstr->bytes \"0xff0080\" :with-0x-prefix t)    ; => #(255 0 128)
    (hexstr->bytes \"0a0b0c\")                        ; => #(10 11 12)
  
  Errors:
    Signals an error if with-0x-prefix is T but string doesn't start with \"0x\""
  (let ((hex-string (if with-0x-prefix
                        (progn
                          (when (not (string-starts-with "0x" string))
                            (error (format nil "string :~a not start with 0x" string)))
                          (subseq string 2))
                        string)))
    (bit-smasher::hex-string-to-byte-array hex-string)))

(defun int->hexstr (bignum &key n-bits (big-endian t) with-0x-prefix)
  "Convert an integer directly to a hexadecimal string.
  
  Arguments:
    bignum - The integer to convert
  
  Keyword Arguments:
    n-bits - Number of bits to use for representation (default: nil, auto-determined)
    big-endian - If T, use big-endian byte order (default: T)
    with-0x-prefix - If T, prepend \"0x\" to the result (default: NIL)
  
  Returns:
    A hexadecimal string representation of the integer
  
  Examples:
    (int->hexstr 255)                               ; => \"ff\"
    (int->hexstr 255 :with-0x-prefix t)             ; => \"0xff\"
    (int->hexstr 256 :n-bits 32)                    ; => \"00000100\""
  (bytes->hexstr (int->bytes bignum :n-bits n-bits :big-endian big-endian) :with-0x-prefix with-0x-prefix))

(defun hexstr->int (string &key (big-endian t) with-0x-prefix)
  "Convert a hexadecimal string directly to an integer.
  
  Arguments:
    string - The hexadecimal string to convert
  
  Keyword Arguments:
    big-endian - If T, interpret as big-endian (default: T)
    with-0x-prefix - If T, expects and strips \"0x\" prefix (default: NIL)
  
  Returns:
    An integer representation of the hexadecimal string
  
  Examples:
    (hexstr->int \"ff\")                              ; => 255
    (hexstr->int \"0xff\" :with-0x-prefix t)          ; => 255
    (hexstr->int \"0100\" :big-endian t)              ; => 256"
  (bytes->int (hexstr->bytes string :with-0x-prefix with-0x-prefix) :big-endian big-endian))



;; Usage scenarios:
;; bytes->str (recommended for text)
;;   ✅ Process real text data
;;   ✅ Support international characters
;;   ✅ UTF-8 encoded byte streams
;;   ❌ May throw encoding errors
;; bytes->str2 (for special cases)
;;   ✅ Direct byte-to-character mapping
;;   ✅ Debug binary data
;;   ✅ Handle Latin-1 encoding
;;   ❌ Does not support UTF-8 multi-byte characters

(defun bytes->str (vector)
  "Convert a byte array to a UTF-8 string.
  
  Arguments:
    vector - The byte array to convert
  
  Returns:
    A UTF-8 encoded string
  
  Examples:
    (bytes->str #(72 101 108 108 111))              ; => \"Hello\"
    (bytes->str #(228 189 160 229 165 189))         ; => \"你好\" (Chinese)
  
  Notes:
    - Use this for text data that may contain international characters
    - Supports proper UTF-8 decoding for multi-byte characters
    - May signal encoding errors for invalid UTF-8 sequences"
  (flexi-streams:octets-to-string vector :external-format :utf-8))

(defun bytes->str2 (vector)
  "Convert a byte array to string using direct byte-to-character mapping.
  
  Arguments:
    vector - The byte array to convert
  
  Returns:
    A string with each byte mapped to its corresponding character
  
  Examples:
    (bytes->str2 #(72 101 108 108 111))             ; => \"Hello\"
    (bytes->str2 #(255 128 0))                      ; => \"ÿ\\200\\0\" (Latin-1)
  
  Notes:
    - Use only for Latin-1 encoding or debugging binary data
    - Does NOT support UTF-8 multi-byte characters properly
    - Each byte is directly converted to a character code"
  (map 'string #'code-char (1d-array-to-list vector)))


;; TODO What's the difference here?
;; CLUTILS> (str->bytes "abc")
;; #(97 98 99)
;; CLUTILS> (str->bytes2 "abc")
;; #(97 98 99)

;; CLUTILS> (str->bytes "你好")
;; #(228 189 160 229 165 189)
;; CLUTILS> (str->bytes2 "你好")
;; #(20320 22909)


(defun str->bytes (string)
  "Convert a string to a UTF-8 encoded byte array.
  
  Arguments:
    string - The string to convert
  
  Returns:
    A byte array containing the UTF-8 encoding of the string
  
  Examples:
    (str->bytes \"Hello\")                            ; => #(72 101 108 108 111)
    (str->bytes \"你好\")                              ; => #(228 189 160 229 165 189)
    (str->bytes \"café\")                             ; => #(99 97 102 195 169)
  
  Notes:
    - Properly handles international characters using UTF-8 encoding
    - Multi-byte characters are encoded correctly"
  (flexi-streams:string-to-octets string :external-format :utf-8))

(defun str->bytes2 (string)
  "Convert a string to bytes using direct character-to-byte mapping.
  
  Arguments:
    string - The string to convert
  
  Returns:
    A byte array with character codes
  
  Examples:
    (str->bytes2 \"Hello\")                           ; => #(72 101 108 108 111)
    (str->bytes2 \"你好\")                             ; => #(20320 22909) ; Unicode code points
  
  Notes:
    - Returns character codes, NOT UTF-8 encoding
    - May produce values > 255 for non-ASCII characters
    - Use str->bytes for proper UTF-8 encoding"
  (list->1d-array (loop for c across string collect (char-code c))))


;; Add more about bit-vector related transfer functions
;; TODO


(defun conc-bytes (&rest bytes-sequences)
  "Concatenate multiple byte arrays into a single byte array.
  
  Arguments:
    bytes-sequences - Any number of byte arrays to concatenate
  
  Returns:
    A new byte array containing all input bytes in order
  
  Examples:
    (conc-bytes #(1 2) #(3 4) #(5))                 ; => #(1 2 3 4 5)
    (conc-bytes #(255) #(0 128))                    ; => #(255 0 128)
    (conc-bytes)                                    ; => #()"
  (apply #'concatenate '(vector (unsigned-byte 8)) bytes-sequences))

(defun list-of-bytes->bytes (l)
  "Convert a list of byte arrays into a single concatenated byte array.
  
  Arguments:
    l - A list of byte arrays to concatenate
  
  Returns:
    A single byte array containing all bytes from the input arrays
  
  Examples:
    (list-of-bytes->bytes '(#(1 2) #(3 4)))         ; => #(1 2 3 4)
    (list-of-bytes->bytes '(#(255) #(0) #(128)))    ; => #(255 0 128)
    (list-of-bytes->bytes nil)                      ; => #()
  
  Performance Note:
    Uses reduce with repeated concatenation. For better performance with
    large lists, consider pre-calculating total length and allocating once."
  (if (= 0 (length l))
      (make-initial-zeros-bytes 0)
      ;; TODO: This function uses reduce to concatenate byte arrays repeatedly, which may cause performance bottlenecks.
      ;; Suggestion: If the total length can be calculated in advance, allocate the target array directly and copy in bulk to avoid multiple allocations and copies.
      (reduce #'(lambda (b1 b2) (conc-bytes b1 b2)) l)))

(defun bytes-equal (b1 b2)
  "Check if two byte arrays are equal.
  
  Arguments:
    b1 - First byte array
    b2 - Second byte array
  
  Returns:
    T if the byte arrays have the same length and contents, NIL otherwise
  
  Examples:
    (bytes-equal #(1 2 3) #(1 2 3))                 ; => T
    (bytes-equal #(1 2 3) #(1 2 4))                 ; => NIL
    (bytes-equal #(1 2) #(1 2 3))                   ; => NIL"
  (equalp b1 b2))



(defun bytes->bitstr (bytes)
  "Convert bytes to their binary representation as strings.
  
  Arguments:
    bytes - A byte array to convert
  
  Returns:
    A vector of strings, each containing the 8-bit binary representation
  
  Examples:
    (bytes->bitstr #(1 16))                         ; => #(\"00000001\" \"00010000\")
    (bytes->bitstr #(255 128 0))                    ; => #(\"11111111\" \"10000000\" \"00000000\")
    (bytes->bitstr #(42))                           ; => #(\"00101010\")"
  (map 'vector
       (lambda (byte)
         (format nil "~8,'0B" byte))
       bytes))

(defun hexstr->bitstr (hexstr)
  "Convert a hexadecimal string to binary representation strings.
  
  Arguments:
    hexstr - A hexadecimal string (without 0x prefix)
  
  Returns:
    A vector of 8-bit binary strings
  
  Examples:
    (hexstr->bitstr \"09\")                           ; => #(\"00001001\")
    (hexstr->bitstr \"0901\")                         ; => #(\"00001001\" \"00000001\")
    (hexstr->bitstr \"ff80\")                         ; => #(\"11111111\" \"10000000\")
  
  Notes:
    - Each pair of hex digits becomes one 8-bit binary string
    - Input should have even number of characters"
  (bytes->bitstr (hexstr->bytes hexstr)))
