(in-package :iutils)

;; sequence must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8))
;; use the below implementation
;; (defun sha256 (sequence)
;;   (ironclad:digest-sequence :sha256 sequence))

(defun sha256-from-str (str)
  "Compute SHA-256 hash of a string.
  
  Arguments:
    str - The string to hash
  
  Returns:
    A byte array containing the 32-byte SHA-256 hash
  
  Examples:
    (sha256-from-str \"hello\")    ; => 32-byte hash
    (bytes->hexstr (sha256-from-str \"hello\"))  ; => \"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\"
  
  Notes:
    - The string is first converted to UTF-8 bytes before hashing
    - Returns a 32-byte (256-bit) hash"
  (ironclad:digest-sequence :sha256 (iutils:str->bytes str)))

;;; copy from https://github.com/mrwhythat/bp/crypto/hash.lisp
(defun sha1 (bytes)
  "Compute SHA-1 hash of a byte array.
  
  Arguments:
    bytes - A byte array to hash (any vector will be coerced)
  
  Returns:
    A byte array containing the 20-byte SHA-1 hash
  
  Examples:
    (sha1 #(72 101 108 108 111))  ; => 20-byte hash of \"Hello\"
    (bytes->hexstr (sha1 (str->bytes \"hello\")))  ; => \"aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d\"
  
  Notes:
    - SHA-1 is considered weak for cryptographic purposes
    - Returns a 20-byte (160-bit) hash
    - Consider using SHA-256 or SHA-3 for new applications"
  (let ((digester (ironclad:make-digest 'ironclad:sha1))
        (byte-array (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
                        bytes
                        (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (ironclad:update-digest digester byte-array)
    (ironclad:produce-digest digester)))

(defun sha256 (bytes)
  "Compute SHA-256 hash of a byte array.
  
  Arguments:
    bytes - A byte array to hash (any vector will be coerced)
  
  Returns:
    A byte array containing the 32-byte SHA-256 hash
  
  Examples:
    (sha256 #(72 101 108 108 111))  ; => 32-byte hash of \"Hello\"
    (bytes->hexstr (sha256 (str->bytes \"hello\")))  ; => \"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\"
  
  Notes:
    - SHA-256 is part of the SHA-2 family
    - Returns a 32-byte (256-bit) hash
    - Suitable for most cryptographic applications"
  (let ((digester (ironclad:make-digest 'ironclad:sha256))
        (byte-array (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
                        bytes
                        (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (ironclad:update-digest digester byte-array)
    (ironclad:produce-digest digester)))

(defun ripemd160 (bytes)
  "Compute RIPEMD-160 hash of a byte array.
  
  Arguments:
    bytes - A byte array to hash (any vector will be coerced)
  
  Returns:
    A byte array containing the 20-byte RIPEMD-160 hash
  
  Examples:
    (ripemd160 #(72 101 108 108 111))  ; => 20-byte hash
    (bytes->hexstr (ripemd160 (str->bytes \"hello\")))  ; => \"108f07b8382412612c048d07d13f814118445acd\"
  
  Notes:
    - RIPEMD-160 produces a 20-byte (160-bit) hash
    - Commonly used in Bitcoin addresses
    - Less common than SHA family but still secure"
  (let ((digester (ironclad:make-digest 'ironclad:ripemd-160))
        (byte-array (if (typep bytes '(simple-array (unsigned-byte 8) (*)))
                        bytes
                        (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (ironclad:update-digest digester byte-array)
    (ironclad:produce-digest digester)))

(defun hash256 (bytes)
  "Compute double SHA-256 hash (SHA-256(SHA-256(bytes))).
  
  Arguments:
    bytes - A byte array to hash twice
  
  Returns:
    A byte array containing the 32-byte double SHA-256 hash
  
  Examples:
    (hash256 #(1 2 3 4))  ; => 32-byte hash
    (bytes->hexstr (hash256 (str->bytes \"hello\")))  ; => \"9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50\"
  
  Notes:
    - Also known as SHA-256d or double SHA-256
    - Used extensively in Bitcoin for block hashing
    - Provides protection against length extension attacks"
  (sha256 (sha256 bytes)))

(defun hash160 (bytes)
  "Compute RIPEMD-160(SHA-256(bytes)) hash.
  
  Arguments:
    bytes - A byte array to hash
  
  Returns:
    A byte array containing the 20-byte hash
  
  Examples:
    (hash160 #(1 2 3 4))  ; => 20-byte hash
    (bytes->hexstr (hash160 (str->bytes \"hello\")))  ; => \"b6a9c8c230722b7c748331a8b450f05566dc7d0f\"
  
  Notes:
    - Also known as Bitcoin's HASH160
    - Used for generating Bitcoin addresses from public keys
    - Combines SHA-256's security with RIPEMD-160's shorter output"
  (ripemd160 (sha256 bytes)))

;;; copy from https://github.com/mrwhythat/bp/crypto/random.lisp
(defun random-bytes (size)
  "Generate cryptographically secure random bytes.
  
  Arguments:
    size - Number of random bytes to generate
  
  Returns:
    A byte array of the specified size filled with random bytes
  
  Examples:
    (random-bytes 16)   ; => #(... 16 random bytes ...)
    (random-bytes 32)   ; => #(... 32 random bytes ...)
    (length (random-bytes 64))  ; => 64
  
  Notes:
    - Uses /dev/urandom for cryptographically secure randomness
    - Suitable for generating keys, nonces, and salts
    - On non-Unix systems, this may need platform-specific implementation"
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence buffer urandom))
    buffer))

;;; secp256k1 refert to copy from https://github.com/mrwhythat/bp/crypto/secp256k1.lisp
;;; didn't copy here because it relay on cffi.

