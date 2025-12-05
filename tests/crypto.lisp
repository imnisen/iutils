(in-package :cl-user)
(defpackage iutils-test.crypto
  (:use :cl :iutils :prove))
(in-package :iutils-test.crypto)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test sha256-from-str
(subtest "sha256-from-str"
  ;; Test with known SHA256 hashes
  (is (bytes->hexstr (sha256-from-str "hello")) 
      "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
  (is (bytes->hexstr (sha256-from-str "")) 
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
  (is (bytes->hexstr (sha256-from-str "The quick brown fox jumps over the lazy dog"))
      "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")
  (is (bytes->hexstr (sha256-from-str "abc"))
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))

;; Test sha1
(subtest "sha1"
  ;; Test with known SHA1 hashes
  (let ((hello-bytes (str->bytes "hello")))
    (is (bytes->hexstr (sha1 hello-bytes))
        "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"))
  
  (let ((empty-bytes (str->bytes "")))
    (is (bytes->hexstr (sha1 empty-bytes))
        "da39a3ee5e6b4b0d3255bfef95601890afd80709"))
  
  (let ((fox-bytes (str->bytes "The quick brown fox jumps over the lazy dog")))
    (is (bytes->hexstr (sha1 fox-bytes))
        "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")))

;; Test sha256
(subtest "sha256"
  ;; Test with known SHA256 hashes
  (let ((hello-bytes (str->bytes "hello")))
    (is (bytes->hexstr (sha256 hello-bytes))
        "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"))
  
  (let ((empty-bytes (str->bytes "")))
    (is (bytes->hexstr (sha256 empty-bytes))
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
  
  (let ((abc-bytes (str->bytes "abc")))
    (is (bytes->hexstr (sha256 abc-bytes))
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))

;; Test ripemd160
(subtest "ripemd160"
  ;; Test with known RIPEMD-160 hashes
  (let ((hello-bytes (str->bytes "hello")))
    (is (bytes->hexstr (ripemd160 hello-bytes))
        "108f07b8382412612c048d07d13f814118445acd"))
  
  (let ((empty-bytes (str->bytes "")))
    (is (bytes->hexstr (ripemd160 empty-bytes))
        "9c1185a5c5e9fc54612808977ee8f548b2258d31"))
  
  (let ((abc-bytes (str->bytes "abc")))
    (is (bytes->hexstr (ripemd160 abc-bytes))
        "8eb208f7e05d987a9b044a8e98c6b087f15a0bfc")))

;; Test hash256 (double SHA256)
(subtest "hash256"
  ;; hash256 is SHA256(SHA256(data))
  (let ((hello-bytes (str->bytes "hello")))
    ;; First verify our intermediate SHA256
    (let ((first-hash (sha256 hello-bytes)))
      (is (bytes->hexstr first-hash)
          "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
      ;; Then verify the double hash
      (is (bytes->hexstr (hash256 hello-bytes))
          "9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50")))
  
  ;; Test with empty string
  (let ((empty-bytes (str->bytes "")))
    (is (bytes->hexstr (hash256 empty-bytes))
        "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456")))

;; Test hash160 (SHA256 then RIPEMD-160)
(subtest "hash160"
  ;; hash160 is RIPEMD-160(SHA256(data))
  (let ((hello-bytes (str->bytes "hello")))
    ;; First verify our intermediate SHA256
    (let ((sha256-hash (sha256 hello-bytes)))
      (is (bytes->hexstr sha256-hash)
          "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
      ;; Then verify the hash160 result
      (is (bytes->hexstr (hash160 hello-bytes))
          "b6a9c8c230722b7c748331a8b450f05566dc7d0f")))
  
  ;; Test with empty string
  (let ((empty-bytes (str->bytes "")))
    (is (bytes->hexstr (hash160 empty-bytes))
        "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb")))

;; Test random-bytes
(subtest "random-bytes"
  ;; Test that it returns the correct length
  (is (length (random-bytes 16)) 16)
  (is (length (random-bytes 32)) 32)
  (is (length (random-bytes 0)) 0)
  
  ;; Test that it returns different values (probabilistic test)
  (let ((r1 (random-bytes 16))
        (r2 (random-bytes 16)))
    (ok (not (bytes-equal r1 r2)) "random-bytes should return different values"))
  
  ;; Test that all values are bytes (0-255)
  (let ((random-data (random-bytes 100)))
    (ok (every (lambda (b) (and (>= b 0) (<= b 255))) 
               (coerce random-data 'list))
        "All values should be valid bytes (0-255)")))

;; Test integration - using multiple functions together
(subtest "integration tests"
  ;; Test converting string -> bytes -> hash -> hex
  (let ((test-str "test message"))
    (is (bytes->hexstr (sha256-from-str test-str))
        (bytes->hexstr (sha256 (str->bytes test-str)))))
  
  ;; Test that hash functions work with various input types
  (let ((test-bytes #(1 2 3 4 5)))
    (is-type (sha1 test-bytes) 'simple-array)
    (is-type (sha256 test-bytes) 'simple-array)
    (is-type (ripemd160 test-bytes) 'simple-array)
    (is-type (hash256 test-bytes) 'simple-array)
    (is-type (hash160 test-bytes) 'simple-array)))

(finalize)
