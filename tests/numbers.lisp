(in-package :cl-user)
(defpackage iutils-test.numbers
  (:use :cl :iutils :prove))
(in-package :iutils-test.numbers)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test ensure-number
(subtest "ensure-number"
  ;; Test with numbers (should return as-is)
  (is (ensure-number 42) 42)
  (is (ensure-number -123) -123)
  (is (ensure-number 0) 0)
  (is (ensure-number 3.14) 3.14)
  
  ;; Test with strings
  (is (ensure-number "123") 123)
  (is (ensure-number "-456") -456)
  (is (ensure-number "0") 0)
  (is (ensure-number "+789") 789)
  
  ;; Test with strings containing junk (parse-integer with :junk-allowed t)
  (is (ensure-number "123abc") 123
      "Should parse number part before junk")
  (is (ensure-number "42 and some text") 42
      "Should parse initial number")
  
  ;; Test cases that return NIL (non-numeric strings with :junk-allowed t)
  (is (ensure-number "abc") nil
      "Should return NIL for non-numeric string")
  (is (ensure-number "") nil
      "Should return NIL for empty string")
  
  ;; Test error cases (non-string, non-number types)
  (is-error (ensure-number 'symbol) 'error
            "Should error on symbol")
  (is-error (ensure-number nil) 'error
            "Should error on nil")
  (is-error (ensure-number '(1 2 3)) 'error
            "Should error on list"))

;; Test number->string
(subtest "number->string"
  ;; Test basic conversion (base 10 - default)
  (is (number->string 123) "123")
  (is (number->string -456) "-456")
  (is (number->string 0) "0")
  (is (number->string 3.14) "3.14")
  
  ;; Test with different bases
  (is (number->string 255 :base 16) "FF")
  (is (number->string 255 :base 2) "11111111")
  (is (number->string 255 :base 8) "377")
  
  ;; Test more base conversions
  (is (number->string 10 :base 2) "1010")
  (is (number->string 10 :base 16) "A")
  (is (number->string 100 :base 16) "64")
  
  ;; Test edge cases
  (is (number->string 0 :base 2) "0")
  (is (number->string 0 :base 16) "0")
  (is (number->string 1 :base 2) "1")
  
  ;; Test round-trip conversion with string->number
  (let ((test-numbers '(0 1 42 255 1000 -123)))
    (loop for n in test-numbers
          do (is (string->number (number->string n)) n
                 (format nil "Round-trip conversion for ~a" n)))))

;; Integration tests
(subtest "integration tests"
  ;; Test ensure-number with number->string
  (is (number->string (ensure-number "42")) "42"
      "Parse string and convert back")
  
  ;; Test with different bases
  (is (number->string (ensure-number "255") :base 16) "FF"
      "Parse decimal string and convert to hex")
  
  ;; Test type checking pipeline
  (let ((mixed-inputs '(123 "456")))
    (loop for input in mixed-inputs
          do (is-type (ensure-number input) 'number
                      (format nil "ensure-number should always return a number for ~a" input)))))

(finalize)
