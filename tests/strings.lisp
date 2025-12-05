(in-package :cl-user)
(defpackage iutils-test.strings
  (:use :cl :iutils :prove))
(in-package :iutils-test.strings)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test split-string-to-integer-list
;; Note: This function parses space/whitespace separated integers, not individual digits
(subtest "split-string-to-integer-list"
  (is (split-string-to-integer-list "1 2 3 4 5") '(1 2 3 4 5))
  (is (split-string-to-integer-list "9 8 7 6 5 4 3 2 1 0") '(9 8 7 6 5 4 3 2 1 0))
  (is (split-string-to-integer-list "") '())
  (is (split-string-to-integer-list "0 0 7") '(0 0 7))
  (is (split-string-to-integer-list "12345") '(12345))  ; single integer
  (is (split-string-to-integer-list "10 20 30") '(10 20 30)))

;; Test split-string-to-char-list
(subtest "split-string-to-char-list"
  (is (split-string-to-char-list "hello") '(#\h #\e #\l #\l #\o))
  (is (split-string-to-char-list "123") '(#\1 #\2 #\3))
  (is (split-string-to-char-list "") '())
  (is (split-string-to-char-list "a") '(#\a)))

;; Test split-string-to-string-list
(subtest "split-string-to-string-list"
  (is (split-string-to-string-list "hello") '("h" "e" "l" "l" "o"))
  (is (split-string-to-string-list "123") '("1" "2" "3"))
  (is (split-string-to-string-list "") '())
  (is (split-string-to-string-list "a") '("a")))

;; Test ensure-string
(subtest "ensure-string"
  (is (ensure-string "hello") "hello")
  (is (ensure-string 'symbol) "SYMBOL")
  (is (ensure-string #\a) "a")
  (is (ensure-string 123) "123")
  (is (ensure-string 12.34) "12.34")
  (is (ensure-string nil) "NIL")
  (is (ensure-string '(1 2 3)) "(1 2 3)"))

;; Test string->integer
(subtest "string->integer"
  (is (string->integer "123") 123)
  (is (string->integer "0") 0)
  (is (string->integer "-123") -123)
  (is (string->integer "+123") 123)
  
  ;; Test with different radixes
  (is (string->integer "1010" :radix 2) 10)
  (is (string->integer "FF" :radix 16) 255)
  (is (string->integer "377" :radix 8) 255)
  
  ;; Test error cases (should signal errors)
  (is-error (string->integer "abc") 'error)
  (is-error (string->integer "12.34") 'error)
  (is-error (string->integer "") 'error))

;; Test string->number
(subtest "string->number"
  (is (string->number "123") 123)
  (is (string->number "0") 0)
  (is (string->number "-123") -123)
  (is (string->number "12.34") 12.34)
  (is (string->number "-12.34") -12.34)
  (is (string->number "1.23e2") 123.0)
  (is (string->number "1/2") 1/2)
  (is (string->number "3/4") 3/4)
  
  ;; Test error cases
  ;; "abc" will be read as a symbol ABC
  (is (string->number "abc") 'abc)
  ;; Empty string causes EOF error
  (is-error (string->number "") 'error)) 

;; Test conc-strings
(subtest "conc-strings"
  (is (conc-strings "hello" " " "world") "hello world")
  (is (conc-strings "a" "b" "c") "abc")
  (is (conc-strings "") "")
  (is (conc-strings "single") "single")
  (is (conc-strings "one" "") "one")
  (is (conc-strings "" "two") "two"))

;; Test string+
(subtest "string+"
  (is (string+ "hello" " " "world") "hello world")
  (is (string+ "a" "b" "c") "abc")
  (is (string+ "") "")
  (is (string+ "single") "single")
  (is (string+ "one" "") "one")
  (is (string+ "" "two") "two"))

;; Test string-index
(subtest "string-index"
  ;; Default behavior (return single character as string)
  (is (string-index "hello" 0) "h")
  (is (string-index "hello" 1) "e")
  (is (string-index "hello" 4) "o")
  (is (string-index "hello" -1) "o")
  (is (string-index "hello" -5) "h")
  
  ;; With charp = t (return character)
  (is (string-index "hello" 0 :charp t) #\h)
  (is (string-index "hello" 1 :charp t) #\e)
  (is (string-index "hello" 4 :charp t) #\o)
  (is (string-index "hello" -1 :charp t) #\o)
  (is (string-index "hello" -5 :charp t) #\h)
  
  ;; Out of bounds should trigger assertion error
  (is-error (string-index "hello" 5) 'error)
  (is-error (string-index "hello" -6) 'error)
  (is-error (string-index "hello" 5 :charp t) 'error)
  (is-error (string-index "hello" -6 :charp t) 'error))

;; Test string-starts-with
(subtest "string-starts-with"
  (ok (string-starts-with "hel" "hello"))
  (ok (string-starts-with "hello" "hello"))
  (ok (string-starts-with "" "hello"))
  (ok (string-starts-with "" ""))
  (ok (not (string-starts-with "bye" "hello")))
  (ok (not (string-starts-with "hello world" "hello"))))

;; Test string-ends-with
(subtest "string-ends-with"
  (ok (string-ends-with "llo" "hello"))
  (ok (string-ends-with "hello" "hello"))
  (ok (string-ends-with "" "hello"))
  (ok (string-ends-with "" ""))
  (ok (not (string-ends-with "bye" "hello")))
  (ok (not (string-ends-with "hello world" "world"))))

;; Test string-contains-p
(subtest "string-contains-p"
  (ok (string-contains-p "ell" "hello"))
  (ok (string-contains-p "hello" "hello"))
  (ok (string-contains-p "" "hello"))
  (ok (string-contains-p "h" "hello"))
  (ok (string-contains-p "o" "hello"))
  (ok (not (string-contains-p "bye" "hello")))
  (ok (not (string-contains-p "hello world" "hello"))))

;; Test mkstr
(subtest "mkstr"
  (is (mkstr "hello" " " "world") "hello world")
  (is (mkstr 'hello " " 'world) "HELLO WORLD")
  (is (mkstr 1 " + " 2 " = " 3) "1 + 2 = 3")
  (is (mkstr) "")
  (is (mkstr nil) "NIL"))

;; Test symb
(subtest "symb"
  (is (symb "hello" "-" "world") '|hello-world|)
  (is (symb 'hello '- 'world) 'hello-world)
  (is (symb "test" 123) '|test123|)
  (is-type (symb "test") 'symbol))

;; Test reread
(subtest "reread"
  (is (reread "123") 123)
  (is (reread "hello") 'hello)
  (is (reread "(1 2 3)") '(1 2 3))
  (is (reread "\"hello\"") "hello"))

;; Test explode
(subtest "explode"
  (is (explode 'hello) '(|H| |E| |L| |L| |O|))
  (is (explode 'test123) '(|T| |E| |S| |T| |1| |2| |3|))
  (is (explode 'a) '(|A|)))

(finalize)
