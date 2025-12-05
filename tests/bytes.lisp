(in-package :cl-user)
(defpackage iutils-test.bytes
  (:use :cl :iutils :prove))
(in-package :iutils-test.bytes)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test make-bytes-from-list
(subtest "make-bytes-from-list"
  (is (make-bytes-from-list '(1 2 3 4)) #(1 2 3 4) :test #'equalp)
  (is (make-bytes-from-list '(0 255)) #(0 255) :test #'equalp)
  (is (make-bytes-from-list '()) #() :test #'equalp)
  (is-type (make-bytes-from-list '(1 2 3)) 'simple-array
           "returns a simple-array"))

;; Test make-initial-zeros-bytes
(subtest "make-initial-zeros-bytes"
  (is (make-initial-zeros-bytes 5) #(0 0 0 0 0) :test #'equalp)
  (is (make-initial-zeros-bytes 0) #() :test #'equalp)
  (is (make-initial-zeros-bytes 1) #(0) :test #'equalp))

;; Test bytes->int and int->bytes
(subtest "bytes->int and int->bytes conversion"
  ;; Big-endian tests
  (is (bytes->int #(0 0 1 0) :big-endian t) 256)
  (is (bytes->int #(1 0 0 0) :big-endian t) 16777216)
  (is (bytes->int #(255 255) :big-endian t) 65535)
  
  ;; Little-endian tests
  (is (bytes->int #(0 1 0 0) :big-endian nil) 256)
  (is (bytes->int #(0 0 0 1) :big-endian nil) 16777216)
  (is (bytes->int #(255 255) :big-endian nil) 65535)
  
  ;; Test with start/end
  (is (bytes->int #(1 2 3 4) :start 1 :end 3 :big-endian t) 515)
  
  ;; Round-trip tests
  (is (int->bytes 256 :n-bits 32 :big-endian t) #(0 0 1 0) :test #'equalp)
  (is (int->bytes 256 :n-bits 32 :big-endian nil) #(0 1 0 0) :test #'equalp)
  (is (int->bytes 65535 :n-bits 16) #(255 255) :test #'equalp)
  
  ;; Test round-trip conversion
  (loop for n in '(0 1 255 256 65535 16777215)
        do (is (bytes->int (int->bytes n :n-bits 32)) n)))

;; Test bytes->hexstr
(subtest "bytes->hexstr"
  (is (bytes->hexstr #(255 0 15)) "ff000f")
  (is (bytes->hexstr #(255 0 15) :with-0x-prefix t) "0xff000f")
  (is (bytes->hexstr #()) "")
  (is (bytes->hexstr #() :with-0x-prefix t) "0x")
  (is (bytes->hexstr #(1 2 3 4)) "01020304")
  (is (bytes->hexstr #(16 32 48)) "102030"))

;; Test hexstr->bytes
(subtest "hexstr->bytes"
  (is (hexstr->bytes "ff000f") #(255 0 15) :test #'equalp)
  (is (hexstr->bytes "0xff000f" :with-0x-prefix t) #(255 0 15) :test #'equalp)
  (is (hexstr->bytes "01020304") #(1 2 3 4) :test #'equalp)
  (is (hexstr->bytes "") #() :test #'equalp)
  (is (hexstr->bytes "0x" :with-0x-prefix t) #() :test #'equalp)
  
  ;; Test round-trip conversion
  (let ((test-bytes #(0 1 15 16 255 128)))
    (is (hexstr->bytes (bytes->hexstr test-bytes)) test-bytes :test #'equalp)))

;; Test int->hexstr and hexstr->int
(subtest "int->hexstr and hexstr->int"
  (is (int->hexstr 255) "ff")
  (is (int->hexstr 255 :with-0x-prefix t) "0xff")
  (is (int->hexstr 256) "0100")  ; 256 = bytes [1, 0] = "0100"
  (is (int->hexstr 256 :n-bits 16) "0100")
  (is (int->hexstr 256 :n-bits 32) "00000100")
  
  (is (hexstr->int "ff") 255)
  (is (hexstr->int "0xff" :with-0x-prefix t) 255)
  (is (hexstr->int "0100") 256)  ; hex strings need even length for byte conversion
  (is (hexstr->int "00000100") 256)
  
  ;; Test round-trip
  (loop for n in '(0 1 255 256 65535 16777215)
        do (is (hexstr->int (int->hexstr n)) n)))

;; Test bytes->str and str->bytes
(subtest "bytes->str and str->bytes (UTF-8)"
  (is (bytes->str #(104 101 108 108 111)) "hello")
  (is (bytes->str #()) "")
  
  (is (str->bytes "hello") #(104 101 108 108 111) :test #'equalp)
  (is (str->bytes "") #() :test #'equalp)
  
  ;; Test round-trip
  (let ((test-strings '("hello" "world" "")))
    (loop for s in test-strings
          do (is (bytes->str (str->bytes s)) s))))

;; Test bytes->str2 and str->bytes2 (direct character mapping)
(subtest "bytes->str2 and str->bytes2 (direct character mapping)"
  (is (bytes->str2 #(72 101 108 108 111)) "Hello")
  (is (bytes->str2 #()) "")
  (is (bytes->str2 #(65 66 67)) "ABC")
  
  (is (str->bytes2 "Hello") #(72 101 108 108 111) :test #'equalp)
  (is (str->bytes2 "") #() :test #'equalp)
  (is (str->bytes2 "ABC") #(65 66 67) :test #'equalp)
  
  ;; Test round-trip with ASCII characters only
  (let ((test-strings '("Hello" "ABC" "123")))
    (loop for s in test-strings
          do (is (bytes->str2 (str->bytes2 s)) s))))

;; Test conc-bytes
(subtest "conc-bytes"
  (is (conc-bytes #(1 2) #(3 4)) #(1 2 3 4) :test #'equalp)
  (is (conc-bytes #(1) #(2) #(3)) #(1 2 3) :test #'equalp)
  (is (conc-bytes #()) #() :test #'equalp)
  (is (conc-bytes #(1 2) #()) #(1 2) :test #'equalp)
  (is (conc-bytes #() #(1 2)) #(1 2) :test #'equalp)
  (is (conc-bytes) #() :test #'equalp))

;; Test list-of-bytes->bytes
(subtest "list-of-bytes->bytes"
  (is (list-of-bytes->bytes '(#(1 2) #(3 4))) #(1 2 3 4) :test #'equalp)
  (is (list-of-bytes->bytes '(#(1) #(2) #(3))) #(1 2 3) :test #'equalp)
  (is (list-of-bytes->bytes '()) #() :test #'equalp)
  (is (list-of-bytes->bytes '(#())) #() :test #'equalp))

;; Test bytes-equal
(subtest "bytes-equal"
  (ok (bytes-equal #(1 2 3) #(1 2 3)))
  (ok (bytes-equal #() #()))
  (ok (not (bytes-equal #(1 2 3) #(1 2 4))))
  (ok (not (bytes-equal #(1 2) #(1 2 3))))
  (ok (not (bytes-equal #(1 2 3) #(1 2)))))

;; Test bytes->bitstr
(subtest "bytes->bitstr"
  (is (bytes->bitstr #(255)) #("11111111") :test #'equalp)
  (is (bytes->bitstr #(0)) #("00000000") :test #'equalp)
  (is (bytes->bitstr #(170)) #("10101010") :test #'equalp)  ; 170 = 0xAA
  (is (bytes->bitstr #(15 240)) #("00001111" "11110000") :test #'equalp)  ; 15 = 0x0F, 240 = 0xF0
  (is (bytes->bitstr #()) #() :test #'equalp))

;; Test hexstr->bitstr
(subtest "hexstr->bitstr"
  (is (hexstr->bitstr "ff") #("11111111") :test #'equalp)
  (is (hexstr->bitstr "00") #("00000000") :test #'equalp)
  (is (hexstr->bitstr "aa") #("10101010") :test #'equalp)
  (is (hexstr->bitstr "0ff0") #("00001111" "11110000") :test #'equalp)
  (is (hexstr->bitstr "") #() :test #'equalp))

(finalize)
