(in-package :cl-user)
(defpackage iutils-test.hashtables
  (:use :cl :iutils :prove))
(in-package :iutils-test.hashtables)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test make-hashtable
(subtest "make-hashtable"
  ;; Test with simple key-value pairs (plist format)
  (let ((ht (make-hashtable '(a 1 b 2 c 3))))
    (is (gethash 'a ht) 1)
    (is (gethash 'b ht) 2)
    (is (gethash 'c ht) 3)
    (is (hash-table-count ht) 3))
  
  ;; Test with string keys (default test is 'equal)
  (let ((ht (make-hashtable '("foo" 10 "bar" 20))))
    (is (gethash "foo" ht) 10)
    (is (gethash "bar" ht) 20))
  
  ;; Test with different test function
  (let ((ht (make-hashtable '(a 1 b 2) :test 'eq)))
    (is (gethash 'a ht) 1)
    (is (hash-table-test ht) 'eq))
  
  ;; Test empty hash table
  (let ((ht (make-hashtable '())))
    (is (hash-table-count ht) 0))
  
  ;; Test with nil value
  (let ((ht (make-hashtable '(a nil b 2))))
    (multiple-value-bind (value exists-p) (gethash 'a ht)
      (is value nil)
      (ok exists-p "Key 'a' should exist even with nil value"))))

;; Test set-hashtable
(subtest "set-hashtable"
  ;; Create an empty hash table and set values (plist format)
  (let ((ht (make-hash-table :test 'equal)))
    (set-hashtable ht '(a 1 b 2 c 3))
    (is (gethash 'a ht) 1)
    (is (gethash 'b ht) 2)
    (is (gethash 'c ht) 3))
  
  ;; Test with :allow-nil nil (should skip nil values)
  (let ((ht (make-hash-table :test 'equal)))
    (set-hashtable ht '(a 1 b nil c 3) :allow-nil nil)
    (is (gethash 'a ht) 1)
    (is (gethash 'c ht) 3)
    (multiple-value-bind (value exists-p) (gethash 'b ht)
      (declare (ignore value))
      (ok (not exists-p) "Key 'b' should not exist when allow-nil is nil")))
  
  ;; Test with :allow-nil t (default, should include nil values)
  (let ((ht (make-hash-table :test 'equal)))
    (set-hashtable ht '(a 1 b nil c 3) :allow-nil t)
    (multiple-value-bind (value exists-p) (gethash 'b ht)
      (is value nil)
      (ok exists-p "Key 'b' should exist with nil value when allow-nil is t"))))

;; Test hashtable-key-to-list
(subtest "hashtable-key-to-list"
  (let ((ht (make-hashtable '(a 1 b 2 c 3))))
    (let ((keys (hashtable-key-to-list ht)))
      (is (length keys) 3)
      (ok (member 'a keys))
      (ok (member 'b keys))
      (ok (member 'c keys))))
  
  ;; Test with empty hash table
  (let ((ht (make-hashtable '())))
    (is (hashtable-key-to-list ht) '())))

;; Test hashtable-value-to-list
(subtest "hashtable-value-to-list"
  (let ((ht (make-hashtable '(a 1 b 2 c 3))))
    (let ((values (hashtable-value-to-list ht)))
      (is (length values) 3)
      (ok (member 1 values))
      (ok (member 2 values))
      (ok (member 3 values))))
  
  ;; Test with empty hash table
  (let ((ht (make-hashtable '())))
    (is (hashtable-value-to-list ht) '())))

;; Test hashtable-to-list
(subtest "hashtable-to-list"
  ;; Test basic conversion
  (let ((ht (make-hashtable '(a 1 b 2))))
    (let ((lst (hashtable-to-list ht)))
      (is (length lst) 2)
      (ok (or (and (equal (first lst) '(a 1)) (equal (second lst) '(b 2)))
              (and (equal (first lst) '(b 2)) (equal (second lst) '(a 1))))
          "Should contain the correct key-value pairs")))
  
  ;; Test with empty hash table
  (let ((ht (make-hashtable '())))
    (is (hashtable-to-list ht) '()))
  
  ;; Test roundtrip conversion
  (let* ((original-plist '(x 10 y 20 z 30))
         (ht (make-hashtable original-plist))
         (converted (hashtable-to-list ht)))
    (is (length converted) 3)
    (loop for (key value) on original-plist by #'cddr
          do (ok (member (list key value) converted :test #'equal)
                 (format nil "Pair (~a ~a) should be in converted list" key value)))))

;; Test print-hashtable
(subtest "print-hashtable"
  ;; Just test that it doesn't error - actual output testing would require capturing stdout
  (let ((ht (make-hashtable '(a 1 b 2))))
    (ok (null (print-hashtable ht)) "print-hashtable should return NIL"))
  
  ;; Test with empty hash table
  (let ((ht (make-hashtable '())))
    (ok (null (print-hashtable ht)) "print-hashtable should return NIL for empty table")))

;; Test getmultihash
(subtest "getmultihash"
  ;; Test nested hash tables
  (let* ((inner1 (make-hashtable '(x 10 y 20)))
         (inner2 (make-hashtable `(foo ,inner1 bar 30)))
         (outer (make-hashtable `(level1 ,inner2))))
    
    ;; Test getting nested values
    (is (getmultihash '(level1 bar) outer) 30)
    (is (getmultihash '(level1 foo x) outer) 10)
  (is (getmultihash '(level1 foo y) outer) 20)
    
    ;; Test non-existent keys
    (is (getmultihash '(level1 baz) outer) nil)
    (is (getmultihash '(nonexistent) outer) nil)
    
    ;; Test single key
    (is (getmultihash '(level1) outer) inner2 :test #'eq))
  
  ;; Test with simple hash table
  (let ((ht (make-hashtable '(a 1 b 2))))
    (is (getmultihash '(a) ht) 1)
    (is (getmultihash '(b) ht) 2)))

;; Test must-gethash
(subtest "must-gethash"
  ;; Test successful retrieval
  (let ((ht (make-hashtable '(a 1 b 2))))
    (is (must-gethash 'a ht) 1)
    (is (must-gethash 'b ht) 2))
  
  ;; Test error on missing key
  (let ((ht (make-hashtable '(a 1))))
    (is-error (must-gethash 'nonexistent ht) 'error
              "Should signal an error for non-existent key"))
  
  ;; Test with nil value (should not error if key exists)
  (let ((ht (make-hashtable '(a nil))))
    (is (must-gethash 'a ht) nil 
        "Should return nil value without error when key exists")))

;; Integration tests
(subtest "integration tests"
  ;; Test creating, modifying, and converting hash tables
  (let ((ht (make-hash-table :test 'equal)))
    ;; Set initial values
    (set-hashtable ht '(one 1 two 2 three 3))
    
    ;; Add more values
    (setf (gethash 'four ht) 4)
    (setf (gethash 'five ht) 5)
    
    ;; Check count
    (is (hash-table-count ht) 5)
    
    ;; Get all keys and values
    (let ((keys (hashtable-key-to-list ht))
          (values (hashtable-value-to-list ht)))
      (is (length keys) 5)
      (is (length values) 5)
      (ok (subsetp '(one two three four five) keys :test #'equal))
      (ok (subsetp '(1 2 3 4 5) values :test #'equal)))
    
    ;; Convert to list and back
    (let* ((as-list (hashtable-to-list ht))
           ;; hashtable-to-list returns alist, but make-hashtable expects plist
           ;; So we need to convert alist to plist
           (as-plist (loop for (k v) in as-list
                           collect k collect v))
           (new-ht (make-hashtable as-plist)))
      (is (hash-table-count new-ht) (hash-table-count ht))
      (loop for key in '(one two three four five)
            do (is (gethash key new-ht) (gethash key ht))))))

(finalize)
