(in-package :cl-user)
(defpackage iutils-test.arrays
  (:use :cl :iutils :prove))
(in-package :iutils-test.arrays)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test array-slice
(subtest "array-slice"
  ;; Test basic slicing
  (is (array-slice #(1 2 3 4 5) 1 3) #(2 3) :test #'equalp)
  (is (array-slice #(a b c d e f) 0 2) #(a b) :test #'equalp)
  (is (array-slice #(1 2 3 4 5) 3 5) #(4 5) :test #'equalp)
  
  ;; Test edge cases
  (is (array-slice #(1 2 3) 0 3) #(1 2 3) :test #'equalp
      "Slice entire array")
  (is (array-slice #(1 2 3) 1 1) #() :test #'equalp
      "Empty slice when start equals end")
  (is (array-slice #(1) 0 1) #(1) :test #'equalp
      "Slice single element array")
  
  ;; Test with different element types
  (is (array-slice #("a" "b" "c" "d") 1 3) #("b" "c") :test #'equalp
      "Slice string array")
  (is (array-slice #(#\a #\b #\c #\d) 2 4) #(#\c #\d) :test #'equalp
      "Slice character array"))

;; Test 1d-array-to-list
(subtest "1d-array-to-list"
  ;; Test basic conversion
  (is (1d-array-to-list #(1 2 3 4 5)) '(1 2 3 4 5))
  (is (1d-array-to-list #(a b c)) '(a b c))
  
  ;; Test edge cases
  (is (1d-array-to-list #()) '()
      "Convert empty array")
  (is (1d-array-to-list #(42)) '(42)
      "Convert single element array")
  
  ;; Test with different element types
  (is (1d-array-to-list #("hello" "world")) '("hello" "world")
      "Convert string array")
  (is (1d-array-to-list #(#\a #\b #\c)) '(#\a #\b #\c)
      "Convert character array")
  (is (1d-array-to-list #(nil t nil)) '(nil t nil)
      "Convert array with nil and boolean values")
  
  ;; Test round-trip conversion with list->1d-array
  (let ((test-lists '((1 2 3) (a b c) ("x" "y" "z") ())))
    (loop for lst in test-lists
          do (is (1d-array-to-list (list->1d-array lst)) lst
                 (format nil "Round-trip conversion for ~a" lst)))))

;; Integration tests
(subtest "integration tests"
  ;; Test array-slice followed by 1d-array-to-list
  (let ((arr #(10 20 30 40 50)))
    (is (1d-array-to-list (array-slice arr 1 4)) '(20 30 40)
        "Slice then convert to list"))
  
  ;; Test creating array from list, slicing, and converting back
  (let* ((original-list '(a b c d e f g))
         (arr (list->1d-array original-list))
         (sliced (array-slice arr 2 5))
         (result-list (1d-array-to-list sliced)))
    (is result-list '(c d e)
        "Full pipeline: list -> array -> slice -> list")))

(finalize)
