(in-package :cl-user)
(defpackage iutils-test.sequences
  (:use :cl :iutils :prove))
(in-package :iutils-test.sequences)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test n-push
(subtest "n-push"
  ;; Test basic functionality - push multiple times
  (let ((lst '(d e f)))
    (is (n-push 'c lst 1) '(c d e f))
    (is lst '(d e f) "Original list should not be modified"))
  
  (let ((lst '(d e f)))
    (is (n-push 'a lst 3) '(a a a d e f)))
  
  ;; Test with n = 0 (should return unchanged list)
  (let ((lst '(a b c)))
    (is (n-push 'x lst 0) '(a b c)))
  
  ;; Test with empty list
  (let ((lst '()))
    (is (n-push 'x lst 3) '(x x x)))
  
  ;; Test with different types of elements
  (let ((lst '()))
    (is (n-push "hello" lst 2) '("hello" "hello")))
  
  (let ((lst '()))
    (is (n-push '(a b) lst 2) '((a b) (a b))))
  
  ;; Test that it returns a new list without modifying the original
  (let ((original '(1 2 3)))
    (let ((result (n-push 0 original 2)))
      (is result '(0 0 1 2 3))
      (is original '(1 2 3) "Original list should not be modified"))))

(finalize)
