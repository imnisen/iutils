(in-package :cl-user)
(defpackage iutils-test
  (:use :cl :iutils :prove))
(in-package :iutils-test)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)


;; singlep
(is (singlep '(1)) t)
(is (singlep '(1 2)) nil)
(is (singlep '()) nil)

;; last1
(is (last1 '(1 2 3 4)) 4)
(is (last1 '(1)) 1)
(is (last1 '()) nil)

;; append1
(is (append1 '(1 2) 3) '(1 2 3))
(is (append1 '() 1) '(1))
(defvar *v1* '())
(is (append1 *v1* 1) '(1))
(is *v1* '())

;; nconc1
(is (nconc1 '(1 2) 3) '(1 2 3))

;; ensure-list
(is (ensure-list 1) '(1))
(is (ensure-list '(1)) '(1))

;; remove-if1
(is (remove-if1 #'evenp '(1 2 3 4)) (list t t))

;; group
(is (group '(1 2 3 4 5 6 7) 3) '((1 2 3) (4 5 6) (7)))
(is (group '(1) 3) '((1)))
(is (group '() 3) '())

;; flatten
(is (flatten '((1 2 3))) '(1 2 3))
(is (flatten '(1 2 3)) '(1 2 3))
(is (flatten '()) '())

;; list->1d-array
(is (list->1d-array '(1 2 3)) #(1 2 3) :test #'equalp)
(is (list->1d-array '(1 (2) 3)) #(1 (2) 3) :test #'equalp)
(is (list->1d-array '()) #() :test #'equalp)


(finalize)
