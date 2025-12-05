#|
This file is a part of iutils project.
Copyright (c) 2018 imnisen
|#

(in-package :cl-user)
(defpackage iutils-test-asd
  (:use :cl :asdf))
(in-package :iutils-test-asd)

(defsystem iutils-test
  :author "imnisen"
  :license "MIT"
  :depends-on (:iutils
               :prove)
  :components ((:module "tests"
                :components
                ((:test-file "lists")
                 (:test-file "bytes")
                 (:test-file "strings")
                 (:test-file "crypto")
                 (:test-file "hashtables")
                 (:test-file "time")
                 (:test-file "arrays")
                 (:test-file "numbers")
                 (:test-file "sequences"))))
  :description "Test system for iutils"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
