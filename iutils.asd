#|
This file is a part of iutils project.
Copyright (c) 2018 imnisen
|#

#|
Author: imnisen
|#

(in-package :cl-user)


(defpackage iutils-asd (:use :cl :asdf))
(in-package :iutils-asd)

(defsystem iutils
  :version "0.0.1"
  :author "imnisen"
  :license "MIT"
  :depends-on (:alexandria
               :ironclad
               :bit-smasher
               :local-time

               ;; src/domain
               :drakma
               :flexi-streams
               :yason
               :bordeaux-threads
               :cl-json
               :rutils
               :cl-change-case
               (:version "thnappy" "0.0.1")
               (:version "cl-yaml" "0.1"))
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "lists")
                 (:file "io")
                 (:file "hashtables")
                 (:file "bytes")
                 (:file "bit-vector")
                 (:file "numbers")
                 (:file "print")
                 (:file "strings")
                 (:file "arrays")
                 (:file "setfs-todo")
                 (:file "boolean")
                 (:file "sequences")
                 (:file "time")
                 (:file "crypto")
                 (:file "stream")
                 (:file "file")
                 (:file "snappy")
                 (:file "yaml")
                 (:file "others")))
               )
:description "A comprehensive collection of utility functions for Common Lisp development"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                            #p"README.md"
                            (or *load-pathname* *compile-file-pathname*))
                           :if-does-not-exist nil
                           :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
)
