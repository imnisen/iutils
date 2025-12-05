#!/usr/bin/env sbcl --script

;; Simple test runner for iutils
;; Usage: sbcl --script run-tests.lisp
;; Or make it executable: chmod +x run-tests.lisp && ./run-tests.lisp

(require :asdf)

;; Load Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the system
(handler-case
    (progn
      ;; Ensure prove is available
      (format t "~%Loading prove testing library...~%")
      (ql:quickload :prove :silent t)
      
      ;; Use TAP reporter for better compatibility with dark terminal themes
      ;; (the default reporter uses black text which is invisible on black backgrounds)
      (setf (symbol-value (intern "*DEFAULT-REPORTER*" :prove)) :tap)
      
      (format t "~%Running tests...~%")
      (asdf:test-system :iutils-test)
      
      (format t "~%All tests completed!~%"))
  (error (e)
    (format t "~%Error during testing: ~A~%" e)
    (sb-ext:exit :code 1)))

(sb-ext:exit :code 0)
