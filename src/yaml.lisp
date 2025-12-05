(in-package :iutils)

(defun read-yaml-file (p)
  (cl-yaml:parse p))