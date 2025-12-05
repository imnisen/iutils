(in-package :iutils)

(defun n-push (obj place n)
  (dotimes (i n place)
    (push obj place)))
