(in-package :iutils)

(defun ensure-number (x)
  (cond ((stringp x) (parse-integer x :junk-allowed t))
        ((numberp x) x)
        (t (error (format nil "can't convert ~a to number" x) ))))


;; Type convert
(defun number->string (a-number &key (base 10))
  "Convert number to string"
  (write-to-string a-number :base base))
