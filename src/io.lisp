(in-package :iutils)

;;; I/O
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "("
                        (apply #'read-line args)
                        ")"))))
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.'~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))
