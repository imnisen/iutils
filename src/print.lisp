(in-package :iutils)

(defun print-and-return (x)
  (format t "~&---Start----~&~a~&---End----~&" x)
  x)

;; Note, we already have print-hashtable in hashtable.lisp
(defun print-hash (a-hash-table)
  (format t "Length: ~A" (hash-table-count a-hash-table))
  (format t "~&---Start---")
  (loop for value being the hash-values of a-hash-table
        using (hash-key key)
        do (format t "~&~A  ->  ~A" key value))
  (format t "~&---End----"))


(defun print-cons (a-cons)
  (format t "~&---Start---")
  (loop for x in a-cons
        do (format t "~&~a~&" x))
  (format t "~&---End----"))


(defun print-hash-and-return (h)
  (print-hash h)
  h)

(defun print-cons-and-return (a-cons)
  (print-cons a-cons)
  a-cons)

(defmacro p-r (x)
  `(print-and-return ,x))
