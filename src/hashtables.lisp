(in-package :iutils)

;; (defun make-hashtable (&rest init-values &key (test 'equal test-supplied-p) &allow-other-keys)
;;   "Create hash table conveniently.
;;    (my-makehash :k1 \"v1\" :k2 \"v2\")"
;;   (let ((h (make-hash-table :test test)))
;;     (loop for x on init-values by #'cddr
;;           do (unless (and test-supplied-p (equal (first x) :test))
;;                (setf (gethash (first x) h) (second x))) )
;;     h))

;; there is another convenient way to create a hashtable:
;; (alexandria:alist-hash-table '(("a" . 1) ("b" . 2)))
(defun make-hashtable (content &key (test 'equal))
  (let ((ht (make-hash-table :test test)))
    (loop for x on content by #'cddr
          do (setf (gethash (first x) ht) (second x)))
    ht))

;; (defun set-hashtable (h  &rest set-values &key (allow-nil t supplied-p) &allow-other-keys)
;;   "Change hash table value conveniently with side effect.
;;    Notice: Change passed h.
;;    allow-nil: set whether can set value nil
;;    (my-sethash hash :k1 \"v11\" :k2 nil :allow-nil t)"
;;   (if supplied-p
;;       (if allow-nil
;;           (loop for x on set-values by #'cddr
;;                 do (if (not (equal (first x) :allow-nil))
;;                        (setf (gethash (first x) h) (second x))))
;;           (loop for x on set-values by #'cddr
;;                 do (if (and (not (equal (first x) :allow-nil))
;;                             (second x))
;;                        (setf (gethash (first x) h) (second x)))))
;;       (loop for x on set-values by #'cddr
;;             do (setf (gethash (first x) h) (second x)))))

(defun set-hashtable (ht content &key (allow-nil t))
  (loop for x on content by #'cddr
        do  (unless (and (not allow-nil) (null (second x)))
              (setf (gethash (first x) ht)
                    (second x))))
  ht)


(defun hashtable-key-to-list (a-hash)
  "Return list of hash table keys"
  (loop for key being the hash-keys in a-hash collect key))

(defun hashtable-value-to-list (a-hash)
  "Return list of hash table values"
  (loop for value being the hash-values in a-hash collect value))


;; Type-convert
(defun hashtable-to-list (a-hash-table)
  "Convert hash table to list pair. (hash-to-list h) =>  ((k1 v1) (k2 v2))"
  (loop for value being the hash-values of a-hash-table
        using (hash-key key)
        collect (list key value)))


;; Literal create hash table
;; (set-macro-character #\{
;;                      (lambda (stream char)
;;                        (declare (ignore char))
;;                        (let ((hash (make-hash-table :test 'equal)))
;;                          (loop for (key value)
;;                             on (read-delimited-list #\} stream t)
;;                             by #'cddr
;;                             do (setf (gethash key hash) value))
;;                          hash)))

;; (set-macro-character #\} (get-macro-character #\) nil))


;; ;; Change print format of hash table
;; (set-pprint-dispatch 'hash-table
;;                      (lambda (str ht)
;;                        (format str "{~{~{~S => ~S~}~^, ~}}"
;;                                (loop for key being the hash-keys of ht
;;                                      for value being the hash-values of ht
;;                                      collect (list key value)))))

(defun print-hashtable (ht)
  (format t "{~{~{~S => ~S~}~^, ~}}"
          (loop for key being the hash-keys of ht
                for value being the hash-values of ht
                collect (list key value))))

;; need to think
(defun getmultihash (keys-list ht)
  (loop :with r := nil
        :with h := ht
        :for key :in keys-list
        :do (setf r (gethash key h)
                  h r)
        :finally (return r)))


(defun must-gethash (key hashtable)
  (multiple-value-bind (value found-p)
      (gethash key hashtable)
    (if found-p value (error (format t "~a not found in hashtable" key)))))
