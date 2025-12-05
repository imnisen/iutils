(in-package :iutils)

;;;
;;; bytes and stream
;;;
(defun bytes->input-stream (b)
  (flex:make-in-memory-input-stream b))

;; TODO latter
;; (defun read-all-bytes (stream &optional (eof-error-p t) eof-value)
;;   ;; check if eof
;;   (flex:peek-byte stream nil eof-error-p eof-value)

;;   (make-bytes-from-list (loop :for b := (read-byte stream nil nil)
;;                     :while b :collect b)))

;; simple version, no eof suff
;; return all bytes stream have, if no bytes, simple return nil
(defun read-all-bytes (stream)
  (when (flex:peek-byte stream nil nil nil)
    (make-bytes-from-list (loop :for b := (read-byte stream nil nil)
                      :while b :collect b))))
;; TODO think about alex:read-stream-content-into-byte-vector replace this



;; TODO add several read-related functions from Alexandria
;; read-stream-content-into-string
;; read-file-into-string
;; read-stream-content-into-byte-vector
;; read-file-into-byte-vector

(defun read-exact (num-bytes stream)
  (let* ((b (make-array num-bytes :element-type '(unsigned-byte 8)))
         (position (read-sequence b stream)))
    (if (= num-bytes position)
        b
        (error (format nil "Try to read ~a, only get ~a bytes" num-bytes position))))) ;; TODO the error is not approprite, need to use another method to tell the caller.
