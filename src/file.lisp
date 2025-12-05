(in-package :iutils)

(defun read-file-to-bytes (filename)
  "Read all bytes from a file into an unsigned-byte 8 array"
  (with-open-file (stream filename
                          :element-type '(unsigned-byte 8)
                          :direction :input)
    (let* ((length (file-length stream))
           (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))
