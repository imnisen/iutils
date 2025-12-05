(in-package :iutils)

;; bits is alias of bit-vector

;;;
;;; TODO: The conversion here doesn't consider endianness, temporarily unused
;;; The bitsmash library doesn't handle endianness properly, which is problematic.
;;;

(defun make-initial-bits (length)
  (make-array length :element-type 'bit))

(defun bytes->bits (b)
  (bitsmash:octets->bits b))

(defun bits->bytes (b)
  (bitsmash:bits->octets b))

(defun int->bits (i)
  (bitsmash:int->bits i))

(defun bits->int (b)
  (bitsmash:bits->int b))


;;; Add equal function
(defun bit-vector-equalp (bv1 bv2 &key (test #'=))
  "Compare two bit vectors element by element using the specified test function.

  Args:
    bv1: First bit vector to compare
    bv2: Second bit vector to compare
    test: Function to use for element comparison (default: #'=)

  Returns:
    T if bit vectors have same length and all corresponding elements are equal according to test function"
  (and (= (length bv1) (length bv2))
       (every test bv1 bv2)))


;; TODO where to put this function
(defun hex-to-bit-list (hex-string &optional (bits 8))
  "convert hex string to bit list with length n"
  (let* ((num (parse-integer hex-string :start 2 :radix 16))
         (bit-list '()))
    (loop for i from (1- bits) downto 0
          do (push (if (logbitp i num) 1 0) bit-list))
    (reverse bit-list)))
