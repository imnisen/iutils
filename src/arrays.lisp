(in-package :iutils)

(defun array-slice (a-array start end)
  "Array slice just like python lst[1:2].
   Returned array dispatched to passed array.
   When start or end longer than length of array passed in, use length of array instead.
   Not support negative slice now."
  (let* ((l (length a-array))
         (new-start (if (> start l) l start))
         (new-end (if (> end l) l end))
         (new-array-length (- new-end new-start)))
    (make-array new-array-length
                :element-type (array-element-type a-array)
                :displaced-to a-array
                :displaced-index-offset new-start)))


(defun 1d-array-to-list (a-array)
  (loop for i below (array-dimension a-array 0)
        collect (aref a-array i)))
