(in-package :iutils)

;; (proclaim '(inline last1 singlep append1 nconc1 ensure-list))

(defun singlep (lst)
  "Check if it is a single element list"
  (and (consp lst) (not (cdr lst))))

(defun last1 (lst)
  "Get last element of list"
  (car (last lst)))

(defun append1 (lst obj)
  "When append a list with a obj, no need to make list by hand"
  (append lst (list obj)))

(defun nconc1 (lst obj)
  "When nconc a list with a obj, no need to make list by hand"
  (nconc lst (list obj)))

(defun ensure-list (obj)
  "Make sure it is a list"
  (if (listp obj) obj (list obj)))

(defun remove-if1 (fn lst)
  "Like a simple remove-if, but return the apply fn to each element list "
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  "Group source into each with n numbers of elements unless the last is not enough"
  (when (<= n 0) (error "length not greater than zero"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Flat the list x"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; (defun flatten-1 (l)
;;   "Just flat the one level"
;;   (loop :with result := '()
;;         :for x :in l
;;         :if (listp x)
;;         :do (setf result (append result x))
;;         :else
;;         :do (setf result (append result (list x)))
;;         :finally (return result)))

(defun flatten-1 (l)
  "Just flat the one level"
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (append (ensure-list (car l))
                   (flatten-1 (cdr l))))))

(defun longer (x y)
  "Check if x longer then y; More efficient then only (> (length x) (length y))"
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))


(defun filter (fn lst)
  "Apply fn to each in the lst, collect each result when it is not nil"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))


;; prune to remove-if is copy-tree to copy-list
(defun prune (test tree)
  "remove each element in the tree if test"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               (rec tree nil)))
                      acc
                      (cons (car tree) acc)))))
    (rec tree '())))


;; CL-USER> (shuffle '(a b c) '( 1 2 3 4))
;; (A 1 B 2 C 3 4)
(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

;; CL-USER> (shuffle '(a b c) '( 1 2 3 4))
;; (A 1 B 2 C 3)
;; (defun shuffle (l1 l2)
;;   (let (result)
;;     (mapcar #'(lambda (x y) (push x result) (push y result))
;;             l1
;;             l2)
;;     (nreverse result)))



;;; Type convert
(defun list->1d-array (a-list)
  "Convert list to one dimension array"
  (make-array (length a-list)
              :initial-contents a-list))

(defun list->hashtable (a-list &key (test #'equal))
  "Convert list to hash:  (hash-from-list '((k1 v1) (k2 v2)))"
  (let ((h (make-hash-table :test test)))
    (loop for pair in a-list
          do (setf (gethash (first pair) h) (second pair)))
    h))

(defun alist->plist (alist)
  "Returns a property list containing the same keys and values as the
association list ALIST in the same order."
  (let (plist)
    (dolist (pair alist)
      (push (car pair) plist)
      (push (cdr pair) plist))
    (nreverse plist)))

(defun plist->alist (plist)
  "Returns an association list containing the same keys and values as the
property list PLIST in the same order."
  (let (alist)
    (do ((tail plist (cddr tail)))
        ((safe-endp tail) (nreverse alist))
      (push (cons (car tail) (cadr tail)) alist))))

(declaim (inline safe-endp))
(defun safe-endp (x)
  (declare (optimize safety))
  (endp x))

(defun list->vector (list)
  "Convert `list` into a vector."
  (check-type list list)
  (coerce list 'vector))

(defun sequence->list (seq)
  "Convert the sequence `seq` into a list."
  (check-type seq sequence)
  (coerce seq 'list))



;; search

(defun find2 (fn lst)
  "Find the first element in lst that when apply with fn that returns not nil; Return the element and the apply value"
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Check if x is before y in the lst;Notice if x appears then no y appears, before still return true, which is a flaw"
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Check if x appears after y in the lst; Don't have flaws as function before"
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))


(defun duplicate (obj lst &key (test #'eql))
  "Check if obj is duplicate in lst"
  (member obj (cdr (member obj lst :test test))
          :test test))


;; Example:
;; CL-USER> (split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8 9 10))
;; (1 2 3 4)
;; (5 6 7 8 9 10)
(defun split-if (fn lst)
  "Apply fn to each element in lst, when the resut is not nil, return the two split part of the lst"
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))


;; Example:
;; CL-USER> (most #'length '((a b) (a b c) (a) (e f g)))
;; (A B C)
;; 3
(defun most (fn lst)
  "Apply fn to each element in lst, return which the element and each max result"
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))


;; Example:
;; CL-USER> (most #'length '((a b) (a b c) (a) (e f g)))
;; (A B C)
;; 3
;; The difference with function 'most' is it returns all element if they return equal result, not the first as 'most'
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))


;; Example:
;; CL-USER> (best #'> '(1 2 3 4 5))
;; 5
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))


(defun in (element lst &key (test #'equal))
  (ensure-bool (find element lst :test test)))

(defun not-in (element lst &key (test #'equal))
  (not (in element lst :test test)))



;;; Map
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  "Like mapcan, but no afraid to change the arg lst; Because mapcan use nconc to concatenate lists, nconc is destructive"
  (apply #'append (apply #'mapcar fn lsts)))


;; Example:
;; CL-USER> (mapcars #'sqrt '(1 2) '(3 4))
;; (1.0 1.4142135 1.7320508 2.0)
(defun mapcars (fn &rest lsts)
  "When you need to mapcar to more than one list, you don't need to cons them first"
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

;; mapcar for tree
;; Example:
;; CL-USER> (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))
;; (11 (22 (33) 44))
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))


(defun lists-equalp (list1 list2 &key (test #'equal))
  "Compare two lists element by element using the specified test function.
  
  Args:
    list1: First list to compare
    list2: Second list to compare
    test: Function to use for element comparison (default: #'equal)
    
  Returns:
    T if lists have same length and all corresponding elements are equal according to test function"
  (and (= (length list1) (length list2))
       (every test list1 list2)))
