(in-package :iutils)
;;; TODO add test case and export

;; Setf all args to val
(defmacro allf (val &rest args)
  (alexandria:with-gensyms (v)
    `(let ((,v ,val))
       (setf ,@(mapcan #'(lambda (a)
                           `(,a ,v))
                       args)))))

;; Setf all args to nil
(defmacro nilf (&rest args)
  `(allf nil ,@args))

;; Setf all args to t
(defmacro tf (&rest args)
  `(allf t ,@args))

;; Like (setf x (nconc x y ...))
(define-modify-macro concf (obj) nconc)

;; Like `push` to last end of list
(defun conc1f/function (place obj)
  (nconc place (list obj)))
(define-modify-macro conc1f (obj) conc1f/function)

;; Like `pushnew` to last end of list
(defun concnew/function (place obj &rest args)
  (if (apply #'member obj place args)
      place
      (nconc place (list obj))))
(define-modify-macro concnew (obj &rest args) concnew/function)


;; Macro for setting a place to a value with access to the old value as 'it!'
;; This version captures the value of 'place' once at the beginning
(defmacro setfit1 (place value)
  "Sets PLACE to VALUE. The old value of PLACE is available as 'it!' in VALUE expression.
  Note: 'it!' is bound to the initial value of PLACE (evaluated once)."
  `(let ((it! ,place))
     (setf ,place ,value)))

;; This version re-evaluates 'place' each time 'it!' is referenced
(defmacro setfit2 (place value)
  "Sets PLACE to VALUE. The current value of PLACE is available as 'it!' in VALUE expression.
  Note: 'it!' re-evaluates PLACE each time it's referenced."
  `(symbol-macrolet ((it! ,place))
     (setf it! ,value)))
