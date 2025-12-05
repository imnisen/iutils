(in-package :iutils)

;; refer links
;; https://quickref.common-lisp.net/s-utils.html
;; https://github.com/vseloved/date-utils/blob/master/dates.lisp
;; * https://github.com/dlowe-net/local-time/blob/master/src/local-time.lisp

;; It seems a good choice to use local-time package.
;; timestamp is a Type in local-time.


;; get now unix time stamp
(defun now-unix ()
  (local-time:timestamp-to-unix (local-time:now)))
