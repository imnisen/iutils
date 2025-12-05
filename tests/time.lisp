(in-package :cl-user)
(defpackage iutils-test.time
  (:use :cl :iutils :prove))
(in-package :iutils-test.time)

;; NOTE: To run this test file, execute `(asdf:test-system :iutils)' in your Lisp.

(plan nil)

;; Test now-unix
(subtest "now-unix"
  ;; Test that it returns a positive integer
  (let ((timestamp (now-unix)))
    (is-type timestamp 'integer "Should return an integer")
    (ok (> timestamp 0) "Should return a positive timestamp"))
  
  ;; Test that consecutive calls return increasing or equal values
  (let ((t1 (now-unix)))
    (sleep 0.1)  ; Small delay to ensure time has passed
    (let ((t2 (now-unix)))
      (ok (>= t2 t1) "Later timestamp should be greater than or equal to earlier one")))
  
  ;; Test that the timestamp is reasonable (after year 2020, before year 2100)
  (let ((timestamp (now-unix))
        (year-2020 1577836800)  ; Unix timestamp for 2020-01-01
        (year-2100 4102444800)) ; Unix timestamp for 2100-01-01
    (ok (> timestamp year-2020) "Timestamp should be after year 2020")
    (ok (< timestamp year-2100) "Timestamp should be before year 2100"))
  
  ;; Test multiple calls in quick succession
  (let ((timestamps (loop repeat 5 collect (now-unix))))
    (ok (every #'integerp timestamps) "All timestamps should be integers")
    ;; They should all be very close (within 1 second)
    (let ((min-time (apply #'min timestamps))
          (max-time (apply #'max timestamps)))
      (ok (<= (- max-time min-time) 1) 
          "Multiple quick calls should return timestamps within 1 second"))))

(finalize)
