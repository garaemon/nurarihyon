;;================================================
;; conditions.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 1)
		   (space 0)))

(in-package :nurarihyon)

(define-condition vector-dimension-mismatch
    (simple-error)
  ((required-dimension :initarg :required-dimension
                       :reader vector-dimension-mismatch-required-dimension)
   (vector :initarg :vector
           :reader vector-dimension-mismatch-vector))
  (:report
   (lambda (c s)
     (format s "vector dimension mismatch: ~A is required to be ~D dimension"
             (vector-dimension-mismatch-vector c)
             (vector-dimension-mismatch-required-dimension c)))))
