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

(define-condition devided-by-zero
    (simple-error)
  ()                                    ;no slots
  (:report
   (lambda (c s)
     (format s "a denominator equals to 0.")))
  (:documentation
   "this is a condition to be raise when a denominator equals to 0.

CommonLisp Specification does not provide a condition for deviding-by-zero.
So, nurarihyon defines its own condition for that."))

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
             (vector-dimension-mismatch-required-dimension c))))
  (:documentation
   "this is a condition to be raisen when calculation cannot be continued
because vector dimension is not suitable."))

(define-condition matrix-dimensions-mismatch
    (simple-error)
  ((required-dimensions :initarg :required-dimensions
                        :reader matrix-dimensions-mismatch-required-dimensions)
   (matrix :initarg :matrix
           :reader matrix-dimensions-mismatch-matrix))
  (:report
   (lambda (c s)
     (format
      s "matrix dimension mismatch: ~A is required to be (~D, ~D) dimensions"
      (matrix-dimensions-mismatch-matrix c)
      (car (matrix-dimensions-mismatch-required-dimensions c))
      (cadr (matrix-dimensions-mismatch-required-dimensions c)))))
  (:documentation
   "this is a condition to be raisen when calculation cannot be continued
because matrix dimension is not suitable."))
