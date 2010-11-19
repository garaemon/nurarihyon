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
   "this is a condition to be signaled if a denominator equals to 0.

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
   "this is a condition to be signaled when calculation cannot be continued
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
   "this is a condition to be signaled when calculation cannot be continued
because matrix dimension is not suitable."))

(define-condition index-out-of-matrix-range
    (simple-error)
  ((index :initarg :index
          :reader index-out-of-matrix-range-index)
   (matrix :initarg :matrix
           :reader index-out-of-matrix-range-matrix)
   (formatter :initarg :formatter
              :reader index-out-of-matrix-range-formatter))
  (:report
   (lambda (c s)
     (format
      s "~A is out of ~A range of ~A"
      (index-out-of-matrix-range-index c)
      (index-out-of-matrix-range-formatter c)
      (index-out-of-matrix-range-matrix c))))
  (:documentation
   "INDEX-OUT-OF-MATRIX-RANGE is defined as super class of
INDEX-OUT-OF-MATRIX-ROW-RANGE and INDEX-OUT-OF-MATRIX-COLUMN-RANGE."))

(define-condition index-out-of-matrix-row-range
    (index-out-of-matrix-range)
  ()
  (:default-initargs :formatter "row")
  (:documentation
   "this is a condition to be signaled when the index which
a user try to access is out of row range of a matrix"))

(define-condition index-out-of-matrix-column-range
    (index-out-of-matrix-range)
  ()
  (:default-initargs :formatter "column")
  (:documentation
   "this is a condition to be signaled when the index which
a user try to access is out of column range of a matrix"))
     