;;================================================
;; vector.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 1)
		   (space 0)))

(in-package :nurarihyon)

(eval-when (:compile-toplevel)
  (enable-nurarihyon-reader-syntax))

;; condition
(define-condition vector-dimension-mismatch
    (simple-error)
  (required-dimension vector)
  (:report
   (lambda (c s)
     (format s "vector dimension mismatch: ~A is required to be ~D dimension"
             vector
             required-dimension))))

;;==================================
;; in-package utility
(defmacro x (a)
  "take the first element of the argument."
  `(aref ,a 0))
(defmacro y (a)
  "take the second element of the argument."
  `(aref ,a 1))
(defmacro z (a)
  "take the third element of the argument."
  `(aref ,a 2))
;;
;;==================================

(declaim (inline vector-dimension))
(defun vector-dimension (a)
  "Return  dimension of a vector. the vector muse be a simple-array
of double-float.

 example::

   (vector-dimension #d(1.0d0 2.0d0 3.0d0)) => 3"
  (declare (type (simple-array double-float) a))
  (the fixnum (array-dimension a 0)))

(declaim (inline make-vector))
(defun make-vector (dim &key (initial-element 0.0d0))
  "Allocate a vector with dim dimension. you can use :initial-element
keyword to specify the value of the vector.

 example::

   (make-vector 3) => #d(0.0d0 0.0d0 0.0d0)
   (make-vector 2 :initial-element 2.0d0) => #d(2.0d0 2.0d0)"
  (declare (type fixnum dim)
           (type double-float initial-element))
  (the (simple-array double-float)
    (make-array dim :element-type 'double-float
                :initial-element initial-element)))

(declaim (inline make-vector3))
(defun make-vector3 (&key (initial-element 0.0d0))
  "Allocate a 3-dimension vector. this function is specialized for 3 dimension.
:initial-element allows you to specify the values of the vector.

 example::

   (make-vector3) => #d(0.0d0 0.0d0 0.0d0)
   (make-vector3 :initial-element 2.0d0) => #d(2.0d0 2.0d0 2.0d0)"
  (declare (type double-float initial-element))
  (the (simple-array double-float (3))
    (make-array 3 :element-type 'double-float
                :initial-element initial-element)))

(declaim (inline make-vector4))
(defun make-vector4 (&key (initial-element 0.0d0))
  "Allocate a 4-dimension vector. this function is specialized for 4 dimension.
:initial-element allows you to specify the values of the vector.

 example::

   (make-vector4) => #d(0.0d0 0.0d0 0.0d0 0.0d0)
   (make-vector4 :initial-element 2.0d0) => #d(2.0d0 2.0d0 2.0d0 2.0d0)"
  (declare (type double-float initial-element))
  (the (simple-array double-float (4))
    (make-array 4 :element-type 'double-float
                :initial-element initial-element)))

(declaim (inline double-vector))
(defun double-vector (&rest args)
  "this is a utility function to make a double vector. this function will
create the vector which has ARGS as contents.

 example::

   (double-vector 1 2 3) => #d(1.0d0 2.0d0 3.0d0)"
  (the (simple-array double-float)
    (make-array (length args) :element-type 'double-float
                   :initial-contents
                   (mapcar #'(lambda (x) (coerce x 'double-float)) args))))

(defmacro with-vector-dimension-bind-and-check ((dim a b) &rest args)
  "verificate the vector A and B has the same length dimension and
bind the dimension to DIM when evaluating ARGS.

If they does not have the save dimension, this macro will raise an error."
  `(let ((,dim (vector-dimension ,a)))
     (declare (type fixnum ,dim))
     (if (= ,dim (vector-dimension ,b))
         (progn ,@args)
         (error (make-condition 'vector-dimension-mismatch
                                :vector ,b
                                :required-dimension ,dim)))))

(declaim (inline copy-vector))
(defun copy-vector (a &optional (b (make-vector (vector-dimension a))))
  "copy the double vector A to B and return B.

You can specify B, the second argument, to reduce heap allocation.
If not, COPY-VECTOR will allocate another vector which has
the same length to A.

A and B) must be a (simple-array double-float) and have the same length."
  (declare (type (simple-array double-float) a b))
  (the (simple-array double-float) (copy-vector* a b)))

(defun copy-vector* (a b)
  "this is a low level api to copy the double vector A to B and return B.
A and B must be a (simple-array double-float) and have the same length."
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (dotimes (i dim)
      (setf [ b i] [a i]))
    (the (simple-array double-float) b)))

;; vector operators
(defun v+ (a b &optional (c nil))
  "add two vectors, A and B, and return the result.

A and B are required to be a (simple-array double-float) and have the same
length.

You can use C, the third argument, to reduce heap allocation.

 example::

  (v+ #d(1 2) #d(3 4)) => #d(4 6)
  (let ((buf (make-vector 2)))
    (v+ #d(1 2) #d(3 4) buf)            ;=> #d(4 6)
    ...)"
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (let ((c (or c (make-vector dim))))
      (declare (type (simple-array double-float) c))
      (dotimes (i dim)
        (setf [c i] (+ [a i] [b i])))
      (the (simple-array double-float) c))))

(defun v- (a b &optional (c nil))
  "substitute B from A and return the result.

A and B are required to be a (simple-array double-float) and have the same
length.

You can use C, the third argument, to reduce heap allocation.

 example::

   (v- #d(3 4) #d(1 2)) => #d(2 2)
   (let ((buf (make-vector 2)))
    (v- #d(1 2) #d(3 4) buf)            ;=> #d(-2 -2)
    ...)"
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (let ((c (or c (make-vector dim))))
      (declare (type (simple-array double-float) c))
      (dotimes (i dim) (setf [c i] (- [a i] [b i])))
      (the (simple-array double-float) c))))

;; dot product
(defun v. (a b)
  "calculate a dot product of A and B.

A and B are required to be a (simple-array double-float) and have the same
length.

.. math::

    \sigma_{i} A_{i}B_{i}"
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (let ((ret 0.0d0))
      (declare (type double-float ret))
      (dotimes (i dim)
        (+== ret (the double-float (* [a i] [b i]))))
      (the double-float ret))))

;; cross product
(defun v* (a b &optional (c nil))
  "calculate a cross product of A and B.

A and B are required to be a (simple-array double-float (3)).

You can use C, the third argument, to reduce heap allocation.

.. math::

   \bold{C} = \bold{A} \times \bold{B}"
  (declare (type (simple-array double-float (3)) a b))
  (with-array-dimension-check*
      ((a b) '(3))                      ;dimension must be 3
    (let ((c (or c (make-vector 3))))
      (declare (type (simple-array double-float (3)) c))
      (let ((xa (x a)) (xb (x b))
            (ya (y a)) (yb (y b))
            (za (z a)) (zb (z b)))
        (declare (type double-float xa xb ya yb za zb))
        (setf [c 0] (- (* ya zb) (* za yb)))
        (setf [c 1] (- (* za xb) (* xa zb)))
        (setf [c 2] (- (* xa yb) (* ya xb))))
      (the (simple-array double-float (3)) c))))

;; scale function
(defun vscale (k vec &optional (buf nil))
  "scale a vector, VEC with K. K is a double-float and K is
a (simple-array double-float).

You can use BUF, the third argument, to reduce heap allocation.

 example::

   (vscale -1.0 #d(1 2 3)) => #d(-1 -2 -3)"
  (declare (type double-float k)
           (type (simple-array double-float) vec))
  (let ((dim (vector-dimension vec)))
    (declare (type fixnum dim))
    (let ((buf (or buf (make-vector dim))))
      (declare (type (simple-array double-float) buf))
      (dotimes (i dim)
        (setf [buf i] (the double-float (* k [vec i]))))
      (the (simple-array double-float) buf))))

(declaim (inline norm))
(defun norm (a)
  "calculate norm of the vector A.

A is required to be a (simple-array double-float)."
  (declare (type (simple-array double-float) a))
  (the double-float (sqrt (v. a a))))

(defun distance (a b)
  "calculate th distance between A and B.

A and B are required to be a (simple-array double-float) and have the
same length."
  (declare (type (simple-array double-float) a b))
  (the double-float (norm (v- a b))))

(defun eps-vector= (a b &optional (diff +eps+))
  "returns T if the vectors A and B is near enough."
  (declare (type (simple-array double-float) a b)
           (type double-float diff))
  (eps= (distance a b) 0.0d0 diff))

(defun normalize-vector (a
                         &optional (result (make-vector (vector-dimension a))))
  "scale a vector A into a unit vector.

You can use the second argument, RESULT, to reduce heap allocation.
In order to specify RESULT, you need give a double vector which has the
same length to A.

 example::

    (normalize-vector #d(2 0 0)) => #(1 0 0)"
  (let ((len (norm a)))
    (declare (type double-float len))
    (the (simple-array double-float) (vscale (/ 1.0d0 len) a result))))

(defun vector-sum (vec)
  "calculate summation of a vector like.

 example::

  (vector-sum #d(1.0d0 2.0d0 3.0d0)) => 6.0d0"
  (declare (type (simple-array double-float) vec))
  (let ((ret 0.0d0))
    (declare (type double-float ret))
    (dotimes (i (vector-dimension vec))
      (declare (type fixnum i))
      (+== ret [vec i]))
    (the double-float ret)))

;; utility, not fast
(defun make-random-vector (dim &key (min -10000.0d0) (max 10000.0d0))
  "make a double vector whose dimension equals to DIM, and fill the vector
with the random values between MIN and MAX."
  (declare (type fixnum dim)
           (type double-float min max))
  (let ((v (make-vector dim)))
    (declare (type (simple-array double-float) v))
    (dotimes (i dim)
      (declare (type fixnum i))
      (let ((r (random-range min max)))
        (declare (type double-float r))
        (setf [v i] r)))
    v))

(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
