;;================================================
;; vector.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 0)
		   (space 0)))

(in-package :nurarihyon)

(eval-when (:compile-toplevel)
  (enable-nurarihyon-reader-syntax))

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

;; template for make-**-vector
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

If they does not have the save dimension, this macro will raise an error.
"
  `(let ((,dim (vector-dimension ,a)))
     (declare (type fixnum ,dim))
     (if (= ,dim (vector-dimension ,b))
         (progn ,@args)
         (error "vector dimension mismatch"))))

(defun copy-vector (a b)
  "copy the double vector A to B and return B.
A and B must be a (simple-array double-float) and have the same length."
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (dotimes (i dim)
      (setf [ b i] [a i]))
    (the (simple-array double-float) b)))

;; vector operators
;; add
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

;; sub
(defun v- (a b &optional (c nil))
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (let ((c (or c (make-vector dim))))
      (declare (type (simple-array double-float) c))
      (dotimes (i dim) (setf [c i] (- [a i] [b i])))
      (the (simple-array double-float) c))))

;; dot product
(defun v. (a b)
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (let ((ret 0.0d0))
      (declare (type double-float ret))
      (dotimes (i dim)
        (+== ret (the double-float (* [a i] [b i]))))
      (the double-float ret))))

;; cross product
(defun v* (a b &optional (c nil))
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
(defun scale (k vec &optional (buf nil))
  (declare (type double-float k)
           (type (simple-array double-float) vec))
  (let ((dim (vector-dimension vec)))
    (declare (type fixnum dim))
    (let ((buf (or buf (make-vector dim))))
      (declare (type (simple-array double-float) buf))
      (dotimes (i dim)
        (setf [buf i] (the double-float (* k [vec i]))))
      (the (simple-array double-float) buf))))

;; norm function
(defun norm (a)
  (declare (type (simple-array double-float) a))
  (the double-float (sqrt (v. a a))))

;; distance function
(defun distance (a b)
  (declare (type (simple-array double-float) a b))
  (the double-float (norm (v- a b))))

(defun eps-vector= (a b &optional (diff +eps+))
  (declare (type (simple-array double-float) a b)
           (type double-float diff))
  (eps= (distance a b) 0.0d0 diff))

(defun normalize-vector (a
                         &optional (result (make-vector (vector-dimension a))))
  (let ((len (norm a)))
    (declare (type double-float len))
    (the (simple-array double-float) (scale (/ 1.0d0 len) a result))))

(defun vector-sum (vec)
  "calculate a summation of a vector like.

example::
  (vector-sum #d(1.0d0 2.0d0 3.0d0)) => 6.0d0"
  (declare (type (simple-array double-float) vec))
  (let ((ret 0.0d0))
    (declare (type double-float ret))
    (dotimes (i (vector-dimension vec))
      (declare (type fixnum i))
      (+== ret (aref vec i)))
    (the double-float ret)))

;; not fast implementation
(defun vector-mean (vecs)               ;...?
  (declare (type list vecs))
  (the (simple-array double-float)
    (scale (/ 1.0d0 (length vecs)) (reduce #'v+ vecs))))

;; utility, not fast
(defun make-random-vector (dim &key (min -10000.0d0) (max 10000.0d0))
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

;; (defun vector-range (start &optional stop (step 1.0d0))
;;   "Return evenly spaced values within a specified interval like
;; numpy.arange.
;; example::
;;  (vector-range 3.0d0) => #d(0.0d0 1.0d0 2.0d0)
;;  (vector-range 2.0d0 5.0d0) => #d(2.0d0 3.0d0 4.0d0 5.0d0)
;;  (vector-range 3 7 2)"
;;   (let ((n (if stop (/ (- stop start) step) start)) ;length of vector
;;         (start-num (if stop start 0.0d0)))
;;     (declare (type fixnum n)
;;              (type double-float start-num))
;;     (let ((ret (make-vector n)))
;;       (declare (type (simple-array double-float) ret))
;;       (dotimes (i n)
;;         (declare (type fixnum i))
;;         (setf (aref ret i) (+ start-num (* i step))))
;;       (the (simple-array double-float) ret))))


(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
