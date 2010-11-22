;;================================================
;; vector.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

;;(declaim (optimize (speed 3) (safety 0) (debug 1) (space 0)))

(in-package :nurarihyon)

(enable-nurarihyon-reader-syntax)

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

(declaim-inline-nhfun vector-dimension)
(define-nhfun vector-dimension (vec)
  "Return  dimension of a vector. the vector muse be a simple-array
of double-float.

 example::

  (vector-dimension #d(1.0d0 2.0d0 3.0d0)) => 3"
  (declare (type (simple-array double-float) vec))
  (the fixnum (array-dimension vec 0)))

(declaim-inline-nhfun make-vector)
(define-nhfun make-vector (dim &key (initial-element 0.0d0))
  "Allocate a vector with dim dimension. you can use :initial-element
keyword to specify the value of the vector.

 example::

   (make-vector 3) => #d(0.0d0 0.0d0 0.0d0)
   (make-vector 2 :initial-element 2.0d0) => #(2.0d0 2.0d0)"
  (declare (type fixnum dim)
           (type double-float initial-element))
  (the (simple-array double-float)
    (make-array dim :element-type 'double-float
                :initial-element initial-element)))

(declaim-inline-nhfun make-vector3)
(define-nhfun make-vector3 (&key (initial-element 0.0d0))
  "Allocate a 3-dimension vector. this function is specialized for 3 dimension.
:initial-element allows you to specify the values of the vector.

 example::

   (make-vector3) => #(0.0d0 0.0d0 0.0d0)
   (make-vector3 :initial-element 2.0d0) => #(2.0d0 2.0d0 2.0d0)"
  (declare (type double-float initial-element))
  (the (simple-array double-float (3))
    (make-array 3 :element-type 'double-float
                :initial-element initial-element)))

(declaim-inline-nhfun make-vector4)
(define-nhfun make-vector4 (&key (initial-element 0.0d0))
  "Allocate a 4-dimension vector. this function is specialized for 4 dimension.
:initial-element allows you to specify the values of the vector.

 example::

   (make-vector4) => #(0.0d0 0.0d0 0.0d0 0.0d0)
   (make-vector4 :initial-element 2.0d0) => #(2.0d0 2.0d0 2.0d0 2.0d0)"
  (declare (type double-float initial-element))
  (the (simple-array double-float (4))
    (make-array 4 :element-type 'double-float
                :initial-element initial-element)))

(declaim-inline-nhfun double-vector)
(define-nhfun double-vector (&rest args)
  "this is a utility function to make a double vector. this function will
create the vector which has ARGS as contents.

 example::

   (double-vector 1 2 3) => #(1.0d0 2.0d0 3.0d0)"
  (the (simple-array double-float)
    (make-array (length args) :element-type 'double-float
                   :initial-contents
                   (mapcar #'(lambda (x) (coerce x 'double-float)) args))))

(define-compiler-macro with-2vector-dimension-bind-and-check
    ((dim a b) &rest args)
  "verificate the vector A and B has the same length dimension and
bind the dimension to DIM when evaluating ARGS.

If they does not have the save dimension, this macro will raise a condition
vector-dimension-mismatch."
  (if *nurarihyon-optimization*
      `(let ((,dim ($vector-dimension ,a)))
         ,@args)
      `(let ((,dim ($vector-dimension ,a)))
         (declare (type fixnum ,dim))
         (if (= ,dim ($vector-dimension ,b))
             (progn ,@args)
             (error 'vector-dimension-mismatch
                    :vector ,b :required-dimension ,dim)))))

(define-compiler-macro with-ensure-vector-dimension
    ((vec dim) &rest form)
  "ensure VEC has a DIM dimension. if not, vector-dimension-mismatch is
signaled.

if *NURARIHYON-OPTIMIZATION* is T, this macro do nothing and is expanded into
progn."
  (if *nurarihyon-optimization*
      `(progn ,@form)
      `(if (= ,dim ($vector-dimension ,vec))
           (progn ,@form)
           (error 'vector-dimension-mismatch
                  :vector ,vec :required-dimension ,dim))))

(define-compiler-macro with-ensure-2vectors-dimension* (((a b) dim) &rest form)
  "verificate two double vectors, A and B, have DIM dimension.
if not, WITH-ENSURE-2VECTORS-DIMENSION* raises VECTOR-DIMENSION-MISMATCH
condition.

if *NURARIHYON-OPTIMIZATION* is T, nothing is done and FORM is expanded
into progn."
  `(with-ensure-vector-dimension (,a ,dim)
     (with-ensure-vector-dimension (,b ,dim)
       ,@form)))

(define-compiler-macro with-ensure-2vectors-dimension ((a b) &rest form)
  "verificate two double vectors, A and B, have the same dimension.
if not, WITH-ENSURE-2VECTORS-DIMENSION raises VECTOR-DIMENSION-MISMATCH
condition.

if *NURARIHYON-OPTIMIZATION* is T, nothing is done and FORM is expanded
into progn."
  (let ((dim (gensym)))
    (if *nurarihyon-optimization*
        `(progn ,@form)                 ;not needed?
        `(let ((,dim (vector-dimension ,a)))
           (with-ensure-vector-dimension (,b ,dim)
             ,@form)))))

(declaim-inline-nhfun copy-vector)
(define-nhfun copy-vector (a &optional (b nil))
  "copy the double vector A to B and return B.

You can specify B, the second argument, to reduce heap allocation.
If not, COPY-VECTOR will allocate another vector which has
the same length to A.

A and B) must be a (simple-array double-float) and have the same length."
  (declare (type (simple-array double-float) a))
  (let ((b (or b ($make-vector ($vector-dimension a)))))
    (declare (type (simple-array double-float) b))
    (with-ensure-2vectors-dimension (a b)
      (the (simple-array double-float) ($copy-vector* a b)))))

(define-nhfun copy-vector* (a b)
  "this is a low level api to copy the double vector A to B and return B.
A and B must be a (simple-array double-float) and have the same length."
  (declare (type (simple-array double-float) a b))
  (with-2vector-dimension-bind-and-check (dim a b)
    (dotimes (i dim)
      (setf [ b i] [a i]))
    (the (simple-array double-float) b)))

;; vector operators
(define-nhfun v+ (a b &optional (c nil))
  "add two vectors, A and B, and return the result.

A and B are required to be a (simple-array double-float) and have the same
length.

You can use C, the third argument, to reduce heap allocation.

 example::

  (v+ #d(1 2) #d(3 4)) => #(4 6)
  (let ((buf (make-vector 2)))
    (v+ #d(1 2) #d(3 4) buf)            ;=> #(4 6)
    ...)"
  (declare (type (simple-array double-float) a b))
  (with-2vector-dimension-bind-and-check (dim a b)
    (let ((c (or c ($make-vector dim))))
      (declare (type (simple-array double-float) c))
      (dotimes (i dim)
        (setf [c i] (+ [a i] [b i])))
      (the (simple-array double-float) c))))

(define-nhfun v- (a b &optional (c nil))
  "substitute B from A and return the result.

A and B are required to be a (simple-array double-float) and have the same
length.

You can use C, the third argument, to reduce heap allocation.

 example::

   (v- #d(3 4) #d(1 2)) => #(2 2)
   (let ((buf (make-vector 2)))
    (v- #d(1 2) #d(3 4) buf)            ;=> #(-2 -2)
    ...)"
  (declare (type (simple-array double-float) a b))
  (with-2vector-dimension-bind-and-check (dim a b)
    (let ((c (or c ($make-vector dim))))
      (declare (type (simple-array double-float) c))
      (dotimes (i dim) (setf [c i] (- [a i] [b i])))
      (the (simple-array double-float) c))))

;; dot product
(define-nhfun v. (a b)
  "calculate a dot product of A and B.

A and B are required to be a (simple-array double-float) and have the same
length.

.. math::

    v.(A, B) = \sigma_{i} A_{i}B_{i}"
  (declare (type (simple-array double-float) a b))
  (with-2vector-dimension-bind-and-check (dim a b)
    (let ((ret 0.0d0))
      (declare (type double-float ret))
      (dotimes (i dim)
        (+== ret (the double-float (* [a i] [b i]))))
      (the double-float ret))))

;; cross product
(define-nhfun v* (a b &optional (c nil))
  "calculate a cross product of A and B.

A and B are required to be a (simple-array double-float (3)).

You can use C, the third argument, to reduce heap allocation.

.. math::

   \bold{C} = \bold{A} \times \bold{B}"
  (declare (type (simple-array double-float (3)) a b))
  ;;(with-2vectors-dimension-check
  (with-ensure-2vectors-dimension*
      ((a b) 3)                      ;dimension must be 3
    (let ((c (or c ($make-vector 3))))
      (declare (type (simple-array double-float (3)) c))
      (let ((xa (x a)) (xb (x b))
            (ya (y a)) (yb (y b))
            (za (z a)) (zb (z b)))
        (declare (type double-float xa xb ya yb za zb))
        (setf [c 0] (- (* ya zb) (* za yb)))
        (setf [c 1] (- (* za xb) (* xa zb)))
        (setf [c 2] (- (* xa yb) (* ya xb))))
      (the (simple-array double-float (3)) c))))

(declaim-inline-nhfun vector-angle)
(define-nhfun vector-angle (a b)
  "return the angle between A and B"
  (declare (type (simple-array double-float) a b))
  (with-ensure-2vectors-dimension (a b)
    (let ((dot ($v. a b))               ; |a||b|cos(theta)
          (|a| ($norm a))
          (|b| ($norm b)))
      (declare (type double-float dot |a| |b|))
      (let ((|ab| (* |a| |b|)))
        (declare (type double-float |ab|))
        (if (= |ab| 0.0d0)
            (error 'devided-by-zero)
            (the double-float (acos (/ dot (* |ab|)))))))))

;; scale function
(define-nhfun vscale (k vec &optional (buf nil))
  "scale a vector, VEC with K. K is a double-float and K is
a (simple-array double-float).

You can use BUF, the third argument, to reduce heap allocation.

 example::

   (vscale -1.0 #d(1 2 3)) => #(-1 -2 -3)"
  (declare (type double-float k)
           (type (simple-array double-float) vec))
  (let ((dim ($vector-dimension vec)))
    (declare (type fixnum dim))
    (let ((buf (or buf ($make-vector dim))))
      (declare (type (simple-array double-float) buf))
      (dotimes (i dim)
        (setf [buf i] (the double-float (* k [vec i]))))
      (the (simple-array double-float) buf))))

(declaim-inline-nhfun norm)
(define-nhfun norm (a)
  "calculate norm of the vector A.

A is required to be a (simple-array double-float)."
  (declare (type (simple-array double-float) a))
  (let ((s ($v. a a)))
    (declare (type double-float s))
    (the double-float (sqrt s))))

(declaim-inline-nhfun distance)
(define-nhfun distance (a b)
  "calculate th distance between A and B.

A and B are required to be a (simple-array double-float) and have the
same length."
  (declare (type (simple-array double-float) a b))
  (with-ensure-2vectors-dimension (a b)
    (the double-float ($norm ($v- a b)))))

(declaim-inline-nhfun eps-vector=)
(define-nhfun eps-vector= (a b &optional (diff +eps+))
  "returns T if the vectors A and B is near enough."
  (declare (type (simple-array double-float) a b)
           (type double-float diff))
  (with-ensure-2vectors-dimension (a b)
    (eps= ($distance a b) 0.0d0 diff)))

(declaim-inline-nhfun normalize-vector)
(define-nhfun normalize-vector (a &optional (result nil))
  "scale a vector A into a unit vector.

You can use the second argument, RESULT, to reduce heap allocation.
In order to specify RESULT, you need give a double vector which has the
same length to A.

 example::

    (normalize-vector #d(2 0 0)) => #(1 0 0)"
  (declare (type (simple-array double-float) a))
  (let ((len ($norm a))
        (dim ($vector-dimension a)))
    (declare (type double-float len)
             (type fixnum dim))
    (let ((result (or result ($make-vector dim))))
      (declare (type (simple-array double-float) result))
      (the (simple-array double-float) ($vscale (/ 1.0d0 len) a result)))))

(define-nhfun vector-sum (vec)
  "calculate summation of a vector.

 example::

  (vector-sum #d(1.0d0 2.0d0 3.0d0)) => 6.0d0"
  (declare (type (simple-array double-float) vec))
  (let ((ret 0.0d0))
    (declare (type double-float ret))
    (dotimes (i ($vector-dimension vec))
      (declare (type fixnum i))
      (+== ret [vec i]))
    (the double-float ret)))

(define-nhfun make-random-vector (dim &key (min -10000.0d0) (max 10000.0d0))
  "make a double vector whose dimension equals to DIM, and fill the vector
with the random values between MIN and MAX."
  (declare (type fixnum dim)
           (type double-float min max))
  (let ((v ($make-vector dim)))
    (declare (type (simple-array double-float) v))
    (dotimes (i dim)
      (declare (type fixnum i))
      (let ((r ($random-range min max)))
        (declare (type double-float r))
        (setf [v i] r)))
    v))

(disable-nurarihyon-reader-syntax)
