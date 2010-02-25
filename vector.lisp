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
  `(aref ,a 0))
(defmacro y (a)
  `(aref ,a 1))
(defmacro z (a)
  `(aref ,a 2))
;;
;;==================================

(declaim (inline vector-dimension))
(defun vector-dimension (a)
  (declare (type simple-array a))
  (the fixnum (array-dimension a 0)))

;; template for make-**-vector
(defun make-vector (dim &key (initial-element 0.0d0))
  (declare (type fixnum dim)
           (type double-float initial-element))
  (the (simple-array double-float)
    (make-array dim :element-type 'double-float
                :initial-element initial-element)))

(defun make-vector3 (&key (initial-element 0.0d0))
  (declare (type double-float initial-element))
  (the (simple-array double-float (3))
    (make-array 3 :element-type 'double-float
                :initial-element initial-element)))

(defun double-vector (&rest args)
  (the (simple-array double-float)
    (make-array (length args) :element-type 'double-float
                   :initial-contents
                   (mapcar #'(lambda (x) (coerce x 'double-float)) args))))

(defmacro with-vector-dimension-bind-and-check ((dim a b) &rest args)
  ;;  check dimensions of vecs are equal or not
  `(let ((,dim (vector-dimension ,a)))
     (declare (type fixnum ,dim))
     (if (= ,dim (vector-dimension ,b))
         (progn ,@args)
         (error "vector dimension mismatch"))))

(defmacro with-vector-dimension-bind-and-check* ((dim a b n) &rest args)
  ;;  check dimensions of vecs are equal or not
  `(let ((,dim (vector-dimension ,a)))
     (declare (type fixnum ,dim))
     (if (and (= ,dim ,n)
              (= ,dim (vector-dimension ,b)))
         (progn ,@args)
         (error "vector dimension mismatch"))))

(defun copy-vector (a b)
  (declare (type (simple-array double-float) a b))
  (with-vector-dimension-bind-and-check (dim a b)
    (dotimes (i dim)
      (setf [ b i] [a i]))
    (the (simple-array double-float) b)))

;; vector operators
;; add
(defun v+ (a b &optional (c nil))
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
      (setf [c 0] (- (* (y a) (z b)) (* (z a) (y b))))
      (setf [c 1] (- (* (z a) (x b)) (* (x a) (z b))))
      (setf [c 2] (- (* (x a) (y b)) (* (y a) (x b))))
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

;; not fast implementation
(defun vector-mean (vecs)
  (declare (type list vecs))
  (the (simple-array double-float)
    (scale (/ 1.0d0 (length vecs)) (reduce #'v+ vecs))))

;; utility, not fast
(defun make-random-vector (dim &key (min -10000.0d0) (max 10000.0d0))
  (declare (type fixnum dim)
           (type double-float min max))
  (let ((v (make-vector dim)))
    (declare (type (simple-array single-float) v))
    (dotimes (i dim)
      (declare (type fixnum i))
      (let ((r (random-range min max)))
        (declare (type double-float v))
        (setf [v i] r)))
    v))

(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
