;;================================================
;; base.lisp
;;
;; all of the basic operators and constructors
;; are defined for real, integer(fixnum), float(single-float)
;; and double(double-float).
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 0)
		   (space 0)))

(in-package :nurarihyon)

;; constant
(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +e+ (exp 1.0d0))
  (defconstant +pi+ pi)
  (defconstant +2pi+ (* 2.0d0 +pi+))
  (defconstant +pi/2+ (/ +pi+ 2.0d0))
  (defconstant +pi/4+ (/ +pi+ 4.0d0))
  (defconstant +eps+ 0.0001d0))

(eval-when (:compile-toplevel)
  (enable-aref-reader-syntax))

;; operator utility
(defmacro with-array-dimension-check ((a b) &rest args)
  ;;  check dimensions of vecs are equal or not
   `(if (equal (array-dimensions ,a) (array-dimensions ,b))
        (progn ,@args)
        (error "vector dimension mismatch")))

(defmacro with-array-dimensions-bind-and-check ((dims a b) &rest args)
  ;;  check dimensions of vecs are equal or not
  `(let ((,dims (array-dimensions ,a)))
     (if (equal ,dims (array-dimensions ,b))
         (progn ,@args)
         (error "vector dimension mismatch"))))

(defmacro with-array-dimension-check* ((vecs dim) &rest args)
  ;;  check dimensions of vecs are equal to dim or not
  `(if (and (equal (array-dimensions ,(car vecs)) ,dim)
            (equal (array-dimensions ,(cadr vecs)) ,dim))
       (progn ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimension-check-trans ((a b) &rest args)
  ;;  The number of vecs must be two.
  `(if (= (cadr (array-dimensions ,a))
	  (car (array-dimensions ,b)))
       (progn ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimension-check-trans* ((a b) dim &rest args)
  ;;  The number of vecs must be two.
  `(if (and (equal (array-dimensions ,a) ,dim)
            (equal (reverse (array-dimensions ,b)) ,dim))
       (progn ,@args)
       (error "vector dimension mismatch")))

;; for utility functions
(defmacro -== (a b)
  `(progn (setf ,a (- ,a ,b))))

(defmacro +== (a b)
  `(progn (setf ,a (+ ,a ,b))))

(defmacro *== (a b)
  `(progn (setf ,a (* ,a ,b))))

(defmacro /== (a b)
  `(progn (setf ,a (/ ,a ,b))))

;; (defun mv* (mat vec &optional (result nil))
;;   "return vector.
;;    mat = n x m  vec = 1 x m
;;    +---------+     +-+
;;    |         |     | |
;;    |         |  x  | |
;;    |         |     | |
;;    +---------+     +-+
;;   "
;;   (declare (type (simple-array single-float) mat vec))
;;   (let ((mat-dimensions (array-dimensions mat))
;;         (vec-dimension (car (array-dimensions vec))))
;;      (declare (type fixnum vec-dimension)
;;               (type list mat-dimensions))
;;     ;; error check for dimension
;;     (when (not (= vec-dimension (cadr mat-dimensions)))
;;       (error "dimension mismatch"))
;;     (let ((column-dimension (cadr mat-dimensions)))
;;       (declare (type fixnum column-dimension))
;;       (let ((fv (make-float-vector column-dimension)))
;;         (declare (type (simple-array single-float) fv))
;;         (dotimes (n column-dimension)
;;           (let ((element 0.0))
;;             (declare (type single-float element))
;;             (dotimes (m vec-dimension)
;;               (declare (type fixnum m))
;;               (setf element (+ element (* (aref mat n m) (aref vec m)))))
;;             (setf (aref fv n) element)))
;;         (if result
;;             (copy-vector fv result)     ; copy fv -> result
;;             fv)))))

;; utility
(defun rad2deg (rad)
  "convert from radian to degree"
  (declare (type double-float rad))
  (the double-float (* rad (/ 360.0d0 +2pi+))))

(defun deg2rad (deg)
  "convert from degree to radian"
  (declare (type double-float deg))
  (the double-float (* deg (/ +2pi+ 360.d0))))

;; (defun list->vector (list)
;;   "convert list to float vector"
;;   (coerce list '(array single-float 1)))

;; (defun vector->list (vec)
;;   "convert vector to list"
;;   (coerce vec 'cons))

;; ;; あほい
;; (defun list->matrix (list)
;;   "convert list to matrix."
;;   (let ((mat (make-float-matrix (length list) (length (car list)))))
;;     (dotimes (i (length list))
;;       (dotimes (j (length (car list)))
;;         (setf (aref mat i j) (coerce (elt (elt list i) j) 'single-float))))
;;     mat))

;; ;; あほい
;; (defun matrix->list (mat)
;;   (let* ((dims (array-dimensions mat))
;;          (row (car dims))
;;          (col (cadr dims)))
;;     (let ((ret nil))
;;     (dotimes (i row)
;;       (push (coerce (matrix-row mat i) 'cons) ret))
;;     (nreverse ret))))
    

;; (defun print-matrix (mat)
;;   (let ((dims (array-dimensions mat)))
;;     (dotimes (i (car dims))
;;       (dotimes (j (cadr dims))
;;         (format t "~0,3f " (aref mat i j))
;;         )
;;       (format t "~%")
;;       )
;;     ))

(defun random-range (min max)
  "return random value between min and max."
  (declare (type number min max))
  (let ((d (- max min)))
    (declare (type number d))
    (+ (random d) min)))

;; ;; x x x x x
;; ;; x x x x x
;; ;; x x x x x
;; (defun matrix-row (mat id)
;;   (declare (type (simple-array single-float) mat)
;;            (type fixnum id))
;;   (let ((size (cadr (array-dimensions mat))))
;;     (let ((ret (make-float-vector size)))
;;       (dotimes (i size)
;;         (setf (aref ret i) (aref mat id i)))
;;       ret)))

;; (defun (setf matrix-row) (val mat id)
;;   (declare (type (simple-array single-float) mat val)
;;            (type fixnum id))
;;   (let ((size (cadr (array-dimensions mat))))
;;     (declare (type fixnum size))
;;     (dotimes (i size)
;;       (declare (type fixnum i))
;;       (setf (aref mat id i) (aref val i)))
;;     mat))

;; (defun matrix-column (mat id)
;;   (declare (type (simple-array single-float) mat)
;;            (type fixnum id))
;;   (let ((size (car (array-dimensions mat))))
;;     (declare (type fixnum size))
;;     (let ((ret (make-float-vector size)))
;;       (dotimes (i size)
;;         (setf (aref ret i) (aref mat i id)))
;;       ret)))

;; (defun normalize-vector (v)
;;   (declare (type (simple-array single-float) v))
;;   (let ((k (/ 1.0 (norm v))))
;;     (scale k v)))

(declaim (inline ->double-float))
(defun ->double-float (val)
  (coerce val 'double-float))

(declaim (inline ->single-float))
(defun ->single-float (val)
  (coerce val 'single-float))

;; eps=
(defmacro defeps= (name type default-diff)
  `(defun ,name (a b &optional (diff ,default-diff))
     (declare (type ,type a b diff))
     (< (abs (- a b)) diff)))

;; (defun eps-matrix= (a b &optional (diff +eps+))
;;   "returns t if matrix a and b is near enough."
;;   (declare (type (simple-array single-float) a b)
;; 	   (type single-float diff))
;;   (let ((a-dims (array-dimensions a))
;; 	(b-dims (array-dimensions b)))
;;     (declare (type list a-dims b-dims))
;;     (if (equal a-dims b-dims)
;; 	(progn
;; 	  (dotimes (i (car a-dims))
;; 	    (declare (type fixnum i))
;; 	    (dotimes (j (cadr a-dims))
;; 	      (declare (type fixnum j))
;; 	      (if (not (eps= (aref a i j) (aref b i j) diff))
;; 		  (return-from eps-matrix= nil))
;; 	      ))
;; 	  t)				;if passed
;; 	nil)))

(defeps= eps= real +eps+)
(defeps= ieps= fixnum 1)
(defeps= feps= single-float #.(coerce +eps+ 'single-float))
(defeps= deps= double-float +eps+)

(eval-when (:compile-toplevel)
  (disable-aref-reader-syntax))
