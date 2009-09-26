;;================================================
;; base.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
;;(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)
		   (space 0)))

(in-package :nurarihyon)

;; constant
(eval-when (:compile-toplevel :load-toplevel)
  (defconstant +e+ (exp 1.0))
  (defconstant +pi+ (coerce pi 'single-float))
  (defconstant +2pi+ (* 2.0 +pi+))
  (defconstant +pi/2+ (/ +pi+ 2.0))
  (defconstant +pi/4+ (/ +pi+ 4.0))
  (defconstant +eps+ 0.0001))


;;==================================
;; in-package utility
(declaim (inline x))
(defun x (a)
  (declare (type (simple-array single-float) a))
  (aref a 0))

(declaim (inline y))
(defun y (a)
  (declare (type (simple-array single-float) a))
  (aref a 1))

(declaim (inline z))
(defun z (a)
  (declare (type (simple-array single-float) a))
  (aref a 2))
;;
;;==================================

(defun eps= (a b &optional (diff +eps+))
  "returns t if a is nearly equal to b."
  (declare (type single-float a b diff))
  (< (abs (- a b)) diff))

(defun eps-vector= (a b &optional (diff +eps+))
  "returns t if vector a and b is near enough."
  (declare (type simple-array a b)
	   (type single-float diff))
  (eps= (distance a b) 0.0 diff))

(defun eps-matrix= (a b &optional (diff +eps+))
  "returns t if matrix a and b is near enough."
  (declare (type simple-array a b)
	   (type single-float diff))
  (let ((a-dims (array-dimensions a))
	(b-dims (array-dimensions b)))
    (declare (type list a-dims b-dims))
    (if (equal a-dims b-dims)
	(progn
	  (dotimes (i (car a-dims))
	    (declare (type fixnum i))
	    (dotimes (j (cadr a-dims))
	      (declare (type fixnum j))
	      (if (not (eps= (aref a i j) (aref b i j) diff))
		  (return nil))
	      ))
	  t)				;if passed
	nil)))

(defun make-integer-vector (dim &key (initial-element 0))
  "allocate integer vector.
   You have to set dimension, dimension must be a fixnum.
   You can also give :initial-element, to fill vector as initial value."
  (declare (type fixnum dim)
           (type fixnum initial-element))
  (make-array dim
              :element-type 'fixnum
              :initial-element initial-element))

(defun make-float-vector (dim &key (initial-element 0.0))
  "allocate float vector.
   You have to set dimension, dimension must be a fixnum.
   You can also give :initial-element, to fill vector as initial value."
  (declare (type fixnum dim)
           (type single-float initial-element))
  (make-array dim
              :element-type 'single-float
              :initial-element initial-element))

(defun make-float-matrix (row column &key (initial-element 0.0))
  "allocate float matrix.
   You have to set row and column, fixnum."
  (declare (type fixnum row column)
           (type single-float initial-element))
  (make-array (list row column)
              :element-type 'single-float
              :initial-element initial-element))

(defun make-identity-matrix (dim)
  "allocate identity matrix (double matrix)."
  (declare (type fixnum dim))
  (let ((mat (make-float-matrix dim dim)))
    (declare (type (simple-array single-float) mat))
    (dotimes (i dim)
      (declare (type fixnum i))
      (setf (aref mat i i) 1.0))
    mat))

(defun float-vector (&rest args)
  (make-array (length args) :element-type 'single-float
              :initial-contents (mapcar #'(lambda (x) (coerce x 'single-float)) args)))

;; operator utility
(defmacro with-array-dimension-check (vecs &rest args)
  "check dimensions of vecs are equal or not"
   `(if (equal (array-dimensions ,(car vecs))
               (array-dimensions ,(cadr vecs)))
        (progn
          ,@args)
        (error "vector dimension mismatch")))

(defmacro with-array-dimension-check* ((vecs dim) &rest args)
  "check dimensions of vecs are equal to dim or not"
  `(if (and (equal (array-dimensions ,(car vecs))
                   ,dim)
            (equal (array-dimensions ,(cadr vecs))
                   ,dim))
       (progn
         ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimension-check-trans ((a b) &rest args)
  "The number of vecs must be two."
  `(if (= (cadr (array-dimensions ,a))
	  (car (array-dimensions ,b)))
       (progn
	 ,@args)
       (error "vector dimension mismatch")))

(defmacro with-array-dimension-check-trans* ((a b) dim &rest args)
  "The number of vecs must be two."
  `(if (and (equal (array-dimensions ,a)
                   ,dim)
            (equal (reverse (array-dimensions ,b))
                   ,dim))
       (progn
         ,@args)
       (error "vector dimension mismatch")))

(defmacro with-square-matrix-check (mat &rest args)
  (let ((dims (gensym)))
    `(let ((,dims (array-dimensions ,mat)))
       (if (= (car ,dims)
              (cadr ,dims))
           (progn
             ,@args)
           (error "array is not identity matrix")
           ))))

(defun copy-vector (a b)
  "copy elements from vector a to vector b.
   this is a destructive function."
  (declare (type (simple-array single-float) a b))
  (with-array-dimension-check (a b)
    (let ((dims (array-dimensions a)))
      (dotimes (i (car dims))
        (setf (aref b i)
              (aref a i))
        )
      b)))

(defun copy-matrix (a b)
  "copy elements from matrix a to matrix b.
   this is a destructive function."
  (declare (type (simple-array single-float) a b))
  (with-array-dimension-check (a b)
    (let ((dims (array-dimensions a)))
      (dotimes (i (car dims))
        (dotimes (j (cadr dims))
          (setf (aref b i j)
                (aref a i j))
          ))
      b)))

;; for utility functions
(defmacro -== (a b)
  `(progn (setf ,a (- ,a ,b))))

(defmacro +== (a b)
  `(progn (setf ,a (+ ,a ,b))))

(defmacro *== (a b)
  `(progn (setf ,a (* ,a ,b))))

(defmacro /== (a b)
  `(progn (setf ,a (/ ,a ,b))))

;; vector operators
;; add
(defun v+ (a b &optional (c nil))
  "calculate addition of vector a and b.
   You can give vector c as a buffer."
  (declare (type simple-array a b))
  (with-array-dimension-check (a b)
    (let ((dim (length a)))
      (if (null c)
          (setf c (make-float-vector dim)))
      (dotimes (i dim)
        (setf (aref c i)
              (+ (aref a i) (aref b i))))
      c)))

;; sub
(defun v- (a b &optional (c nil))
  "calculate subtraction of vector a and b.
   You can give vector c as a buffer."
  (declare (type simple-array a b))
  (with-array-dimension-check (a b)
    (let ((dim (length a)))
      (if (null c)
          (setf c (make-float-vector dim)))
      (dotimes (i dim)
        (setf (aref c i)
              (- (aref a i) (aref b i))))
      c)))

;; dot product
(defun v. (a b)
  "calculate dot product"
  (declare (type simple-array a b))
  (with-array-dimension-check (a b)
    (let ((dim (length a))
          (ret 0.0))
      (declare (type fixnum dim)
	       (type single-float ret))
      (dotimes (i dim)
	(declare (type fixnum i))
        (setf ret (+ ret (* (aref a i) (aref b i)))))
      ret)))

;; cross product
(defun v* (a b &optional (c nil))
  "calculate cross product."
  (declare (type simple-array a b))
  (with-array-dimension-check*
      ((a b) '(3))
      (if (null c)
          (setf c (make-float-vector 3)))
      (setf (aref c 0)
            (- (* (y a)
                  (z b))
               (* (z a)
                  (y b))))
      (setf (aref c 1)
            (- (* (z a)
                  (x b))
               (* (x a)
                  (z b))))
      (setf (aref c 2)
            (- (* (x a)
                  (y b))
               (* (y a)
                  (x b))))
      c))

(defun scale (k vec &optional (buf))
  (declare (type number k)
           (type (simple-array single-float) vec))
  (let ((dim (car (array-dimensions buf))))
    (declare (type fixnum dim))
    (if (null buf)
        (setf buf (make-float-vector dim)))
    (dotimes (i dim)
      (setf (aref buf i) (* k (aref vec i))))
    buf))

(declaim (inline norm))
(defun norm (a)
  "a is vector. returns length of a"
  (declare (type simple-array a))
  (sqrt (v. a a)))

(declaim (inline distance))
(defun distance (a b)
  "calulate Euqlid distance between a and b"
  (declare (type simple-array a b))
  (norm (v- a b)))

;; matrix operators
(defun m+ (a b &optional (c nil))
  "calculate addition of two matrices.
   You can give the 3rd argument as a buffer"
  (with-array-dimension-check (a b)
    (let ((dims (array-dimensions a)))
      (if (null c)
          (setf c (make-float-matrix (car dims) (cadr dims))))
      (dotimes (i (car dims))
        (dotimes (j (cadr dims))
          (setf (aref c i j)
                (+ (aref a i j)
                   (aref b i j)))
          ))
      c)))

(defun m- (a b &optional (c nil))
  "calculate sub
   You can give the 3rd argument as a buffer"
  (with-array-dimension-check (a b)
    (let ((dims (array-dimensions a)))
      (if (null c)
          (setf c (make-float-matrix (car dims) (cadr dims))))
      (dotimes (i (car dims))
        (dotimes (j (cadr dims))
          (setf (aref c i j)
                (- (aref a i j)
                   (aref b i j)))
          ))
      c)))

(defun m* (a b &optional (c nil))
  "calculate product of two matricies.
   You can give the third parameter as a buffer."
  (declare (type simple-array a b))
  (with-array-dimension-check-trans (a b)
    (let ((dims-a (array-dimensions a))
          (dims-b (array-dimensions b)))
      (declare (type list dims-a dims-b))
      (if (null c)
          (setf c (make-float-matrix (car dims-a) (cadr dims-b))))
      ;;(declare (type (simple-array single-float) c))
      (if (not (eq b c))
          (let ((tmpv (make-float-vector (cadr dims-b))))
            (declare (type (simple-array single-float) tmpv))
            (dotimes (i (car dims-a))
              (dotimes (j (cadr dims-b))
                (let ((tmp 0.0))
                  (declare (type single-float tmp))
                  (dotimes (k (cadr dims-a))
                    (setf tmp (+ tmp (* (aref a i k)
                                        (aref b k j)))))
                  (setf (aref tmpv j) tmp)
                  ))
              (dotimes (k (cadr dims-b))
                ;; copy tmpv -> c[i, *]
                (setf (aref c i k) (aref tmpv k))
                )))
          (let ((tmpv (make-float-vector (car dims-a))))
	    (declare (type (simple-array single-float) tmpv))
            (dotimes (i (cadr dims-b))
              (dotimes (j (car dims-a))
                ;; c[j, i] = \sum_k a[j, k] * b[k, i]
                (let ((tmp 0.0))
                  (declare (type single-float tmp))
                  (dotimes (k (cadr dims-a))
                    (setf tmp (+ tmp (* (aref b k i)
                                        (aref a j k)))))
                  (setf (aref tmpv j) tmp)
                  ))
              (dotimes (k (cadr dims-b))
                ;; copy tmpv -> c[i, *]
                (setf (aref c k i) (aref tmpv k))
                )
              )
            ))
      c)))

(defun flip (mat &optional (result nil))
  "transpose matrix.
   You can give the 2nd argument as a buffer."
  (declare (type (simple-array single-float) mat))
  (let ((dims (array-dimensions mat)))
    (declare (list dims))
    (if (null result)
        (setf result (make-float-matrix (cadr dims) (car dims))))
    (dotimes (i (car dims))
      (dotimes (j (cadr dims))
        (setf (aref result j i) (aref mat i j))
        ))
    result))

(declaim (inline inverse-matrix))
(defun inverse-matrix (&rest args)
  (apply #'m-1 args))

(defun m-1 (mat &optional (result nil) (lu-mat nil))
  (with-square-matrix-check mat
    (let* ((dim (car (array-dimensions mat)))
           (pivot (make-integer-vector dim)))
      (if (null lu-mat)
          (setq lu-mat (make-float-matrix dim dim)))
      (if (null result)
          (setf result (make-float-matrix dim dim)))
      (copy-matrix mat lu-mat) ;; mat -> lu-mat
      (let ((determinant (lu-decompose lu-mat pivot)))
        ;; lu-mat --> LU分解後の行列
        (if (eps= determinant 0.0)     ;if ( det != 0 )
            ;; 行列式が0だと逆行列は求められない
            nil
            (progn
              (do ((k 0 (1+ k)))        ;for ( k = 0; k < n; k++ )
                  ((not (< k dim)))
                ;; forward
                (do ((i 0 (1+ i)))      ;for ( i = 0; i < n; i++ )
                    ((not (< i dim)))
                  (let* ((ii (aref pivot i))           ;ii = ip[i]
                         (tmp (if (= ii k) 1.0 0.0)))  ;t = (ii == k);
                    (do ((j 0 (1+ j)))                 ;for ( j = 0; j < i; j++ )
                        ((not (< j i)))
                      (-== tmp         ;t -== a[ii][j] * a_inv[j][k]
                           (* (aref lu-mat ii j)
                              (aref result j k))))
                    (setf (aref result i k) tmp) ;a_inv[i][k] = t
                    ))
                ;; backward
                (do ((i (1- dim) (1- i))) ;for ( i = n - 1; i >= 0; i-- )
                    ((not (>= i 0)))
                  (let ((tmp (aref result i k)) ;t = a_inv[i][k]l
                        (ii (aref pivot i)))    ;ii = ip[i]
                    (do ((j (1+ i) (1+ j)))     ;for ( j = i + 1; j < n; j++ )
                        ((not (< j dim)))
                      (-== tmp          ;t -== a[ii][j] * a_inv[j][k]
                           (* (aref lu-mat ii j)
                              (aref result j k))))
                    (setf (aref result i k) ;a_inv[i][k] = t / a[ii][i]
                          (/ tmp (aref lu-mat ii i)))
                    ))
                )
              result))))))

;; matをLU分解する
;; destructive function!!
(defun lu-decompose (result pivot)
  (declare (type simple-array result)
           (type (simple-array fixnum) pivot))
  (let ((dimension (car (array-dimensions result))))
    (let ((weight (make-float-vector dimension))) ;weight = new_vector(n);
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; initialize weight vector
    ;;
    ;; i: 0 ... dimension
    (do ((i 0 (1+ i)))                  ;for (k = 0; k < n; k++){
        ((not (< i dimension)))
      (setf (aref pivot i) i)           ;ip[k] = k;
      (let ((u 0.0))                   ;u = 0;
        ;; search max value in i th row
        ;; j: 0 ... dimension
        (do ((j 0 (1+ j)))              ;for (j = 0; j < n; j++){
            ((not (< j dimension)))
          (let ((tmp (abs (aref result i j)))) ;t = fabs(a[k][j])
            (if (> tmp u)                      ;if ( t > u )
                (progn
                  (setf u tmp)          ;u = t;
                  ))))     
        (if (eps= u 0.0)                    ;if ( u== 0.0 )
            (return-from lu-decompose 0.0)) ;goto EXIT;
        (setf (aref weight i) (/ 1.0 u))    ;weight[k] = 1 / u;
        ))
    ;;
    ;; finish initializing weight vector
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let ((det 1.0))                   ;det = 1
      ;; k: 0 ... dimension
      (do ((k 0 (1+ k)))                ;for ( k = 0; k < n; k++)
          ((not (< k dimension)))
       (let ((u -1.0)                 ;u = -1
              (ii 0)
              (j 0))
          ;; i: k ... dimension
          ;; find max element in (k,k) ... (i,k)
          ;; max value will be set in u
          ;; its index will be set in j
          ;; ii is a original pivot
          (do ((i k (1+ i)))            ;for ( i = k; i < n; i++ )
              ((not (< i dimension)))
            (setf ii (aref pivot i))    ;ii = ip[i]
            (let ((tmp (* (abs (aref result ii k)) (aref weight ii)))) ;t = fabs(a[ii][k] * weight[ii]
              (if (> tmp u)             ;if ( t > u )
                  (progn
                    (setf u tmp)        ;u = t
                    (setf j i)))))      ;j = i
          (let ((ik (aref pivot j)))    ;ik = ip[j]
            (if (not (= j k))           ;if ( j != k )
                (progn
                  (setf (aref pivot j) (aref pivot k)) ;ip[j] = ip[k]
                  (setf (aref pivot k) ik)             ;ip[k] = ik;
                  (setf det (- det))                   ;det = -det;
                  ))
            (let ((u (aref result ik k))) ;u = a[ik][k];
              (*== det u)                         ;det *== u;
              (if (eps= u 0.0)           ;if ( u == 0 )
                  (return-from lu-decompose det)) ;goto EXIT;
              (do ((i (1+ k) (1+ i)))             ;for ( i = k + 1; i < n; i++ )
                  ((not (< i dimension)))
                (let ((ii (aref pivot i))) ;ii = ip[i]
                  (setf (aref result ii k) (/ (aref result ii k) u)) ;t = (a[ii][k] /== u);
                  (let ((tmp (aref result ii k)))
                    (do ((j (1+ k) (1+ j))) ;for ( j = k + 1; j < n; j++ )
                        ((not (< j dimension)))
                      (-== (aref result ii j) (* tmp (aref result ik j))) ;a[ii][j] -== t * a[ik][j];
                      )
                    ))
                ))
            ))
        )
      det))))                           ;return determination


(defun mv* (mat vec &optional (result nil))
  "return vector.
   mat = n x m  vec = 1 x m
   +---------+     +-+
   |         |     | |
   |         |  x  | |
   |         |     | |
   +---------+     +-+
  "
  (declare (type (simple-array single-float) mat vec))
  (let ((mat-dimensions (array-dimensions mat))
        (vec-dimension (car (array-dimensions vec))))
     (declare (type fixnum vec-dimension)
              (type list mat-dimensions))
    ;; error check for dimension
    (when (not (= vec-dimension (cadr mat-dimensions)))
      (error "dimension mismatch"))
    (let ((column-dimension (cadr mat-dimensions)))
      (declare (type fixnum column-dimension))
      (let ((fv (make-float-vector column-dimension)))
        (declare (type (simple-array single-float) fv))
        (dotimes (n column-dimension)
          (let ((element 0.0))
            (declare (type single-float element))
            (dotimes (m vec-dimension)
              (declare (type fixnum m))
              (setf element (+ element (* (aref mat n m) (aref vec m)))))
            (setf (aref fv n) element)))
        (if result
            (copy-vector fv result)     ; copy fv -> result
            fv)))))

;; utility
(defun rad2deg (rad)
  "convert from radian to degree"
  (declare (type single-float rad))
  (* rad (/ 360.0 +2pi+)))

(defun deg2rad (deg)
  "convert from degree to radian"
  (declare (type single-float deg))
  (* deg (/ +2pi+ 360)))

(defun list->vector (list)
  "convert list to float vector"
  (coerce list '(array single-float 1)))

(defun vector->list (vec)
  "convert vector to list"
  (coerce vec 'cons))

;; あほい
(defun list->matrix (list)
  "convert list to matrix."
  (let ((mat (make-float-matrix (length list) (length (car list)))))
    (dotimes (i (length list))
      (dotimes (j (length (car list)))
        (setf (aref mat i j) (coerce (elt (elt list i) j) 'single-float))))
    mat))

(defun print-matrix (mat)
  (let ((dims (array-dimensions mat)))
    (dotimes (i (car dims))
      (dotimes (j (cadr dims))
        (format t "~0,3f " (aref mat i j))
        )
      (format t "~%")
      )
    ))

(defun random-range (min max)
  "return random value between min and max."
  (declare (type number min max))
  (let ((d (- max min)))
    (declare (type number d))
    (+ (random d) min)))

;; x x x x x
;; x x x x x
;; x x x x x
(defun matrix-row (mat id)
  (declare (type (simple-array single-float) mat)
           (type fixnum id))
  (let ((size (cadr (array-dimensions mat))))
    (let ((ret (make-float-vector size)))
      (dotimes (i size)
        (setf (aref ret i) (aref mat id i)))
      ret)))

(defun matrix-column (mat id)
  (declare (type (simple-array single-float) mat)
           (type fixnum id))
  (let ((size (car (array-dimensions mat))))
    (let ((ret (make-float-vector size)))
      (dotimes (i size)
        (setf (aref ret i) (aref mat i id)))
      ret)))
