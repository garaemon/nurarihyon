;;================================================
;; matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (speed 3) (safety 0) (debug 1) (space 0)))

(in-package :nurarihyon)
(eval-when (:compile-toplevel)
  (enable-nurarihyon-reader-syntax))

;; util
(defmacro with-matrix-trans-dimension-bind-and-check ((n m n-dash m-dash a b)
                                                      &rest args)
  "let A NxM matrix and B N'xM'. ARGS will be evaluated
only when M equals to N'. before evaluating ARGS, this macro binds
the length of column of A to N and row to M.

if M does not equals to N', this macro will raise a condition."
  (let ((a-dims (gensym))
        (b-dims (gensym)))
    `(let ((,a-dims (matrix-dimensions ,a))
           (,b-dims (matrix-dimensions ,b)))
       (declare (type list ,a-dims ,b-dims))
       (let ((,n (car ,a-dims))
             (,m (cadr ,a-dims))
             (,n-dash (car ,b-dims))
             (,m-dash (cadr ,b-dims)))
         (declare (type fixnum ,n ,m ,n-dash ,m-dash))
         (if (= ,m ,n-dash)
             (progn ,@args)
             (error "matrix dimensions mismatch"))))))

(defmacro with-square-matrix-bind-and-check ((dim mat) &rest args)
  "let MAT NxM matrix. ARGS will be evaluated only when N equals to M,
it means MAT is a square matrix. before evaluate ARGS, this macro binds
the dimension of MAT (N) to DIM."
  (let ((dim2 (gensym)))
    (let ((dims (gensym)))
      `(let ((,dims (array-dimensions ,mat)))
         (declare (type list ,dims))
         (let ((,dim (car ,dims))
               (,dim2 (cadr ,dims)))
           (declare (type fixnum ,dim ,dim2))
           (if (= ,dim ,dim2)
               (progn ,@args)
               (error "array is not identity matrix")))))))

(defmacro with-matrix-dimension-bind-and-check ((row column a b) &rest args)
  "let A NxM matrix and B N'xM' matrix, and ARGS will be evaluated when
N equals to N' and M equals to M'. and ARGS will be evaluated with binding
N to ROW and M to COLUMN."
  (let ((a-dims (gensym))
        (b-dims (gensym))
        (b-row (gensym))
        (b-column (gensym)))
    ;;  check dimensions of vecs are equal or not
    `(let ((,a-dims (matrix-dimensions ,a))
           (,b-dims (matrix-dimensions ,b)))
       (declare (type list ,a-dims ,b-dims))
       (let ((,row (car ,a-dims))
             (,column (cadr ,a-dims))
             (,b-row (car ,b-dims))
             (,b-column (cadr ,b-dims)))
         (declare (type fixnum ,row ,column ,b-row ,b-column))
         (if (and (= ,row ,b-row) (= ,column ,b-column))
             (progn ,@args)
             (error "matrix dimensions mismatch"))))))

(declaim (inline matrix-dimensions))
(defun matrix-dimensions (a)
  "let A NxM matrix. matrix-dimensions returns (N M).
A must be a simple-array of double-float."
  (declare (type (simple-array double-float) a))
  (the list (array-dimensions a)))

(declaim (inline matrix-row-dimension))
(defun matrix-row-dimension (a)
  "let A NxM matrix. matrix-row-dimension returns N.
A must be a simple-array of double-float."
  (declare (type (simple-array double-float) a))
  (the fixnum (array-dimension a 0)))

(declaim (inline matrix-column-dimension))
(defun matrix-column-dimension (a)
    "let A NxM matrix. matrix-row-dimension returns M.
A must be a simple-array of double-float."
  (declare (type (simple-array double-float) a))
  (the fixnum (array-dimension a 1)))

(declaim (inline make-matrix))
(defun make-matrix (row column &key (initial-element 0.0d0))
  "make a ROWxCOLUMN matrix. the matrix is a siple-array of double-float.
you can use :initial-element keyword to specify the contents of the matrix.
values of :initial-element must be a double-float.

 example::

   (make-matrix 2 2) => #2A((0.0d0 0.0d0) (0.0d0 0.0d0)).
   (make-matrix 2 2 :initial-element 3.0d0) => #2A((3.0d0 3.0d0)
                                                   (3.0d0 3.0d0))"
  (declare (type fixnum row column)
           (type double-float initial-element))
  (the (simple-array double-float)
       (make-array (list row column) :element-type 'double-float
                   :initial-element initial-element)))

(declaim (inline make-matrix33))
(defun make-matrix33 (&key (initial-element 0.0d0))
  "make a 3x3 matrix. the matrix is a simple-array of double-float.
you can use :initial-element keyword to specify the contents of the matrix.
values of :initial-element must be a double-float.

 example::

    (make-matrix33) => #2A((0.0d0 0.0d0 0.0d0)
                           (0.0d0 0.0d0 0.0d0)
                           (0.0d0 0.0d0 0.0d0))
    (make-matrix33 :initial-element 4.0d0) => #2A((4.0d0 4.0d0 4.0d0)
                                                  (4.0d0 4.0d0 4.0d0)
                                                  (4.0d0 4.0d0 4.0d0))"
  (declare (type double-float initial-element))
  (the (simple-array double-float (3 3))
    (make-array '(3 3) :element-type 'double-float
                :initial-element initial-element)))

(declaim (inline make-matrix44))
(defun make-matrix44 (&key (initial-element 0.0d0))
  "make a 4x4 matrix. the matrix is a simple-aray of double-float.
you can use :initial-element keyword to specify the contents of the matrix.
values of :initial-element must be a double-float.

 example::

    (make-matrix44) => #2A((0.0d0 0.0d0 0.0d0 0.0d0)
                           (0.0d0 0.0d0 0.0d0 0.0d0)
                           (0.0d0 0.0d0 0.0d0 0.0d0)
                           (0.0d0 0.0d0 0.0d0 0.0d0))
    (make-matrix44 :initial-element 3.0d0) => #2A((3.0d0 3.0d0 3.0d0 3.0d0)
                                                  (3.0d0 3.0d0 3.0d0 3.0d0)
                                                  (3.0d0 3.0d0 3.0d0 3.0d0)
                                                  (3.0d0 3.0d0 3.0d0 3.0d0))"
  (declare (type double-float initial-element))
  (the (simple-array double-float (4 4))
    (make-array '(4 4) :element-type 'double-float
                :initial-element initial-element)))

(defun double-matrix (&rest args)
  "this is a utility function to make a double matrix. this function will
create the vector which has ARGS as contents.

 example::

   (double-matrix '(1 2 3 4) '(4 5 6 7)) => #2A((1.0d0 2.0d0 3.0d0 4.0d0)
                                                (4.0d0 5.0d0 6.0d0 7.0d0))"
  (let ((row (length args))
        (column (length (car args))))
    (let ((mat (make-matrix row column)))
      (declare (type (simple-array double-float) mat))
      (dotimes (i row)
        (dotimes (j column)
          (setf [mat i j] (coerce (elt (elt args i) j) 'double-float))))
      (the (simple-array double-float) mat))))

(defun make-identity-matrix (dim)
  "make an identity DIMxDIM matrix."
  (declare (type fixnum dim))
  (let ((mat (make-matrix dim dim)))
    (declare (type (simple-array double-float) mat))
    (dotimes (i dim) (setf [mat i i] 1.0d0))
    (the (simple-array double-float) mat)))

(defun make-identity-matrix3 ()
  "make an 3x3 identity matrix"
  (let ((mat (make-matrix33)))
    (declare (type (simple-array double-float (3 3)) mat))
    (setf [mat 0 0] 1.0d0)
    (setf [mat 1 1] 1.0d0)
    (setf [mat 2 2] 1.0d0)
    (the (simple-array double-float (3 3)) mat)))

(defun make-identity-matrix4 ()
  "make an 4x4 identity matrix"
  (let ((mat (make-matrix44)))
    (declare (type (simple-array double-float (4 4)) mat))
    (setf [mat 0 0] 1.0d0)
    (setf [mat 1 1] 1.0d0)
    (setf [mat 2 2] 1.0d0)
    (setf [mat 3 3] 1.0d0)
    (the (simple-array double-float (4 4)) mat)))

(declaim (inline copy-matrix))
(defun copy-matrix (a &optional (b (apply #'make-matrix (matrix-dimensions a))))
  "copy the double matrix A to B and return B.

You can specify B, the second argument, to reduce heap allocation.
If not, COPY-MATRIX will allocate another matrix which has
the same demensions to A.

A and B must be a (simple-array double-float) and have the same length."
  (declare (type (simple-array double-float) a b))
  (the (simple-array double-float) (copy-matrix* a b)))

(defun copy-matrix* (a b)
    "this is a low level api to copy the double matrix A to B and return B.
A and B must be a (simple-array double-float) and have the same dimensions."
  (declare (type (simple-array double-float) a b))
  (with-matrix-dimension-bind-and-check (row column a b)
    (dotimes (i row)
      (dotimes (j column)
        (setf [b i j] [a i j]))))
  (the (simple-array double-float) b))

;; matrix operators
(defun m+ (a b &optional (c nil))
  "add two matrix, A and B, put the result into C and return C.
If you does not specify C, M+ will allocate another matrix in the heap.

A, B and C must have the same dimensions and be (simple-array double-float)."
  (declare (type (simple-array double-float) a b))
  (with-matrix-dimension-bind-and-check (row column a b)
    (let ((c (or c (make-matrix row column))))
      (declare (type (simple-array double-float) c))
      (dotimes (i row)
        (dotimes (j column)
          (setf [c i j] (+ [a i j] [b i j]))))
      (the (simple-array double-float) c))))

(defun m- (a b &optional (c nil))
  "substitute B from A, put the result into C and return C.
If you does not specify C, M- will allocate another matrix in the heap.

A, B and C must have the same dimensions and be (simple-array double-float)."
  (declare (type (simple-array double-float) a b))
  (with-matrix-dimension-bind-and-check (row column a b)
    (let ((c (or c (make-matrix row column))))
      (declare (type (simple-array double-float) c))
      (dotimes (i row)
        (dotimes (j column)
          (setf [c i j] (- [a i j] [b i j]))))
      (the (simple-array double-float) c))))

(defun m* (a b &optional (c nil))
  "multiply two matrix, A and B, put the result into C and return C.
If you does not specify C, M+ will allocate another matrix in the heap.

A, B and C must have the same dimensions and be (simple-array double-float).
.. math::

     C = AB"
  (declare (type (simple-array double-float) a b))
  (with-matrix-trans-dimension-bind-and-check
      (dims-a-row dims-a-column dims-b-row dims-b-column a b)
    (let ((c (or c (make-matrix dims-a-row dims-b-column))))
      (declare (type (simple-array double-float) c))
      (if (not (eq b c))                ;i need to dispatch here
          (let ((tmpv (make-vector dims-b-column)))
            (declare (type (simple-array double-float) tmpv))
            (dotimes (i dims-a-row)
              (declare (type fixnum i))
              (dotimes (j dims-b-column)
                (let ((tmp 0.0d0))
                  (declare (type double-float tmp))
                  (dotimes (k dims-a-column)
                    (+== tmp (* [a i k] [b k j])))
                  (setf [tmpv j] tmp)))
              (dotimes (k dims-b-column)
                ;; copy tmpv -> c[i, *]
                (setf [c i k] [tmpv k]))))
          (let ((tmpv (make-vector dims-a-row))) ;else
            (declare (type (simple-array double-float) tmpv))
            (dotimes (i dims-b-column)
              (declare (type fixnum i))
              (dotimes (j dims-a-row)
                ;; c[j, i] = \sum_k a[j, k] * b[k, i]
                (let ((tmp 0.0d0))
                  (declare (type double-float tmp))
                  (dotimes (k dims-a-column)
                    (+== tmp (* [b k i] [a j k])))
                  (setf [tmpv j] tmp)))
              (dotimes (k dims-b-column)
                ;; copy tmpv -> c[i, *]
                (setf [c k i] [tmpv k]))))) ;end of if
      (the (simple-array double-float) c))))

;; multiply with vector
(defun mv* (mat vec &optional (result nil))
  "multiply MAT and VEC, put the result into RESULT and return RESULT.

if you does not specify RESULT, M+ will allocate another matrix in the heap.

let A NxM matrix, it means MAT,  and B D dimension vector, it means VEC,
MV* report an error if M does not equal to D.

.. math::

    R = AB

where R is RESULT."
  (declare (type (simple-array double-float) mat vec))
  (let ((mat-dimensions (matrix-dimensions mat))
        (vec-dimension (vector-dimension vec)))
     (declare (type fixnum vec-dimension)
              (type list mat-dimensions))
     (let ((mat-row (car mat-dimensions))
           (mat-column (cadr mat-dimensions)))
       (declare (type fixnum mat-row mat-column))
       (when (not (= vec-dimension mat-column)) ; error check for dimension
         (error "dimension mismatch"))
       (let ((fv (or result (make-vector mat-column))))
         (declare (type (simple-array double-float) fv))
         (dotimes (n mat-column)
           (let ((element 0.0d0))
             (declare (type double-float element))
             (dotimes (m vec-dimension)
               (declare (type fixnum m))
               (+== element (* [mat n m] [vec m])))
             (setf [fv n] element)))
         (the (simple-array double-float) fv)))))

;; transpose
(defun transpose (mat &optional (result nil))
  "calculat a transpose matrix of MAT, put the result into RESULT and
return RESULT.

If you does not specify RESULT, TRANSPOSE allocates another matrinx in
the heap.

MAT and RESULT must be (simple-array double-float).
LET MAT NxM matrix and RESULT N'xM' matrix, N must equal to M' and
M must equal to N'."
  (declare (type (simple-array double-float) mat))
  (let ((dims (matrix-dimensions mat)))
    (declare (list dims))
    (let ((row (car dims))
          (column (cadr dims)))
      (declare (type fixnum row column))
      (let ((result (or result (make-matrix column row))))
        (declare (type (simple-array double-float) result))
        (dotimes (i row)
          (dotimes (j column)
            (setf [result j i] [mat i j])))
        (the (simple-array double-float) result)))))

;; destructive function!!
(defun lu-decompose (result pivot)
  (declare (type (simple-array double-float) result)
           (type (simple-array fixnum) pivot))
     (let ((dimension (matrix-row-dimension result)))
       (declare (type fixnum dimension))
       (let ((weight (make-vector dimension))) ;weight = new_vector(n);
         (declare (type (simple-array double-float) weight))
         ;; initialize weight vector
         ;; i: 0 ... dimension
         (dotimes (i dimension)         ;for (k = 0; k < n; k++){
           (declare (type fixnum i))
           (setf [pivot i] i)           ;ip[k] = k;
           (let ((u 0.0d0))             ;u = 0;
             (declare (type double-float u))
             ;; search max value in i th row
             ;; j: 0 ... dimension
             (dotimes (j dimension)     ;for (j = 0; j < n; j++){
               (let ((tmp (abs [result i j]))) ;t = fabs(a[k][j])
                 (declare (type double-float tmp))
                 (if (> tmp u)                      ;if ( t > u )
                     (setq u tmp))))                ;u = t;
             (if (eps= u 0.0d0)                ;if ( u== 0.0 )
                 (return-from lu-decompose 0.0d0))         ;goto EXIT;
             (setf [weight i] (/ 1.0d0 u)))) ;weight[k] = 1 / u;
    ;; finish initializing weight vector
    (let ((det 1.0d0))                  ;det = 1
      (declare (type double-float det))
      ;; k: 0 ... dimension
      (dotimes (k dimension)            ;for ( k = 0; k < n; k++)
        (let ((u (- 1.0d0))             ;u = -1
              (ii 0)
              (j 0))
          (declare (type double-float u)
                   (type fixnum ii j))
          ;; i: k ... dimension
          ;; find max element in (k,k) ... (i,k)
          ;; max value will be set in u
          ;; its index will be set in j
          ;; ii is a original pivot
          (do ((i k (1+ i)))            ;for ( i = k; i < n; i++ )
              ((not (< i dimension)))
            (declare (type fixnum i))
            (setq ii [pivot i])       ;ii = ip[i]
            ;;t = fabs(a[ii][k] * weight[ii]
            (let ((tmp (abs (* [result ii k] [weight ii]))))
              (declare (type double-float tmp))
              (when (> tmp u)                      ;if ( t > u )
                (setq u tmp)                       ;u = t
                (setq j i))))                      ;j = i
          (let ((ik [pivot j]))                    ;ik = ip[j]
            (declare (type fixnum ik))
            (when (not (= j k))          ;if ( j != k )
              (setf [pivot j] [pivot k]) ;ip[j] = ip[k]
              (setf [pivot k] ik)        ;ip[k] = ik;
              (setq det (- det)))        ;det = -det;
            (let ((u [result ik k]))     ;u = a[ik][k];
              (declare (type double-float u))
              (*== det u)                  ;det *== u;
              (if (eps= u 0.0d0)      ;if ( u == 0 )
                  (return-from lu-decompose (the double-float det))) ;goto EXIT
              (do ((i (1+ k) (1+ i)))   ;for ( i = k + 1; i < n; i++ )
                  ((not (< i dimension)))
                (declare (type fixnum i))
                (let ((ii [pivot i])) ;ii = ip[i]
                  (declare (type fixnum ii))
                  ;;t = (a[ii][k] /== u);
                  (/== [result ii k] u)
                  (let ((tmp [result ii k]))
                    (declare (type double-float tmp))
                    (do ((j (1+ k) (1+ j))) ;for ( j = k + 1; j < n; j++ )
                        ((not (< j dimension)))
                      (declare (type fixnum j))
                      ;;a[ii][j] -== t * a[ik][j];
                      (-== [result ii j] (* tmp [result ik j]))))))))))
      (the double-float det)))))        ;return determination

(defun m-1 (mat &optional (result nil) (lu-mat nil))
  "calculate an inverse matrix of MAT, put the result into RESULT
and return RESULT.

you can specify RESULT and LU-MAT in order to reduce heap allocation.

MAT, RESULT and LU-MAT must be (simple-array double-float) and have the
same dimensions."
  (declare (type (simple-array double-float) mat))
  (with-square-matrix-bind-and-check (dim mat)
    (let* ((pivot (make-array dim :element-type 'fixnum)))
      (declare (type (simple-array fixnum) pivot))
      (let ((lu-mat (or lu-mat (make-matrix dim dim)))
            (result (or result (make-matrix dim dim))))
        (declare (type (simple-array double-float) lu-mat result))
        (copy-matrix mat lu-mat) ;; mat -> lu-mat
        (let ((determinant (lu-decompose lu-mat pivot)))
          ;; lu-mat --> lu-decomposed matrix
          (if (eps= determinant 0.0d0)     ;if ( det != 0 )
              ;; if determinant equals to 0, inverse matrix
              ;; cannot be calculated.
              nil
              (progn
                (dotimes (k dim)
                  ;; forward
                  (dotimes (i dim)
                    (declare (type fixnum i))
                    (let* ((ii [pivot i])          ;ii = ip[i]
                           (tmp (if (= ii k) 1.0d0 0.0d0))) ;t = (ii == k);
                      (declare (type fixnum ii)
                               (type double-float tmp))
                      (dotimes (j i)    ;for ( j = 0; j < i; j++ )
                        (-== tmp        ;t -== a[ii][j] * a_inv[j][k]
                             (* [lu-mat ii j] [result j k])))
                      (setf [result i k] tmp))) ;a_inv[i][k] = t
                  ;; backward
                  (do ((i (1- dim) (1- i))) ;for ( i = n - 1; i >= 0; i-- )
                      ((not (>= i 0)))
                    (declare (type fixnum i))
                    (let ((tmp [result i k]) ;t = a_inv[i][k]l
                          (ii [pivot i]))    ;ii = ip[i]
                      (declare (type double-float tmp)
                               (type fixnum ii))
                      (do ((j (1+ i) (1+ j))) ;for ( j = i + 1; j < n; j++ )
                          ((not (< j dim)))
                        (declare (type fixnum j))
                        (-== tmp     ;t -== a[ii][j] * a_inv[j][k]
                             (* [lu-mat ii j] [result j k])))
                      (setf [result i k] ;a_inv[i][k] = t / a[ii][i]
                            (/ tmp [lu-mat ii i])))))
                result)))))))

(declaim (inline inverse-matrix))
(defun inverse-matrix (&rest args)
  "alias function of M-1"
  (apply #'m-1 args))

(defun matrix-row (mat id)
  "return the ID-th row of MAT.

for optimization,
MAT must be a (simple-array double-float) and ID must be a fixnum."
  (declare (type (simple-array double-float) mat)
           (type fixnum id))
  (let ((size (matrix-column-dimension mat)))
    (declare (type fixnum size))
    (let ((ret (make-vector size)))
      (declare (type (simple-array double-float) ret))
      (dotimes (i size)
        (setf [ret i] [mat id i]))
      (the (simple-array double-float) ret))))

(defun (setf matrix-row) (val mat id)
  (declare (type (simple-array double-float) mat val)
           (type fixnum id))
  (let ((size (matrix-column-dimension mat)))
    (declare (type fixnum size))
    (dotimes (i size)
      (declare (type fixnum i))
      (setf [mat id i] [val i]))
    (the (simple-array double-float) val)))

(defun matrix-column (mat id)
  (declare (type (simple-array double-float) mat)
           (type fixnum id))
  (let ((size (matrix-row-dimension mat)))
    (declare (type fixnum size))
    (let ((ret (make-vector size)))
      (declare (type (simple-array double-float) ret))
      (dotimes (i size)
        (setf [ret i] [mat i id]))
      (the (simple-array double-float) ret))))

(defun (setf matrix-column) (val mat id)
  (declare (type (simple-array double-float) mat val)
           (type fixnum id))
  (let ((size (matrix-row-dimension mat)))
    (declare (type fixnum size))
    (dotimes (i size)
      (declare (type fixnum i))
      (setf [mat i id] [val i]))
    (the (simple-array double-float) val)))

(defun matrix-diagonal (mat)
  (declare (type (simple-array double-float) mat))
  (with-square-matrix-bind-and-check (dim mat)
    (let ((ret (make-vector dim)))
      (declare (type (simple-array double-float) ret))
      (dotimes (i dim)
        (setf [ret i] [mat i i]))
      ret)))

(defun (setf matrix-diagonal) (val mat)
  (declare (type (simple-array double-float) val mat))
  (with-square-matrix-bind-and-check (dim mat)
    (dotimes (i dim)
      (setf [mat i i] [val i]))
    val))

(defun matrix-trace (mat)
  (declare (type (simple-array double-float) mat))
  (let ((ret 0.0d0))
    (declaim (type double-float ret))
    (with-square-matrix-bind-and-check (dim mat)
       (dotimes (i dim)
         (+== ret [mat i i])))
    (the double-float ret)))

(defun matrix-determinant (mat &optional (lu-mat nil))
  (declare (type (simple-array double-float) mat))
  (with-square-matrix-bind-and-check (dim mat)
    (let* ((pivot (make-array dim :element-type 'fixnum)))
      (declare (type (simple-array fixnum) pivot))
      (let ((lu-mat (or lu-mat (make-matrix dim dim))))
        (declare (type (simple-array double-float) lu-mat))
        (copy-matrix mat lu-mat)
        (the double-float (lu-decompose lu-mat pivot))))))

(defun eps-matrix= (a b &optional (diff +eps+))
  "returns t if matrix a and b is near enough."
  (declare (type (simple-array double-float) a b)
	   (type double-float diff))
  (let ((a-dims (matrix-dimensions a))
	(b-dims (matrix-dimensions b)))
    (declare (type list a-dims b-dims))
    (if (equal a-dims b-dims)
	(let ((row (car a-dims))
              (column (cadr a-dims)))
          (declare (type fixnum row column))
	  (dotimes (i row)
	    (declare (type fixnum i))
	    (dotimes (j column)
	      (declare (type fixnum j))
	      (if (not (eps= [a i j] [b i j] diff))
		  (return-from eps-matrix= nil))
	      ))
          t)                            ;if passed
	nil)))


(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
