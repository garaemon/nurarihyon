;;================================================
;; matrix.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 0)
		   (space 0)))

(in-package :nurarihyon)
(eval-when (:compile-toplevel)
  (enable-aref-reader-syntax))

;; util
(defmacro with-matrix-trans-dimension-bind-and-check ((n m n-dash m-dash
                                                       a b) &rest args)
  ;;  a: NxM
  ;;  b: N'xM'
  ;; this macro checking M = N' or not
  (let ((a-dims (gensym))
        (b-dims (gensym)))
    `(let ((,a-dims (matrix-dimensions ,a))
           (,b-dims (matrix-dimensions ,b)))
       (declare (type list ,a-dims ,b-dims))
       (let ((,n (car ,a-dims))
             (,m (cadr ,a-dims))
             (,n-dash (car ,b-dims))
             (,m-dash (cadr ,b-dims)))
         (declare (type unsigned-byte ,n ,m ,n-dash ,m-dash))
         (if (= ,m ,n-dash)
             (progn ,@args)
             (error "matrix dimensions mismatch"))))))
         
(defmacro with-square-matrix-bind-and-check ((dim mat) &rest args)
  ;; checking mat is square matrix or not
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
  (declare (type simple-array a))
  (the list (array-dimensions a)))

(declaim (inline matrix-row-dimension))
(defun matrix-row-dimension (a)
  (declare (type simple-array a))
  (the fixnum (car (array-dimensions a))))

(declaim (inline matrix-column-dimension))
(defun matrix-column-dimension (a)
  (declare (type simple-array a))
  (the fixnum (cadr (array-dimensions a))))

(defun make-matrix (row column &key (initial-element 0.0d0))
  (declare (type unsigned-byte row column)
           (type double-float initial-element))
  (the (simple-array double-float)
       (make-array (list row column) :element-type 'double-float
                   :initial-element initial-element)))

(defun make-identity-matrix (dim)
  (declare (type unsigned-byte dim))
  (let ((mat (make-matrix dim dim)))
    (declare (type (simple-array double-float) mat))
    (dotimes (i dim) (setf [mat i i] 1.0d0))
    (the (simple-array double-float) mat)))

(defun copy-matrix (a b)
  (declare (type (simple-array double-float) a b))
  (with-matrix-dimension-bind-and-check (row column a b)
    (dotimes (i row)
      (dotimes (j column)
        (setf [b i j] [a i j]))))
  (the (simple-array double-float) b))

;; matrix operators
;; add
(defun m+ (a b &optional (c nil))
  (declare (type (simple-array double-float) a b))
  (with-matrix-dimension-bind-and-check (row column a b)
    (let ((c (or c (make-matrix row column))))
      (declare (type (simple-array double-float) c))
      (dotimes (i row)
        (dotimes (j column)
          (setf [c i j] (+ [a i j] [b i j]))))
      (the (simple-array double-float) c))))

;; sub
(defun m- (a b &optional (c nil))
  (declare (type (simple-array double-float) a b))
  (with-matrix-dimension-bind-and-check (row column a b)
    (let ((c (or c (make-matrix row column))))
      (declare (type (simple-array double-float) c))
      (dotimes (i row)
        (dotimes (j column)
          (setf [c i j] (- [a i j] [b i j]))))
      (the (simple-array double-float) c))))

;; multiply
(defun m* (a b &optional (c nil))
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
                (setf [c k i] [tmpv k]))))
          )                      ;end of if
      (the (simple-array double-float) c))))

;; multiply with vector
(defun mv* (mat vec &optional (result nil))
  "return vector.
   mat = n x m  vec = (1 x)m
   +---------+     +-+
   |         |     | |
   |         |  x  | |
   |         |     | |
   +---------+     +-+
  "
  (declare (type (simple-array double-float) mat vec))
  (let ((mat-dimensions (matrix-dimensions mat))
        (vec-dimension (vector-dimensions vec)))
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

;; matをLU分解する
;; destructive function!!
(defun lu-decompose (result pivot)
  (declare (type (simple-array double-float) result)
           (type (simple-array unsigned-byte) pivot))
     (let ((dimension (matrix-row-dimension result)))
       (declare (type unsigned-byte dimension))
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
    (let ((det 1.0d0))                   ;det = 1
      (declare (type double-float det))
      ;; k: 0 ... dimension
      (dotimes (k dimension)            ;for ( k = 0; k < n; k++)
        (let ((u (- 1.0d0))              ;u = -1
              (ii 0)
              (j 0))
          (declare (type double-float u)
                   (type unsigned-byte ii j))
          ;; i: k ... dimension
          ;; find max element in (k,k) ... (i,k)
          ;; max value will be set in u
          ;; its index will be set in j
          ;; ii is a original pivot
          (do ((i k (1+ i)))            ;for ( i = k; i < n; i++ )
              ((not (< i dimension)))
            (declare (type unsigned-byte i))
            (setq ii [pivot i])       ;ii = ip[i]
            ;;t = fabs(a[ii][k] * weight[ii]
            (let ((tmp (abs (* [result ii k] [weight ii]))))
              (declare (type double-float tmp))
              (when (> tmp u)                      ;if ( t > u )
                (setq u tmp)                       ;u = t
                (setq j i))))                      ;j = i
          (let ((ik [pivot j]))                    ;ik = ip[j]
            (declare (type unsigned-byte ik))
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
                (declare (type unsigned-byte i))
                (let ((ii [pivot i])) ;ii = ip[i]
                  (declare (type unsigned-byte ii))
                  ;;t = (a[ii][k] /== u);
                  (/== [result ii k] u)
                  (let ((tmp [result ii k]))
                    (declare (type double-float tmp))
                    (do ((j (1+ k) (1+ j))) ;for ( j = k + 1; j < n; j++ )
                        ((not (< j dimension)))
                      (declare (type unsigned-byte j))
                      ;;a[ii][j] -== t * a[ik][j];
                      (-== [result ii j] (* tmp [result ik j]))))))))))
      (the double-float det)))))        ;return determination

(defun m-1 (mat &optional (result nil) (lu-mat nil))
  (with-square-matrix-bind-and-check (dim mat)
    (let* ((pivot (make-array dim :element-type 'unsigned-byte)))
      (declare (type (simple-array unsigned-byte) pivot))
      (let ((lu-mat (or lu-mat (make-matrix dim dim)))
            (result (or result (make-matrix dim dim))))
        (declare (type (simple-array double-float) lu-mat result))
        (copy-matrix mat lu-mat) ;; mat -> lu-mat
        (let ((determinant (lu-decompose lu-mat pivot)))
          ;; lu-mat --> LU分解後の行列
          (if (eps= determinant 0.0d0)     ;if ( det != 0 )
              ;; 行列式が0だと逆行列は求められない
              nil
              (progn
                (dotimes (k dim)
                  ;; forward
                  (dotimes (i dim)
                    (declare (type fixnum i))
                    (let* ((ii [pivot i])          ;ii = ip[i]
                           (tmp (if (= ii k) 1.0d0 0.0d0))) ;t = (ii == k);
                      (declare (type unsigned-byte ii)
                               (type double-float tmp))
                      (dotimes (j i)    ;for ( j = 0; j < i; j++ )
                        (-== tmp        ;t -== a[ii][j] * a_inv[j][k]
                             (* [lu-mat ii j] [result j k])))
                      (setf [result i k] tmp))) ;a_inv[i][k] = t
                  ;; backward
                  (do ((i (1- dim) (1- i))) ;for ( i = n - 1; i >= 0; i-- )
                      ((not (>= i 0)))
                    (declare (type unsigned-byte i))
                    (let ((tmp [result i k]) ;t = a_inv[i][k]l
                          (ii [pivot i]))    ;ii = ip[i]
                      (declare (type double-float tmp)
                               (type unsigned-byte ii))
                      (do ((j (1+ i) (1+ j))) ;for ( j = i + 1; j < n; j++ )
                          ((not (< j dim)))
                        (declare (type unsigned-byte j))
                        (-== tmp     ;t -== a[ii][j] * a_inv[j][k]
                             (* [lu-mat ii j] [result j k])))
                      (setf [result i k] ;a_inv[i][k] = t / a[ii][i]
                            (/ tmp [lu-mat ii i])))))
                result)))))))

(declaim (inline inverse-matrix))
(defun inverse-matrix (&rest args)
  (apply #'m-1 args))

(defun matrix-row (mat id)
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

(eval-when (:compile-toplevel)
  (disable-aref-reader-syntax))