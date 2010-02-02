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
  (let ((dims (gensym)))
    `(let ((,dims (array-dimensions ,mat)))
       (declare (type list ,dims))
       (let ((,dim (car ,dims)))
         (declare (type fixnum ,dim))
         (if (= ,dim (cadr ,dims))
           (progn ,@args)
           (error "array is not identity matrix"))))))

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

;; template for make-**-matrix
(defmacro defmake-matrix (name type initial-element)
  `(defun ,name (row column &key (initial-element ,initial-element))
     (declare (type fixnum row column)
              (type ,type initial-element))
     (the (simple-array ,type)
       (make-array (list row column) :element-type ',type
                   :initial-element initial-element))))

;; template for  make-**-identity-matrix
(defmacro defmake-identity-matrix (name type matrix-constructor element)
  `(defun ,name (dim)
     (declare (type fixnum dim))
     (let ((mat (,matrix-constructor dim dim)))
       (declare (type (simple-array ,type) mat))
       (dotimes (i dim)
         (setf [mat i i] ,element))
       (the (simple-array ,type) mat))))

(defmacro defcopy-matrix (name type)
  `(defun ,name (a b)
     (declare (type (simple-array ,type) a b))
     (with-matrix-dimension-bind-and-check (row column a b)
         (dotimes (i row)
           (dotimes (j column)
             (setf [b i j] [a i j]))))
     (the (simple-array ,type) b)))

;;template
;; matrix operators
;; add
(defmacro defm+ (name type const-func)
  `(defun ,name (a b &optional (c nil))
     (declare (type (simple-array ,type) a b))
     (with-matrix-dimension-bind-and-check (row column a b)
       (let ((c (or c (,const-func row column))))
         (dotimes (i row)
           (dotimes (j column)
             (setf [c i j] (+ [a i j] [b i j]))))
         (the (simple-array ,type) c)))))

;; sub
(defmacro defm- (name type const-func)
  `(defun ,name (a b &optional (c nil))
     (declare (type (simple-array ,type) a b))
     (with-matrix-dimension-bind-and-check (row column a b)
       (let ((c (or c (,const-func row column))))
         (dotimes (i row)
           (dotimes (j column)
             (setf [c i j] (- [a i j] [b i j]))))
         (the (simple-array ,type) c)))))

(defmacro defm* (name type matrix-constructor vector-constructor
                 initial-value)
  `(defun ,name (a b &optional (c nil))
     (declare (type (simple-array ,type) a b))
     (with-matrix-trans-dimension-bind-and-check
         (dims-a-row dims-a-column dims-b-row dims-b-column a b)
       (let ((c (or c (,matrix-constructor dims-a-row dims-b-column))))
         (declare (type (simple-array ,type) c))
         (if (not (eq b c))         ;i need to dispatch here
             (let ((tmpv (,vector-constructor dims-b-column)))
               (declare (type (simple-array ,type) tmpv))
               (dotimes (i dims-a-row)
                 (dotimes (j dims-b-column)
                   (let ((tmp ,initial-value))
                     (declare (type ,type tmp))
                     (dotimes (k dims-a-column)
                       (+== tmp (* [a i k] [b k j])))
                     (setf [tmpv j] tmp)))
                 (dotimes (k dims-b-column)
                   ;; copy tmpv -> c[i, *]
                   (setf [c i k] [tmpv k]))))
             (let ((tmpv (,vector-constructor dims-a-row))) ;else
               (declare (type (simple-array ,type) tmpv))
               (dotimes (i dims-b-column)
                 (dotimes (j dims-a-row)
                   ;; c[j, i] = \sum_k a[j, k] * b[k, i]
                   (let ((tmp ,initial-value))
                     (declare (type ,type tmp))
                     (dotimes (k dims-a-column)
                       (+== tmp (* [b k i] [a j k])))
                     (setf [tmpv j] tmp)))
                 (dotimes (k dims-b-column)
                   ;; copy tmpv -> c[i, *]
                   (setf [c k i] [tmpv k]))))
             )                      ;end of if
         (the (simple-array ,type) c))))
  )

(defmacro deftranspose (name type matrix-constructor)
  `(defun ,name (mat &optional (result nil))
     (declare (type (simple-array ,type) mat))
     (let ((dims (array-dimensions mat)))
       (declare (list dims))
       (let ((row (car dims))
             (column (cadr dims)))
         (declare (type fixnum row column))
         (let ((result (or result (,matrix-constructor column row))))
           (declare (type (simple-array ,type) result))
           (dotimes (i row)
             (dotimes (j column)
               (setf [result j i] [mat i j])))
           (the (simple-array ,type) result)))))
  )

;; matをLU分解する
;; destructive function!!
(defmacro deflu-decompose (name type vector-constructor zero one eps-func)
  `(defun ,name (result pivot)
     (declare (type (simple-array ,type) result)
              (type (simple-array fixnum) pivot))
     (let ((dimension (matrix-row-dimension result)))
       (declare (type unsigned-byte dimension))
       (let ((weight (,vector-constructor dimension))) ;weight = new_vector(n);
         (declare (type (simple-array ,type) weight))
         ;; initialize weight vector
         ;; i: 0 ... dimension
         (dotimes (i dimension)         ;for (k = 0; k < n; k++){
           (setf [pivot i] i)           ;ip[k] = k;
           (let ((u ,zero))             ;u = 0;
             (declare (type ,type u))
             ;; search max value in i th row
             ;; j: 0 ... dimension
             (dotimes (j dimension)     ;for (j = 0; j < n; j++){
               (let ((tmp (abs [result i j]))) ;t = fabs(a[k][j])
                 (declare (type ,type tmp))
                 (if (> tmp u)                      ;if ( t > u )
                     (setq u tmp))))                ;u = t;
             (if (,eps-func u ,zero)                ;if ( u== 0.0 )
                 (return-from ,name ,zero))         ;goto EXIT;
             (setf [weight i] (/ ,one u)))) ;weight[k] = 1 / u;
    ;; finish initializing weight vector
    (let ((det ,one))                   ;det = 1
      (declare (type ,type det))
      ;; k: 0 ... dimension
      (dotimes (k dimension)            ;for ( k = 0; k < n; k++)
        (let ((u (- ,one))              ;u = -1
              (ii 0)
              (j 0))
          (declare (type ,type u)
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
              (declare (type ,type tmp))
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
              (declare (type ,type u))
              (*== det u)                  ;det *== u;
              (if (,eps-func u ,zero)      ;if ( u == 0 )
                  (return-from ,name det)) ;goto EXIT;
              (do ((i (1+ k) (1+ i)))   ;for ( i = k + 1; i < n; i++ )
                  ((not (< i dimension)))
                (declare (type unsigned-byte i))
                (let ((ii [pivot i])) ;ii = ip[i]
                  (declare (type unsigned-byte ii))
                  ;;t = (a[ii][k] /== u);
                  (/== [result ii k] u)
                  (let ((tmp [result ii k]))
                    (declare (type ,type tmp))
                    (do ((j (1+ k) (1+ j))) ;for ( j = k + 1; j < n; j++ )
                        ((not (< j dimension)))
                      (declare (type unsigned-byte j))
                      ;;a[ii][j] -== t * a[ik][j];
                      (-== [result ii j] (* tmp [result ik j]))))))))))
      (the ,type det)))))                           ;return determination
  )



;; (declaim (inline inverse-matrix))
;; (defun inverse-matrix (&rest args)
;;   (apply #'m-1 args))
(defmacro defm-1 (name type matrix-constructor matrix-copy lu-decompose-func
                  zero one)
  `(defun ,name (mat &optional (result nil) (lu-mat nil))
     (with-square-matrix-bind-and-check (dim mat)
       (let* ((pivot (make-integer-vector dim)))
         (declare (type (simple-array fixnum) pivot))
         (let ((lu-mat (or lu-mat (,matrix-constructor dim dim)))
               (result (or result (,matrix-constructor dim dim))))
           (declare (type (simple-array ,type) lu-mat result))
           (,matrix-copy mat lu-mat) ;; mat -> lu-mat
           (let ((determinant (,lu-decompose-func lu-mat pivot)))
             ;; lu-mat --> LU分解後の行列
             (if (eps= determinant 0.0)     ;if ( det != 0 )
                 ;; 行列式が0だと逆行列は求められない
                 nil
                 (progn
                   (do ((k 0 (1+ k)))   ;for ( k = 0; k < n; k++ )
                       ((not (< k dim)))
                     ;; forward
                     (do ((i 0 (1+ i))) ;for ( i = 0; i < n; i++ )
                         ((not (< i dim)))
                       (let* ((ii (aref pivot i))          ;ii = ip[i]
                              (tmp (if (= ii k) ,one ,zero))) ;t = (ii == k);
                         (do ((j 0 (1+ j))) ;for ( j = 0; j < i; j++ )
                             ((not (< j i)))
                           (-== tmp     ;t -== a[ii][j] * a_inv[j][k]
                                (* (aref lu-mat ii j) (aref result j k))))
                         (setf (aref result i k) tmp) ;a_inv[i][k] = t
                         ))
                     ;; backward
                     (do ((i (1- dim) (1- i))) ;for ( i = n - 1; i >= 0; i-- )
                         ((not (>= i 0)))
                       (let ((tmp (aref result i k)) ;t = a_inv[i][k]l
                             (ii (aref pivot i)))    ;ii = ip[i]
                         (do ((j (1+ i) (1+ j))) ;for ( j = i + 1; j < n; j++ )
                             ((not (< j dim)))
                           (-== tmp     ;t -== a[ii][j] * a_inv[j][k]
                                (* (aref lu-mat ii j) (aref result j k))))
                         (setf (aref result i k) ;a_inv[i][k] = t / a[ii][i]
                               (/ tmp (aref lu-mat ii i)))
                         ))
                     )
              result)))))))
  )

;; definition
(defmake-matrix make-matrix real 0)
(defmake-matrix make-integer-matrix fixnum 0)
(defmake-matrix make-float-matrix single-float 0.0)
(defmake-matrix make-double-matrix double-float 0.0d0)

(defmake-identity-matrix make-identity-matrix
    real make-matrix 0)
(defmake-identity-matrix make-integer-identity-matrix
    fixnum make-integer-matrix 0)
(defmake-identity-matrix make-float-identity-matrix
    single-float make-float-matrix 0.0)
(defmake-identity-matrix make-double-identity-matrix
    double-float make-double-matrix 0.0d0)

(defcopy-matrix copy-matrix real)
(defcopy-matrix copy-integer-matrix fixnum)
(defcopy-matrix copy-float-matrix single-float)
(defcopy-matrix copy-double-matrix double-float)

(defm+ m+ real make-matrix)
(defm+ im+ fixnum make-integer-matrix)
(defm+ fm+ single-float make-float-matrix)
(defm+ dm+ double-float make-double-matrix)

(defm- m- real make-matrix)
(defm- im- fixnum make-integer-matrix)
(defm- fm- single-float make-float-matrix)
(defm- dm- double-float make-double-matrix)

(defm* m* real make-matrix make-vector 0)
(defm* im* fixnum make-integer-matrix make-integer-vector 0)
(defm* fm* single-float make-float-matrix make-float-vector 0.0)
(defm* dm* double-float make-double-matrix make-double-vector 0.0d0)

(deftranspose transpose real make-matrix)
(deftranspose itranspose fixnum make-integer-matrix)
(deftranspose ftranspose single-float make-float-matrix)
(deftranspose dtranspose double-float make-double-matrix)

(deflu-decompose lu-decompose real make-vector 0.0 1.0 eps=)
(deflu-decompose flu-decompose single-float make-float-vector 0.0 1.0 feps=)
(deflu-decompose dlu-decompose double-float make-double-vector
                 0.0d0 1.0d0 deps=)

(eval-when (:compile-toplevel)
  (disable-aref-reader-syntax))
