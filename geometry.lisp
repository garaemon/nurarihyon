;;================================================
;; geometry.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (speed 3)
		   (safety 0)
		   (debug 0)
		   (space 0)))
;; for debugging
;; (declaim (optimize (safety 3)
;;                   (debug 3)))

(in-package :nurarihyon)

(eval-when (:compile-toplevel)
  (enable-aref-reader-syntax))

(alexandria:define-constant +x-axis+ #d(1.0 0.0 0.0) :test #'eps-vector=)
(alexandria:define-constant +y-axis+ #d(0.0 1.0 0.0) :test #'eps-vector=)
(alexandria:define-constant +z-axis+ #d(0.0 0.0 1.0) :test #'eps-vector=)

(defun rotation-matrix (ang vec &optional (result (make-matrix 3 3)))
  "returns a matrix which rotates ang[rad] around vec."
  (declare (type (simple-array double-float (3)) vec)
           (type (simple-array double-float (3 3)) result)
           (type double-float ang))
  (let ((cos (cos ang))
        (sin (sin ang)))
    (declare (type double-float cos sin))
    (let ((1-cos (- 1.0d0 cos)))
      (declare (type double-float 1-cos))
      (let ((vx (x vec))
            (vy (y vec))
            (vz (z vec)))
        (declare (type double-float vx vy vz))
        ;; ugly implementation!!
        (setf [result 0 0] (+ (* vx vx 1-cos) cos))
        (setf [result 0 1] (- (* vx vy 1-cos) (* vz sin)))
        (setf [result 0 2] (+ (* vz vx 1-cos) (* vy sin)))
        (setf [result 1 0] (+ (* vx vy 1-cos) (* vz sin)))
        (setf [result 1 1] (+ (* vy vy 1-cos) cos))
        (setf [result 1 2] (- (* vy vz 1-cos) (* vx sin)))
        (setf [result 2 0] (- (* vz vx 1-cos) (* vy sin)))
        (setf [result 2 1] (+ (* vy vz 1-cos) (* vx sin)))
        (setf [result 2 2] (+ (* vz vz 1-cos) cos))
        )))
  (the (simple-array double-float (3 3)) result))

;; matをang, axis, worldpにしたがって回転させる
;; 結果はresultに入れる.
;; matは3x3の行列.
(defun rotate-matrix (mat ang axis
                      &optional
                      (worldp nil)
                      (result (make-matrix 3 3)))
  "This is a destructive function.
   rotate mat ang[rad] around axis.
   mat    ... 3x3 float matrix.
   axis   ... one of :x,:y or :z
   worldp ... nil -> local, multiple rot from the right
              t   -> world, multiple rot from the left
   result ... calculation buffer"
  (declare (type (simple-array double-float (3 3)) mat result)
           (type double-float ang)
           (type symbol worldp axis))
  (let ((cos (cos ang))
        (sin (sin ang)))
    (declare (type double-float cos sin))
    (let ((s2 (case axis
                ((:x :-y :z)
                 (if worldp sin (- sin)))
                ((:-x :y :-z)
                 (if worldp (- sin) sin))
                (t
                 (error "unkown axis"))))
          (s1 (case axis
                ((:x :-y :z)
                 (if worldp (- sin) sin))
                ((:-x :y :-z)
                 (if worldp sin (- sin)))
                (t
                 (error "unkown axis"))))
          (k1 (case axis
                ((:x :-x) 1)
                ((:y :-y :z :-z) 0)
                (t
                 (error "unkown axis"))))
          (k2 (case axis
                ((:x :-x :y :-y) 2)
                ((:z :-z) 1)
                (t
                 (error "unkown axis")))))
      (declare (type double-float s1 s2)
               (type fixnum k1 k2))
      (if (not (eq result mat))
          (copy-matrix mat result))
      (dotimes (i 3)
        (declare (type fixnum i))
        (if worldp
            (let ((f1 (+ (* cos [result k1 i])
                         (* s1 [result k2 i])))
                  (f2 (+ (* s2 [result k1 i])
                         (* cos [result k2 i]))))
              (declare (type double-float f1 f2))
              (setf [result k1 i] f1)
              (setf [result k2 i] f2))
            (let ((f1 (+ (* cos [result i k1])
                         (* s1 [result i k2])))
                  (f2 (+ (* s2 [result i k1])
                         (* cos [result i k2]))))
              (declare (type double-float f1 f2))
              (setf [result i k1] f1)
              (setf [result i k2] f2))))
      (the (simple-array double-float (3 3)) result))))

;; function: euler-matrix
;; 指定されたオイラー角にしたがって,
;; その回転を表現する3x3の行列を返す
(defun euler-matrix (az ay az2)
  "returns matrix represented by eular angular"
  (declare (type double-float az ay az2))
  (let ((r (rotation-matrix az +z-axis+)))
    (declare (type (simple-array double-float (3 3)) r))
    (rotate-matrix r ay :y nil r)
    (rotate-matrix r az2 :z nil r)
    (the (simple-array double-float (3 3)) r)))

(defun rpy-matrix (az ay ax)
  "returns 3x3 matrix represented by rotate angle around x, y and z axis"
  (declare (type double-float ax ay az))
  (let ((r (rotation-matrix ax +x-axis+)))
    (declare (type (simple-array double-float (3 3)) r))
    (rotate-matrix r ay :y t r)
    (rotate-matrix r az :z t r)
    (the (simple-array double-float (3 3)) r)))

;; EusLisp Implementation -> matrix.c::INV_RPY
;; mat = x x x
;;       x x x
;;       x x x
;; a = atan2(mat(3), mat(0))
;;   = atan2(mat(0,1), mat(0,0))
;; sa = sin(a)
;; ca = cos(a)
;; b = atan2(-mat(6), ca*mat(0) + sa*mat(3))
;;   = atan2(-mat(0,2), ca*mat(0,0) + sa*mat(0,1))
;; c = atan2(sa*mat(2)-ca*mat(5), -sa*mat(1)+ca*mat(4))
;;   = atan2(sa*mat(0,2)-ca*mat(1,2), -sa*mat(0,1)+ca*mat(1,1))
(defun rpy-angle (mat)
  "returns matrix's rpy angle in two means."
  (declare (type (simple-array double-float (3 3)) mat))
  (let ((result 
         (let* ((a (atan [mat 0 1] [mat 0 0]))
                (sa (sin a))
                (ca (cos a)))
           (declare (type double-float sa ca))
           (let ((b (atan (- [mat 0 2])
                          (+ (* ca [mat 0 0]) (* sa [mat 0 1]))))
                 (c (atan (- (* sa [mat 0 2]) (* ca [mat 1 2]))
                          (- (* ca [mat 1 1]) (* sa [mat 0 1])))))
             (declare (type double-float b c))
             (let ((ret (make-vector 3)))
               (declare (type (simple-array double-float (3)) ret))
               (setf [ret 0] a)
               (setf [ret 1] b)
               (setf [ret 2] c)
               ret))))
        (result2
         (let* ((a (+ +pi+ (atan [mat 0 1] [mat 0 0])))
                (sa (sin a))
                (ca (cos a)))
           (declare (type double-float sa ca))
           (let ((b (atan (- [mat 0 2])
                           (+ (* ca [mat 0 0]) (* sa [mat 0 1]))))
                 (c (atan (- (* sa [mat 0 2]) (* ca [mat 1 2]))
                          (- (* ca [mat 1 1]) (* sa [mat 0 1])))))
             (declare (type double-float b c))
             (let ((ret (make-vector 3)))
               (declare (type (simple-array double-float (3)) ret))
               (setf [ret 0] a)
               (setf [ret 1] b)
               (setf [ret 2] c)
               ret)))))
    (declare (type (simple-array double-float (3)) result result2))
    (values (the (simple-array double-float (3)) result)
            (the (simple-array double-float (3)) result2))))

(declaim (inline axis->vec))
(defun axis->vec (axis)
  "returns vector appropriate to axis.
   axis must be a :x, :y or :z."
  (case axis
    (:x +x-axis+)
    (:y +y-axis+)
    (:z +z-axis+)
    (t axis)))

(eval-when (:compile-toplevel)
  (disable-aref-reader-syntax))
