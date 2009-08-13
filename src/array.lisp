;;================================================
;; array.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))

(in-package :nurarihyon)

;; x軸
(defconstant +x-axis+ (make-array 3 :element-type 'single-float
				  :initial-contents '(1.0 0.0 0.0)))
;; y軸
(defconstant +y-axis+ (make-array 3 :element-type 'single-float
				  :initial-contents '(0.0 1.0 0.0)))
;; z軸
(defconstant +z-axis+ (make-array 3 :element-type 'single-float
				  :initial-contents '(0.0 0.0 1.0)))

(defun rotation-matrix (ang vec &optional (result (make-float-matrix 3 3)))
  "returns a matrix which rotates ang[rad] around vec."
  (declare (type (simple-array single-float (3)) vec)
           (type (simple-array single-float (3 3)) result)
           (type single-float ang))
  (let ((cos (cos ang))
        (sin (sin ang)))
    (declare (type single-float cos sin))
    (let ((1-cos (- 1.0 cos)))
      (declare (type single-float 1-cos))
      (let ((vx (x vec))
            (vy (y vec))
            (vz (z vec)))
        (declare (type single-float vx vy vz))
        (setf (aref result 0 0) (+ (* vx vx 1-cos) cos))
        (setf (aref result 0 1) (- (* vx vy 1-cos) (* vz sin)))
        (setf (aref result 0 2) (+ (* vz vx 1-cos) (* vy sin)))
        (setf (aref result 1 0) (+ (* vx vy 1-cos) (* vz sin)))
        (setf (aref result 1 1) (+ (* vy vy 1-cos) cos))
        (setf (aref result 1 2) (- (* vy vz 1-cos) (* vx sin)))
        (setf (aref result 2 0) (- (* vz vx 1-cos) (* vy sin)))
        (setf (aref result 2 1) (+ (* vy vz 1-cos) (* vx sin)))
        (setf (aref result 2 2) (+ (* vz vz 1-cos) cos))
        )))
  result)

;; matをang, axis, worldpにしたがって回転させる
;; 結果はresultに入れる
(defun rotate-matrix (mat ang axis
                      &optional
                      (worldp nil)
                      (result (make-float-matrix 3 3)))
  "This is a destructive function.
   matをaxis周りにangだけ回転する.
   mat    ... 3x3 float matrix.
   axis   ... one of :x,:y or :z
   worldp ... nil -> local, multiple rot from the right
              t   -> world, multiple rot from the left"
  (declare (type (simple-array single-float (3 3)) mat result)
           (type single-float ang)
           (symbol worldp axis))
  (let ((cos (cos ang))
        (sin (sin ang)))
    (declare (type single-float cos sin))
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
                ((:y :-y :z :-z) 0)))
          (k2 (case axis
                ((:x :-x :y :-y) 2)
                ((:z :-z) 1))))
      (declare (type single-float s1 s2)
               (type fixnum k1 k2))
      (if (not (eq result mat))
          (copy-matrix mat result))
      (dotimes (i 3)
        (declare (type fixnum i))
        (if worldp
            (progn
              (let ((f1 (+ (* cos (aref result k1 i))
                           (* s1 (aref result k2 i))))
                    (f2 (+ (* s2 (aref result k1 i))
                           (* cos (aref result k2 i)))))
                (declare (type single-float f1 f2))
                (setf (aref result k1 i) f1)
                (setf (aref result k2 i) f2)
                ))
            (progn
              (let ((f1 (+ (* cos (aref result i k1))
                           (* s1 (aref result i k2))))
                    (f2 (+ (* s2 (aref result i k1))
                           (* cos (aref result i k2)))))
                (declare (type single-float f1 f2))
                (setf (aref result i k1) f1)
                (setf (aref result i k2) f2)
                )))
        )))
  result)

(defun euler-matrix (az ay az2)
  (declare (type single-float az ay az2))
  (let ((r (rotation-matrix az +z-axis+)))
    (rotate-matrix r ay :y nil r)
    (rotate-matrix r az2 :y nil r)
    r))

(defun rpy-matrix (az ay ax)
  (declare (type single-float ax ay az))
  (let ((r (rotation-matrix ax +x-axis+)))
    (declare (type (array single-float (3 3)) r))
    (rotate-matrix r ay :y t r)
    (rotate-matrix r az :z t r)
    r))

(defun rpy-angle (mat)
  (declare (type (array single-float (3 3))))
  )

(declaim (inline axis->vec))
(defun axis->vec (axis)
  (case axis
    (:x +x-axis+)
    (:y +y-axis+)
    (:z +z-axis+))
  )

