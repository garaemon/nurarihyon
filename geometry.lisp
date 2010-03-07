;;================================================
;; geometry.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================
(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
;; for debugging
;; (declaim (optimize (safety 3)
;;                   (debug 3)))

(in-package :nurarihyon)

(eval-when (:compile-toplevel)
  (enable-nurarihyon-reader-syntax))

(declaim (type (simple-array double-float (3)) +x-axis+ +y-axis+ +z-axis+))
(alexandria:define-constant +x-axis+ #.#d(1.0 0.0 0.0) :test #'eps-vector=)
(alexandria:define-constant +y-axis+ #.#d(0.0 1.0 0.0) :test #'eps-vector=)
(alexandria:define-constant +z-axis+ #.#d(0.0 0.0 1.0) :test #'eps-vector=)

(defun rotation-matrix (ang vec &optional (result (make-matrix33)))
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
        (setf [result 2 2] (+ (* vz vz 1-cos) cos)))))
  (the (simple-array double-float (3 3)) result))

(defun rotate-matrix-local (mat ang vec &optional (result (make-matrix33)))
  "rotate mat by a rotation matrix represented by ang and vec.
The rotation matrix is in local coordination same as mat, so
we muliply it to mat from left"
  (declare (type (simple-array double-float (3 3)) mat result)
           (type (simple-array double-float (3)) vec)
           (type double-float ang))
  (if (eq mat result)
      ;; if mat == result, we cannot use result as a buffer of rotation-matrix
      (m* (rotation-matrix ang vec) mat result)
      (m* (rotation-matrix ang vec mat result) result))
  (the (simple-array double-float (3 3)) result))

(defun rotate-matrix-world (mat ang vec &optional (result (make-matrix33)))
  "rotate mat by a rotation matrix represented by ang and vec.
The rotation matrix is in the world coordination, so
we muliply it to mat from right"
  (declare (type (simple-array double-float (3 3)) mat result)
           (type (simple-array double-float (3)) vec)
           (type double-float ang))
  (if (eq mat result)
      ;; if mat == result, we cannot use result as a buffer of rotation-matrix
      (m* mat (rotation-matrix ang vec) result)
      (m* mat (rotation-matrix ang vec result) result))
  (the (simple-array double-float (3 3)) result))

(defun rpy-matrix (az ay ax &optional (result (make-matrix33)))
  "make a rotation matrix and the result is set to 'result'.
The rotation matrix has been rotated by ax radian around
x-axis, ay radian around y-axis and az radian around z-axis.
All of the rotations is calculated in
world coordination. You can extract (az ay ax) from result using rpy-angle."
  (declare (type double-float ax ay az)
           (type (simple-array double-float (3 3)) result))
  (rotation-matrix ax +x-axis+ result)
  (rotate-matrix-world result ay +y-axis+ result)
  (rotate-matrix-world result az +z-axis+ result)
  (the (simple-array double-float (3 3)) result))

(defun euler-matrix (az ay az2 &optional (result (make-matrix33)))
  "make a rotation matrix and the result is set to 'result'.
az, ay and az2 are euler parameters to represent a rotation.
The rotation matrix has been rotated by az radian around local z-axis,
ay radian around local y-axis and az2 radian around local z-axis.
You can extract (az ay az2) from result using euler-angle."
  (declare (type double-float az ay az2)
           (type (simple-array double-float (3 3)) result))
  (rotation-matrix az +z-axis+ result)
  (rotate-matrix-local result ay +y-axis+ result)
  (rotate-matrix-local result az2 +z-axis+ result)
  (the (simple-array double-float (3 3)) result))

(defun rpy-angle (mat)
  "extract the angles.
Let M `mat':
az = atan2(M[0, 1], mat[0, 0]) or atan2(M[0, 1], mat[0, 0]) + pi
            because, tan(theta) = tan(theta + pi).
s = sin(az)
c = cos(az)
ay = atan2(-M[0, 2], c * M[0, 0] + s*  M[0, 1])
ax = atan2(s * M[0, 2] - c * M[1, 2], -s * M[0, 1] + c * M[1, 1])
"
  (declare (type (simple-array double-float (3 3)) mat))
  (labels ((calc-y-and-x (az)
             (declare (type double-float az))
             (let ((s (sin az))
                   (c (cos az)))
               (declare (type double-float s c))
               (let ((ay (atan (- [mat 0 2])
                               (+ (* c [mat 0 0]) (* s [mat 0 1]))))
                     (ax (atan (- (* s [mat 0 2]) (* c [mat 1 2]))
                               (- (* c [mat 1 1]) (* s [mat 0 1])))))
                 (declare (type double-float ay ax))
                 (list az ay ax)))))
    (let ((az1 (atan [mat 0 1] [mat 0 0]))
          (az2 (+ az1 +pi+)))
      (values (calc-y-and-x az1) (calc-y-and-x az2)))))
      

(defun euler-angle (mat)
  (declare (type (simple-array double-float (3 3)) mat))
  )

(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))
