;;================================================
;; quaternion.lisp
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

;; in nurarihyon, quaternion is represented as  angle-axis quaternion.
(defun matrix33->quaternion (mat &optional (q (make-vector4)))
  "convert a 3x3 matrix to a quaternion.
   reference is http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm"
  (declare (type (simple-array double-float (4)) q)
           (type (simple-array double-float (3 3)) mat))
  (let ((trace (matrix-trace mat)))
    (declare (type double-float trace))
    (cond
     ((> trace 0.0d0)
      (let ((s (/ 0.5d0 (sqrt (+ 1.0d0 trace)))))
        (declare (type double-float s))
        (setf [q 0] (/ 0.25d0 s))
        (setf [q 1] (* s (- [mat 2 1] [mat 1 2])))
        (setf [q 2] (* s (- [mat 0 2] [mat 2 0])))
        (setf [q 3] (* s (- [mat 1 0] [mat 0 1])))))
     ((and (> [mat 0 0] [mat 1 1]) (> [mat 0 0] [mat 2 2]))
      (let ((s (* 2.0d0 (sqrt (+ 1.0d0 [mat 0 0]
                                 (- [mat 1 1]) (- [mat 2 2]))))))
        (declare (type double-float s))
        (setf [q 0] (/ (- [mat 2 1] [mat 1 2]) s))
        (setf [q 1] (* 0.25d0 s))
        (setf [q 2] (/ (+ [mat 0 1] [mat 1 0]) s))
        (setf [q 3] (/ (+ [mat 0 2] [mat 2 0]) s))))
     ((> [mat 1 1] [mat 2 2])
      (let ((s (* 2.0d0 (sqrt (+ 1.0d0 [mat 1 1]
                                 (- [mat 0 0]) (- [mat 2 2]))))))
        (declare (type double-float s))
        (setf [q 0] (/ (- [mat 0 2] [mat 2 0]) s))
        (setf [q 1] (/ (+ [mat 0 1] [mat 1 0]) s))
        (setf [q 2] (* 0.25d0 s))
        (setf [q 3] (/ (+ [mat 1 2] [mat 2 1]) s))))
     (t
      (let ((s (* 2.0d0 (sqrt (+ 1.0d0 [mat 2 2]
                                 (- [mat 0 0]) (- [mat 1 1]))))))
        (declare (type double-float s))
        (setf [q 0] (/ (- [mat 1 0] [mat 0 1]) s))
        (setf [q 1] (/ (+ [mat 0 2] [mat 2 0]) s))
        (setf [q 2] (/ (+ [mat 1 2] [mat 2 1]) s))
        (setf [q 3] (* 0.25d0 s))))))
  (normalize-vector q q))

(defun quaternion->matrix33 (q &optional (mat (make-matrix33)))
  "convert a quaternion to 3x3 matrix"
  )

(eval-when (:compile-toplevel)
  (disable-nurarihyon-reader-syntax))

