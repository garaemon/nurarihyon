;;================================================
;; quaternion.lisp
;;
;; written by R.Ueda (garaemon)
;;================================================

;; in nurarihyon, quaternion is represented as  angle-axis quaternion.
;; it means [w; (x, y, z)] is represented as #(w x y z)

(in-package :nurarihyon)

(enable-nurarihyon-reader-syntax)

(defmacro qx (q)
  "accessor for x element of quaternion"
  `(aref ,q 1))

(defmacro qy (q)
  "accessor for y element of quaternion"
  `(aref ,q 2))

(defmacro qz (q)
  "accessor for z element of quaternion"
  `(aref ,q 3))

(defmacro qw (q)
  "accessor for w element of quaternion"
  `(aref ,q 0))

(declaim-inline-nhfun identity-quaternion)
(define-nhfun identity-quaternion ()
  "make an identity quaternion.
it means [1; #(0 0 0)]"
  (let ((ret ($make-vector4)))
    (declare (type (simple-array double-float (4)) ret))
    (setf (qw ret) 1.0d0)
    (the (simple-array double-float (4)) ret)))

(define-nhfun matrix33->quaternion (mat &optional (q nil))
  "convert a 3x3 matrix to a quaternion.
   reference is http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm.

the return vector is always normalized."
  (declare (type (simple-array double-float (3 3)) mat))
  (let ((q (or q ($make-vector4))))
    (declare (type (simple-array double-float (4)) q))
    (with-ensure-vector-dimension
        (q 4)
      (let ((trace ($matrix-trace mat)))
        (declare (type double-float trace))
        (cond
          ((> trace 0.0d0)
           (let ((s (/ 0.5d0 (sqrt (+ 1.0d0 trace)))))
             (declare (type double-float s))
             (setf (qw q) (/ 0.25d0 s))
             (setf (qx q) (* s (- [mat 2 1] [mat 1 2])))
             (setf (qy q) (* s (- [mat 0 2] [mat 2 0])))
             (setf (qz q) (* s (- [mat 1 0] [mat 0 1])))))
          ((and (> [mat 0 0] [mat 1 1]) (> [mat 0 0] [mat 2 2]))
           (let ((s (* 2.0d0 (sqrt (+ 1.0d0 [mat 0 0]
                                      (- [mat 1 1]) (- [mat 2 2]))))))
             (declare (type double-float s))
             (setf (qw q) (/ (- [mat 2 1] [mat 1 2]) s))
             (setf (qx q) (* 0.25d0 s))
             (setf (qy q) (/ (+ [mat 0 1] [mat 1 0]) s))
             (setf (qz q) (/ (+ [mat 0 2] [mat 2 0]) s))))
          ((> [mat 1 1] [mat 2 2])
           (let ((s (* 2.0d0 (sqrt (+ 1.0d0 [mat 1 1]
                                      (- [mat 0 0]) (- [mat 2 2]))))))
             (declare (type double-float s))
             (setf (qw q) (/ (- [mat 0 2] [mat 2 0]) s))
             (setf (qx q) (/ (+ [mat 0 1] [mat 1 0]) s))
             (setf (qy q) (* 0.25d0 s))
             (setf (qz q) (/ (+ [mat 1 2] [mat 2 1]) s))))
          (t
           (let ((s (* 2.0d0 (sqrt (+ 1.0d0 [mat 2 2]
                                      (- [mat 0 0]) (- [mat 1 1]))))))
             (declare (type double-float s))
             (setf (qw q) (/ (- [mat 1 0] [mat 0 1]) s))
             (setf (qx q) (/ (+ [mat 0 2] [mat 2 0]) s))
             (setf (qy q) (/ (+ [mat 1 2] [mat 2 1]) s))
             (setf (qz q) (* 0.25d0 s))))))
      (the (simple-array double-float (4)) ($normalize-vector q q)))))

(define-nhfun quaternion->matrix33 (q &optional (mat nil))
  "convert a quaternion to 3x3 matrix"
  (declare (type (simple-array double-float (4)) q))
  (let ((mat (or mat ($make-matrix33))))
    (declare (type (simple-array double-float (3 3)) mat))
    (with-ensure-matrix-dimensions
        (mat 3 3)
      (let ((qw (qw q)) (qx (qx q)) (qy (qy q)) (qz (qz q)))
        (declare (type double-float qw qx qy qz))
        (setf [mat 0 0] (- 1.0d0 (* 2.0d0 qy qy) (* 2.0d0 qz qz)))
        (setf [mat 0 1] (- (* 2.0d0 qx qy) (* 2.0d0 qz qw)))
        (setf [mat 0 2] (+ (* 2.0d0 qx qz) (* 2.0d0 qy qw)))
        (setf [mat 1 0] (+ (* 2.0d0 qx qy) (* 2.0d0 qz qw)))
        (setf [mat 1 1] (- 1.0d0 (* 2.0d0 qx qx) (* 2.0d0 qz qz)))
        (setf [mat 1 2] (- (* 2.0d0 qy qz) (* 2.0d0 qx qw)))
        (setf [mat 2 0] (- (* 2.0d0 qx qz) (* 2.0d0 qy qw)))
        (setf [mat 2 1] (+ (* 2.0d0 qy qz) (* 2.0d0 qx qw)))
        (setf [mat 2 2] (- 1.0d0 (* 2.0d0 qx qx) (* 2.0d0 qy qy)))
        (the (simple-array double-float (3 3)) mat)))))

(declaim-inline-nhfun quaternion-axis)
(define-nhfun quaternion-axis (q &optional (buf nil))
  "returns the axis of a quaternion.
You can use the optional argument to avoid allocation.
reference is http://www.euclideanspace.com/maths/geometry/rotations/conversions/quaternionToAngle/index.htm"
  (declare (type (simple-array double-float (4)) q))
  (let ((buf (or buf ($make-vector3))))
    (declare (type (simple-array double-float (3)) buf))
    (with-ensure-vector-dimension
        (buf 3)
      (let ((qw (qw q)))
        (declare (type double-float qw))
        (if (eps= qw 1.0d0)             ;to avoid to devide by zero
            (progn
              (setf (x buf) 0.0d0)
              (setf (y buf) 0.0d0)
              (setf (z buf) 0.0d0))
            (let ((r1-qw^2 (sqrt (- 1.0d0 (* qw qw)))))
              (declare (type double-float r1-qw^2))
              (setf (x buf) (/ (qx q) r1-qw^2))
              (setf (y buf) (/ (qy q) r1-qw^2))
              (setf (z buf) (/ (qz q) r1-qw^2))))
        (the (simple-array double-float (3)) buf)))))

(declaim-inline-nhfun quaternion-angle)
(define-nhfun quaternion-angle (q)
  "return an angle of a quaternion in radian"
  (declare (type (simple-array double-float (4)) q))
  (let ((qw (qw q))
        (qx (qx q))
        (qy (qy q))
        (qz (qz q)))
    (declare (type double-float qw qx qy qz))
    (let ((sin (sqrt (+ (* qx qx) (* qy qy) (* qz qz)))))
      (declare (type double-float sin))
      (let ((theta/2 (atan sin qw)))
        (declare (type double-float theta/2))
        (let ((ret (* 2.0d0 theta/2)))
          (declare (type double-float ret))
          (the double-float ret))))))

(declaim-inline-nhfun quaternion-conjugate)
(define-nhfun quaternion-conjugate (q &optional (buf nil))
  "return a conjugate of a quaternion.
You can use optional argument to avoid allocation."
  (let ((buf (or buf ($make-vector4))))
    (declare (type (simple-array double-float (4)) q buf))
    (with-ensure-vector-dimension
        (buf 4)
      (setf (qw buf) (qw q))
      (setf (qx buf) (- (qx q)))
      (setf (qy buf) (- (qy q)))
      (setf (qz buf) (- (qz q)))
      (the (simple-array double-float (4)) buf))))

(disable-nurarihyon-reader-syntax)
