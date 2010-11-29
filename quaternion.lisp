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

(declaim-inline-nhfun make-identity-quaternion)
(define-nhfun make-identity-quaternion ()
  "make an identity quaternion.
it means [1; #(0 0 0)]"
  (let ((ret ($make-vector4)))
    (declare (type (simple-array double-float (4)) ret))
    (setf (qw ret) 1.0d0)
    (the (simple-array double-float (4)) ret)))

(declaim-inline-nhfun q*)
(define-nhfun q* (q1 q2 &optional (result ($make-vector4)))
  "multiply two quaternions and put the result into RESULT"
  (declare (type (simple-array double-float (4)) q1 q2 result))
  (let ((q1x (qx q1)) (q1y (qy q1)) (q1z (qz q1)) (q1w (qw q1))
        (q2x (qx q2)) (q2y (qy q2)) (q2z (qz q2)) (q2w (qw q2)))
    (declare (type double-float q1x q1y q1z q1w q2x q2y q2z q2w))
    (setf (qw result) (- (* q1w q2w) (* q1x q2x) (* q1y q2y) (* q1z q2z)))
    (setf (qx result) (+ (- (* q1z q2y)) (* q1w q2x) (* q1x q2w) (* q1y q2z)))
    (setf (qy result) (+ (- (* q1x q2z)) (* q1w q2y) (* q1y q2w) (* q1z q2x)))
    (setf (qw result) (+ (- (* q1y q2x)) (* q1w q2z) (* q1z q2w) (* q1x q2y)))
    (the (simple-array double-float (4)) result)))

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
           (let ((ss (sqrt (+ 1.0d0 [mat 0 0]
                              (- [mat 1 1]) (- [mat 2 2])))))
             (declare (type double-float ss))
             (let ((s (* 2.0d0 ss)))
               (declare (type double-float s))
               (setf (qw q) (/ (- [mat 2 1] [mat 1 2]) s))
               (setf (qx q) (* 0.25d0 s))
               (setf (qy q) (/ (+ [mat 0 1] [mat 1 0]) s))
               (setf (qz q) (/ (+ [mat 0 2] [mat 2 0]) s)))))
          ((> [mat 1 1] [mat 2 2])
           (let ((ss (sqrt (+ 1.0d0 [mat 1 1]
                              (- [mat 0 0]) (- [mat 2 2])))))
             (declare (type double-float ss))
             (let ((s (* 2.0d0 ss)))
               (declare (type double-float s))
               (setf (qw q) (/ (- [mat 0 2] [mat 2 0]) s))
               (setf (qx q) (/ (+ [mat 0 1] [mat 1 0]) s))
               (setf (qy q) (* 0.25d0 s))
               (setf (qz q) (/ (+ [mat 1 2] [mat 2 1]) s)))))
          (t
           (let ((ss (sqrt (+ 1.0d0 [mat 2 2]
                              (- [mat 0 0]) (- [mat 1 1])))))
             (declare (type double-float ss))
             (let ((s (* 2.0d0 ss)))
               (declare (type double-float s))
               (setf (qw q) (/ (- [mat 1 0] [mat 0 1]) s))
               (setf (qx q) (/ (+ [mat 0 2] [mat 2 0]) s))
               (setf (qy q) (/ (+ [mat 1 2] [mat 2 1]) s))
               (setf (qz q) (* 0.25d0 s)))))))
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
You can use optional argument to avoid allocation.

if the given quaternion is Ai + Bj + Ck + D, the conjugate of it is
-Ai -Bj -Ck + D."
  (declare (type (simple-array double-float (4)) q))
  (let ((buf (or buf ($make-vector4))))
    (declare (type (simple-array double-float (4)) buf))
    (with-ensure-vector-dimension
        (buf 4)
      (setf (qw buf) (qw q))
      (setf (qx buf) (- (qx q)))
      (setf (qy buf) (- (qy q)))
      (setf (qz buf) (- (qz q)))
      (the (simple-array double-float (4)) buf))))

(disable-nurarihyon-reader-syntax)
