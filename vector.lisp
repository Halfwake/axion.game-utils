(in-package :axion.game-utils)

(declaim (optimize (space 0) (speed 3)))

(deftype ax-vector () '(simple-array single-float (3)))
(defstruct (ax-vector
             (:type (vector single-float))
             (:constructor %make-vector (&optional x y z))
             (:conc-name v))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(declaim (inline make-vector))
(defun make-vector (&optional (x 0.0) (y 0.0) (z 0.0))
  #+sbcl
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (%make-vector (float x 1.0) (float y 1.0) (float z 1.0)))

(defmacro with-vector ((prefix vec) &body body)
  `(with-accessors ((,(symbolicate prefix "X") vx)
                    (,(symbolicate prefix "Y") vy)
                    (,(symbolicate prefix "Z") vz))
     ,vec
     ,@body))

(defmacro with-vectors (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(with-vector ,(car binds)
                  (with-vectors ,(cdr binds) ,@body))))

(defun vector-test ()
  "Time the result of multiplying 1 million vectors"
  (time
    (let ((v (make-vector)))
      (loop repeat 1000000
            do (vector-multiply-* v v v)))))

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-copy-*))
(declaim (inline vector-copy-*))
(defun vector-copy-* (src dest)
  "Copy a vector's components to another vector"
  (with-vectors ((s src) (d dest))
    (psetf dx sx
           dy sy
           dz sz))
  dest)

(declaim (ftype (function (ax-vector) ax-vector) vector-copy))
(declaim (inline vector-copy))
(defun vector-copy (src)
  "Copy a vector's components to a new vector"
  (vector-copy-* src (make-vector)))

(declaim (ftype (function (ax-vector) ax-vector) vector-clear))
(declaim (inline vector-clear))
(defun vector-clear (src)
  "Zero all components of a vector"
  (with-vector (s src)
    (psetf sx 0.0
           sy 0.0
           sz 0.0))
  src)

(declaim (ftype (function (ax-vector) list) vector->list))
(defun vector->list (src)
  "Convert a vector to a list of its components"
  (with-vector (s src)
    (list sx sy sz)))

(declaim (ftype (function (ax-vector) ax-vector) vector-negate-*))
(declaim (inline vector-negate-*))
(defun vector-negate-* (src)
  "Negate a vector's components"
  (with-vector (s src)
    (psetf sx (- sx)
           sy (- sy)
           sz (- sz)))
  src)

(declaim (ftype (function (ax-vector) ax-vector) vector-negate))
(defun vector-negate (src)
  "Negate a vector's components as a new vector"
  (vector-negate-* (vector-copy src)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-add-*))
(declaim (inline vector-add-*))
(defun vector-add-* (src1 src2 dest)
  "Store the sum of two vectors in an existing vector"
  (with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (+ s1x s2x)
           dy (+ s1y s2y)
           dz (+ s1z s2z)))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-add))
(defun vector-add (src1 src2)
  "Store the sum of two vectors in a new vector"
  (vector-add-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-subtract-*))
(declaim (inline vector-subtract-*))
(defun vector-subtract-* (src1 src2 dest)
  "Store the difference of two vectors in an existing vector"
  (with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (- s1x s2x)
           dy (- s1y s2y)
           dz (- s1z s2z)))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-subtract))
(defun vector-subtract (src1 src2)
  "Store the difference of two vectors in a new vector"
  (vector-subtract-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-multiply-*))
(declaim (inline vector-multiply-*))
(defun vector-multiply-* (src1 src2 dest)
  "Store the product of two vectors in an existing vector"
  (with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (* s1x s2x)
           dy (* s1y s2y)
           dz (* s1z s2z)))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-multiply))
(defun vector-multiply (src1 src2)
  "Store the product of two vectors in a new vector"
  (vector-multiply-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector single-float) ax-vector) vector-scale-*))
(declaim (inline vector-scale-*))
(defun vector-scale-* (src scalar)
  "Scale the length of a vector"
  (with-vector (s src)
    (psetf sx (* sx scalar)
           sy (* sy scalar)
           sz (* sz scalar)))
  src)

(declaim (ftype (function (ax-vector single-float) ax-vector) vector-scale))
(defun vector-scale (src scalar)
  "Scale the length of a vector as a new vector"
  (vector-scale-* (vector-copy src) scalar))

(declaim (ftype (function (ax-vector) ax-vector) vector-reverse-*))
(declaim (inline vector-reverse-*))
(defun vector-reverse-* (src)
  "Compute the vector pointing in the opposite direction"
  (vector-scale-* src -1.0))

(declaim (ftype (function (ax-vector) ax-vector) vector-reverse))
(defun vector-reverse (src)
  "Compute the vector pointing in the opposite direction as a new vector"
  (vector-reverse-* (vector-copy src)))

(declaim (ftype (function (ax-vector) single-float) vector-length))
(declaim (inline vector-length))
(defun vector-length (src)
  "Compute the Euclidean length of a vector"
  #+sbcl
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-vector (s src)
    (sqrt (+ (* sx sx)
             (* sy sy)
             (* sz sz)))))

(declaim (ftype (function (ax-vector) ax-vector) vector-normalize-*))
(declaim (inline vector-normalize-*))
(defun vector-normalize-* (src)
  "Convert a vector to a unit vector"
  (let ((magnitude (vector-length src)))
    (unless (zerop magnitude)
      (with-vector (s src)
        (psetf sx (/ sx magnitude)
               sy (/ sy magnitude)
               sz (/ sz magnitude))))
    src))

(declaim (ftype (function (ax-vector) ax-vector) vector-normalize))
(defun vector-normalize (src)
  "Convert a vector into a unit vector as a new vector"
  (vector-normalize-* (vector-copy src)))

(declaim (ftype (function (ax-vector) ax-vector) vector-round-*))
(declaim (inline vector-round-*))
(defun vector-round-* (src)
  (with-vector (s src)
    (psetf sx (fround sx)
           sy (fround sy)
           sz (fround sz)))
  src)

(declaim (ftype (function (ax-vector) ax-vector) vector-round))
(defun vector-round (src)
  (vector-round-* (vector-copy src)))

(declaim (ftype (function (ax-vector) ax-vector) vector-positive-*))
(declaim (inline vector-positive-*))
(defun vector-positive-* (src)
  "Set all of a vector's components to be positive"
  (with-vector (s src)
    (psetf sx (abs sx)
           sy (abs sy)
           sz (abs sz)))
  src)

(declaim (ftype (function (ax-vector) ax-vector) vector-positive))
(defun vector-positive (src)
  "Set all of a vector's components to be positive, copied to a new vector"
  (vector-positive-* (vector-copy src)))

(declaim (ftype (function (ax-vector ax-vector ax-vector) ax-vector)
                vector-cross-*))
(declaim (inline vector-cross-*))
(defun vector-cross-* (src1 src2 dest)
  "Compute the cross product of two vectors to an existing vector"
  (with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (- (* s1y s2z) (* s1z s2y))
           dy (- (* s1z s2x) (* s1x s2z))
           dz (- (* s1x s2y) (* s1y s2x))))
  dest)

(declaim (ftype (function (ax-vector ax-vector) ax-vector) vector-cross))
(defun vector-cross (src1 src2)
  "Compute the cross product of two vectors to a new vector"
  (vector-cross-* src1 src2 (make-vector)))

(declaim (ftype (function (ax-vector ax-vector) single-float) vector-dot))
(defun vector-dot (src1 src2)
  "Compute the dot product of two vectors"
  (with-vectors ((s1 src1) (s2 src2))
    (+ (* s1x s2x)
       (* s1y s2y)
       (* s1z s2z))))

(declaim (ftype (function (ax-vector ax-vector ax-vector) single-float)
                vector-box))
(defun vector-box (src1 src2 src3)
  "Compute the box product of three vectors"
  (vector-dot (vector-cross src1 src2) src3))

(declaim (ftype (function (ax-vector ax-vector) single-float) vector-distance))
(declaim (inline vector-distance))
(defun vector-distance (src1 src2)
  "Compute the Euclidean distance between two vectors"
  (with-vectors ((s1 src1) (s2 src2))
    (sqrt (+ (expt (- (vx src2) (vx src1)) 2)
             (expt (- (vy src2) (vy src1)) 2)
             (expt (- (vz src2) (vz src2)) 2)))))

(declaim (ftype (function (ax-vector ax-vector single-float &optional boolean)
                          ax-vector) vector-translate-*))
(declaim (inline vector-translate-*))
(defun vector-translate-* (src direction distance &optional normalizep)
  "Calculate a vector that is translated along a directional vector by the
   given distance"
  (when normalizep
    (vector-normalize-* direction))
  (vector-add-* src (vector-scale direction distance) src))

(declaim (ftype (function (ax-vector ax-vector single-float &optional boolean)
                          ax-vector) vector-translate))
(defun vector-translate (src direction distance &optional normalizep)
  "Calculate a vector that is translated along a directional vector by the
   given distance, as a new vector"
  (vector-translate-* (vector-copy src) direction distance normalizep))

(declaim (ftype (function (ax-vector)) vector-zero-p))
(defun vector-zero-p (src)
  "Check if the vector's components are of zero length"
  (with-vector (s src)
    (and (zerop sx)
         (zerop sy)
         (zerop sz))))

(declaim (ftype (function (ax-vector ax-vector &key (:tolerance single-float)))
                vector-close-p))
(defun vector-close-p (src1 src2 &key (tolerance *tolerance*))
  "Checks if the distance between two points is below the tolerance level"
  (< (vector-distance src1 src2) tolerance))

(declaim (ftype (function (ax-vector ax-vector)) vector-direction=))
(defun vector-direction= (src1 src2)
  "Check if two vectors are pointing in the same direction"
  (and (not (or (vector-zero-p src1) (vector-zero-p src2)))
       (vector-close-p (vector-normalize src1)
                       (vector-normalize src2))))

(declaim (ftype (function (ax-vector ax-vector)) vector-parallel-p))
(defun vector-parallel-p (src1 src2)
  "Check if two vectors are parallel to each other"
  (or (vector-direction= src1 src2)
      (vector-direction= src1 (vector-reverse src2))))
