(in-package :axion.game-utils)

(declaim (optimize (space 0) (speed 3)))

(deftype vec () '(simple-array single-float (3)))
(defstruct (vec
             (:type (vector single-float))
             (:constructor %make-vector (&optional x y z))
             (:conc-name v))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(declaim (inline vec))
(defun vec (&optional (x 0.0) (y 0.0) (z 0.0))
  #+sbcl
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (%make-vector (float x 1.0) (float y 1.0) (float z 1.0)))

(defmacro %with-vector ((prefix vec) &body body)
  (let ((*package* (find-package "AXION.GAME-UTILS")))
    `(with-accessors ((,(symbolicate prefix "X") vx)
                      (,(symbolicate prefix "Y") vy)
                      (,(symbolicate prefix "Z") vz))
       ,vec
       ,@body)))

(defmacro %with-vectors (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(%with-vector ,(car binds)
                   (%with-vectors ,(cdr binds) ,@body))))

(declaim (ftype (function (vec vec) vec) %vector-copy))
(declaim (inline %vector-copy))
(defun %vector-copy (src dest)
  "Copy a vector's components to another vector"
  (%with-vectors ((s src) (d dest))
    (psetf dx sx
           dy sy
           dz sz))
  dest)

(declaim (inline vcp))
(defun vcp (src)
  (%vector-copy src (vec)))

(declaim (inline vcp*))
(defun vcp* (src dest)
  (%vector-copy src dest))

(declaim (ftype (function (vec) vec) %vector-clear))
(declaim (inline %vector-clear))
(defun %vector-clear (src)
  "Zero all components of a vector"
  (%with-vector (s src)
    (psetf sx 0.0
           sy 0.0
           sz 0.0))
  src)

(declaim (inline vlr*))
(defun vclr* (src)
  (%vector-clear src))

(declaim (ftype (function (vec) list) %vector->list))
(declaim (inline %vector->list))
(defun %vector->list (src)
  "Convert a vector to a list of its components"
  (%with-vector (s src)
    (list sx sy sz)))

(declaim (inline vlist))
(defun vlist (src)
  (%vector->list src))

(declaim (ftype (function (vec) vec) %vector-negate))
(declaim (inline %vector-negate))
(defun %vector-negate (src)
  "Negate a vector's components"
  (%with-vector (s src)
    (psetf sx (- sx)
           sy (- sy)
           sz (- sz)))
  src)

(declaim (inline vneg))
(defun vneg (src)
  (%vector-negate (vcp src)))

(declaim (inline vneg*))
(defun vneg* (src)
  (%vector-negate src))

(declaim (ftype (function (vec vec vec) vec) %vector-add))
(declaim (inline %vector-add))
(defun %vector-add (src1 src2 dest)
  "Add two vectors"
  (%with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (+ s1x s2x)
           dy (+ s1y s2y)
           dz (+ s1z s2z)))
  dest)

(declaim (inline vadd))
(defun vadd (src1 src2)
  (%vector-add src1 src2 (vec)))

(declaim (inline vadd*))
(defun vadd* (src1 src2 dest)
  (%vector-add src1 src2 dest))

(declaim (ftype (function (vec vec vec) vec) %vector-subtract))
(declaim (inline %vector-subtract))
(defun %vector-subtract (src1 src2 dest)
  "Subtract a vector from another vector"
  (%with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (- s1x s2x)
           dy (- s1y s2y)
           dz (- s1z s2z)))
  dest)

(declaim (inline vsub))
(defun vsub (src1 src2)
  (%vector-subtract src1 src2 (vec)))

(declaim (inline vsub*))
(defun vsub* (src1 src2 dest)
  (%vector-subtract src1 src2 dest))

(declaim (ftype (function (vec vec vec) vec) %vector-multiply))
(declaim (inline %vector-multiply))
(defun %vector-multiply (src1 src2 dest)
  "Multiply two vectors"
  (%with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (* s1x s2x)
           dy (* s1y s2y)
           dz (* s1z s2z)))
  dest)

(declaim (ftype (function (vec vec) vec) vmult))
(declaim (inline vmult))
(defun vmult (src1 src2)
  (%vector-multiply src1 src2 (vec)))

(declaim (ftype (function (vec vec vec) vec) vmult*))
(declaim (inline vmult*))
(defun vmult* (src1 src2 dest)
  (%vector-multiply src1 src2 dest))

(declaim (ftype (function (vec single-float) vec) %vector-scale))
(declaim (inline %vector-scale))
(defun %vector-scale (src scalar)
  "Scale the length of a vector"
  (%with-vector (s src)
    (psetf sx (* sx scalar)
           sy (* sy scalar)
           sz (* sz scalar)))
  src)

(declaim (inline vscale))
(defun vscale (src scalar)
  (%vector-scale (vcp src) scalar))

(declaim (inline vscale*))
(defun vscale* (src scalar)
  (%vector-scale src scalar))

(declaim (ftype (function (vec) vec) %vector-reverse))
(declaim (inline %vector-reverse))
(defun %vector-reverse (src)
  "Compute the vector pointing in the opposite direction"
  (%vector-scale src -1.0))

(declaim (inline vrev))
(defun vrev (src)
  (%vector-reverse (vcp src)))

(declaim (inline vrev*))
(defun vrev* (src)
  (%vector-reverse src))

(declaim (ftype (function (vec) single-float) %vector-length))
(declaim (inline %vector-length))
(defun %vector-length (src)
  "Compute the Euclidean length of a vector"
  #+sbcl
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (%with-vector (s src)
    (sqrt (+ (* sx sx)
             (* sy sy)
             (* sz sz)))))

(declaim (inline vlen))
(defun vlen (src)
  (%vector-length src))

(declaim (ftype (function (vec) vec) %vector-normalize))
(declaim (inline %vector-normalize))
(defun %vector-normalize (src)
  "Convert a vector to a unit vector"
  (let ((magnitude (%vector-length src)))
    (unless (zerop magnitude)
      (%with-vector (s src)
        (psetf sx (/ sx magnitude)
               sy (/ sy magnitude)
               sz (/ sz magnitude))))
    src))

(declaim (inline vnorm))
(defun vnorm (src)
  (%vector-normalize (vcp src)))

(declaim (inline vnorm*))
(defun vnorm* (src)
  (%vector-normalize src))

(declaim (ftype (function (vec) vec) %vector-round))
(declaim (inline %vector-round))
(defun %vector-round (src)
  "Round a vector's components to the nearest whole number"
  (%with-vector (s src)
    (psetf sx (fround sx)
           sy (fround sy)
           sz (fround sz)))
  src)

(declaim (inline vround))
(defun vround (src)
  (%vector-round (vcp src)))

(declaim (inline vround*))
(defun vround* (src)
  (%vector-round src))

(declaim (ftype (function (vec) vec) %vector-positive))
(declaim (inline %vector-positive))
(defun %vector-positive (src)
  "Set all of a vector's components to be positive"
  (%with-vector (s src)
    (psetf sx (abs sx)
           sy (abs sy)
           sz (abs sz)))
  src)

(declaim (inline vpos))
(defun vpos (src)
  (%vector-positive (vcp src)))

(declaim (inline vpos*))
(defun vpos* (src)
  (%vector-positive src))

(declaim (ftype (function (vec vec vec) vec) %vector-cross))
(declaim (inline %vector-cross))
(defun %vector-cross (src1 src2 dest)
  "Compute the cross product of two vectors"
  (%with-vectors ((s1 src1) (s2 src2) (d dest))
    (psetf dx (- (* s1y s2z) (* s1z s2y))
           dy (- (* s1z s2x) (* s1x s2z))
           dz (- (* s1x s2y) (* s1y s2x))))
  dest)

(declaim (inline vcross))
(defun vcross (src1 src2)
  (%vector-cross src1 src2 (vec)))

(declaim (inline vcross*))
(defun vcross* (src1 src2 dest)
  (%vector-cross src1 src2 dest))

(declaim (ftype (function (vec vec) single-float) %vector-dot))
(declaim (inline %vector-dot))
(defun %vector-dot (src1 src2)
  "Compute the dot product of two vectors"
  (%with-vectors ((s1 src1) (s2 src2))
    (+ (* s1x s2x)
       (* s1y s2y)
       (* s1z s2z))))

(declaim (inline vdot))
(defun vdot (src1 src2)
  (%vector-dot src1 src2))

(declaim (inline vbox))
(defun vbox (src1 src2 src3)
  (vdot (vcross src1 src2) src3))

(declaim (ftype (function (vec vec) single-float) %vector-distance))
(declaim (inline %vector-distance))
(defun %vector-distance (src1 src2)
  "Compute the Euclidean distance between two vectors"
  (%with-vectors ((s1 src1) (s2 src2))
    (sqrt (+ (expt (- (vx src2) (vx src1)) 2)
             (expt (- (vy src2) (vy src1)) 2)
             (expt (- (vz src2) (vz src2)) 2)))))

(declaim (inline vdist))
(defun vdist (src1 src2)
  (%vector-distance src1 src2))

(declaim (ftype (function (vec vec single-float &optional boolean) vec)
                %vector-translate))
(declaim (inline %vector-translate))
(defun %vector-translate (src direction distance &optional normalizep)
  "Calculate a vector that is translated along a directional vector by the
   given distance"
  (when normalizep
    (%vector-normalize direction))
  (%vector-add src (%vector-scale direction distance) src))

(declaim (inline vtrans))
(defun vtrans (src direction distance &optional normalizep)
  (%vector-translate (vcp src) direction distance normalizep))

(declaim (inline vtrans*))
(defun vtrans* (src direction distance &optional normalizep)
  (%vector-translate src direction distance normalizep))

(declaim (ftype (function (vec)) %vector-zero-p))
(declaim (inline %vector-zero-p))
(defun %vector-zero-p (src)
  "Check if all of a vector's components are of zero length"
  (%with-vector (s src)
    (and (zerop sx)
         (zerop sy)
         (zerop sz))))

(declaim (inline vzerop))
(defun vzerop (src)
  (%vector-zero-p src))

(declaim (ftype (function (vec vec &key (:tolerance single-float)))
                %vector-close-p))
(declaim (inline %vector-close-p))
(defun %vector-close-p (src1 src2 &key (tolerance *tolerance*))
  "Checks if the distance between two points is below the tolerance level"
  (< (%vector-distance src1 src2) tolerance))

(declaim (inline vclosep))
(defun vclosep (src1 src2 &key (tolerance *tolerance*))
  (%vector-close-p src1 src2 :tolerance tolerance))

(declaim (ftype (function (vec vec)) %vector-direction-p))
(declaim (inline %vector-direction-p))
(defun %vector-direction-p (src1 src2)
  "Check if two vectors are pointing in the same direction"
  (and (not (or (%vector-zero-p src1) (%vector-zero-p src2)))
       (%vector-close-p (%vector-normalize src1)
                        (%vector-normalize src2))))

(declaim (inline vdirp))
(defun vdirp (src1 src2)
  (%vector-direction-p src1 src2))

(declaim (ftype (function (vec vec)) %vector-parallel-p))
(declaim (inline %vector-parallel-p))
(defun %vector-parallel-p (src1 src2)
  "Check if two vectors are parallel to each other"
  (or (%vector-direction-p src1 src2)
      (%vector-direction-p src1 (vrev src2))))

(declaim (inline vparp))
(defun vparp (src1 src2)
  (%vector-parallel-p src1 src2))

(defun %vector-test ()
  "Time the result of multiplying 1 million vectors"
  (time
    (let ((v (vec)))
      (loop repeat 1000000
            do (vmult* v v v))))) 
