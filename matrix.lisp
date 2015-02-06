(in-package :axion.game-utils)

(declaim (optimize (space 0) (speed 3)))

(deftype mat () '(simple-array single-float (16)))
(defstruct (mat
             (:type (vector single-float))
             (:constructor mat (&optional m00 m01 m02 m03
                                          m10 m11 m12 m13
                                          m20 m21 m22 m23
                                          m30 m31 m32 m33))
             (:conc-name nil))
  (m00 0.0 :type single-float)
  (m01 0.0 :type single-float)
  (m02 0.0 :type single-float)
  (m03 0.0 :type single-float)
  (m10 0.0 :type single-float)
  (m11 0.0 :type single-float)
  (m12 0.0 :type single-float)
  (m13 0.0 :type single-float)
  (m20 0.0 :type single-float)
  (m21 0.0 :type single-float)
  (m22 0.0 :type single-float)
  (m23 0.0 :type single-float)
  (m30 0.0 :type single-float)
  (m31 0.0 :type single-float)
  (m32 0.0 :type single-float)
  (m33 0.0 :type single-float))

(defmacro %with-matrix ((prefix mat) &body body)
  (let ((*package* (find-package "AXION.GAME-UTILS")))
    `(with-accessors ((,(symbolicate prefix "00") m00)
                      (,(symbolicate prefix "01") m01)
                      (,(symbolicate prefix "02") m02)
                      (,(symbolicate prefix "03") m03)
                      (,(symbolicate prefix "10") m10)
                      (,(symbolicate prefix "11") m11)
                      (,(symbolicate prefix "12") m12)
                      (,(symbolicate prefix "13") m13)
                      (,(symbolicate prefix "20") m20)
                      (,(symbolicate prefix "21") m21)
                      (,(symbolicate prefix "22") m22)
                      (,(symbolicate prefix "23") m23)
                      (,(symbolicate prefix "30") m30)
                      (,(symbolicate prefix "31") m31)
                      (,(symbolicate prefix "32") m32)
                      (,(symbolicate prefix "33") m33))
       ,mat
       ,@body)))

(defmacro %with-matrices (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(%with-matrix ,(car binds)
       (%with-matrices ,(cdr binds) ,@body))))

(set-pprint-dispatch
  'mat
  #'(lambda (stream pobj)
      #+sbcl
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (%with-matrix (m pobj)
        (print-unreadable-object (pobj stream)
          (format
            stream "~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a"
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33)))))

(declaim (ftype (function (mat mat) mat) %matrix-copy))
(defun %matrix-copy (src dest)
  "Make a copy of a matrix"
  (%with-matrices ((s src) (d dest))
    (psetf d00 s00 d01 s01 d02 s02 d03 s03
           d10 s10 d11 s11 d12 s12 d13 s13
           d20 s20 d21 s21 d22 s22 d23 s23
           d30 s30 d31 s31 d32 s32 d33 s33))
  dest)

(declaim (inline matcp))
(defun matcp (src)
  (%matrix-copy src (mat)))

(declaim (inline matcp*))
(defun matcp* (src dest)
  (%matrix-copy src dest))

(declaim (ftype (function (mat) mat) %matrix-identity))
(defun %matrix-identity (src)
  "Set a matrix to the identity matrix"
  (%with-matrix (m src)
    (psetf m00 1.0 m01 0.0 m02 0.0 m03 0.0
           m10 0.0 m11 1.0 m12 0.0 m13 0.0
           m20 0.0 m21 0.0 m22 1.0 m23 0.0
           m30 0.0 m31 0.0 m32 0.0 m33 1.0))
  src)

(declaim (inline matid))
(defun matid ()
  (%matrix-identity (mat)))

(declaim (inline matid*))
(defun matid* (src)
  (%matrix-identity src))

(declaim (ftype (function (mat &key (:tolerance single-float)) mat)
                %matrix-stabilize))
(defun %matrix-stabilize (src &key (tolerance *tolerance*))
  "Force each matrix element to 0 if below the tolerance level"
  (%with-matrix (m src)
    (macrolet ((stabilize (place)
                 `(when (< (abs ,place) tolerance)
                    (setf ,place 0.0))))
      (stabilize m00)
      (stabilize m01)
      (stabilize m02)
      (stabilize m03)
      (stabilize m10)
      (stabilize m11)
      (stabilize m12)
      (stabilize m13)
      (stabilize m20)
      (stabilize m21)
      (stabilize m22)
      (stabilize m23)
      (stabilize m30)
      (stabilize m31)
      (stabilize m32)
      (stabilize m33)))
  src)

(declaim (inline matstab))
(defun matstab (src &key (tolerance *tolerance*))
  (%matrix-stabilize (matcp src) :tolerance tolerance))

(declaim (inline matstab*))
(defun matstab* (src &key (tolerance *tolerance*))
  (%matrix-stabilize src :tolerance tolerance))

(declaim (ftype (function (mat mat mat) mat) %matrix-multiply))
(defun %matrix-multiply (src1 src2 dest)
  "Multiply two matrices"
  (%with-matrices ((a src1) (b src2) (d dest))
    (psetf d00 (+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30))
           d10 (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30))
           d20 (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30))
           d30 (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30))
           d01 (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31))
           d11 (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31))
           d21 (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31))
           d31 (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31))
           d02 (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32))
           d12 (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32))
           d22 (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32))
           d32 (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32))
           d03 (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33))
           d13 (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33))
           d23 (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33))
           d33 (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33))))
  dest)

(declaim (inline matmult))
(defun matmult (src1 src2)
  (%matrix-multiply src1 src2 (mat)))

(declaim (inline matmult*))
(defun matmult* (src1 src2 dest)
  (%matrix-multiply src1 src2 dest))

(declaim (ftype (function (mat mat) mat) %matrix-transpose))
(defun %matrix-transpose (src dest)
  "Convert all rows into columns, and all columns into rows"
  (unless (eq src dest)
    (matcp* src dest))
  (%with-matrix (m dest)
    (rotatef m10 m01)
    (rotatef m20 m02)
    (rotatef m30 m03)
    (rotatef m21 m12)
    (rotatef m31 m13)
    (rotatef m32 m23))
  dest)

(declaim (inline mattransp))
(defun mattransp (src)
  (%matrix-transpose src (mat)))

(declaim (inline mattransp*))
(defun mattransp* (src dest)
  (%matrix-transpose src dest))

(declaim (ftype (function (vec mat mat) mat) %matrix-rotate))
(defun %matrix-rotate (vec src dest)
  "Apply a rotation transformation to a matrix"
  (let ((rotation (matid)))
    (macrolet ((rot (axis s c &body body)
                 `(let ((,s (sin ,axis))
                        (,c (cos ,axis)))
                    ,@body
                    (matmult* src rotation dest)
                    (matcprot* dest src))))
      (%with-matrix (m rotation)
        (rot (vz vec) s c
          (psetf m00 c
                 m10 s
                 m01 (- s)
                 m11 c))
        (rot (vx vec) s c
          (psetf m00 1.0
                 m10 0.0
                 m20 0.0
                 m01 0.0
                 m11 c
                 m21 s
                 m02 0.0
                 m12 (- s)
                 m22 c))
        (rot (vy vec) s c
          (psetf m00 c
                 m10 0.0
                 m20 (- s)
                 m01 0.0
                 m11 1.0
                 m21 0.0
                 m02 s
                 m12 0.0
                 m22 c)))))
  (matstab* src))

(declaim (inline matrot))
(defun matrot (vec src)
  (%matrix-rotate vec src (matid)))

(declaim (inline matrot*))
(defun matrot* (vec src dest)
  (%matrix-rotate vec src dest))

(declaim (ftype (function (vec mat) mat) %matrix-translate))
(defun %matrix-translate (vec src)
  "Apply a translation transformation to a matrix"
  (%with-matrix (m src)
    (psetf m03 (vx vec)
           m13 (vy vec)
           m23 (vz vec)))
  src)

(declaim (inline mattransl))
(defun mattransl (vec)
  (%matrix-translate vec (matid)))

(declaim (inline mattransl*))
(defun mattransl* (vec src)
  (%matrix-translate vec src))

(declaim (ftype (function (vec mat) mat) %matrix-scale))
(defun %matrix-scale (vec src)
  "Apply a scale transformation to a matrix"
  (matid* src)
  (%with-matrix (m src)
    (psetf m00 (vx vec)
           m11 (vy vec)
           m22 (vz vec)))
  src)

(declaim (inline matscale))
(defun matscale (vec)
  (%matrix-scale vec (matid)))

(declaim (inline matscale*))
(defun matscale* (vec src)
  (%matrix-scale vec src))

(declaim (ftype (function (mat double-float vec) mat) %matrix-rotate-around))
(defun %matrix-rotate-around (src angle axis)
  "Rotate a transformation matrix around an axis by the given angle"
  (let* ((axis (vnorm axis))
         (c (cos angle))
         (1-c (- 1.0 c))
         (s (sin angle))
         (xs (* (vx axis) s))
         (ys (* (vy axis) s))
         (zs (* (vz axis) s))
         (xx (* (vx axis) (vx axis)))
         (xy (* (vx axis) (vy axis)))
         (xz (* (vx axis) (vz axis)))
         (yy (* (vy axis) (vy axis)))
         (yz (* (vy axis) (vz axis)))
         (zz (* (vz axis) (vz axis))))
    (%with-matrix (m src)
      (psetf m00 (float (+ (* xx 1-c) c) 1.0)
             m10 (float (+ (* xy 1-c) zs) 1.0)
             m20 (float (- (* xz 1-c) ys) 1.0)
             m30 0.0
             m01 (float (- (* xy 1-c) zs) 1.0)
             m11 (float (+ (* yy 1-c) c) 1.0)
             m21 (float (+ (* yz 1-c) xs) 1.0)
             m31 0.0
             m02 (float (+ (* xz 1-c) ys) 1.0)
             m12 (float (- (* yz 1-c) xs) 1.0)
             m22 (float (+ (* zz 1-c) c) 1.0)
             m32 0.0
             m03 0.0
             m13 0.0
             m23 0.0
             m33 1.0))
    (matstab* src)))

(declaim (inline matrota))
(defun matrota (angle axis)
  (%matrix-rotate-around (matid) angle axis))

(declaim (inline matrota*))
(defun matrota* (src angle axis)
  (%matrix-rotate-around src angle axis))

(declaim (ftype (function (vec mat) vec) %matrix-get-translation))
(defun %matrix-get-translation (vec src)
  "Put the translation column of a matrix into a vector"
  (%with-matrix (m src)
    (psetf (vx vec) m03
           (vy vec) m13
           (vz vec) m23))
  vec)

(declaim (inline matgettransl))
(defun matgettransl (src)
  (%matrix-get-translation (vec) src))

(declaim (inline matgettransl*))
(defun matgettransl* (vec src)
  (%matrix-get-translation vec src))

(declaim (ftype (function (mat mat) mat) %matrix-copy-rotation))
(defun %matrix-copy-rotation (src dest)
  "Copy the rotation transformation of a matrix"
  (%with-matrices ((s src) (d dest))
    (psetf d00 s00 d01 s01 d02 s02
           d10 s10 d11 s11 d12 s12
           d20 s20 d21 s21 d22 s22))
  dest)

(declaim (inline matcprot))
(defun matcprot (src)
  (%matrix-copy-rotation src (matid)))

(declaim (inline matcprot*))
(defun matcprot* (src dest)
  (%matrix-copy-rotation src dest))

(declaim (ftype (function (mat vec vec) vec) %matrix-apply))
(defun %matrix-apply (src point dest)
  "Multiply a transformation matrix by a point"
  (%with-matrix (m src)
    (psetf (vx dest) (+ (* m00 (vx point))
                        (* m01 (vy point))
                        (* m02 (vz point))
                        (* m03 1.0))
           (vy dest) (+ (* m10 (vx point))
                        (* m11 (vy point))
                        (* m12 (vz point))
                        (* m13 1.0))
           (vz dest) (+ (* m20 (vx point))
                        (* m21 (vy point))
                        (* m22 (vz point))
                        (* m23 1.0))))
  dest)

(declaim (inline matapply))
(defun matapply (src point)
  (%matrix-apply src point (vec)))

(declaim (inline matapply*))
(defun matapply* (src point dest)
  (%matrix-apply src point dest))

(defun %matrix-test ()
  "Time the result of multiplying 1 million matrices"
  (time
    (let ((m (matid)))
      (loop repeat 1000000
            do (matmult* m m m)))))
