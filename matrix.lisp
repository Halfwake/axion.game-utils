(in-package :axion.game-utils)

(declaim (optimize (space 0) (speed 3)))

(deftype ax-matrix () '(simple-array single-float (16)))
(defstruct (ax-matrix
             (:type (vector single-float))
             (:constructor make-matrix (&optional m00 m01 m02 m03
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

(defmacro with-matrix ((prefix matrix) &body body)
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
     ,matrix
     ,@body))

(defmacro with-matrices (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(with-matrix ,(car binds)
       (with-matrices ,(cdr binds) ,@body))))

(set-pprint-dispatch
  'ax-matrix
  #'(lambda (stream pobj)
      #+sbcl
      (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
      (with-matrix (m pobj)
        (print-unreadable-object (pobj stream)
          (format
            stream "~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a"
            m00 m01 m02 m03
            m10 m11 m12 m13
            m20 m21 m22 m23
            m30 m31 m32 m33)))))

(defun matrix-test ()
  "Time the result of multiplying 1 million matrices"
  (time
    (let ((m (matrix-identity)))
      (loop repeat 1000000
            do (matrix-multiply-* m m m)))))

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix) matrix-copy-*))
(defun matrix-copy-* (src dest)
  "Copy a matrix to an existing matrix"
  (with-matrices ((s src) (d dest))
    (psetf d00 s00 d01 s01 d02 s02 d03 s03
           d10 s10 d11 s11 d12 s12 d13 s13
           d20 s20 d21 s21 d22 s22 d23 s23
           d30 s30 d31 s31 d32 s32 d33 s33))
  dest)

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-copy))
(defun matrix-copy (src)
  "Copy a matrix to a new matrix"
  (matrix-copy-* src (make-matrix)))

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-identity-*))
(defun matrix-identity-* (src)
  "Set a matrix to the identity matrix"
  (with-matrix (m src)
    (psetf m00 1.0 m01 0.0 m02 0.0 m03 0.0
           m10 0.0 m11 1.0 m12 0.0 m13 0.0
           m20 0.0 m21 0.0 m22 1.0 m23 0.0
           m30 0.0 m31 0.0 m32 0.0 m33 1.0))
  src)

(declaim (ftype (function () ax-matrix) matrix-identity))
(defun matrix-identity ()
  "Create a new identity matrix"
  (matrix-identity-* (make-matrix)))

(declaim (ftype (function (ax-matrix &key (:tolerance single-float)) ax-matrix)
                matrix-stabilize-*))
(defun matrix-stabilize-* (src &key (tolerance *tolerance*))
  "Force each matrix element to 0 if below the tolerance level"
  (with-matrix (m src)
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

(declaim (ftype (function (ax-matrix &key (:tolerance single-float)) ax-matrix)
                matrix-stabilize))
(defun matrix-stabilize (src &key (tolerance *tolerance*))
  "Force each matrix element to 0 if below the tolerance level as a new matrix"
  (matrix-stabilize-* (matrix-copy src) :tolerance tolerance))

(declaim (ftype (function (ax-matrix ax-matrix ax-matrix) ax-matrix)
                matrix-multiply-*))
(defun matrix-multiply-* (src1 src2 dest)
  "Store the product of two matrices in an existing matrix"
  (with-matrices ((a src1) (b src2) (d dest))
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

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix) matrix-multiply))
(defun matrix-multiply (src1 src2)
  "Store the product of two matrices in a new matrix"
  (matrix-multiply-* src1 src2 (make-matrix)))

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix) matrix-transpose-*))
(defun matrix-transpose-* (src dest)
  "Convert all rows into columns, and all columns into rows"
  (unless (eq src dest)
    (matrix-copy-* src dest))
  (with-matrix (m dest)
    (rotatef m10 m01)
    (rotatef m20 m02)
    (rotatef m30 m03)
    (rotatef m21 m12)
    (rotatef m31 m13)
    (rotatef m32 m23))
  dest)

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-transpose))
(defun matrix-transpose (src)
  "Convert all rows into columns, and all columns into rows, as a new matrix"
  (matrix-transpose-* src (make-matrix)))

(declaim (ftype (function (ax-vector ax-matrix ax-matrix) ax-matrix)
                matrix-rotate-*))
(defun matrix-rotate-* (vec src dest)
  "Apply a rotation transformation to a matrix"
  (let ((rotation (matrix-identity)))
    (macrolet ((rot (axis s c &body body)
                 `(let ((,s (sin ,axis))
                        (,c (cos ,axis)))
                    ,@body
                    (matrix-multiply-* src rotation dest)
                    (matrix-copy-rotation-* dest src))))
      (with-matrix (m rotation)
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
  (matrix-stabilize-* src))

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix) matrix-rotate))
(defun matrix-rotate (vec src)
  "Apply a rotation transformation to a matrix as a new matrix"
  (matrix-rotate-* vec src (matrix-identity)))

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix) matrix-translate-*))
(defun matrix-translate-* (vec src)
  "Apply a translation transformation to a matrix"
  (with-matrix (m src)
    (psetf m03 (vx vec)
           m13 (vy vec)
           m23 (vz vec)))
  src)

(declaim (ftype (function (ax-vector) ax-matrix) matrix-translate))
(defun matrix-translate (vec)
  "Apply a translation transformation to a matrix as a new matrix"
  (matrix-translate-* vec (matrix-identity)))

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix) matrix-scale-*))
(defun matrix-scale-* (vec src)
  "Apply a scale transformation to a matrix"
  (matrix-identity-* src)
  (with-matrix (m src)
    (psetf m00 (vx vec)
           m11 (vy vec)
           m22 (vz vec)))
  src)

(declaim (ftype (function (ax-vector) ax-matrix) matrix-scale))
(defun matrix-scale (vec)
  "Apply a scale transformation to a matrix as a new matrix"
  (matrix-scale-* vec (matrix-identity)))

(declaim (ftype (function (ax-matrix double-float ax-vector) ax-matrix)
                matrix-rotate-around-*))
(defun matrix-rotate-around-* (src angle axis)
  "Rotate a transformation matrix around an axis by the given angle"
  (let* ((axis (vector-normalize axis))
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
    (with-matrix (m src)
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
    (matrix-stabilize-* src)))

(declaim (ftype (function (double-float ax-vector) ax-matrix)
                matrix-rotate-around))
(defun matrix-rotate-around (angle axis)
  "Rotate a transformation matrix around an axis by the given angle, as
   a new matrix."
  (matrix-rotate-around-* (matrix-identity) angle axis))

(declaim (ftype (function (ax-vector ax-matrix) ax-vector)
                matrix-get-translation-*))
(defun matrix-get-translation-* (vec src)
  "Put the translation column of a matrix into the given vector"
  (with-matrix (m src)
    (psetf (vx vec) m03
           (vy vec) m13
           (vz vec) m23))
  vec)

(declaim (ftype (function (ax-matrix) ax-vector) matrix-get-translation))
(defun matrix-get-translation (src)
  "Put the translation column of a matrix into a new vector"
  (matrix-get-translation-* (make-vector) src))

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix)
                matrix-copy-rotation-*))
(defun matrix-copy-rotation-* (src dest)
  "Copy the rotation transformation of a matrix"
  (with-matrices ((s src) (d dest))
    (psetf d00 s00 d01 s01 d02 s02
           d10 s10 d11 s11 d12 s12
           d20 s20 d21 s21 d22 s22))
  dest)

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-copy-rotation))
(defun matrix-copy-rotation (src)
  "Copy the rotation transformation to a new matrix"
  (matrix-copy-rotation-* src (matrix-identity)))

(declaim (ftype (function (ax-matrix ax-vector ax-vector) ax-vector)
                matrix-apply-*))
(defun matrix-apply-* (src point dest)
  "Multiply a transformation matrix by a point"
  (with-matrix (m src)
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

(declaim (ftype (function (ax-matrix ax-vector) ax-vector) matrix-apply))
(defun matrix-apply (src point)
  "Multiply a transformation matrix by a point as a new vector"
  (matrix-apply-* src point (make-vector)))

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix)
                matrix-convert-to-opengl-*))
(declaim (inline matrix-convert-to-opengl-*))
(defun matrix-convert-to-opengl-* (src dest)
  "Convert a matrix into a matrix suitable for OpenGL"
  (declare ((simple-array single-float (16)) dest))
  (with-matrix (m src)
    (psetf (aref dest 0) m00
           (aref dest 1) m10
           (aref dest 2) m20
           (aref dest 3) m30
           (aref dest 4) m01
           (aref dest 5) m11
           (aref dest 6) m21
           (aref dest 7) m31
           (aref dest 8) m02
           (aref dest 9) m12
           (aref dest 10) m22
           (aref dest 11) m32
           (aref dest 12) m03
           (aref dest 13) m13
           (aref dest 14) m23
           (aref dest 15) m33)
    dest))

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-convert-to-opengl))
(defun matrix-convert-to-opengl (src)
  "Convert a matrix to a new matrix suitable for OpenGL"
  (let ((dest (make-array 16
                          :element-type 'single-float
                          :initial-element 0.0)))
    (declare ((simple-array single-float (16)) dest))
    (matrix-convert-to-opengl-* src dest)
    dest))

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix)
                matrix-convert-from-opengl-*))
(declaim (inline matrix-convert-from-opengl-*))
(defun matrix-convert-from-opengl-* (src dest)
  "Convert a matrix in OpenGL format"
  (declare ((simple-array single-float (16)) dest))
  (with-matrix (m dest)
    (psetf m00 (aref src 0)
           m10 (aref src 1)
           m20 (aref src 2)
           m30 (aref src 3)
           m01 (aref src 4)
           m11 (aref src 5)
           m21 (aref src 6)
           m31 (aref src 7)
           m02 (aref src 8)
           m12 (aref src 9)
           m22 (aref src 10)
           m32 (aref src 11)
           m03 (aref src 12)
           m13 (aref src 13)
           m23 (aref src 14)
           m33 (aref src 15))
    dest))

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-convert-from-opengl))
(defun matrix-convert-from-opengl (src)
  "Convert a matrix in OpenGL format to a new matrix"
  (declare ((simple-array single-float (16)) src))
  (matrix-convert-from-opengl-* src (make-matrix)))
