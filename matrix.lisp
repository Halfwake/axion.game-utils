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
  (with-matrices ((a src) (b dest))
    (psetf b00 a00 b01 a01 b02 a02 b03 a03
           b10 a10 b11 a11 b12 a12 b13 a13
           b20 a20 b21 a21 b22 a22 b23 a23
           b30 a30 b31 a31 b32 a32 b33 a33))
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

(declaim (ftype (function (ax-matrix ax-matrix ax-matrix) ax-matrix)
                matrix-multiply-*))
(defun matrix-multiply-* (src1 src2 dest)
  "Store the product of two matrices in an existing matrix"
  (with-matrices ((a src1) (b src2) (c dest))
    (psetf c00 (+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30))
           c10 (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30))
           c20 (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30))
           c30 (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30))
           c01 (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31))
           c11 (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31))
           c21 (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31))
           c31 (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31))
           c02 (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32))
           c12 (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32))
           c22 (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32))
           c32 (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32))
           c03 (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33))
           c13 (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33))
           c23 (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33))
           c33 (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33))))
  dest)

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix)
                matrix-multiply))
(defun matrix-multiply (src1 src2)
  "Store the product of two matrices in a new matrix"
  (matrix-multiply-* src1 src2 (make-matrix)))

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix)
                matrix-translate-*))
(defun matrix-translate-* (vec dest)
  "Add a translation vector to a matrix"
  (with-matrix (m dest)
    (psetf m03 (+ m03 (vx vec))
           m13 (+ m13 (vy vec))
           m23 (+ m23 (vz vec))))
  dest)

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix) matrix-translate))
(defun matrix-translate (vec dest)
  "Add as translation vector to a matrix as a new matrix"
  (matrix-translate-* vec (matrix-copy dest)))

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

(declaim (ftype (function (ax-matrix ax-vector ax-vector) ax-vector)
                matrix-apply-*))
(defun matrix-apply-* (basis point dest)
  "Multiply a basis matrix by a point vector stored in the given destination"
  (with-matrix (m basis)
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
(defun matrix-apply (basis point)
  "Multiply a basis matrix by a point vector stored in a new vector"
  (matrix-apply-* basis point (make-vector)))

(declaim (ftype (function (ax-matrix ax-matrix) ax-matrix)
                matrix-copy-rotation-*))
(defun matrix-copy-rotation-* (src dest)
  "Copy the rotation vectors from the source to the destination matrix"
  (with-matrices ((s src) (d dest))
    (psetf d00 s00 d01 s01 d02 s02
           d10 s10 d11 s11 d12 s12
           d20 s20 d21 s21 d22 s22))
  dest)

(declaim (ftype (function (ax-matrix) ax-matrix) matrix-copy-rotation))
(defun matrix-copy-rotation (src)
  "Copy the rotation vectors from the source to a new matrix"
  (matrix-copy-rotation-* src (matrix-identity)))

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix) matrix-rotate-*))
(defun matrix-rotate-* (vec src)
  "Rotate a matrix"
  (let ((dest (make-matrix))
        (rotation (make-matrix))
        (x (vx vec))
        (y (vy vec))
        (z (vz vec)))
    (with-matrix (m rotation)
      (matrix-identity-* rotation)
      (psetf m00 (cos z) m10 (sin z) m01 (- (sin z)) m11 (cos z))
      (matrix-multiply-* src rotation dest)
      (matrix-copy-rotation-* dest src)
      (psetf m00 1.0 m10 0.0 m20 0.0
             m01 0.0 m11 (cos x) m21 (sin x)
             m02 0.0 m12 (- (sin x)) m22 (cos x))
      (matrix-multiply-* src rotation dest)
      (matrix-copy-rotation-* dest src)
      (psetf m00 (cos y) m10 0.0 m20 (- (sin y))
             m01 0.0 m11 1.0 m21 0.0
             m02 (sin y) m12 0.0 m22 (cos y))
      (matrix-multiply-* src rotation dest)
      (matrix-copy-rotation-* dest src)))
  (matrix-stabilize-* src 1e-9))

(declaim (ftype (function (ax-vector ax-matrix) ax-matrix) matrix-rotate))
(defun matrix-rotate (vec src)
  "Rotate a matrix as a new matrix"
  (matrix-rotate-* vec (matrix-copy src)))

(declaim (ftype (function (ax-matrix single-float) ax-matrix)
                matrix-stabilize-*))
(defun matrix-stabilize-* (src tolerance)
  "Force each matrix element to 0 if below the tolerance level"
  (with-matrix (m src)
    (macrolet ((stabilize (place tol)
                 `(when (< (abs ,place) ,tol)
                    (setf ,place 0.0))))
      (stabilize m00 tolerance)
      (stabilize m01 tolerance)
      (stabilize m02 tolerance)
      (stabilize m03 tolerance)
      (stabilize m10 tolerance)
      (stabilize m11 tolerance)
      (stabilize m12 tolerance)
      (stabilize m13 tolerance)
      (stabilize m20 tolerance)
      (stabilize m21 tolerance)
      (stabilize m22 tolerance)
      (stabilize m23 tolerance)
      (stabilize m30 tolerance)
      (stabilize m31 tolerance)
      (stabilize m32 tolerance)
      (stabilize m33 tolerance)))
  src)

(declaim (ftype (function (ax-matrix single-float) ax-matrix)
                matrix-stabilize))
(defun matrix-stabilize (src tolerance)
  "Force each matrix element to 0 if below the tolerance level as a new matrix"
  (matrix-stabilize-* (matrix-copy src) tolerance))

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
