(in-package :axion.game-utils)

(defun gl-get-id (item)
  (cffi:foreign-enum-value '%gl:enum item :errorp nil))

(defun gl-send-matrix (matrix)
  #+(or cmucl sbcl ccl)
  (if (typep matrix '(simple-array single-float (16)))
    (cffi-sys:with-pointer-to-vector-data (p matrix)
      (%gl:mult-transpose-matrix-f p))
    (gl:mult-transpose-matrix matrix))
  #-(or cmucl sbcl ccl)
  (gl:mult-transpose-matrix matrix))

(defun gl-draw-arrays (vao mode count)
  (gl:bind-vertex-array vao)
  (%gl:draw-arrays mode 0 count))
