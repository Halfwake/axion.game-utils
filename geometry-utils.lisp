(in-package :axion.game-utils)

(defun move-by (vec mat &optional (movingp nil))
  (matrix-multiply-* (matrix-translate vec) mat mat)
  (unless movingp
    (vector-clear vec)))

(defun rotate-by (vec mat &optional (rotatingp nil))
  (matrix-rotate-* vec mat mat)
  (unless rotatingp
    (vector-clear vec)))

(defun point-line-distance (start end point)
  "Calculate the shortest distance between a line and a point"
  (let ((line (vector-subtract end start))
        (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line v))
       (vector-length line)))) 
