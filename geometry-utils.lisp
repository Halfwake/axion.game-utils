(in-package :axion.game-utils)

(defun move-by (vec mat &optional movingp)
  (matrix-multiply-* (matrix-translate vec) mat mat)
  (unless movingp
    (vector-clear vec)))

(defun rotate-by (vec mat &optional rotatingp)
  (matrix-rotate-* vec mat mat)
  (unless rotatingp
    (vector-clear vec)))

(defun point-line-distance (line point)
  "Calculate the shortest distance between a line and a point"
  (let* ((start (aref line 0))
         (end (aref line 1))
         (line-length (vector-subtract end start))
         (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line-length v))
       (vector-length line-length))))
