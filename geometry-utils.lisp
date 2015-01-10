(in-package :axion.game-utils)

(defun move-by (vec mat &optional movingp)
  (matrix-multiply-* (matrix-translate vec) mat mat)
  (unless movingp
    (vector-clear vec)))

(defun rotate-by (vec mat &optional rotatingp)
  (matrix-rotate-* vec mat mat)
  (unless rotatingp
    (vector-clear vec)))

(defun rpms->radians (rotation hz)
  (flet ((radians (rpms)
           (if (zerop rpms)
             0.0
             (float (* rpms (/ (/ (* 2 pi) 60) hz)) 1.0))))
    (apply #'make-vector (map 'list #'radians rotation))))

(defun point-line-distance (line point)
  "Calculate the shortest distance between a line and a point"
  (let* ((start (aref line 0))
         (end (aref line 1))
         (line-length (vector-subtract end start))
         (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line-length v))
       (vector-length line-length))))
