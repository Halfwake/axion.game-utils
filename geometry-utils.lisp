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
  "Calculate radians from a rotation vector in RPMs"
  (flet ((radians (rpms)
           (if (zerop rpms)
             0.0
             (float (* rpms (/ (/ (* 2 pi) 60) hz)) 1.0))))
    (apply #'make-vector (map 'list #'radians rotation))))

(defun line-midpoint (start end)
  "Calculate the mid point of a line segment"
  (make-vector (/ (+ (vx start) (vx end)) 2)
               (/ (+ (vy start) (vy end)) 2)
               (/ (+ (vz start) (vz end)) 2)))

(defun point-line-distance (line point)
  "Calculate the shortest distance between a line and a point"
  (let* ((start (aref line 0))
         (end (aref line 1))
         (line-length (vector-subtract end start))
         (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line-length v))
       (vector-length line-length))))
