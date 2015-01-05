(in-package :axion.game-utils)

(defun point-line-distance (start end point)
  "Calculate the shortest distance between a line and a point"
  (let ((line (vector-subtract end start))
        (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line v))
       (vector-length line)))) 
