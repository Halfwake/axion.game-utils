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

(defun line-direction (start end)
  "Gives the direction of the line segment represented by 2 points"
  (vector-subtract end start))

(defun line-midpoint (start end)
  "Calculate the mid point of a line segment"
  (vector-translate start
                    (line-direction start end)
                    (/ (vector-distance start end) 2)))

(defun project-plane (vec plane-normal)
  "Project a vector onto the plane perpendicular to plane-normal"
  (let* ((plane-normal (vector-normalize plane-normal))
         (dot (vector-dot vec plane-normal))
         (scaled (vector-scale plane-normal dot)))
    (vector-subtract vec scaled)))

(defun line-plane-intersect (line-start line-end plane-point plane-normal)
  "Return the point that a line intersects with a plane"
  (let ((direction (vector-normalize (line-direction line-start line-end))))
    (unless (zerop (vector-dot direction plane-normal))
      (vector-normalize-* plane-normal)
      (let* ((w (vector-subtract line-start plane-point))
             (s (/ (- (vector-dot plane-normal w))
                   (vector-dot plane-normal direction))))
        (vector-translate line-start direction s)))))

(defun point-line-distance (line point)
  "Calculate the shortest distance between a line and a point"
  (let* ((start (aref line 0))
         (end (aref line 1))
         (line-length (vector-subtract end start))
         (v (vector-subtract point start)))
    (/ (vector-length (vector-cross line-length v))
       (vector-length line-length))))
