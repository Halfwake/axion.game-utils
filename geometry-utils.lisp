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

(defun get-rotation (matrix &key axis)
  "Get the rotation vector associated with the given axis, or return
   multiple values of each axis if no axis is given"
  (with-matrix (m matrix)
    (let ((x (make-vector m00 m10 m20))
          (y (make-vector m01 m11 m21))
          (z (make-vector m02 m12 m22)))
      (case axis
        ((:x) x)
        ((:y) y)
        ((:z) z)
        ((nil) (values x y z))))))

(defun set-direction (direction)
  "Create a vector rotated along the z axis to be facing the given direction"
  (let ((start (/ (* pi 2) -12))
        (inc (/ (* pi 2) 6)))
    (loop with i = 0
          for d in '(:ne :e :se :sw :w :nw)
          for z = (+ start (* inc (- i)))
          do (incf i)
             (when (eq d direction)
               (return (make-vector 0 0 z))))))

(defun get-angle (vec1 vec2)
  "Calculate the angle between two vectors"
  (let ((dot (vector-dot vec1 vec2))
        (len (* (vector-length vec1) (vector-length vec2))))
    (if (not (= len 0))
      (acos (clamp (/ dot len) -1 1))
      0.0)))

(defun line-direction (start end)
  "Gives the direction of the line segment represented by 2 points"
  (vector-normalize (vector-subtract end start)))

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
  (let ((direction (line-direction line-start line-end)))
    (unless (zerop (vector-dot direction plane-normal))
      (vector-normalize-* plane-normal)
      (let* ((w (vector-subtract line-start plane-point))
             (s (/ (- (vector-dot plane-normal w))
                   (vector-dot plane-normal direction))))
        (vector-translate line-start direction s)))))

(defun point-line-distance (start end point)
  "Calculate the shortest distance between a line and a point"
  (let* ((direction (line-direction start end))
         (intersect (line-plane-intersect start end point direction)))
    (vector-distance point intersect)))
