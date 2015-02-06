(in-package :axion.game-utils)

(defun move-by (vec mat &optional movingp)
  (matmult* (mattransl vec) mat mat)
  (unless movingp
    (vclr* vec)))

(defun rotate-by (vec mat &optional rotatingp)
  (matrot* vec mat mat)
  (unless rotatingp
    (vclr* vec)))

(defun rpms->radians (rotation dt)
  "Calculate radians per delta time as a vector from a sequence of RPMs"
  (flet ((radians (rpms)
           (* (/ (* rpms 2 pi) 60) dt)))
    (apply #'vec (map 'list #'radians rotation))))

(defun vector-floats (data)
  "Converts a sequence to a vector of floats"
  (apply #'vec (map 'list #'float data)))

(defun get-rotation (matrix &key axis)
  "Get the rotation vector associated with the given axis from a matrix, or
   return multiple values of each axis if no axis is given"
  (with-matrix (m matrix)
    (let ((x (vec m00 m10 m20))
          (y (vec m01 m11 m21))
          (z (vec m02 m12 m22)))
      (case axis
        ((:x) x)
        ((:y) y)
        ((:z) z)
        ((nil) (values x y z))))))

(defun set-direction (direction)
  "Create a vector rotated along the Z axis to be facing the given direction"
  (let ((start (/ (* pi 2) -12))
        (inc (/ (* pi 2) 6))
        (result (vec)))
    (loop with i = 0
          for d in '(:ne :e :se :sw :w :nw)
          for z = (+ start (* inc (- i)))
          do (incf i)
             (when (eq d direction)
               (setf (vz result) (float z 1.0))))
    result))

(defun get-angle (vec1 vec2)
  "Calculate the angle between two vectors"
  (let ((dot (vdot vec1 vec2))
        (len (* (vlen vec1) (vlen vec2))))
    (if (not (= len 0))
      (acos (clamp (/ dot len) -1 1))
      0.0)))

(defun line-direction (start end)
  "Gives the direction of the line segment represented by 2 points"
  (vnorm (vsub end start)))

(defun line-midpoint (start end)
  "Calculate the mid point of a line segment"
  (vtrans start
                    (line-direction start end)
                    (/ (vdist start end) 2)))

(defun line-plane-intersect (line-start line-end plane-point plane-normal)
  "Return the point that a line intersects with a plane"
  (let* ((direction (vsub line-start line-end))
         (dot-dir-plane (vdot direction plane-normal))
         (plane-line (vsub line-start plane-point)))
    (unless (zerop dot-dir-plane)
      (let ((p (/ (- (vdot plane-normal plane-line)) dot-dir-plane)))
        (vtrans line-start direction p)))))

(defun point-line-distance (start end point)
  "Calculate the shortest distance between a line and a point"
  (let* ((direction (line-direction start end))
         (intersect (line-plane-intersect start end point direction)))
    (vdist point intersect)))
