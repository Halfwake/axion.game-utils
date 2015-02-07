(in-package :axion.game-utils)

(defun axial->cube (src)
  "Convert a 2-D vector representing axial coordinates to a 3-D vector
   representing cube coordinates"
  (%with-vector (s src)
    (vec sy (- (- sy) sx) sx)))

(defun cube->axial (src)
  "Convert a 3-D vector representing cube coordinates to a 2-D vector
   representing axial coordinates"
  (%with-vector (s src)
    (vec sz sx)))

(defun cube->hex (src)
  "Convert a 3-D vector representing cube coordinates to a 2-D vector
   representing hexagon offset coordinates"
  (%with-vector (s src)
    (let ((y (/ (- sz (mod sz 2)) 2)))
      (vec (+ sx y) sz))))

(defun hex->cube (src)
  "Convert a 2-D vector representing hexagon offset coordinates to a 3-D vector
   representing cube coordinates"
  (%with-vector (s src)
    (let* ((x (- sx (/ (- sy (mod sy 2)) 2)))
           (y (- (- x) sy)))
      (vec x y sy))))

(defun hex-distance (src dest)
  "Calculate the distance in tiles from a pair of vectors representing
   hexagon offset coordinates"
  (%with-vectors ((s (hex->cube src)) (d (hex->cube dest)))
    (let ((x (abs (- sx dx)))
          (y (abs (- sy dy)))
          (z (abs (- sz dz))))
      (max x y z))))

(defun hex-round (src)
  "Calculate the nearest hexagon from a 3-D vector respresenting cube
   coordinates"
  (let* ((dest (vround (hex->cube src)))
         (shift (vpos (vsub dest src))))
    (%with-vectors ((d dest) (s shift))
      (cond
        ((and (> sx sy) (> sx sz))
         (setf dx (- (- dy) dz)))
        ((> sy sz)
         (setf dy (- (- dx) dz)))
        (t (setf dz (- (- dx) dy)))))
    (cube->hex dest)))

(defun hex-neighbor (src direction)
  "Calculate a hexagon's neighbor given its coordinates and a directional
   vector. Directional vectors represent the cardinal coordinates of each
   edge of a hexagon oritented with its points facing up and down.
   Example: Hexagon 2,2 with direction -1,1 (northwest) would return the
   coordinates 1,1"
  (%with-vector (d direction)
    (let ((directions `#(,(vec 1 -1 0)
                         ,(vec 1 0 -1)
                         ,(vec 0 1 -1)
                         ,(vec -1 1 0)
                         ,(vec -1 0 1)
                         ,(vec 0 -1 1)))
           (index (mod (+ 6 (round (/ (* 6 (atan dy dx)) (* pi 2)))) 6)))
      (cube->hex (vadd (hex->cube src) (aref directions index))))))

(defun hex-neighbors (src map-size)
  "Return a list of vectors representing all neighbors of the given hexagon
   coordinates, excluding neighbors that fall outside of the given map
   dimensions.
   Note: Map dimensions are the size of the array, as given by
   ARRAY-DIMENSIONS, not the indices of the array which start at 0. This is
   why we do a VSUB* call below."
  (let ((directions `#(,(vec -1 -1)
                       ,(vec 1 -1)
                       ,(vec 1 0)
                       ,(vec 1 1)
                       ,(vec -1 1)
                       ,(vec -1 0))))
    (vsub* map-size (vec 1 1) map-size)
    (loop for direction across directions
          for neighbor = (hex-neighbor src direction)
          unless (or (vnegp neighbor)
                     (vnegp (vsub map-size neighbor)))
          collect neighbor)))

(defun hex-neighbors-p (src target)
  "Check if two hexagon coordinates are neighbors"
  (let ((directions `#(,(vec -1 -1)
                       ,(vec 1 -1)
                       ,(vec 1 0)
                       ,(vec 1 1)
                       ,(vec -1 1)
                       ,(vec -1 0))))
    (loop for direction across directions
          for neighbor = (hex-neighbor src direction)
          do (when (equalp target neighbor)
               (return t)))))
