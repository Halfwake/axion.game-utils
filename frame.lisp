(in-package :axion.game-utils)

(defclass frame ()
  ((init :accessor init
          :initform (running-time))
   (now :accessor now
        :initform (running-time))
   (before :accessor before
           :initform (running-time))
   (dt :accessor dt
       :initform 1/60)
   (frame-time :accessor frame-time
               :initform 0)
   (accumulator :accessor accumulator
                :initform 0)
   (units-per-sec :reader units-per-sec
                  :initform internal-time-units-per-second)
   (frames :accessor frames
           :initform 0)
   (interval :reader interval
             :initarg :interval
             :initform 5.0)))

(defgeneric step-frame (frame &optional debugp))

(defun running-time ()
  (get-internal-real-time))

(defmethod step-frame ((frame frame) &optional (debugp nil))
  (with-slots (init
               now
               before
               dt
               frame-time
               accumulator
               units-per-sec
               update-hz
               frames
               interval) frame
    (setf now (running-time)
          frame-time (/ (- now before) units-per-sec))
    (incf accumulator frame-time)
    (setf before now)
    (incf frames)
    (let ((seconds (/ (- now init) units-per-sec)))
      (when (and debugp (> seconds interval))
        (format t "FPS: ~,2f~%" (/ frames (float interval)))
        (setf frames 0
              init (running-time))))))
