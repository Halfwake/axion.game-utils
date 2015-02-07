(in-package :axion.game-utils)

(defgeneric key-down (scancode repeat-p &key))
(defgeneric key-up (scancode repeat-p &key))
(defgeneric mouse-down (button coords &key))
(defgeneric mouse-up (button coords &key))

(defun get-key-name (scancode)
  (let ((key (sdl2-ffi.functions:sdl-get-key-from-scancode scancode)))
    (sdl2-ffi.functions:sdl-get-key-name key)))

(defmethod key-down :after (scancode repeat-p &key (debugp nil))
  (when (and (not repeat-p) debugp)
    (format t "Key pressed: ~a~%" (get-key-name scancode))))

(defmethod key-up :after (scancode repeat-p &key (debugp nil))
  (when (and (not repeat-p) debugp)
    (format t "Key released: ~a~%" (get-key-name scancode))))

(defmethod mouse-down :after (button coords &key (debugp nil))
  (when debugp
    (format t "Mouse button pressed: ~a at ~a~%" button coords)))

(defmethod mouse-up :after (button coords &key (debugp nil))
  (when debugp
    (format t "Mouse button released: ~a at ~a~%" button coords)))
