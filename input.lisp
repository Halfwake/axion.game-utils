(in-package :axion.game-utils)

(defmethod key-down :after (scancode repeat-p &key (debugp nil))
  (when (and (not repeat-p) debugp)
    (format t "Key pressed: ~a~%" scancode)))

(defmethod key-up :after (scancode repeat-p &key (debugp nil))
  (when (and (not repeat-p) debugp)
    (format t "Key released: ~a~%" scancode)))

(defmethod mouse-down :after (button coords &key (debugp nil))
  (when debugp
    (format t "Mouse button pressed: ~a at ~a~%" button coords)))

(defmethod mouse-up :after (button coords &key (debugp nil))
  (when debugp
    (format t "Mouse button released: ~a at ~a~%" button coords)))
