(defpackage #:axion.game-utils
  (:use :cl :alexandria :split-sequence)
  (:export *system*
           :get-path
           :read-data
           :load-texture
           :image->texture
           :ax-vector
           :vx
           :vy
           :vz
           :make-vector
           :vector-copy-*
           :vector-copy
           :vector-clear
           :vector-modify
           :vector->list
           :vector-negate-*
           :vector-negate
           :vector-add-*
           :vector-add
           :vector-subtract-*
           :vector-subtract
           :vector-multiply-*
           :vector-multiply
           :vector-scale-*
           :vector-scale
           :vector-length
           :vector-normalize-*
           :vector-normalize
           :vector-cross-*
           :vector-cross
           :vector-dot
           :vector-distance
           :ax-matrix
           :make-matrix
           :matrix-copy-*
           :matrix-copy
           :matrix-identity-*
           :matrix-identity
           :matrix-multiply-*
           :matrix-multiply
           :matrix-translate-*
           :matrix-translate
           :matrix-get-translation
           :matrix-apply-*
           :matrix-apply
           :matrix-copy-rotation-*
           :matrix-copy-rotation
           :matrix-rotate-*
           :matrix-rotate
           :matrix-stabilize-*
           :matrix-stabilize
           :matrix-convert-to-opengl-*
           :matrix-convert-to-opengl
           :matrix-convert-from-opengl-*
           :matrix-convert-from-opengl
           :load-obj
           :gl-get-id
           :gl-bind-texture
           :gl-send-matrix
           :gl-draw-arrays))
