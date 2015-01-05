(defsystem #:axion.game-utils
  :name "axion.game-utils"
  :author "axion <axedcode@gmail.com>"
  :description "Various utilities to aid with game development"
  :serial t
  :depends-on (:cl-opengl
               :cl-glu
               :sdl2kit
               :png-read
               :alexandria
               :split-sequence)
  :components ((:file "packages")
               (:file "utils")
               (:file "opengl")
               (:file "vector")
               (:file "matrix")
               (:file "geometry-utils")
               (:file "hexagon")
               (:file "obj-parser")
               (:file "input")
               (:file "frame")))
