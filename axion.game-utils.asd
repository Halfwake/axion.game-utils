(defsystem #:axion.game-utils
  :name "axion.game-utils"
  :author "axion <axedcode@gmail.com>"
  :version "0.1"
  :license "GPLv3+"
  :description "Various utilities to aid with game development"
  :depends-on (:alexandria
               :split-sequence
               :png-read
               :cl-heap
               :cl-opengl
               :cl-glu
               :sdl2kit)
  :serial t
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
