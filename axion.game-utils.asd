(defsystem axion.game-utils
  :name "axion.game-utils"
  :author "axion <axedcode@gmail.com>"
  :description "Various utilities to aid with game development"
  :serial t
  :depends-on (:alexandria :png-read :split-sequence)
  :components ((:file "packages")
               (:file "vector")
               (:file "matrix")
               (:file "obj-parser")))
