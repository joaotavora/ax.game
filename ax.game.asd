(defsystem #:ax.game
  :name "Axion's Game Library"
  :description "A utility library for game development."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-heap
               #:ax.misc.fs)
  :serial t
  :components ((:file "package")
               (:file "math-vector")
               (:file "math-matrix")
               (:file "math-util")
               (:file "grid")
               (:file "grid-hex")
               (:file "grid-square")
               (:file "path")
               (:file "path-astar")
               (:file "entity")
               (:file "entity-manager")))
