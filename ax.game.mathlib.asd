(defsystem #:ax.game.mathlib
  :name "Game Math Library"
  :description "A generic math library suitable for games."
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :depends-on (#:alexandria)
  :pathname "mathlib"
  :serial t
  :components ((:file "package")
               (:file "vector")
               (:file "matrix")
               (:file "hexagon")
               (:file "util")))
