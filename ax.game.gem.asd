(defsystem #:ax.game.gem
  :name "Game Entity Manager"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "An entity manager for use in game development."
  :depends-on (#:alexandria
               #:ax.misc.fs)
  :pathname "gem"
  :serial t
  :components ((:file "package")
               (:file "entity")))
