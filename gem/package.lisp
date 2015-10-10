(in-package :cl-user)

(defpackage #:ax.game.gem
  (:use #:cl
        #:alexandria
        #:ax.misc.fs)
  (:nicknames #:gem)
  (:export #:entity
           #:value
           #:component
           #:make-entity
           #:load-prototypes))

(in-package :gem)
