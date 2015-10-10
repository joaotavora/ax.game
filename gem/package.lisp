(in-package :cl-user)

(defpackage #:ax.game.gem
  (:use #:cl
        #:alexandria
        #:ax.misc.fs)
  (:nicknames #:gem)
  (:export #:entity
           #:attr
           #:component
           #:make-entity
           #:*prototype-path*
           #:prototype
           #:load-prototypes))

(in-package :gem)
