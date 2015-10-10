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
           #:*prototype-data*
           #:prototype
           #:load-prototypes))

(in-package :gem)
