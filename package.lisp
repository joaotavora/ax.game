(in-package :cl-user)

(defpackage #:ax.game
  (:use #:cl
        #:alexandria
        #:cl-heap
        #:sdl2.kit
        #:ax.misc.fs)
  (:nicknames #:agl)

  ;; math
  (:export #:vec
           #:vx
           #:vy
           #:vz
           #:vcp
           #:vcp!
           #:vstab!
           #:vstab
           #:vclr!
           #:vlist
           #:v+!
           #:v+
           #:v-!
           #:v-
           #:v*!
           #:v*
           #:vscale*
           #:vscale
           #:vlen
           #:vnorm!
           #:vnorm
           #:vround!
           #:vround
           #:vpos!
           #:vpos
           #:vneg!
           #:vneg
           #:vcross!
           #:vcross
           #:vdot
           #:vbox
           #:vdist
           #:vtrans!
           #:vtrans
           #:vzerop
           #:vclosep
           #:vdirp
           #:vparp
           #:vplusp
           #:vminusp
           #:mcp
           #:mstab!
           #:mstab
           #:mid!
           #:mid
           #:m*!
           #:m*
           #:mcprot!
           #:mcprot
           #:mrot!
           #:mrot
           #:mtrans!
           #:mtrans
           #:mrota!
           #:mrota
           #:mgettrans!
           #:mgettrans
           #:mapply!
           #:mapply
           #:minvt!
           #:minvt
           #:mgetrot
           #:mview
           #:mpersp
           #:move-by
           #:rotate-by
           #:rpms->radians
           #:set-direction
           #:get-angle
           #:line-direction
           #:line-midpoint
           #:line-plane-intersect
           #:point-line-distance
           #:frustum-planes
           #:point-in-frustum-p)

  ;; grid
  (:export #:cardinal->direction
           #:possible-directions
           #:tile-distance
           #:tile-neighbor
           #:tile-neighbors
           #:tile-neighbors-p
           #:tile-directions
           #:tile-penalty
           #:hex
           #:cube->hex
           #:hex->cube
           #:square)

  ;; path
  (:export #:agent
           #:find-path
           #:a*)

  ;; entity
  (:export #:entity
           #:attrs
           #:attr
           #:component
           #:make-entity
           #:*prototype-path*
           #:prototype
           #:load-prototypes)

  ;; input
  (:export #:get-key-name
           #:key-down
           #:key-up
           #:mouse-coords
           #:mouse-down
           #:mouse-up
           #:mouse-move
           #:mouse-scroll))
