(in-package :cl-user)

(defpackage #:ax.game.mathlib
  (:use #:cl
        #:alexandria)
  (:nicknames #:gml)

  ;; vector
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
           #:vrev!
           #:vrev
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
           #:vminusp)

  ;; matrix
  (:export #:mcp!
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
           #:mpersp)

  ;; hexagon
  (:export #:cube->hex
           #:hex->cube
           #:hex-distance
           #:hex-round
           #:hex-neighbor
           #:hex-neighbors
           #:hex-neighbors-p
           #:hex-directions)
  ;; util
  (:export #:move-by
           #:rotate-by
           #:rpms->radians
           #:set-direction
           #:get-angle
           #:line-direction
           #:line-midpoint
           #:line-plane-intersect
           #:point-line-distance
           #:frustum-planes
           #:point-in-frustum-p))

(in-package :gml)
