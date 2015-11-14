;;;; math-vector.lisp
;;;; tools to use and manipulate geometric vectors.

(in-package :agl)

(declaim (optimize (debug 3) (space 0) (speed 3)))

(defvar *tolerance* 1e-7)

(deftype vec () '(simple-array single-float (3)))
(defstruct (vec
             (:type (vector single-float))
             (:constructor %vec (&optional x y z))
             (:copier vcp)
             (:conc-name v))
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float))

(declaim (ftype (function (&optional number number number) vec) vec))
(defun vec (&optional (x 0) (y 0) (z 0))
  (%vec (float x 1.0) (float y 1.0) (float z 1.0)))

(defmacro %with-vector ((prefix vec) &body body)
  (let ((*package* (find-package :agl)))
    `(with-accessors ((,(symbolicate prefix "X") vx)
                      (,(symbolicate prefix "Y") vy)
                      (,(symbolicate prefix "Z") vz))
       ,vec
       ,@body)))

(defmacro %with-vectors (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(%with-vector
       ,(car binds)
       (%with-vectors ,(cdr binds) ,@body))))

(declaim (ftype (function (vec &key (:tolerance single-float)) vec) vstab!))
(defun vstab! (source &key (tolerance *tolerance*))
  "Adjust any of `SOURCE` vector's values to zero if below `TOLERANCE`.
Destructively modifies `SOURCE`."
  (%with-vector (s source)
    (macrolet ((stabilize (place)
                 `(when (< (abs ,place) tolerance)
                    (setf ,place 0.0))))
      (stabilize sx)
      (stabilize sy)
      (stabilize sz)))
  source)

(declaim (ftype (function (vec &key (:tolerance single-float)) vec) vstab))
(defun vstab (source &key (tolerance *tolerance*))
  "Adjust any of `SOURCE` vector's values to zero if below `TOLERANCE`.
Returns a copy of `SOURCE`."
  (vstab! (vcp source) :tolerance tolerance))

(declaim (ftype (function (vec vec) vec) vcp!))
(defun vcp! (source dest)
  "Copy `SOURCE` vector's values into `DEST`.
Destructively modifies `DEST`."
  (%with-vectors ((s source) (d dest))
    (psetf dx sx
           dy sy
           dz sz))
  dest)

(declaim (ftype (function (vec) vec) vclr!))
(defun vclr! (source)
  "Reset all of `SOURCE` vector's components to zero.
Destructively modifies `SOURCE`."
  (%with-vector (s source)
    (psetf sx 0.0
           sy 0.0
           sz 0.0))
  source)

(declaim (ftype (function (vec) list) vlist))
(defun vlist (source)
  "Create a list of `SOURCE` vector's components."
  (%with-vector (s source)
    (list sx sy sz)))

(declaim (ftype (function (vec vec vec) vec) v+!))
(defun v+! (source1 source2 dest)
  "Add `SOURCE1` to `SOURCE2`, storing the result into `DEST`.
Destructively modifies `DEST`."
  (%with-vectors ((s1 source1) (s2 source2) (d dest))
    (psetf dx (+ s1x s2x)
           dy (+ s1y s2y)
           dz (+ s1z s2z)))
  dest)

(declaim (ftype (function (vec vec) vec) v+))
(defun v+ (source1 source2)
  "Add `SOURCE1` to `SOURCE`, storing the result into a new vector."
  (v+! source1 source2 (vec)))

(declaim (ftype (function (vec vec vec) vec) v-!))
(defun v-! (source1 source2 dest)
  "Subtract `SOURCE2` from `SOURCE1`, storing the result into `DEST`.
Destructively modifies `DEST`."
  (%with-vectors ((s1 source1) (s2 source2) (d dest))
    (psetf dx (- s1x s2x)
           dy (- s1y s2y)
           dz (- s1z s2z)))
  dest)

(declaim (ftype (function (vec vec) vec) v-))
(defun v- (source1 source2)
  "Subtract `SOURCE2` from `SOURCE1`, storing the result into a new vector."
  (v-! source1 source2 (vec)))

(declaim (ftype (function (vec vec vec) vec) v*!))
(defun v*! (source1 source2 dest)
  "Multiply `SOURCE1` by `SOURCE2`, storing the result into `DEST`.
Destructively modifies `DEST`."
  (%with-vectors ((s1 source1) (s2 source2) (d dest))
    (psetf dx (* s1x s2x)
           dy (* s1y s2y)
           dz (* s1z s2z)))
  dest)

(declaim (ftype (function (vec vec) vec) v*))
(defun v* (source1 source2)
  "Multiply `SOURCE1` by `SOURCE2`, storing the result into a new vector."
  (v*! source1 source2 (vec)))

(declaim (ftype (function (vec single-float) vec) vscale!))
(defun vscale! (source scalar)
  "Scale vector `SOURCE` by the factor `SCALAR`.
Destructively modifies `SOURCE`."
  (%with-vector (s source)
    (psetf sx (* sx scalar)
           sy (* sy scalar)
           sz (* sz scalar)))
  source)

(declaim (ftype (function (vec single-float) vec) vscale))
(defun vscale (source scalar)
  "Scale vector `SOURCE` by the factor `SCALAR`.
Returns a copy of `SOURCE`."
  (vscale! (vcp source) scalar))

(declaim (ftype (function (vec) single-float) vlen))
(defun vlen (source)
  "Compute the length of vector `SOURCE`."
  (%with-vector (s source)
    (sqrt (+ (* sx sx)
             (* sy sy)
             (* sz sz)))))

(declaim (ftype (function (vec) vec) vnorm!))
(defun vnorm! (source)
  "Normalize the vector `SOURCE`.
Destructively modifies `SOURCE`."
  (let ((magnitude (vlen source)))
    (unless (zerop magnitude)
      (%with-vector (s source)
        (psetf sx (/ sx magnitude)
               sy (/ sy magnitude)
               sz (/ sz magnitude))))
    source))

(declaim (ftype (function (vec) vec) vnorm))
(defun vnorm (source)
  "Normalize the vector `SOURCE`.
Returns a copy of `SOURCE`."
  (vnorm! (vcp source)))

(declaim (ftype (function (vec) vec) vround!))
(defun vround! (source)
  "Round all of `SOURCE` vector's components to the nearest whole number.
Destructively modifies `SOURCE`."
  (%with-vector (s source)
    (psetf sx (float (floor (+ sx 1/2)) 1.0)
           sy (float (floor (+ sy 1/2)) 1.0)
           sz (float (floor (+ sz 1/2)) 1.0)))
  source)

(declaim (ftype (function (vec) vec) vround))
(defun vround (source)
  "Round all of `SOURCE` vector's components to the nearest whole number.
Returns a copy of `SOURCE`."
  (vround! (vcp source)))

(declaim (ftype (function (vec) vec) vpos!))
(defun vpos! (source)
  "Convert each of `SOURCE` vector's components to be positive.
Destructively modifies `SOURCE`."
  (%with-vector (s source)
    (psetf sx (abs sx)
           sy (abs sy)
           sz (abs sz)))
  source)

(declaim (ftype (function (vec) vec) vpos))
(defun vpos (source)
  "Convert each of `SOURCE` vector's components to be positive.
Returns a copy of `SOURCE`."
  (vpos! (vcp source)))

(declaim (ftype (function (vec) vec) vneg!))
(defun vneg! (source)
  "Negate each of `SOURCE` vector's components.
Destructively modifies `SOURCE`."
  (vscale! source -1.0))

(declaim (ftype (function (vec) vec) vneg))
(defun vneg (source)
  "Negate each of `SOURCE` vector's components.
Returns a copy of `SOURCE`."
  (vneg! (vcp source)))

(declaim (ftype (function (vec vec vec) vec) vcross!))
(defun vcross! (source1 source2 dest)
  "Calculate the cross-product of `SOURCE1` and `SOURCE2`, storing the result into `DEST`.
Destructively modifies `DEST`."
  (%with-vectors ((s1 source1) (s2 source2) (d dest))
    (psetf dx (- (* s1y s2z) (* s1z s2y))
           dy (- (* s1z s2x) (* s1x s2z))
           dz (- (* s1x s2y) (* s1y s2x))))
  dest)

(declaim (ftype (function (vec vec) vec) vcross))
(defun vcross (source1 source2)
  "Calculate the cross-product of `SOURCE1` and `SOURCE2`, storing the result into a new vector."
  (vcross! source1 source2 (vec)))

(declaim (ftype (function (vec vec) single-float) vdot))
(defun vdot (source1 source2)
  "Calculate the dot-product of `SOURCE1` and `SOURCE2`."
  (%with-vectors ((s1 source1) (s2 source2))
    (+ (* s1x s2x)
       (* s1y s2y)
       (* s1z s2z))))

(declaim (ftype (function (vec vec vec) single-float) vbox))
(defun vbox (source1 source2 source3)
  "Calculate the box-product of `SOURCE1`, `SOURCE2` and `SOURCE3`."
  (vdot (vcross source1 source2) source3))

(declaim (ftype (function (vec vec) single-float) vdist))
(defun vdist (source1 source2)
  "Calculate the distance between points `SOURCE1` and `SOURCE2`."
  (%with-vectors ((s1 source1) (s2 source2))
    (sqrt (+ (expt (- s2x s1x) 2)
             (expt (- s2y s1y) 2)
             (expt (- s2z s1z) 2)))))

(declaim (ftype (function (vec vec single-float &optional boolean) vec) vtrans!))
(defun vtrans! (source direction distance &optional normalizep)
  "Translate vector `SOURCE` along `DIRECTION` by `DISTANCE`.
Destructively modifies `SOURCE`."
  (when normalizep
    (vnorm! direction))
  (v+! source (vscale! direction distance) source))

(declaim (ftype (function (vec vec single-float &optional boolean) vec) vtrans))
(defun vtrans (source direction distance &optional normalizep)
  "Translate vector `SOURCE` along `DIRECTION` by `DISTANCE`."
  (vtrans! (vcp source) direction distance normalizep))

(declaim (ftype (function (vec) boolean) vzerop))
(defun vzerop (source)
  "Test whether all of `SOURCE` vector's components are zero."
  (%with-vector (s (vstab source))
    (and (zerop sx)
         (zerop sy)
         (zerop sz))))

(declaim (ftype (function (vec vec &key (:tolerance single-float)) boolean) vclosep))
(defun vclosep (source1 source2 &key (tolerance *tolerance*))
  "Test whether the distance between points `SOURCE1` and `SOURCE2` are less than `TOLERANCE`."
  (< (vdist source1 source2) tolerance))

(declaim (ftype (function (vec vec) boolean) vdirp))
(defun vdirp (source1 source2)
  "Test whether vectors `SOURCE1` and `SOURCE2` are pointing in the same direction."
  (and (not (or (vzerop source1) (vzerop source2)))
       (vclosep (vnorm! source1) (vnorm! source2))))

(declaim (ftype (function (vec vec) boolean) vparp))
(defun vparp (source1 source2)
  "Test whether vectors `SOURCE1` and `SOURCE2` are parallel to each other."
  (or (vdirp source1 source2)
      (vdirp source1 (vneg source2))))

(declaim (ftype (function (vec) boolean) vplusp))
(defun vplusp (source)
  "Test whether all of `SOURCE` vector's components are positive."
  (%with-vector (s source)
    (or (plusp sx)
        (plusp sy)
        (plusp sz))))

(declaim (ftype (function (vec) boolean) vminusp))
(defun vminusp (source)
  "Test whether all of `SOURCE` vector's components are negative."
  (%with-vector (s source)
    (or (minusp sx)
        (minusp sy)
        (minusp sz))))

(defun %vector-test ()
  (time
    (loop with v = (vec)
          repeat 1000000
          do (v*! v v v))))
