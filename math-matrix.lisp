;;;; math-matrix.lisp
;;;; tools to use and manipulate matrices

(in-package :agl)

(declaim (optimize (debug 3) (space 0) (speed 3)))

(defvar *tolerance* 1e-7)

(deftype matrix () '(simple-array single-float (16)))
(defstruct (matrix
             (:type (vector single-float))
             (:copier mcp)
             (:constructor mat (&optional m00 m01 m02 m03
                                          m10 m11 m12 m13
                                          m20 m21 m22 m23
                                          m30 m31 m32 m33))
             (:conc-name nil))
  (m00 0.0 :type single-float)
  (m01 0.0 :type single-float)
  (m02 0.0 :type single-float)
  (m03 0.0 :type single-float)
  (m10 0.0 :type single-float)
  (m11 0.0 :type single-float)
  (m12 0.0 :type single-float)
  (m13 0.0 :type single-float)
  (m20 0.0 :type single-float)
  (m21 0.0 :type single-float)
  (m22 0.0 :type single-float)
  (m23 0.0 :type single-float)
  (m30 0.0 :type single-float)
  (m31 0.0 :type single-float)
  (m32 0.0 :type single-float)
  (m33 0.0 :type single-float))

(defmacro %with-matrix ((prefix matrix) &body body)
  (let ((*package* (find-package :agl)))
    `(with-accessors ((,(symbolicate prefix "00") m00)
                      (,(symbolicate prefix "01") m01)
                      (,(symbolicate prefix "02") m02)
                      (,(symbolicate prefix "03") m03)
                      (,(symbolicate prefix "10") m10)
                      (,(symbolicate prefix "11") m11)
                      (,(symbolicate prefix "12") m12)
                      (,(symbolicate prefix "13") m13)
                      (,(symbolicate prefix "20") m20)
                      (,(symbolicate prefix "21") m21)
                      (,(symbolicate prefix "22") m22)
                      (,(symbolicate prefix "23") m23)
                      (,(symbolicate prefix "30") m30)
                      (,(symbolicate prefix "31") m31)
                      (,(symbolicate prefix "32") m32)
                      (,(symbolicate prefix "33") m33))
       ,matrix
       ,@body)))

(defmacro %with-matrices (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(%with-matrix ,(car binds)
       (%with-matrices ,(cdr binds) ,@body))))

(defun %matrix-print (stream matrix)
  (print-unreadable-object (matrix stream)
    (%with-matrix (m matrix)
      (format
        stream "~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a~%  ~a ~a ~a ~a"
        m00 m10 m20 m30
        m01 m11 m21 m31
        m02 m12 m22 m32
        m03 m13 m23 m33))))
(set-pprint-dispatch 'matrix '%matrix-print 1)

(declaim (ftype (function (matrix matrix) matrix) mcp!))
(defun mcp! (source dest)
  "Copy `SOURCE` matrix's values into `DEST`."
  (%with-matrices ((s source) (d dest))
    (psetf d00 s00 d01 s01 d02 s02 d03 s03
           d10 s10 d11 s11 d12 s12 d13 s13
           d20 s20 d21 s21 d22 s22 d23 s23
           d30 s30 d31 s31 d32 s32 d33 s33))
  dest)

(declaim (ftype (function (matrix &key (:tolerance single-float)) matrix) mstab!))
(defun mstab! (source &key (tolerance *tolerance*))
  "Adjust any of `SOURCE` matrix's values to zero if below `TOLERANCE`.
Destructively modifies `SOURCE`."
  (%with-matrix (m source)
    (macrolet ((stabilize (place)
                 `(when (< (abs ,place) tolerance)
                    (setf ,place 0.0))))
      (stabilize m00)
      (stabilize m01)
      (stabilize m02)
      (stabilize m03)
      (stabilize m10)
      (stabilize m11)
      (stabilize m12)
      (stabilize m13)
      (stabilize m20)
      (stabilize m21)
      (stabilize m22)
      (stabilize m23)
      (stabilize m30)
      (stabilize m31)
      (stabilize m32)
      (stabilize m33)))
  source)

(declaim (ftype (function (matrix &key (:tolerance single-float)) matrix) mstab))
(defun mstab (source &key (tolerance *tolerance*))
  "Adjust any of `SOURCE` matrix's values to zero if below `TOLERANCE`.
Returns a copy of `SOURCE`."
  (mstab! (mcp source) :tolerance tolerance))

(declaim (ftype (function (matrix) matrix) mid!))
(defun mid! (source)
  "Modify `SOURCE` matrix to be an identity matrix.
Destructively modifies `SOURCE`."
  (%with-matrix (m source)
    (psetf m00 1.0 m10 0.0 m20 0.0 m30 0.0
           m01 0.0 m11 1.0 m21 0.0 m31 0.0
           m02 0.0 m12 0.0 m22 1.0 m32 0.0
           m03 0.0 m13 0.0 m23 0.0 m33 1.0))
  source)

(declaim (ftype (function () matrix) mid))
(defun mid ()
  "Create an identity matrix."
  (mid! (mat)))

(declaim (ftype (function (matrix matrix matrix) matrix) m*!))
(defun m*! (source1 source2 dest)
  "Multiply `SOURCE1` by `SOURCE2` modifying `DEST` with the result.
Destructively modifies `DEST`."
  (%with-matrices ((a source1) (b source2) (d dest))
    (psetf d00 (+ (* a00 b00) (* a10 b01) (* a20 b02) (* a30 b03))
           d10 (+ (* a00 b10) (* a10 b11) (* a20 b12) (* a30 b13))
           d20 (+ (* a00 b20) (* a10 b21) (* a20 b22) (* a30 b23))
           d30 (+ (* a00 b30) (* a10 b31) (* a20 b32) (* a30 b33))
           d01 (+ (* a01 b00) (* a11 b01) (* a21 b02) (* a31 b03))
           d11 (+ (* a01 b10) (* a11 b11) (* a21 b12) (* a31 b13))
           d21 (+ (* a01 b20) (* a11 b21) (* a21 b22) (* a31 b23))
           d31 (+ (* a01 b30) (* a11 b31) (* a21 b32) (* a31 b33))
           d02 (+ (* a02 b00) (* a12 b01) (* a22 b02) (* a32 b03))
           d12 (+ (* a02 b10) (* a12 b11) (* a22 b12) (* a32 b13))
           d22 (+ (* a02 b20) (* a12 b21) (* a22 b22) (* a32 b23))
           d32 (+ (* a02 b30) (* a12 b31) (* a22 b32) (* a32 b33))
           d03 (+ (* a03 b00) (* a13 b01) (* a23 b02) (* a33 b03))
           d13 (+ (* a03 b10) (* a13 b11) (* a23 b12) (* a33 b13))
           d23 (+ (* a03 b20) (* a13 b21) (* a23 b22) (* a33 b23))
           d33 (+ (* a03 b30) (* a13 b31) (* a23 b32) (* a33 b33))))
  dest)

(declaim (ftype (function (matrix matrix) matrix) m*))
(defun m* (source1 source2)
  "Multiply `SOURCE` by `SOURCE2`."
  (m*! source1 source2 (mat)))

(declaim (ftype (function (matrix matrix) matrix) mcprot!))
(defun mcprot! (source dest)
  "Copy the 3x3 rotation matrix of `SOURCE` into `DEST`.
Destructively modifies `DEST`,"
  (%with-matrices ((s source) (d dest))
    (psetf d00 s00 d10 s10 d20 s20
           d01 s01 d11 s11 d21 s21
           d02 s02 d12 s12 d22 s22))
  dest)

(declaim (ftype (function (matrix) matrix) mcprot))
(defun mcprot (source)
  "Copy the 3x3 rotation matrix of `SOURCE` into a new matrix."
  (mcprot! source (mid)))

(declaim (ftype (function (vec matrix matrix) matrix) mrot!))
(defun mrot! (vec source dest)
  "Rotate matrix `SOURCE` by the amount in radians of each axis specified by `VEC`.
Destructively modified `DEST`."
  (let ((rotation (mid)))
    (macrolet ((rot (axis s c &body body)
                 `(let ((,s (sin ,axis))
                        (,c (cos ,axis)))
                    ,@body
                    (m*! source rotation dest)
                    (mcprot! dest source))))
      (%with-matrix (m rotation)
        (rot (vz vec) s c
          (psetf m00 c
                 m01 s
                 m10 (- s)
                 m11 c))
        (rot (vx vec) s c
          (psetf m00 1.0
                 m01 0.0
                 m02 0.0
                 m10 0.0
                 m11 c
                 m12 s
                 m20 0.0
                 m21 (- s)
                 m22 c))
        (rot (vy vec) s c
          (psetf m00 c
                 m01 0.0
                 m02 (- s)
                 m10 0.0
                 m11 1.0
                 m12 0.0
                 m20 s
                 m21 0.0
                 m22 c)))))
  (mstab! source))

(declaim (ftype (function (vec matrix) matrix) mrot))
(defun mrot (vec source)
  "Rotate matrix `SOURCE` by the amount in radians of each axis specified by `VEC`."
  (mrot! vec source (mid)))

(declaim (ftype (function (vec matrix) matrix) mtrans!))
(defun mtrans! (vec source)
  "Translate matrix `SOURCE` by the amounts specified by each axis in `VEC`.
Destructively modified `SOURCE`."
  (%with-matrix (m source)
    (psetf m30 (vx vec)
           m31 (vy vec)
           m32 (vz vec)))
  source)

(declaim (ftype (function (vec) matrix) mtrans))
(defun mtrans (vec)
  "Translate a new matrix by the amounts specified by each axis in `VEC`."
  (mtrans! vec (mid)))

(declaim (ftype (function (matrix float vec) matrix) mrota!))
(defun mrota! (source angle axis)
  "Rotate matrix `SOURCE` around the vector `AXIS` by the specified `ANGLE` in radians.
Destructively modifies `SOURCE`."
  (%with-matrix (m source)
    (%with-vector (v (vnorm axis))
      (let* ((c (cos angle))
             (1-c (- 1.0 c))
             (s (sin angle)))
        (psetf m00 (float (+ (* vx vx 1-c) c) 1.0)
               m01 (float (+ (* vx vy 1-c) (* vz s)) 1.0)
               m02 (float (- (* vx vz 1-c) (* vy s)) 1.0)
               m03 0.0
               m10 (float (- (* vx vy 1-c) (* vz s)) 1.0)
               m11 (float (+ (* vy vy 1-c) c) 1.0)
               m12 (float (+ (* vy vz 1-c) (* vx s)) 1.0)
               m13 0.0
               m20 (float (+ (* vx vz 1-c) (* vy s)) 1.0)
               m21 (float (- (* vy vz 1-c) (* vx s)) 1.0)
               m22 (float (+ (* vz vz 1-c) c) 1.0)
               m23 0.0
               m30 0.0
               m31 0.0
               m32 0.0
               m33 1.0)
        (mstab! source)))))

(declaim (ftype (function (float vec) matrix) mrota))
(defun mrota (angle axis)
  "Rotate a new matrix around the vector `AXIS` by the specified `ANGLE` in radians."
  (mrota! (mid) angle axis))

(declaim (ftype (function (vec matrix) vec) mgettrans!))
(defun mgettrans! (vec source)
  "Copy the translation vector of matrix `SOURCE` into `VEC`.
Destructively modified `VEC`."
  (%with-matrix (m source)
    (psetf (vx vec) m30
           (vy vec) m31
           (vz vec) m32))
  vec)

(declaim (ftype (function (matrix) vec) mgettrans))
(defun mgettrans (source)
  "Copy the translation vector of matrix `SOURCE` into a new vector."
  (mgettrans! (vec) source))

(declaim (ftype (function (matrix vec vec) vec) mapply!))
(defun mapply! (source point dest)
  "Multiply the matrix `SOURCE` by the vector `POINT`, storing the result into vector `DEST`.
Destructively modifies `DEST`."
  (%with-matrix (m source)
    (%with-vector (v dest)
      (psetf vx (+ (* m00 (vx point))
                   (* m10 (vy point))
                   (* m20 (vz point))
                   (* m30 1.0))
             vy (+ (* m01 (vx point))
                   (* m11 (vy point))
                   (* m21 (vz point))
                   (* m31 1.0))
             vz (+ (* m02 (vx point))
                   (* m12 (vy point))
                   (* m22 (vz point))
                   (* m32 1.0)))))
  dest)

(declaim (ftype (function (matrix vec) vec) mapply))
(defun mapply (source point)
  "Multiply the matrix `SOURCE` by the vector `POINT`, storing the result into a new vector."
  (mapply! source point (vec)))

(declaim (ftype (function (matrix matrix) matrix) minvt!))
(defun minvt! (source dest)
  "Invert matrix `SOURCE`, storing the result into `DEST`.
Destructively modifies `DEST`."
  (let ((dest (mcp source)))
    (mcp! source dest)
    (%with-matrix (d dest)
      (rotatef d10 d01)
      (rotatef d20 d02)
      (rotatef d21 d12)
      (psetf d30 (+ (* d00 (- d30)) (* d10 (- d31)) (* d20 (- d32)))
             d31 (+ (* d01 (- d30)) (* d11 (- d31)) (* d21 (- d32)))
             d32 (+ (* d02 (- d30)) (* d12 (- d31)) (* d22 (- d32))))))
  (mstab! dest))

(declaim (ftype (function (matrix) matrix) minvt))
(defun minvt (source)
  "Invert matrix `SOURCE`, storing the result into a new matrix."
  (minvt! source (mat)))

(defun mgetrot (source &key axis)
  "Get the vector specified by `AXIS` of the 3x3 rotation sub-matrix of matrix `SOURCE`.
If no `AXIS` is specified, returns the 3x3 rotation matrix."
  (%with-matrix (m source)
    (let ((x (vec m00 m01 m02))
          (y (vec m10 m11 m12))
          (z (vec m20 m21 m22)))
      (case axis
        ((:x) x)
        ((:y) y)
        ((:z) z)
        ((nil) (values x y z))))))

(declaim (ftype (function (vec vec vec) matrix) mview))
(defun mview (eye target up)
  "Create a view matrix."
  (let* ((dest (mid))
         (f (vnorm (v- target eye)))
         (s (vnorm (vcross f up)))
         (u (vcross s f)))
    (%with-matrix (m dest)
      (%with-vectors ((s s) (u u) (f f))
        (psetf m00 sx
               m01 ux
               m02 (- fx)
               m10 sy
               m11 uy
               m12 (- fy)
               m20 sz
               m21 uz
               m22 (- fz)
               m33 1.0)
        (m*! dest (mtrans (vneg eye)) dest)))
    dest))

(defun mpersp (fov aspect near far)
  "Create a perspective projection matrix."
  (let ((dest (mid))
        (f (float (/ (tan (/ fov 2))) 1.0))
        (z (- near far)))
    (%with-matrix (m dest)
      (psetf m00 (/ f aspect)
             m11 f
             m22 (/ (+ near far) z)
             m32 (/ (* 2 near far) z)
             m23 -1.0))
    dest))

(defun %matrix-test ()
  (time
    (loop with m = (mid)
          repeat 1000000
          do (m*! m m))))
