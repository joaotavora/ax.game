(in-package :gml)

(defun cube->hex (src)
  (%with-vector (s src)
    (let ((y (/ (- sz (mod sz 2)) 2)))
      (vec (+ sx y) sz))))

(defun hex->cube (src)
  (%with-vector (s src)
    (let* ((x (- sx (/ (- sy (mod sy 2)) 2)))
           (y (- (- x) sy)))
      (vec x y sy))))

(defmethod tile-distance ((tile-type (eql 'hex)) tile target)
  (%with-vectors ((s (hex->cube tile)) (d (hex->cube target)))
    (let ((x (abs (- sx dx)))
          (y (abs (- sy dy)))
          (z (abs (- sz dz))))
      (max x y z))))

(defmethod tile-round ((tile-type (eql 'hex)) tile)
  (let* ((dest (vround (hex->cube tile)))
         (shift (vpos (v- dest tile))))
    (%with-vectors ((d dest) (s shift))
      (cond
        ((and (> sx sy) (> sx sz))
         (setf dx (- (- dy) dz)))
        ((> sy sz)
         (setf dy (- (- dx) dz)))
        (t (setf dz (- (- dx) dy)))))
    (cube->hex dest)))

(defmethod tile-neighbor ((tile-type (eql 'hex)) tile direction)
  (%with-vector (d direction)
    (let ((directions `#(,(vec 1 -1 0)
                         ,(vec 1 0 -1)
                         ,(vec 0 1 -1)
                         ,(vec -1 1 0)
                         ,(vec -1 0 1)
                         ,(vec 0 -1 1)))
          (index (mod (+ 6 (round (/ (* 6 (atan dy dx)) (* pi 2)))) 6)))
      (cube->hex (v+ (hex->cube tile) (aref directions index))))))

(defmacro with-hex-neighbors (tile &body body)
  `(loop with dirs = (list :e (vec 1 0)
                           :ne (vec 1 1)
                           :nw (vec -1 1)
                           :w (vec -1 0)
                           :sw (vec -1 -1)
                           :se (vec 1 -1))
         for (key dir) on dirs by #'cddr
         for neighbor = (tile-neighbor 'hex ,tile dir)
         ,@body))

(defmethod tile-neighbors ((tile-type (eql 'hex)) tile map-size)
  (v-! map-size (vec 1 1) map-size)
  (with-hex-neighbors tile
    unless (or (vminusp neighbor)
               (vminusp (v- map-size neighbor)))
    append (list key neighbor)))

(defmethod tile-neighbors-p ((tile-type (eql 'hex)) tile target)
  (with-hex-neighbors tile
    do (when (equalp target neighbor)
         (return key))))

(defmethod tile-directions ((tile-type (eql 'hex)))
  (loop with slice = (/ pi 6)
        with dirs = '(:e :ne :nw :w :sw :se)
        for i from 0 to 10 by 2
        for dir in dirs
        for radians = (* i slice)
        append (list dir (vstab (vec (cos radians) (sin radians))))))
