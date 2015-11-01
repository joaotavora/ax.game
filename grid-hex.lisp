(in-package :agl)

(defmethod possible-directions ((tile-type (eql 'hex)))
  '(:ne :e :se :sw :w :nw))

(defun cube->hex (src)
  (%with-vector (s src)
    (let ((y (/ (- sz (mod sz 2)) 2)))
      (vec (+ sx y) sz))))

(defun hex->cube (src)
  (%with-vector (s src)
    (let* ((x (- sx (/ (- sy (mod sy 2)) 2)))
           (y (- (- x) sy)))
      (vec x y sy))))

(defmethod tile-distance ((tile-type (eql 'hex)) source dest)
  (%with-vectors ((s (hex->cube source)) (d (hex->cube dest)))
    (let ((x (floor (abs (- sx dx))))
          (y (floor (abs (- sy dy))))
          (z (floor (abs (- sz dz)))))
      (max x y z))))

(defmethod tile-neighbor ((tile-type (eql 'hex)) tile direction)
  (%with-vector (d (cardinal->direction direction))
    (let ((directions `#(,(vec 1 -1 0)
                         ,(vec 1 0 -1)
                         ,(vec 0 1 -1)
                         ,(vec -1 1 0)
                         ,(vec -1 0 1)
                         ,(vec 0 -1 1)))
          (index (mod (+ 6 (round (/ (* 6 (atan dy dx)) (* pi 2)))) 6)))
      (cube->hex (v+ (hex->cube tile) (aref directions index))))))

(defmethod tile-neighbors ((tile-type (eql 'hex)) tile grid-size)
  (v-! grid-size (vec 1 1) grid-size)
  (loop with dirs = (possible-directions tile-type)
        for dir in dirs
        for neighbor = (tile-neighbor 'hex tile dir)
        unless (or (vminusp neighbor)
                   (vminusp (v- grid-size neighbor)))
          append (list dir neighbor)))

(defmethod tile-neighbors-p ((tile-type (eql 'hex)) tile target)
  (loop with dirs = (possible-directions tile-type)
        for dir in dirs
        for neighbor = (tile-neighbor 'hex tile dir)
        do (when (equalp target neighbor)
             (return dir))))
