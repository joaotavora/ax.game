(in-package :ax.game)

(defmethod possible-directions ((tile-type (eql 'square)))
  '(:n :ne :e :se :s :sw :w :nw))

(defmethod tile-distance ((tile-type (eql 'square)) source dest)
  (%with-vectors ((s source) (d dest))
    (+ (abs (- sx dx)) (abs (- sy dy)))))

(defmethod tile-neighbor ((tile-type (eql 'square)) tile direction)
  (let ((dir (cardinal->direction direction)))
    (%with-vector (d dir)
      (v- tile (vec dy (- dx))))))

(defmethod tile-neighbors ((tile-type (eql 'square)) tile grid-size)
  (v-! grid-size (vec 1 1) grid-size)
  (loop with dirs = (possible-directions tile-type)
        for dir in dirs
        for neighbor = (tile-neighbor 'square tile dir)
        unless (or (vminusp neighbor)
                   (vminusp (v- grid-size neighbor)))
          append (list dir neighbor)))

(defmethod tile-neighbors-p ((tile-type (eql 'square)) tile target)
  (loop with dirs = (possible-directions tile-type)
        for dir in dirs
        for neighbor = (tile-neighbor 'square tile dir)
        do (when (equalp target neighbor)
             (return dir))))
