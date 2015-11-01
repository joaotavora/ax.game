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
