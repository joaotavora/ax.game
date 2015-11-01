(in-package :ax.game)

(defclass agent ()
  ((tangiblep :reader tangiblep
              :initarg :tangiblep
              :initform t)
   (unpassable :reader unpassable
               :initarg :unpassable
               :initform nil)
   (coords :reader coords
           :initarg :coords
           :initform (vec))
   (facing :accessor facing
           :initarg :facing
           :initform :e)
   (graph-size :reader graph-size
               :initarg :graph-size
               :initform (vec 1024 1024))))

(defun graph-edges (tile-type tile agent)
  "Get a list of all graph edges for the given `AGENT` on `TILE`.
Edges that are in the agent's unpassable list will be filtered out."
  (with-slots (unpassable tangiblep graph-size) agent
    (loop with edges = (tile-neighbors tile-type tile graph-size)
          for (dir edge) on edges by #'cddr
          unless (and (member edge unpassable :test 'equalp)
                      (tangiblep agent))
          nconc (list dir edge))))
