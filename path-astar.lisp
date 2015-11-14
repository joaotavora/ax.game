(in-package :ax.game)

(defmethod find-path ((algorithm (eql 'a*)) tile-type agent)
  (let ((reachable (make-instance 'priority-queue))
        (costs (make-hash-table :test 'equalp))
        (parents (make-hash-table :test 'equalp)))
    (with-slots (coords facing goal) agent
      (flet ((pick-node (node)
               (loop with edges = (graph-edges tile-type node agent)
                     for (dir-key dest) on edges by #'cddr
                     for dir = (getf (tile-directions tile-type) dir-key)
                     for cost = (gethash dest costs)
                     for new-cost = (+ (gethash node costs) (tile-penalty tile-type facing dir))
                     for distance = (tile-distance tile-type node goal)
                     when (or (not cost) (< new-cost cost))
                       do (setf (gethash dest costs) new-cost
                                (gethash dest parents) node)
                          (enqueue reachable (list dest dir-key) (+ new-cost distance))))
             (build (start)
               (unless (equalp start goal)
                 (reverse
                  (cons goal
                        (loop until (equalp current start)
                              for current = (gethash (or current goal) parents)
                              unless (equalp start current)
                              collect current))))))
        (enqueue reachable (list coords facing) 0)
        (setf (gethash coords costs) 0)
        (loop while (peep-at-queue reachable)
              for (node dir-key) = (dequeue reachable)
              do (when (equalp node goal)
                   (return (build coords)))
                 (setf facing dir-key)
                 (pick-node node))))))
