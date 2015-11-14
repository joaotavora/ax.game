(in-package :agl)

(defmethod neighbors ((tile-type (eql 'square)) map x y)
  (remove-if-not
   (lambda (x) (and (< -1 (first x) (array-dimension map 0))
               (< -1 (second x) (array-dimension map 1))))
   `((,x ,(+ y 2))
     (,(- x 2) ,y)
     (,x ,(- y 2))
     (,(+ x 2) ,y))))

(defmethod walk ((tile-type (eql 'square)) map visited x y)
  (flet ((neighbors (x y)
           (shuffle (neighbors tile-type map x y))))
    (setf (aref visited x y) 1)
    (loop with neighbors = (neighbors x y)
          with stack = `((,x ,y ,neighbors))
          while stack
          for (nx ny nn) = (pop stack)
          do (loop while neighbors
                   for (u v) = (car neighbors)
                   do (if (= (aref visited u v) 1)
                          (setf neighbors (cdr neighbors))
                          (progn
                            (setf (aref map u v) 1
                                  (aref map (/ (+ x u) 2) (/ (+ y v) 2)) 1)
                            (push `(,x ,y ,(cdr neighbors)) stack)
                            (setf neighbors (neighbors u v)
                                  x u
                                  y v
                                  (aref visited x y) 1))))
             (setf x nx
                   y ny
                   neighbors nn))))

(defmethod carve ((tile-type (eql 'square)) map x y)
  (let ((visited (make-array (list (array-dimension map 0)
                                   (array-dimension map 1))
                             :element-type 'bit)))
    (setf (aref map x y) 1)
    (walk tile-type map visited x y)))
