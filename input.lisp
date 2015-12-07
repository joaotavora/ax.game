(in-package :agl)

(defun get-key-name (scancode)
  "Return a string representing the name of a key from the specified scancode."
  (let ((key (sdl2-ffi.functions:sdl-get-key-from-scancode scancode)))
    (sdl2-ffi.functions:sdl-get-key-name key)))

(defmethod key-down (scancode &key))

(defmethod key-down :after (scancode &key debugp)
  (when debugp
    (format t "Key pressed: ~A~%" (get-key-name scancode))))

(defmethod key-up (scancode &key))

(defmethod key-up :after (scancode &key debugp)
  (when debugp
    (format t "Key released: ~A~%" (get-key-name scancode))))

(defun mouse-coords (x y window-height)
  "Convert a mouse position from SDL to OpenGL coordinates."
  (let ((y (- window-height y)))
    (vec x y)))

(defmethod mouse-down (button coords &key))

(defmethod mouse-down :after (button coords &key debugp)
  (when debugp
    (let ((x (floor (vx coords)))
          (y (floor (vy coords))))
      (format t "Mouse button pressed: ~a (x:~a, y:~a)~%" button x y))))

(defmethod mouse-up (button coords &key))

(defmethod mouse-up :after (button coords &key debugp)
  (when debugp
    (let ((x (floor (vx coords)))
          (y (floor (vy coords))))
      (format t "Mouse button released: ~a (x:~a, y:~a)~%" button x y))))

(defmethod mouse-move (x y xrel yrel))

(defmethod mouse-scroll (x y xrel yrel))
