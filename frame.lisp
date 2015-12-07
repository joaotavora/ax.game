(in-package :agl)

(defclass frame ()
  ((init :reader init
          :initform (running-time))
   (now :reader now
        :initform (running-time))
   (before :reader before
           :initform (running-time))
   (dt :reader dt
       :initform 1/60)
   (accumulator :reader accumulator
                :initform 0)
   (frames :reader frames
           :initform 0)))

(defun running-time ()
  (get-internal-real-time))

(defun step-frame (frame &key interval debugp)
  (with-slots (init now before accumulator frames) frame
    (setf now (running-time)
          before now)
    (incf accumulator (/ (- now before) internal-time-units-per-second))
    (let ((seconds (/ (- now init) internal-time-units-per-second))
          (fps (/ frames interval)))
      (incf frames)
      (when (and (> seconds interval) debugp)
        (format t "Framerate: ~,3f ms/f, ~,2f fps~%" (/ 1000 fps) fps)
        (setf frames 0
              init now)))))
