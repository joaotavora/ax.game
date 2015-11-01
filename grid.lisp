(in-package :agl)

(defgeneric possible-directions (tile-type)
  (:documentation "Return a list of possible directions for the specified `TILE-TYPE`."))

(defgeneric tile-distance (tile-type source dest)
  (:documentation "Calculate the distance in tiles between `SOURCE` and `DEST`."))

(defgeneric tile-neighbor (tile-type tile direction)
  (:documentation "Get the neighbor of `TILE` specified by `DIRECTION`.
`DIRECTION` is a keyword symbol specifying a cardinal direction, such as `:SE`."))

(defun tile-penalty (tile-type facing dir)
  "Penalty applied to moving to a tile of type `TILE-TYPE`, depending on the `FACING`
direction of an agent, and the direction to move.
`FACING` is a keyword symbol specifying a cardinal direction, such as `:SE`."
  (let ((angle (get-angle (getf (tile-directions tile-type) facing) dir (vec))))
    (/ (1+ (round (abs angle))) 10)))

(defun cardinal->direction (direction)
  "Convert a cardinal direction to a vector representation.
`DIRECTION` is a keyword sumbol specifying a cardinal direction, such as `:SE`."
  (let ((dirs (list :e (vec 1 0)
                    :ne (vec 1 1)
                    :n (vec 0 1)
                    :nw (vec -1 1)
                    :w (vec -1 0)
                    :sw (vec -1 -1)
                    :s (vec 0 -1)
                    :se (vec 1 -1))))
    (getf dirs direction)))

(defun tile-directions (tile-type)
  "Get a list of directions each side of the specified `TILE-TYPE` faces.
Returns a plist mapping cardinal directions to directional vectors."
  (loop with dirs = (possible-directions tile-type)
        with slice = (/ pi (length dirs))
        for i from 0 to (- (* (length dirs) 2) 2) by 2
        for dir in dirs
        for radians = (* i slice)
        append (list dir (vstab (vec (cos radians) (sin radians))))))

(defun tile-neighbors (tile-type tile grid-size)
  "Get a list of all neighbors of the given `TILE` that are present in the given grid, as specified
by `GRID-SIZE`.
Returns a plist mapping cardinal directions to tile coordinates."
  (v-! grid-size (vec 1 1) grid-size)
  (loop with dirs = (possible-directions tile-type)
        for dir in dirs
        for neighbor = (tile-neighbor tile-type tile dir)
        unless (or (vminusp neighbor)
                   (vminusp (v- grid-size neighbor)))
          append (list dir neighbor)))

(defun tile-neighbors-p (tile-type tile target)
  "Tests whether `TARGET` neighbors `TILE`."
  (loop with dirs = (possible-directions tile-type)
        for dir in dirs
        for neighbor = (tile-neighbor tile-type tile dir)
        do (when (equalp target neighbor)
             (return dir))))
