(in-package :agl)

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

(defgeneric possible-directions (tile-type)
  (:documentation "Return a list of possible directions for the specified `TILE-TYPE`."))

(defgeneric tile-distance (tile-type source dest)
  (:documentation "Calculate the distance in tiles between `SOURCE` and `DEST`."))

(defgeneric tile-neighbor (tile-type tile direction)
  (:documentation "Get the neighbor of `TILE` specified by `DIRECTION`.
`DIRECTION` is a keyword symbol specifying a cardinal direction, such as `:SE`."))

(defgeneric tile-neighbors (tile-type tile grid-size)
  (:documentation "Get a list of all neighbors of the given `TILE` that are present in the given
grid, as specified by `GRID-SIZE`.
Returns a plist mapping cardinal directions to tile coordinates."))

(defgeneric tile-neighbors-p (tile-type tile target)
  (:documentation "Tests whether `TARGET` neighbors tile `TILE`."))

(defun tile-directions (tile-type)
  "Get a list of directions each side of the specified `TILE-TYPE` faces.
Returns a plist mapping cardinal directions to directional vectors."
  (loop with dirs = (possible-directions tile-type)
        with slice = (/ pi (length dirs))
        for i from 0 to (- (* (length dirs) 2) 2) by 2
        for dir in dirs
        for radians = (* i slice)
        append (list dir (vstab (vec (cos radians) (sin radians))))))
