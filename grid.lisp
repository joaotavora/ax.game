(in-package :agl)

(defun cardinal->cube (direction)
  "Convert a cardinal direction to cube coordinates."
  (let ((dirs (list :e (vec 1 -1 0)
                    :ne (vec 1 0 -1)
                    :n  (vec 0 1 0)
                    :nw (vec 0 1 -1)
                    :w (vec -1 1 0)
                    :sw (vec -1 0 1)
                    :s (vec 0 -1 0)
                    :se (vec 0 -1 1))))
    (getf dirs direction)))

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

(defgeneric tile-directions (tile-type)
  (:documentation "Get a list of directions each side of the specified `TILE-TYPE` faces.
Returns a plist mapping cardinal directions to directional vectors."))