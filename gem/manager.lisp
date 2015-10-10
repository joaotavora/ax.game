(in-package :gem)

(defclass manager ()
  ((prototypes :accessor prototypes
               :initform (make-hash-table))
   (entities :accessor entities
             :initform nil)))

(defvar *entity-manager* (make-instance 'manager))
(defvar *prototype-path* nil)

(defun prototype (id)
  "Retrieve a prototype by its ID from the manager."
  (gethash id (prototypes *entity-manager*)))

(defun (setf prototype) (prototype id)
  "Store a prototype with a reference of ID in the manager."
  (setf (gethash id (prototypes *entity-manager*)) prototype))

(defun %make-prototype (id attr components)
  "Make a prototype suitable for storing in the manager."
  (make-instance 'entity
                 :id id
                 :attr attr
                 :components (loop :for (name . id) :in components
                                   :for component = (prototype id)
                                   :when component
                                   :collect (cons name component))))

(defun load-prototypes ()
  "Read the prototype definition file and store each prototype in the manager."
  (loop :for (id . (attr components)) :in (read-file *prototype-path*)
        :do (setf (prototype id) (%make-prototype id attr components))))
