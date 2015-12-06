(in-package :agl)

(defclass entity ()
  ((id :reader id
       :initarg :id
       :initform nil)
   (prototype :initarg :prototype
              :initform nil)
   (attrs :initarg :attrs
          :initform nil)
   (components :initarg :components
               :initform nil)))

(defmethod print-object ((o entity) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~S" (id o))))

(defmethod attrs ((e entity))
  "Get a property list of an entity's attributes."
  (loop for (attr . value) in (slot-value e 'attrs)
        collect (make-keyword attr)
        collect value))

(defmethod attr ((e entity) name)
  "Get the value of an entity's attribute."
  (with-slots (prototype attrs) e
    (or (cdr (assoc name attrs))
        (and prototype
             (attr prototype name)))))

(defmethod (setf attr) (value (e entity) name)
  "Set the value of an entity's attribute."
  (with-slots (attrs) e
    (if-let ((cell (assoc name attrs)))
      (setf (cdr cell) value)
      (progn
        (push (cons name value) attrs)
        value))))

(defmethod component ((e entity) path)
  "Retrieve the component of an entity given a path list.
   Path is a list of component names to recurse through."
  (with-slots (components) e
    (when-let ((component (cdr (assoc (car path) components))))
      (if (cdr path)
        (component component (cdr path))
        component))))

(defmethod make-entity ((prototype entity))
  "Create an instance of an entity from a prototype."
  (with-slots (components) prototype
    (make-instance 'entity
                   :prototype prototype
                   :components (loop :for (name . component) :in components
                                     :collect (cons name (make-entity component))))))

(defmethod make-entity (prototype)
  "Create an instance of an entity from the name of a prototype."
  (when-let ((prototype (prototype prototype)))
    (make-entity prototype)))
