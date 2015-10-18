(in-package :gem)

(defclass entity ()
  ((id :reader id
       :initarg :id
       :initform nil)
   (prototype :initarg :prototype
              :initform nil)
   (attr :initarg :attr
         :initform nil)
   (components :initarg :components
               :initform nil)))

(defmethod print-object ((o entity) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~S" (id o))))

(defmethod attr ((e entity) name)
  "Get the value of an entity's attribute."
  (with-slots (prototype attr) e
    (or (cdr (assoc name attr))
        (and prototype
             (attr prototype name)))))

(defmethod (setf attr) (value (e entity) name)
  "Set the value of an entity's attribute."
  (with-slots (attr) e
    (if-let ((cell (assoc name attr)))
      (setf (cdr cell) value)
      (progn
        (push (cons name value) attr)
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
