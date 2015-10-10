(in-package :game.gem)

(defclass entity ()
  ((id :reader id
       :initarg :id
       :initform nil)
   (prototype :reader prototype
              :initarg :prototype
              :initform nil)
   (attr :reader attr
         :initarg :attr
         :initform nil)
   (components :reader components
               :initarg :components
               :initform nil)))

(defmethod print-object ((o entity) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~S" (id o))))

(defmethod value ((e entity) name)
  (with-slots (prototype attr) e
    (or (cdr (assoc name attr))
        (and prototype
             (value prototype name)))))

(defmethod (setf value) (value (e entity) name)
  (with-slots (attr) e
    (if-let ((cell (assoc name attr)))
      (setf (cdr cell) value)
      (progn
        (push (cons name value) attr)
        value))))

(defmethod component ((e entity) path)
  (with-slots (components) e
    (when-let ((component (cdr (assoc (car path) components))))
      (if (cdr path)
        (component component (cdr path))
        component))))

(defmethod make-entity ((e entity))
  (with-slots (components) e
    (make-instance 'entity
                   :prototype e
                   :components (loop :for (name . component) :in components
                                     :collect (cons name (make-entity component))))))

(defun get-components (spec prototypes)
  (loop :for (name . id) :in spec
        :for component = (gethash id prototypes)
        :when component
        :collect (cons name component)))

(defun load-prototypes (file-path)
  (let ((prototypes (make-hash-table)))
    (loop :for (id . (attr c-spec)) :in (read-file file-path)
          :for prototype = (make-instance 'entity
                                          :id id
                                          :attr attr
                                          :components (get-components c-spec prototypes))
          :do (setf (gethash id prototypes) prototype))
    prototypes))
