;;;; http://nostdal.org/ ;;;;

(in-package #:sw-db)


(defclass save (sw-mvc:operation)
  ((object :reader object-of :initarg :object
           :type db-object
           :initform (error ":OBJECT needed.")))

  (:documentation "
This basically represents SQL UPDATE.
SQL INSERT and DELETE is \"defined\" by methods in model-container-table.lisp"))


(defmethod objects-of ((operation save))
  (list (object-of operation)))


(defmethod observables-of append ((operation save))
  (list (object-of operation)
        ;; We need stuff watching the container of this object to be notified also.
        (container-of (object-of operation))))


(defmethod handle ((operation save))
  (put-db-object (object-of operation)))


(defmethod save ((db-object db-object))
  (unless (exists-in-db-p db-object)
    (error "~A does not already exist in database. Did you mean to call ADD instead of SAVE on this object?"
           db-object))
  (handle (make-instance 'save :object db-object)))
