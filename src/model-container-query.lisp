;;;; http://nostdal.org/ ;;;;

(in-package #:sw-db)


#| TODO:
This thing is currently brute force big-time. What needs to be done is to setup
DB-side triggers based on each active query instance.
|#


(defclass query (dlist container-db)
  ((dao-class :reader dao-class-of :initarg :dao-class
              :type symbol
              :initform (error ":DAO-CLASS needed."))
   
   (query :accessor query-of
          :type string)
   
   #| TODO:
   Implementing this really boils down to creating a custom DSL that's compiled
   to one SQL query and one 'application query'. Think about sorting, order .. etc.
   (app-query :accessor app-query-of :initarg :app-query
              :initform nil)
   |#
   
   (dependencies :accessor dependencies-of
                 :type list
                 :initform nil))
  
  (:metaclass mvc-stm-class)
  (:documentation "
Container model representing a SQL query, or its results, vs. a DB backend."))


(defmethod initialize-instance :after ((model query) &key
                                       (dependencies nil dependencies-supplied-p)
                                       (query nil query-supplied-p))
  (unless dependencies-supplied-p
    (error ":DEPENDENCIES needed."))
  (unless query-supplied-p
    (error ":QUERY needed."))
  (setf (slot-value model 'query) query
        (dependencies-of model) dependencies)
  (refresh model))


(defmethod refresh ((model query) &optional operation)
  (merge-into model
              (let ((dao-class (dao-class-of model)))
                (loop :for id :in (with-db-connection (query (query-of model) :column))
                   :collect (get-db-object id dao-class)))))


(defmethod (setf query-of) :after (query (model query))
  (refresh model))


(defmethod (setf dependencies-of) :before (new-dependencies (model query))
  (let ((old-dependencies (dependencies-of model)))
    (when old-dependencies
      ;;(error "(SETF DEPENDENCIES-OF): TODO.")
      #|(dolist (dependency (set-difference old-dependencies new-dependencies))
        ;; This assumes, of course, that there are no other callbacks from DEPENDENCY to MODEL ...
        (remove-object-callback model dependency))|#)
    (dolist (dependency (set-difference new-dependencies old-dependencies))
      (add-object-callback model (container-of dependency)
                           (lambda (operation)
                             (cond
                               ((in (operation :test typep)
                                    'container-add    ;; SQL INSERT.
                                    'container-remove ;; SQL DELETE.
                                    'save)            ;; SQL UPDATE.
                                (refresh model operation))
                           
                               (t
                                (error "Don't know how to handle ~A in context of ~A."
                                       operation model))))))))
