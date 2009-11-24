;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


#| TODO:
This thing is currently brute force big-time. What needs to be done is to setup DB-side triggers based on each
active query instance. These can be moved to the Lisp side once (if) I (have) implement(ed) a query language here.

Implementing this really boils down to creating a custom DSL that's compiled to one SQL query and one 'application
query'. Think about sorting, order .. etc.
|#


(defclass query (dlist container-db)
  ((dao-class :reader dao-class-of :initarg :dao-class
              :type symbol
              :initform (error ":DAO-CLASS needed."))

   (sql-query :accessor sql-query-of
              :type string)

   #|(lisp-query :accessor lisp-query-of :initarg :lisp-query
               :initform nil)|#

   (dependencies :accessor dependencies-of
                 :type list
                 :initform nil
                 :documentation "
When any of these change; the QUERY might need a REFRESH."))

  (:metaclass mvc-class)
  (:documentation "
Container model representing a SQL query, or its results, vs. a DB backend."))


(defmethod initialize-instance :after ((model query) &key
                                       (dependencies nil dependencies-supplied-p)
                                       (sql-query nil sql-query-supplied-p))
  (unless dependencies-supplied-p
    (error ":DEPENDENCIES needed."))
  (unless sql-query-supplied-p
    (error ":SQL-QUERY needed."))
  (setf (slot-value model 'sql-query) sql-query
        (dependencies-of model) dependencies)
  (refresh model))


(defmethod refresh ((model query))
  (if *lazy-db-operations*
      (add-lazy-db-operation 'refresh model)
      (transform-into model
                      #| TODO: We only get the IDs since we might have the objects in cache already, but it might
                      be a good idea to pass a list of IDs to GET-DB-OBJECT and let it sort things in a bulk op.. |#
                      (let ((dao-class (dao-class-of model)))
                        (loop :for id :in (with-db-connection (query (sql-query-of model) :column))
                           :collect (get-db-object id dao-class))))))


(defmethod (setf sql-query-of) :after (query (model query))
  (refresh model))


(defmethod (setf dependencies-of) :before (new-dependencies (query query))
  (let ((old-dependencies (dependencies-of query)))
    (when old-dependencies
      ;;(error "(SETF DEPENDENCIES-OF): TODO.")
      #|(dolist (dependency (set-difference old-dependencies new-dependencies))
        ;; This assumes, of course, that there are no other callbacks from DEPENDENCY to QUERY ...
        (remove-object-callback query dependency))|#)
    (dolist (dependency (set-difference new-dependencies old-dependencies))
      (with-formula query
        (when-let* ((event (event-of (container-of dependency)))
                    (object (object-of event)))
          (when (typep object 'db-object)
            (typecase event
              ;; SQL INSERT and DELETE.
              (container-event
               (when (eq (container-of event) (container-of dependency))
                 (typecase event
                   (container-remove
                    ;; TODO: SW-MVC should export this in form of a EXISTS-IN-CONTAINER-P method or similar.
                    (when-let (node (sw-mvc::node-in-context-of query object))
                      (touch node)
                      (remove object query)))
                   (t
                    (refresh query)))))

              ;; SQL UPDATE.
              (slot-event
               (when (and (eq (context-of event) (container-of dependency))
                          (not (eq *%update-dao-p* object))
                          (exists-in-db-p-of object)
                          (not (gc-p-of object)))
                 (refresh query))))))))))
