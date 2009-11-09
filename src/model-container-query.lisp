;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


#| TODO:
This thing is currently brute force big-time. What needs to be done is to setup DB-side triggers based on each
active query instance. These can be moved to the Lisp side once (if) I (have) implement(ed) a query language here.

Implementing this really boils down to creating a custom DSL that's compiled to one SQL query and one 'application query'. Think about sorting, order .. etc.
|#


(defclass query (dlist container-db)
  ((dao-class :reader dao-class-of :initarg :dao-class
              :type symbol
              :initform (error ":DAO-CLASS needed."))

   (sql-query :accessor sql-query-of
              :type string)

   (lisp-query :accessor lisp-query-of :initarg :lisp-query
               :initform nil)

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


(defmethod refresh ((model query) &optional operation)
  (declare (ignore operation))
  (transform-into model
                  (let ((dao-class (dao-class-of model)))
                    (loop :for id :in (with-db-connection (query (sql-query-of model) :column))
                       :collect (get-db-object id dao-class)))))


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
        (when-let (event (event-of (container-of dependency)))
          (typecase event
            ;; SQL INSERT and DELETE.
            (container-event (when (eq (container-of event) (container-of dependency))
                               (refresh query event)))
            ;; SQL UPDATE.
            (slot-event
             (when (and (exists-in-db-p-of (object-of event))
                        (eq (sw-mvc::context-of event) (container-of dependency)))
               ;; TODO: Need a SW-MVC:MEMBER or FIND or something function as this can get very slow.
               (let ((already-member (member (object-of event) ~~query :test #'eq)))
                 (if (funcall (lisp-query-of query) (object-of event))
                     (when (not already-member)
                       ;; TODO: Ordering.
                       (insert (object-of event) :in query))
                     (when already-member
                       (remove (object-of event) query))))))))))))
