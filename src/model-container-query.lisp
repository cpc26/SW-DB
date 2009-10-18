;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


#| TODO:
This thing is currently brute force big-time. What needs to be done is to setup DB-side triggers based on each
active query instance. These can be moved to the Lisp side once (if) I (have) implement(ed) a query language here. |#


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
              :initform nil) |#

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
                                       (query nil query-supplied-p))
  (unless dependencies-supplied-p
    (error ":DEPENDENCIES needed."))
  (unless query-supplied-p
    (error ":QUERY needed."))
  (setf (slot-value model 'query) query
        (dependencies-of model) dependencies)
  (refresh model))


(defmethod refresh ((model query) &optional operation)
  (transform-into model
                  (let ((dao-class (dao-class-of model)))
                    (with1 (loop :for id :in (with-db-connection (query (query-of model) :column))
                              ;;:do (dbg-prin1 id)
                              :collect (get-db-object id dao-class))
                      #|(dbg-prin1 it)|#)))
  (typecase operation
    (sw-mvc:container-insert
     (dolist (object (objects-of operation))
       (sw-mvc::add-slot-observers object
                                   (lambda (instance slot-name new-value)
                                     (declare (ignore instance slot-name new-value))
                                     (refresh model)))))))


(defmethod (setf query-of) :after (query (model query))
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
          ;; TODO: Some EQ test here, as in sw/src/widgets/container.lisp, I think.
          #|(dbg-prin1 event)|#
          (refresh query)))

      #|(add-object-callback query (container-of dependency)
                           (lambda (operation)
                             (cond
                               ((in (operation :test typep)
                                    'container-add    ;; SQL INSERT.
                                    'container-remove ;; SQL DELETE.
                                    'save)            ;; SQL UPDATE.
                                (refresh query operation))

                               (t
                                (error "Don't know how to handle ~A in context of ~A."
                                       operation query)))))|#)))
