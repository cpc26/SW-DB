;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(defclass db-class (mvc-class dao-class)
  ((container :reader container-of
              :type container))

  (:documentation "Metaclass combining the characteristics of MVC-CLASS (SW-MVC) and
DAO-CLASS (Postmodern)."))


(defmethod validate-superclass ((class db-class) (superclass dao-class))
  t)


(defmethod initialize-instance :after ((class db-class) &key)
  (setf (slot-value class 'container)
        (make-instance 'container)))


(defmethod container-of ((class symbol))
  (container-of (find-class class)))



(defclass db-object (model)
  ((id :col-type serial
       :reader id-of
       :documentation "
Note that this slot stays unbound until the instance has been added to the DB by
PUT-DB-OBJECT or by adding it to a CONTAINER instance."))

  (:metaclass db-class)
  (:keys id)
  (:documentation "
Object representing a row in a DB backend table."))


#|
TODO: Finalization in SBCL == epic fail, because one cannot actually _do_
anything on (or before) GC of an object.
 (defmethod initialize-instance :after ((db-object db-object) &key)
  (sb-ext:finalize db-object
                   (let ((type-name (type-of db-object))
                         (id (id-of db-object)))
                   (lambda ()
                     (format t "DB-OBJECT GCed: ~A ~A~%"
                             id type-name)))))
|#


(defmethod cl-postgres:to-sql-string ((db-object db-object))
  ;; Postmodern refers to and "sees" all DB-OBJECT instances by their ID (slot).
  (cl-postgres:to-sql-string (id-of db-object)))


;; Ensure DB-OBJECT is among the parents of our new persistent class..
(defmethod initialize-instance :around ((class db-class) &rest initargs &key
                                        name
                                        direct-superclasses)
  (if (eq name 'db-object) ;; Don't want it to be a superclass of itself.
      (call-next-method)
      (let ((db-object-class (find-class 'db-object)))
        (if (some (lambda (class) (subtypep class db-object-class))
                  direct-superclasses)
            (call-next-method)
            (apply #'call-next-method class
                   :direct-superclasses (push db-object-class direct-superclasses)
                   initargs)))))


;; ..and ensure it stays that way when the class is re-defined or changed.
(defmethod reinitialize-instance :around ((class db-class) &rest initargs &key
                                          (direct-superclasses nil direct-superclasses-supplied-p))
  (if (eq (class-name class) 'db-object) ;; Don't want it to be a superclass of itself. :NAME is not supplied here.
      (call-next-method)
      (if direct-superclasses-supplied-p
          (let ((db-object-class (find-class 'db-object)))
            (if (some (lambda (class) (subtypep class db-object-class))
                      direct-superclasses)
                (call-next-method)
                (apply #'call-next-method class
                       :direct-superclasses (push db-object-class direct-superclasses)
                       initargs)))
          (call-next-method))))


(defmethod container-of ((db-object db-object))
  "Returns a CONTAINER instance which represents the backend DB table
which holds instances of DB-OBJECT (representations of DB rows)."
  (container-of (class-of db-object)))



(defclass direct-db-object-slotd (postmodern::direct-column-slot)
  ((dao-class :initarg :dao-class :reader dao-class-of))

  (:documentation "
This handles convenient access when dealing with composition of DB-OBJECT instances via the :DAO-CLASS
slot-option."))


(defmethod initialize-instance :after ((slotd direct-db-object-slotd) &key dao-class)
  (when dao-class
    (setf (slot-value slotd 'dao-class)
          (find-class dao-class))))


(defmethod closer-mop:direct-slot-definition-class :around ((class db-class) &key dao-class &allow-other-keys)
  (if dao-class
      (find-class 'direct-db-object-slotd)
      (call-next-method)))


(defmethod dao-slot-class-of ((object db-object) slot)
  "Does SLOT refer to another DB-OBJECT instance? Returns two values:

  * referred-dao-class
  * referring-to-other-dao-class-p"
  (let ((direct-slot (postmodern::slot-column slot)))
    (if (typep direct-slot 'direct-db-object-slotd)
        (values (dao-class-of direct-slot) t)
        (values nil nil))))


(defmethod slot-value-using-class ((class db-class) instance (slotd postmodern::effective-column-slot))
  (let ((value (call-next-method)))
    (when (and sw-mvc::*get-cell-p* (typep value 'cell))
      #| We don't want the code below to mess around with the return-value at this point. In particular not if this
      is called from (SLOT-BOUNDP-USING-CLASS MVC-CLASS T T). |#
      (return-from slot-value-using-class value))
    (if (eq value :null) ;; I really don't like this about Postmodern; this should just be an unbound slot "already".
        (slot-unbound class instance (slot-definition-name slotd))
        (multiple-value-bind (referred-dao-class referring-to-other-dao-class-p)
            (dao-slot-class-of instance slotd)
          (if referring-to-other-dao-class-p
              (if (typep value referred-dao-class)
                  value
                  (multiple-value-bind (dao-object found-p)
                      (get-db-object value (class-name referred-dao-class))
                    (if found-p
                        (prog1 dao-object
                          #|(setf (slot-value-using-class class instance slotd) dao-object)|#)
                        (error
                         "Slot ~A in ~A refers to an object of class ~A with ID ~A which does not exist in the DB."
                         slotd instance referred-dao-class value))))
              value)))))


(defmethod sw-stm:touch-using-class ((instance db-object) (class db-class))
  (dolist (eslotd (class-slots class))
    (with (standard-instance-access instance (slot-definition-location eslotd))
      (typecase it
        (cell (sw-stm:touch it)
              (when (dao-slot-class-of instance eslotd)
                (with (sw-mvc::cell-deref it)
                  (unless (eq it 'sw-mvc::%unbound)
                    (sw-stm:touch (cell-of (id-of it)))))))))))



#|(defmethod finalize-inheritance :after ((class db-class))
  (let ((db-column-slots (dao-table-info class))
        (lisp-column-slots
         (loop :for column-slot :in (postmodern::dao-column-slots class)
            :collect (let ((col-info nil))
                       (push (cons :name (postmodern::slot-sql-name column-slot))
                             col-info)
                       (multiple-value-bind (type can-be-null-p)
                           (s-sql::dissect-type (postmodern::column-type column-slot))
                         (push (cons :type (s-sql:sql-type-name type))
                               col-info)
                         (push (cons :can-be-null-p can-be-null-p)
                               col-info))
                       (when (slot-boundp column-slot 'postmodern::col-default)
                         (push (cons :default (postmodern::column-default column-slot))
                               col-info))
                       col-info))))

    ;; TODO: Rename of slot/column?
    (let ((new-columns nil)
          (removed-columns nil))
      ;; Find new slots/columns.
      (dolist (lisp-column-slot lisp-column-slots)
        (let ((column-name (cdr (assoc :name lisp-column-slot))))
          (unless (find column-name db-column-slots
                        :key (lambda (elt) (cdr (assoc :name elt)))
                        :test #'string=)
            (push lisp-column-slot new-columns)
            ;;(format t "slot ~A about to be added to db~%" column-name)
            )))

      ;; Find removed slots/columns.
      (dolist (db-column-slot db-column-slots)
        (let ((column-name (cdr (assoc :name db-column-slot))))
          ;;(dbg-princ column-name)
          ;;(dbg-princ lisp-column-slots)
          (unless (find column-name lisp-column-slots
                        :key (lambda (elt) (cdr (assoc :name elt)))
                        :test #'string=)
            (push db-column-slot removed-columns)
            ;;(format t "slot ~A about to be removed from db~%" column-name)
            ))))


    ;; Set type.

    ;; Find slots/columns whose type has changed.

    ;; Find slots/columns whos `ATTNOTNULL' state has changed.

    ;; Find slots/colums whos `ATTHASDEF' state has changed.

    ;(warn "TODO: finish finalize-inheritance db-class (sw-db/src/class.lisp)")
    ;(dbg-princ db-column-slots)
    ;(dbg-princ lisp-column-slots)
    ;(dbg-princ (find :attname (first db-column-slots)
    ;                 :key #'car))
    ))|#
