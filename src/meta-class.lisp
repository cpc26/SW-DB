;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(defclass db-class (mvc-class dao-class)
  ((container :reader container-of
              :type container
              :initform (make-instance 'table)))

  (:documentation "Metaclass combining the characteristics of MVC-CLASS (SW-MVC) and
DAO-CLASS (Postmodern)."))


(defmethod validate-superclass ((class db-class) (superclass dao-class))
  t)


(defmethod container-of ((class symbol))
  (container-of (find-class class)))


(defclass db-class-dslotd (mvc-class-dslotd postmodern::direct-column-slot)
  ((dao-class :reader dao-class-of
              :initarg :dao-class ;; TODO: Is this needed?
              :type (or symbol class null)
              :initform nil
              :documentation "
When not NIL, this handles convenient access when dealing with composition of DB-OBJECT.")))


(defmethod initialize-instance :after ((dslotd db-class-dslotd) &key dao-class)
  (when dao-class
    (setf (slot-value dslotd 'dao-class)
          (etypecase dao-class
            (symbol (find-class dao-class))
            (class dao-class)))))


(defclass db-class-eslotd (mvc-class-eslotd postmodern::effective-column-slot)
  ())


(defmethod direct-slot-definition-class ((class db-class) &key
                                         column col-type ;; For Postmodern.
                                         dao-class)      ;; For DB-CLASS-DSLOTD.
  (if (or column col-type)
      (find-class 'db-class-dslotd)
      (progn
        (assert (not dao-class))
        (call-next-method))))


(defmethod compute-effective-slot-definition ((class db-class) name dslotds)
  #| NOTE: At the moment this doesn't check if all DSLOTD's are of the DB-CLASS-DSLOTD kind.
  TODO: Should we perhaps emit a warning if this (not all of them being..) is the case? |#
  (if-let (dslotd (find-if (lambda (dslotd) (typep dslotd 'db-class-dslotd)) dslotds))
    (apply #'make-instance 'db-class-eslotd :direct-slot dslotd
           (sb-pcl::compute-effective-slot-definition-initargs class dslotds)) ;; TODO: closer-mop
    (call-next-method)))


;; Ensure DB-OBJECT is among the parents of our new persistent class..
(defmethod initialize-instance ((class db-class) &rest initargs &key name direct-superclasses)
  (let ((db-object-class (find-class 'db-object nil)))
    ;; Don't want it to be a superclass of itself.
    (if (or (eq name 'db-object)
            (some (lambda (class) (subtypep class db-object-class))
                  direct-superclasses))
        (call-next-method)
        (apply #'call-next-method class
               :direct-superclasses (cons db-object-class direct-superclasses)
               (remove-from-plist initargs :direct-superclasses)))))


;; ..and ensure it stays that way when the class is re-defined or changed.
(defmethod reinitialize-instance ((class db-class) &rest initargs &key
                                  (direct-superclasses nil direct-superclasses-supplied-p))
  ;; Don't want it to be a superclass of itself. :NAME is not supplied here.
  (if (eq (class-name class) 'db-object)
      (call-next-method)
      (if direct-superclasses-supplied-p
          (let ((db-object-class (find-class 'db-object nil)))
            (if (some (lambda (class) (subtypep class db-object-class))
                      direct-superclasses)
                (call-next-method)
                (apply #'call-next-method class
                       :direct-superclasses (cons db-object-class direct-superclasses)
                       (remove-from-plist initargs :direct-superclasses))))
          (call-next-method))))



(defclass db-object (model)
  ((id :col-type serial
       :type integer
       :reader id-of)

   (reference-count :col-type bigint
                    :type integer
                    :reader reference-count-of
                    :initform 0
                    :documentation "When this is <= 0 the DB-OBJECT can be removed from the DB (\"GCed\").")

   (gc-p :col-type boolean
         :type (member t nil)
         :reader gc-p-of
         :initform t
         :documentation "When this is T the DB-OBJECT can be GCed when REFERENCE-COUNT is <= 0.")

   (exists-in-db-p :reader exists-in-db-p-of
                   :initform nil)

   (dirty-p :reader dirty-p-of
            :initform nil)

   (slot-observers :reader slot-observers-of))

  (:metaclass db-class)
  (:keys id)
  (:documentation "
Object representing a row in a DB backend table."))


(defmethod cl-postgres:to-sql-string ((db-object db-object))
  ;; Postmodern refers to and "sees" all DB-OBJECT instances by their ID (slot).
  (unless (exists-in-db-p-of db-object)
    (put-db-object db-object)) ;; After this the ID slot will be bound.
  (cl-postgres:to-sql-string (id-of db-object)))


(defmethod initialize-instance :after ((object db-object) &key)
  ;; Monitor slots so that QUERY instances can detect changes (SQL UPDATE).
  (let ((container (container-of object)))
    (setf (slot-value object 'slot-observers)
          (add-slot-observers object
                              (lambda (instance slot-name)
                                (when (slot-boundp instance slot-name)
                                  #| TODO: In general any slot directly part of DB-OBJECT is not interesting. |#
                                  (unless (in (slot-name) 'id 'reference-count 'gc-p 'slot-observers)
                                    (slot-set instance slot-name container))))
                              'db-class-eslotd))))


(defmethod container-of ((db-object db-object))
  "Returns a CONTAINER instance which represents the backend DB table
which holds instances of DB-OBJECT (representations of DB rows)."
  (container-of (class-of db-object)))


(defmethod dao-slot-class-of ((object db-object) (eslotd db-class-eslotd))
  ":DAO-CLASS dslot option."
  (dao-class-of (postmodern::slot-column eslotd)))


(defmethod remove-reference (referred-dao-class (class db-class) (object db-object) (eslotd db-class-eslotd))
  (when referred-dao-class
    (check-type referred-dao-class db-class)
    (when (slot-boundp-using-class class object eslotd)
      (with (slot-value-using-class class object eslotd)
        (assert (typep it referred-dao-class))
        (when (plusp (reference-count-of it))
          (decf (slot-value it 'reference-count)))))))


;; Our SVUC methods (below) uses this.
(define-variable *%update-dao-p* :value nil)
(defmethod update-dao :around ((dao db-object))
  (let ((*%update-dao-p* dao))
    (call-next-method)))
(defmethod insert-dao :around ((dao db-object))
  (let ((*%update-dao-p* dao))
    (call-next-method)))


(defmethod slot-value-using-class ((class db-class) instance (eslotd db-class-eslotd))
  (let ((value (call-next-method)))
    (if (eq value :null)
        (if *%update-dao-p*
            :null
            (slot-unbound class instance (slot-definition-name eslotd)))
        (if-let (referred-dao-class (dao-slot-class-of instance eslotd))
          #| VALUE can be an INTEGER or the actual instance. If it is an INTEGER we'll fetch the "real instance"
          from the cache or DB. |#
          (if (typep value referred-dao-class)
              value
              (progn
                (check-type value integer)
                (multiple-value-bind (dao-object found-p)
                    (get-db-object value (class-name referred-dao-class))
                  (if found-p
                      dao-object
                      (error
                       "Slot ~A in ~A refers to an object of class ~A with ID ~A which does not exist in the DB."
                       eslotd instance referred-dao-class value)))))
          value))))


(defmethod (setf slot-value-using-class) (new-value (class db-class) instance (eslotd db-class-eslotd))
  (when-let (referred-dao-class (dao-slot-class-of instance eslotd))
    ;; Handle REFERENCE-COUNT for possible old value/reference stored in slot.
    (remove-reference referred-dao-class class instance eslotd)
    #| Handle REFERENCE-COUNT for NEW-VALUE. NEW-VALUE might be an integer when the object is de-serialized from the
    DB; the REFERENCE-COUNT will then be correct as it is. |#
    (unless (typep new-value 'integer)
      (cond
        ((typep new-value referred-dao-class)
         (incf (slot-value new-value 'reference-count)))
        ((typep new-value 'symbol)
         (assert (eq :null new-value)))
        (t
         (error "This should not happen.")))))
  (prog1 (call-next-method)
    (when (and (not (eq *%update-dao-p* instance))
               (slot-boundp instance 'exists-in-db-p)
               (exists-in-db-p-of instance))
      (tf (slot-value instance 'dirty-p))
      (put-db-object instance))))


(defmethod slot-boundp-using-class ((class db-class) instance (eslotd db-class-eslotd))
  (and (call-next-method)
       (not (eq :null (sw-mvc::cell-deref (cell-of (slot-value-using-class class instance eslotd)))))))














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
