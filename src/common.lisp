;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)

;; Our SVUC methods (db-class.lisp) uses this.
(define-variable *%update-dao-p* :value nil)


(define-variable *database-connection-info*
    :value '("temp" "temp" "temp" "localhost" :pooled-p t)
    :doc "SW-DB> (describe 'postmodern:connect)
  Lambda-list: (DATABASE USER PASSWORD HOST &KEY (PORT 5432) POOLED-P (USE-SSL *DEFAULT-USE-SSL*))")


(define-variable *lazy-db-operations*
    :value nil)


(defmacro with-lazy-db-operations (&body body)
  `(flet ((with-lazy-db-operations-body () ,@body))
     (if postmodern:*database*
         (let ((*lazy-db-operations* (prog1 (list nil)
                                       ;; TODO: Think about nesting here.
                                       (assert (not *lazy-db-operations*)))))
           (unwind-protect
                (with-lazy-db-operations-body)
             (handle-lazy-db-operations)))
         (with-lazy-db-operations-body))))


(defmacro with-db-connection (&body body)
  "Ensure that we're connected to the DB. Note that this will not reconnect if we're already connected. This holds
even if *DATABASE-CONNECTION-INFO* changes."
  `(flet ((with-db-connection-body () ,@body))
     (if (or postmodern:*database* (not *database-connection-info*))
         (with-db-connection-body)
         (with-connection *database-connection-info*
           (with-db-connection-body)))))


(defmacro with-db-transaction (&body body)
  `(flet ((with-db-transaction-body () ,@body))
     (if postmodern:*database*
         (with-transaction ()
           (execute "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")
           (with-db-transaction-body))
         (with-db-transaction-body))))


(flet ((remove-db-object (dao)
         (declare (type db-object dao))
         (when (exists-in-db-p-of dao)
           (nilf (slot-value dao 'exists-in-db-p)
                 (slot-value dao 'dirty-p)) ;; Removed from all future consideration.
           (delete-dao dao))))


  (defun handle-lazy-db-operations ()
    (let ((operations (nreverse *lazy-db-operations*))
          (*lazy-db-operations* nil))
      ;; GC phase.
      (dolist (operation operations)
        (when operation
          (with (cdr operation)
            (when (and (typep it 'db-object)
                       (dirty-p-of it)
                       (gc-p-of it)
                       (zerop (reference-count-of it)))
              (remove-db-object it)))))

      ;; Update phase.
      (dolist (operation operations)
        (when operation
          (with (cdr operation)
            (case (car operation)
              (put-db-object
               (when (dirty-p-of it)
                 (put-db-object it)
                 (nilf (slot-value it 'dirty-p)))))))) ;; PUT- should only be done once.

      ;; Refresh phase.
      #| TODO: Here we can do interesting stuff wrt. (prepared?) SQL queries that turn out to be shared between
      QUERY instances, but these might already be shared in the Model -->> View sense. |#
      (dolist (operation operations)
        (when operation
          (case (car operation)
            (refresh
             (refresh (cdr operation)))))))))


(defun add-lazy-db-operation (kind obj)
  (declare ((member refresh put-db-object) kind)
           (type (or db-object query) obj))
  (assert (plusp (length *lazy-db-operations*)) nil
          "Not within scope of WITH-LAZY-DB-OPERATIONS")
  (case kind
    (refresh
     ;; Try to dodge the most obvious redundant operations; the rest is dodged in HANDLE-LAZY-DB-OPERATIONS.
     (pushnew (cons kind obj) *lazy-db-operations*
              :test #'equal))
    (otherwise
     (push (cons kind obj) *lazy-db-operations*))))


(defun get-db-object (id type)
  "Returns (values NIL NIL) when no object with given ID and TYPE was found.
Returns (values object :FROM-CACHE) when object was found in cache.
Returns (values object :FROM-DB) when object had to be fetched from the database."
  (declare (integer id)
           (symbol type))
  (let ((class (find-class type)))
    (flet ((check-cache ()
             (multiple-value-bind (dao found-p) (get-object id class)
               (when found-p
                 (return-from get-db-object (values dao :from-cache))))))
      (check-cache)
      (if-let (dao (get-dao type id))
        (progn
          (tf (slot-value dao 'exists-in-db-p))
          ;; Lock and re-check to dodge possible race with other concurrent GET-DB-OBJECT calls.
          (with-locked-object class
            (check-cache)
            (cache-object dao))
          (values dao :from-db))
        (values nil nil)))))


(defun put-db-object (dao)
  "NOTE: Users are not meant to use this directly; use SW-MVC:INSERT instead."
  (declare (type db-object dao))
  (let ((*%update-dao-p* dao))
    (let ((class (class-of dao)))
      (if *lazy-db-operations*
          (add-lazy-db-operation 'put-db-object dao)
          (if (exists-in-db-p-of dao)
              (update-dao dao)
              (progn
                (tf (slot-value dao 'exists-in-db-p))
                ;; NOTE: Non-sync update of db+cache should be safe "within reason" here.
                (insert-dao dao)
                (with-locked-object class
                  (cache-object dao))))))))


(defun dao-table-info (dao-class)
  "Returns a list of alists containing information about the columns of the DB
table currently representing DAO-CLASS."
  (declare ((or class symbol) dao-class))
  (let ((table-name (s-sql:to-sql-name (dao-table-name dao-class))))
    (query (:select (:as 'pg_attribute.attname 'name)
                    (:as 'pg_type.typname 'type)
                    :from 'pg_attribute
                    :inner-join 'pg_type :on (:= 'pg_type.oid 'pg_attribute.atttypid)
                    :where (:and (:= 'attrelid
                                     (:select 'oid :from 'pg_class :where (:= 'relname table-name)))
                                 (:> 'attnum 0)))
           :alists)))


(defun pg-md5sum (str)
  (declare (string str))
  (string-downcase (apply #'concatenate 'string
                          (mapcar (lambda (octet)
                                    (format nil "~2,'0X" octet))
                                  (coerce (md5:md5sum-sequence str) 'list)))))
