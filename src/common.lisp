;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(define-variable *database-connection-info*
    :value '("temp" "temp" "temp" "localhost" :pooled-p t)
    :doc "SW-DB> (describe 'connect)
  Lambda-list: (DATABASE USER PASSWORD HOST &KEY (PORT 5432) POOLED-P (USE-SSL *DEFAULT-USE-SSL*))")


(defmacro with-db-connection (&body body)
  `(with-connection *database-connection-info*
     ,@body))


(defmethod cl-postgres:to-sql-string ((pointer pointer))
  (cl-postgres:to-sql-string (ptr-value pointer)))


(defun exists-in-db-p (dao)
  (slot-boundp dao 'id))


(defun get-db-object (id type &key (cache-p t))
  "Returns (values NIL NIL) when no object with given ID and TYPE was found.
Returns (values object :FROM-CACHE) when object was found in cache.
Returns (values object :FROM-DB) when object had to be fetched from the database.
If CACHE-P is T (default) the object will be placed in a Lisp-side cache for
fast (hash-table) retrieval later."
  (declare (symbol type))
  (etypecase id
    (integer)
    (db-object (setf id (id-of id)))
    (string (setf id (parse-integer id))))
  (with-locked-object (find-class type)
    (when cache-p
      (multiple-value-bind (dao found-p) (get-object id type)
        (when found-p
          (return-from get-db-object (values dao :from-cache)))))
    (if-let (dao (with-db-connection (get-dao type id)))
      (progn
        (when cache-p
          (cache-object dao))
        (values dao :from-db))
      (values nil nil))))


(defun put-db-object (dao &key (cache-p t))
  (declare (type db-object dao))
  #| NOTE: Not using DB transactions here since SW-STM does it for us already. By the time we get to the commit-bit,
  any concurrency related issues have been resolved. |#
  #| TODO: We touch all slots (STM) here. This is needed because the commit below calls UPDATE-DAO which will also
  touch all slots. Get rid of this, as especially wrt. MVC (dataflow) it'll cause extra overhead. |#
  (sw-stm:touch dao)
  #| TODO: It'd be great if we could group commits like these together and place them within the scope of a single
  WITH-DB-CONNECTION form. Though, we might not save a _lot_ by doing this since Postmodern pools connections for
  us. |#
  (sw-stm:when-commit ()
    (with-locked-object (class-of dao) ;; vs. GET-DB-OBJECT.
      (with-db-connection (save-dao dao))
      (when cache-p
        (cache-object dao)))))


(defun remove-db-object (dao)
  (declare (type db-object dao))
  ;; TODO: See the TODOs in PUT-DB-OBJECT.
  (sw-stm:touch dao)
  (sw-stm:when-commit ()
    (with-locked-object (class-of dao) ;; vs. GET-DB-OBJECT.
      (with-db-connection
        (dolist (dao (mklst dao))
          (delete-dao dao)))
      #| NOTE: I'm not doing this explicitly because it might still be interesting to get hold of an object based
      on only knowing its ID, and even though it is deleted it might still have hard links (GC) multiple places in
      the code. |#
      #|(uncache-object dao)|#)))


(defun dao-table-info (dao-class)
  "Returns a list of alists containing information about the columns of the DB
table currently representing DAO-CLASS."
  (declare ((or class symbol) dao-class))
  (let ((table-name (s-sql:to-sql-name (dao-table-name dao-class))))
    (with-db-connection
      (query (:select (:as 'pg_attribute.attname 'name)
                      (:as 'pg_type.typname 'type)
                      :from 'pg_attribute
                      :inner-join 'pg_type :on (:= 'pg_type.oid 'pg_attribute.atttypid)
                      :where (:and (:= 'attrelid
                                       (:select 'oid :from 'pg_class :where (:= 'relname table-name)))
                                   (:> 'attnum 0)))
             :alists))))
