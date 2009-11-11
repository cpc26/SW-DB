;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(define-variable *database-connection-info*
    :value '("temp" "temp" "temp" "localhost" :pooled-p t)
    :doc "SW-DB> (describe 'postmodern:connect)
  Lambda-list: (DATABASE USER PASSWORD HOST &KEY (PORT 5432) POOLED-P (USE-SSL *DEFAULT-USE-SSL*))")


(defmacro with-db-connection (&body body)
  "Ensure that we're connected to the DB. Note that this will not reconnect if we're already connected. This holds
even if *DATABASE-CONNECTION-INFO* changes."
  `(flet ((body-fn () ,@body))
     (if postmodern:*database*
         (body-fn)
         (with-connection *database-connection-info*
           (body-fn)))))


(define-variable *lazy-db-operations*
    :value nil)


(defun handle-lazy-db-operations ()
  (let ((operations (reverse *lazy-db-operations*))
        (*lazy-db-operations* nil))
    ;; Multiple passes. E.g., we REFRESH last.
    (dolist (operation operations)
      (when operation
        (ecase (car operation)
          (refresh)
          (put-db-object (put-db-object (cdr operation)))
          (remove-db-object (remove-db-object (cdr operation))))))
    (dolist (operation operations)
      (when operation
        (case (car operation)
          (refresh (refresh (cdr operation))))))))


(defun add-lazy-db-operation (kind obj)
  (declare ((member refresh put-db-object remove-db-object) kind)
           (type (or db-object query) obj))
  (assert (plusp (length *lazy-db-operations*)) nil
          "Not within scope of WITH-LAZY-DB-OPERATIONS")
  ;; TODO: This isn't correct; needs more work etc.
  (pushnew (cons kind obj) *lazy-db-operations* :test #'equal))


(defmacro with-lazy-db-operations (&body body)
  `(let ((*lazy-db-operations* (prog1 (list nil)
                                 ;; TODO: Think about nesting here.
                                 (assert (not *lazy-db-operations*)))))
     (unwind-protect
          (progn ,@body)
       (handle-lazy-db-operations))))


(defun get-db-object (id type &key (cache-p t))
  "Returns (values NIL NIL) when no object with given ID and TYPE was found.
Returns (values object :FROM-CACHE) when object was found in cache.
Returns (values object :FROM-DB) when object had to be fetched from the database.
If CACHE-P is T (default) the object will be placed in a Lisp-side cache for
fast (hash-table) retrieval later."
  (declare (integer id)
           (symbol type))
  (let ((class (find-class type)))
    (when cache-p
      (multiple-value-bind (dao found-p) (get-object id class)
        (when found-p
          (return-from get-db-object (values dao :from-cache)))))
    (with-locked-object class
      (if-let (dao (get-dao type id))
        (progn
          (tf (slot-value dao 'exists-in-db-p))
          (when cache-p
            (cache-object dao))
          (values dao :from-db))
        (values nil nil)))))


(defun put-db-object (dao &key (cache-p t))
  "NOTE: Users are not meant to use this directly; use SW-MVC:INSERT instead."
  (declare (type db-object dao))
  (if *lazy-db-operations*
      (add-lazy-db-operation 'put-db-object dao)
      (with-locked-object (class-of dao)
        (if (exists-in-db-p-of dao)
            (update-dao dao)
            (progn
              (insert-dao dao)
              (tf (slot-value dao 'exists-in-db-p))))
        (when cache-p
          (cache-object dao)))))


(defun remove-db-object (dao)
  "NOTE: Users are not meant to use this directly; use SW-MVC:REMOVE instead."
  (declare (type db-object dao))
  (if *lazy-db-operations*
      (add-lazy-db-operation 'remove-db-object dao)
      (with-locked-object (class-of dao)
        (dolist (dao (mklst dao))
          (delete-dao dao)
          (nilf (slot-value dao 'exists-in-db-p))
          #| NOTE: I'm not doing this explicitly because it might still be interesting to get hold of an object based
          on only knowing its ID, and even though it is deleted it might still have hard links (GC) multiple places
          in the code. |#
          #|(uncache-object dao)|#))))


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
(export 'pg-md5sum)
