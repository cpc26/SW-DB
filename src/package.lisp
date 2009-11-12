;;;; http://nostdal.org/ ;;;;

(amx:define-package :sw-db
    :use (:amx :sw-mvc :sw-stm))
(in-package sw-db)


(do-external-symbols (sym (find-package :postmodern))
  (shadowing-import sym))


(export
 '(;; common.lisp
   *database-connection-info* with-db-connection

   with-db-transaction

   add-lazy-db-operation  with-lazy-db-operations

   get-db-object put-db-object

   pg-md5sum


   ;; meta-class.lisp
   db-class
   db-class-dslotd dao-class-of
   db-class-eslotd

   db-object reference-count-of gc-p-of exists-in-db-p-of dirty-p-of slot-observers-of

   dao-slot-class-of


   ;; model-container.lisp
   container-db


   ;; model-container-table.lisp
   table


   ;; model-container-query.lisp
   query dao-class-of sql-query-of lisp-query-of dependencies-of
   refresh
   ))
