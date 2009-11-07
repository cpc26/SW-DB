;;;; http://nostdal.org/ ;;;;

(amx:define-package :sw-db
    :use (:amx :sw-mvc :sw-stm))
(in-package sw-db)


(do-external-symbols (sym (find-package :postmodern))
  (shadowing-import sym))


(export
 '(*database-connection-info*
   with-db-connection
   get-db-object
   put-db-object
   remove-db-object
   sw-db-class
   sw-db
   container-model-db))
