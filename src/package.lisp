;;;; http://nostdal.org/ ;;;;

(defpackage #:sw-db
  (:use #:cl
        #:closer-mop
        #:sw-mvc
        #:postmodern
        #:aromyxo
        #:cl-utilities
        #:alexandria)

  (:shadowing-import-from #:alexandria
                          #:with-gensyms
                          #:compose
                          #:with-unique-names
                          #:copy-array
                          #:once-only)

  (:shadowing-import-from #:sw-mvc
                          #:remove)

  (:shadow #:container)
  

  (:export
   #:*database-connection-info*
   #:get-db-object
   #:put-db-object
   #:remove-db-object
   #:sw-db-class
   #:sw-db
   #:container-model-db
   #:with-db-connection
   ))


(in-package #:sw-db)

  