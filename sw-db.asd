;;;; http://nostdal.org/ ;;;;

(defsystem sw-db
  :depends-on (:sw-mvc
               :postmodern)

  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "bootstrap")
     (:file "read-macros")

     (:file "postmodern-live-patch")
     (:file "common")
     (:file "model-container")
     (:file "model-container-table")
     (:file "db-class")
     (:file "model-container-query")
     (:file "package-export")
     ))))
