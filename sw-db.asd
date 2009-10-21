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
     (:file "postmodern-live-patch")
     (:file "read-macros")
     (:file "common")
     (:file "model-container")
     (:file "model-container-table")
     (:file "meta-class")
     (:file "model-container-query")
     ))))
