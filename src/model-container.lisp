;;;; http://nostdal.org/ ;;;;

(in-package #:sw-db)


(defclass container-db (sw-mvc:container)
  ()

  (:documentation "
This might represent a DB table or the result of a query. This class serves as
a category or designator for all container classes which have a DB-based
backend."))

