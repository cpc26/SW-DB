;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(defclass container (container-db)
  ()

  (:metaclass mvc-class)
  (:documentation "
Container represented by a DB backend table."))


;; This represents SQL INSERT.
(defmethod container-insert ((event container-insert) (container container))
  (assert (null (relative-position-of event)) nil
          "INSERT: Only :IN is supported.")
  (dolist (object (objects-of event))
    (put-db-object object)))


;; This represents SQL DELETE.
(defmethod container-remove ((event container-remove) (container container))
  (dolist (object (objects-of event))
    (remove-db-object object)))


;; ..and SQL UPDATE is represented by PUT-DB-OBJECT (common.lisp).
