;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(defclass table (container-db)
  ()

  (:metaclass mvc-class)
  (:documentation "
Container represented by a DB backend table."))


;; This represents SQL INSERT.
(defmethod container-insert ((event container-insert) (container table))
  (assert (null (relative-position-of event)) nil
          "INSERT: Only :IN is supported.")
  (dolist (object (objects-of event))
    (nilf (slot-value object 'gc-p))
    (tf (slot-value object 'dirty-p))
    (put-db-object object)))


;; This represents SQL DELETE.
(defmethod container-remove ((event container-remove) (container table))
  (dolist (object (objects-of event))
    (tf (slot-value object 'gc-p) ;; Code in HANDLE-LAZY-DB-OPERATIONS will do the actual delete.
        (slot-value object 'dirty-p))
    (let ((class (class-of object)))
      (dolist (eslotd (class-slots class))
        (when (typep eslotd 'db-class-eslotd)
          (remove-reference (dao-slot-class-of object eslotd) class object eslotd))))))


;; ..and SQL UPDATE is represented by SLOT-EVENT (sw-mvc/event-slot.lisp).
