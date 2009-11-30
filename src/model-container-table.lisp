;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(defclass table (container-db)
  ()

  (:metaclass mvc-class)
  (:documentation "
Container model represented by a DB backend table. Since the table might be huge we don't actually mirror it on our end. To examine the actual content of the table or this container abstraction, see the QUERY class."))


;; This represents SQL INSERT.
(defmethod container-insert ((event container-insert) (container table))
  (assert (null (relative-position-of event)) nil
          "INSERT: Only :IN is supported.")
  (dolist (object (objects-of event))
    (check-type object db-object)
    (nilf (slot-value object 'gc-p))
    (tf (slot-value object 'dirty-p))
    (put-db-object object)))


;; This represents SQL DELETE.
(defmethod container-remove ((event container-remove) (container table))
  (dolist (object (objects-of event))
    (check-type object db-object)
    (tf (slot-value object 'gc-p) ;; Code in HANDLE-LAZY-DB-OPERATIONS will do the actual delete.
        (slot-value object 'dirty-p))
    (let ((class (class-of object)))
      (dolist (eslotd (class-slots class))
        (when (typep eslotd 'db-class-eslotd)
          (remove-reference (dao-slot-class-of object eslotd) class object eslotd))))))


;; ..and SQL UPDATE is represented by SLOT-EVENT (sw-mvc/event-slot.lisp).
