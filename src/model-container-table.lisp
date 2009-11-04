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
    (nilf (slot-value object 'gc-p))
    (put-db-object object)))


;; This represents SQL DELETE.
(defmethod container-remove ((event container-remove) (container container))
  (dolist (object (objects-of event))
    (tf (slot-value object 'gc-p))
    (let ((class (class-of object)))
      (dolist (eslotd (class-slots class))
        (when (typep eslotd 'db-class-eslotd)
          ;; TODO: This code is pretty much a copy of the code in (SETF S-V-U-C) for DB-CLASS.
          (remove-reference (dao-slot-class-of object eslotd) class object eslotd))))
    (remove-db-object object)))


;; ..and SQL UPDATE is represented by SLOT-EVENT (sw-mvc/event-slot.lisp).
