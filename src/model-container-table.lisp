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
  (let ((ok-objects nil))
    (dolist (object (objects-of event))
      (multiple-value-bind (not-used restart-p)
          (with-simple-restart (continue-mvc-event
                                "Continue with MVC insert event, but skip the object that caused trouble.")
            (when (exists-in-db-p object)
              (error "~A already exists in database. Did you mean to call SAVE instead of ADD on this object?"
                     object)))
        (declare (ignore not-used))
        (unless restart-p
          (push object ok-objects))))
    (dolist (object ok-objects)
      (put-db-object object))))


;; This represents SQL DELETE.
(defmethod container-remove ((event container-remove) (container container))
  (remove-db-object (objects-of event)))


;; ..and SQL UPDATE is represented in event-save.lisp.
