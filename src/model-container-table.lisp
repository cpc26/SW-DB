;;;; http://nostdal.org/ ;;;;

(in-package #:sw-db)


(defclass container (container-db)
  ()
  
  (:documentation "
Container represented by a DB backend table."))


;; This represents SQL INSERT.
(defmethod container-add ((operation container-add) (container container))
  (let ((ok-objects nil))
    (dolist (object (objects-of operation))
      (multiple-value-bind (not-used restart-p)
          (with-simple-restart (continue-mvc-operation "Continue with MVC add operation, but skip the object that caused trouble.")
            (when (exists-in-db-p object)
              (error "~A already exists in database. Did you mean to call SAVE instead of ADD on this object?"
                     object)))
        (declare (ignore not-used))
        (unless restart-p
          (push object ok-objects))))
    (dolist (object ok-objects)
      (put-db-object object))))


;; This represents SQL DELETE.
(defmethod container-remove ((operation container-remove) (container container))
  (remove-db-object (objects-of operation)))


;; ..and SQL UPDATE is represented in operation-save.lisp.
