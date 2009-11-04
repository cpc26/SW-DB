;;;; http://nostdal.org/ ;;;;

(in-package :postmodern)


(unintern 'postmodern::*direct-column-slot*)


(defclass effective-column-slot (standard-effective-slot-definition)
  ((direct-slot :initarg :direct-slot :reader slot-column)))


(let ((method (find-method #'effective-slot-definition-class nil
                           (mapcar #'find-class '(dao-class))
                           nil)))
  (when method
    (remove-method #'effective-slot-definition-class method)))


(defmethod compute-effective-slot-definition ((class dao-class) name direct-slot-definitions)
  (let ((direct-column-slotd (find-if (lambda (dslotd) (typep dslotd 'direct-column-slot))
                                      direct-slot-definitions)))
    (if direct-column-slotd
        (apply #'make-instance 'effective-column-slot :direct-slot direct-column-slotd
               ;; TODO: closer-mop?
               (sb-pcl::compute-effective-slot-definition-initargs class direct-slot-definitions))
        (call-next-method))))
