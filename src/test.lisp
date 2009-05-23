;;;; http://nostdal.org/ ;;;;

(in-package #:sw-db)

(define-global -generate-repl-output-p- nil)


(defclass location ()
  ((name :col-type string
         :accessor name-of :initarg :name
         :initform ""))
  
  (:metaclass mvc-stm-db-class)
  (:table-name locations))



(defclass person ()
  ((first-name :col-type string
               :accessor first-name-of :initarg :first-name
               :initform "")

   (last-name :col-type string 
              :accessor last-name-of :initarg :last-name
              :initform "")

   (age :col-type integer
        :accessor age-of :initarg :age
        :initform 0)

   (location-id :col-type (or db-null integer) :dao-class location
                :accessor location-of :initarg :location))

  (:metaclass mvc-stm-db-class)
  (:table-name people))



(defclass person-view (view-base)
  ())


(defmethod handle-model-slot-set-event ((view person-view) model event slot-name)
  (when -generate-repl-output-p- 
    (format t "PERSON-VIEW: Update View of ~A based on ~A ~A~%"
            model event slot-name)))



(defclass container-view (view-base)
  ())


(defmethod handle-view-set-object-model ((view container-view) model)
  (when -generate-repl-output-p-
    (format t "CONTAINER-VIEW: initialize View of ~A based on content: ~A~%"
            model ~model)))


(defmethod handle-model-event ((view container-view) model event)
  (when -generate-repl-output-p-
    (format t "CONTAINER-VIEW: update View of ~A based on ~A~%"
            model event)))



(defun test-db-update ()
  (let* ((query-model
          (make-instance 'query
                         :dependencies '(person)
                         :dao-class 'person
                         :query (s-sql:sql (:select 'id :from 'people :where (:> 'age 18)))))
         (result-view (make-instance 'container-view :model query-model))
         (lnostdal (get-db-object 2 'person))
         (person-view (make-instance 'person-view :model lnostdal)))
    (declare (dynamic-extent result-view person-view)
             (ignorable result-view lnostdal person-view))

    (when -generate-repl-output-p- 
      (write-line  "## SQL UPDATE ##")
      (format t "before: ~A~%" (length ~query-model)))
    (setf (age-of lnostdal) 17)
    (save lnostdal)
    (when -generate-repl-output-p-
      (format t "after: ~A~%" (length ~query-model)))
    (setf (age-of lnostdal) 28)
    (save lnostdal)
    (when -generate-repl-output-p- 
      (format t "back to start: ~A~%" (length ~query-model))
      (terpri))

    (when -generate-repl-output-p- 
      (write-line "## SQL INSERT and DELETE ##")
      (format t "before: ~A~%" (length ~query-model)))
    (let ((person (make-instance 'person :first-name "bob" :last-name "uncle" :age 19)))
      (add person (container-of person))
      (when -generate-repl-output-p- 
        (format t "after: ~A~%" (length ~query-model)))
      (remove person (container-of person)))
    (when -generate-repl-output-p- 
      (format t "back to start: ~A~%" (length ~query-model)))))






(defun reset ()
  (with-db-connection
    (ignore-errors (execute (:drop-table 'people)))
    (ignore-errors (execute (:drop-table 'locations)))
    (execute (dao-table-definition 'person))
    (execute (dao-table-definition 'location)))
  
  (put-db-object (make-instance 'location :name "Skien")
                 :cache-p nil)

  (put-db-object (make-instance 'person
                                :first-name "Elin"
                                :last-name "Nøstdal")
                 :cache-p nil)
  (put-db-object (make-instance 'person
                                :location (get-db-object 1 'location)
                                :first-name "lnostdal"
                                :age 28
                                :last-name "Nøstdal")
                 :cache-p nil)
  (put-db-object (make-instance 'person
                                :first-name "Leif Øyvind"
                                :last-name "Nøstdal")
                 :cache-p nil)
  (put-db-object (make-instance 'person
                                :first-name "Tor"
                                :last-name "Nøstdal")
                 :cache-p nil)
  (put-db-object (make-instance 'person
                                :first-name "Lise"
                                :last-name "Nilsen")
                 :cache-p nil))

























#|
(defun test-slot-observer ()
  (let ((person (get-db-object 2 'person)))
    (add-simple-slot-callback 'first-name person
                              (lambda (event)
                                (format t "accessor: ~A, old-value: ~A, new-value: ~A~%"
                                        (first-name-of (instance-of event))
                                        (old-value-of event)
                                        (new-value-of event))))
    ;; Note that this change will not persist unless SAVE is called.
    (setf (first-name-of person) (princ-to-string (random 1000)))
    (first-name-of person)))


(defun test-db-container ()
  (let ((container-view (make-instance 'container-view :model (container-of 'person))))
    (declare (ignore container-view))
    (add (make-instance 'person :first-name "lars")
         (container-of 'person))))


(defun test-db-container-2 ()
         ;; Model.
  (let* ((query-model (make-instance 'query       
                                     :dao-class 'person
                                     :query (s-sql:sql (:select 'id :from 'people :where (:> 'age 18)))))
         ;; View.
         (container-view (make-instance 'container-view :model query-model)))
    (declare (ignore container-view))
    (format t "initial amount of people above 18: ~A~%"
            (length (content-of query-model)))
    (terpri)
    
    (let ((koala (make-instance 'person :first-name "koala_man" :age 25)))
      (add koala (container-of 'person))
      (format t "after adding one more: ~A~%"
              (length (content-of query-model)))
      (terpri)

      (write-line "list all people above 18:")
      (dolist (person (content-of query-model))
        (format t "first-name: ~A~%"
                (first-name-of person)))
      (terpri)

      (remove koala (container-of 'person))
      (format t "after removing the newly added one: ~A~%"
              (length (content-of query-model))))))






;; A widget; presenting a person.
(defclass person-view (view-base)
  ())


;; Code which draws the actual presentation.
(defmethod handle-model-event ((view person-view) (model person) event)
  (format t "generic callback: (handle-model-event ~A ~A ~A), slot-name: ~A~%"
          view model event (slot-name-of event)))


(defun test-slot-observer-2 ()
  (let* ((person (get-db-object 2 'person))                  ;; Model.
         (view  (make-instance 'person-view :model person))) ;; View.
    (add-simple-slot-callback 'last-name person
                              (lambda (event)
                                (format t "specific callback: ~A ~A ~A, slot-name: ~A~%"
                                        view person event (slot-name-of event))))
    (setf (first-name-of person)
          "some new first name")
    (setf (last-name-of person)
          "some new last name")))

|#

