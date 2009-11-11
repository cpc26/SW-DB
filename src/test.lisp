;;;; http://nostdal.org/ ;;;;

(in-package sw-db)
(in-readtable sw-db)


(defclass location ()
  ((name :col-type string
         :accessor name-of :initarg :name
         :initform ""))

  (:metaclass db-class)
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

  (:metaclass db-class)
  (:table-name people))



(defclass person-view (view-base)
  ())


(defmethod (setf model-of) ((person person) (person-view person-view))
  λV42)



(defclass container-view (view-base)
  ())


(defmethod (setf model-of) ((query query) (view container-view))
  λI(when-let (event (event-of query))
      (when (eq query (container-of event))
        #|(format t "EVENT: ~A  VIEW: ~A  OBJECTS: ~A~%" event view (objects-of event))|#)))



(defun test-db-update ()
  (with-db-connection
    (with-sync ()
      (with-transaction ()
        #|(execute "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE")|#
        (let* ((query-model (make-instance 'query
                                           :dependencies '(person)
                                           :dao-class 'person
                                           :lisp-query (lambda (dao) (> (age-of dao) 18))
                                           :sql-query (s-sql:sql (:select 'id :from 'people
                                                                          :where (:and (:> 'age 18)
                                                                                       (:not 'gc-p))))))
               (result-view (make-instance 'container-view :model query-model))
               (lnostdal (get-db-object 2 'person))
               (person-view (make-instance 'person-view :model lnostdal)))
          (declare (ignorable result-view person-view))
          (terpri)

          (let ((cl-postgres:*query-log* nil))
            (dbg-prin1 (query "SELECT current_setting('transaction_isolation');"))
            (write-line  "### SQL UPDATE ###")
            (format t "# before: ~A~%~%" (length ~query-model))
            (with-lazy-db-operations
              (setf (age-of lnostdal) 17)
              (format t "# after (lazy): ~A~%~%" (length ~query-model)))
            (format t "# after: ~A~%~%" (length ~query-model))
            (setf (age-of lnostdal) 29)
            (format t "# back to start: ~A~%~%" (length ~query-model)))

          (let ((person (make-instance 'person :first-name "bob" :last-name "uncle" :age 19)))
            (write-line "### SQL INSERT and DELETE ###")
            (format t "# before: ~A~%~%" (length ~query-model))
            (with-lazy-db-operations
              (insert person :in (container-of person)))
            (format t "# after: ~A~%~%" (length ~query-model))
            (with-lazy-db-operations
              (remove person (container-of person)))
            (format t "# back to start: ~A~%~%" (length ~query-model))))))))


(defun test-db-composition ()
  (with-db-connection
    (with-sync ()
      (with-transaction ()
        (let* ((location (make-instance 'location :name "test"))
               (person-1 (make-instance 'person
                                        :first-name "first-name" :last-name "last-name"
                                        :location location))
               (person-2 (make-instance 'person
                                        :first-name "first-name" :last-name "last-name"
                                        :location location)))
          (dbg-prin1 (reference-count-of location))
          (with-lazy-db-operations
            (insert person-1 :in (container-of person-1))
            (insert person-2 :in (container-of person-2)))
          (with-lazy-db-operations
            (remove person-1 (container-of person-1))
            (remove person-2 (container-of person-2)))
          (dbg-prin1 (reference-count-of location))
          )))))


(defun reset ()
  (with-sync ()
    (with-db-connection
      (ignore-errors (execute (:drop-table 'people)))
      (ignore-errors (execute (:drop-table 'locations)))
      (execute (dao-table-definition 'person))
      (execute (dao-table-definition 'location)))

    (let ((location (make-instance 'location :name "Skien")))
      (insert location :in (container-of location))

      (progn
        (insert (list (make-instance 'person
                                     :location location
                                     :first-name "Elin"
                                     :last-name "Nøstdal")
                      (make-instance 'person
                                     :location location
                                     :first-name "Lars Rune"
                                     :last-name "Nøstdal"
                                     :age 29)
                      (make-instance 'person
                                     :location location
                                     :first-name "Leif Øyvind"
                                     :last-name "Nøstdal")
                      (make-instance 'person
                                     :location location
                                     :first-name "Tor"
                                     :last-name "Nøstdal")
                      (make-instance 'person
                                     :location location
                                     :first-name "Lise"
                                     :last-name "Nilsen"))
                :in (container-of 'person))))))





















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
