;;;; http://nostdal.org/ ;;;;

(in-package :sw-db)


(eval-now
  (shadow '=common-headers=))

(eval-now
  (define-symbol-macro =common-headers=
      (progn
        (in-readtable sw-db)
        #|(declaim #.(optimizations))|#)))