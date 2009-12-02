;;;; http://nostdal.org/ ;;;;

(in-package :sw-db)
=common-headers=


;; TODO: Describe why I do this in some blog-post or something. Short; this also exports symbols from CL, so users should use the SW-DB package instead of the CL package. The point -- besides me being lazy, is that conflicts can be dealt with _once_ and the result/solution trickles town to sub-sub-sub..-packages.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym)
    (export sym))
  (export '(nil))) ;; CL pitfall..
