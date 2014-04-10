;;;; -*- Mode: Lisp -*-

;;;; kitchen-sink.lisp --
;;;; Utilities that are available everywhere, but that I would make my
;;;; library dependent on other libraries (e.g., Alexandria).

(in-package "CLAST")

(defun flatten (l)
  (cond ((null l) l)
        ((atom l) (list l))
        ((consp l)
         (nconc (flatten (first l))
                (flatten (rest l))))
        ))
  

;;;; end of file -- kitchen-sink.lisp --
