;;;; -*- Mode: Lisp -*-

;;;; kitchen-sink.lisp --
;;;; Utilities that are available everywhere, but that I implement
;;;; here to reduce dependencies on other libraries.
;;;;
;;;; See the file COPYING for license and copying information.

(in-package "CLAST")

(defun flatten (l)
  (cond ((null l) l)
        ((atom l) (list l))
        ((consp l)
         (nconc (flatten (first l))
                (flatten (rest l))))
        ))
  

;;;; end of file -- kitchen-sink.lisp --
