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


(defun ensure-lists (l)
  (declare (type list l))
  (mapcar #'(lambda (e) (if (listp e) e (list e))) l))


(defun generic-function-p (x)
  (typep x 'generic-function))


(defun is-generic-function (x)
  (typep x 'generic-function))


(defun compose2 (f g)
  #'(lambda (x)
      (funcall f (funcall g x))))


(defun compose (&optional (f1 #'identity) &rest funs)
  "Return a function that is the composition of several ones. 

If no arguments are passed to COMPOSE the result is essentially
IDENTITY."
  (declare (type function f1))

  (if funs
      #'(lambda (&rest args)
          (funcall f1 (apply (apply #'compose funs) args)))
      #'(lambda (&rest args)
          (apply f1 args))))


;;;; end of file -- kitchen-sink.lisp --
