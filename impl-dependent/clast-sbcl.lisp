;;;; -*- Mode: Lisp -*-

;;;; clast-sbcl.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; See file COPYING for copyright and license information.


(in-package "CLAST")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :sb-cltl2))


;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.

(defun variable-information (variable &optional env)
  (sb-cltl2:variable-information variable env)
  )


(defun function-information (f &optional env)
  (sb-cltl2:function-information f env)
  )


(defun declaration-information (decl-name &optional env)
  (sb-cltl2:function-information decl-name env)
  )


(defun augment-environment (env &rest keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                declare)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'sb-cltl2:augment-environment env keys)
  )


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(sb-cltl2:define-declaration ,decl-name ,lambda-list ,@forms)
  )


(defun parse-macro (name lambda-list body &optional env)
  (sb-cltl2:parse-macro name lambda-list body env)
  )


(defun enclose (lambda-expression &optional env)
  (sb-cltl2:enclose lambda-expression env)
  )


;;;; end of file -- clast-sbcl.lisp --
