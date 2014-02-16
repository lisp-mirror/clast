;;;; -*- Mode: Lisp -*-

;;;; clast-allegro.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; See file COPYING for copyright and license information.


(in-package "CLAST")

;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.
;;;;
;;;; Franz redefined the order of the return values.  Hence this
;;;; implementation wrapper is more complicated.

(defmacro reorder-*information-values-portably (env-function-form)
  `(multiple-value-bind (binding-type
                         locative-cons
                         info
                         local-or-global-p)
       ,env-function-form
     (values binding-type
             local-or-global-p
             info
             locative-cons ; This will never be used in portable code,
                           ; but it is nice to provide it.
             )
     ))


(defun variable-information (variable &optional env)
  (reorder-*information-values-portably
   (sys:variable-information f env t))
  )


(defun function-information (f &optional env)
  (reorder-*information-values-portably
   (sys:function-information f env t t))
  )


(defun declaration-information (decl-name &optional env)
  (reorder-*information-values-portably
   (sys:declaration-information decl-name env))
  )


(defun augment-environment (env &rest all-keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                declare
                                &allow-other-keys)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'sys:augment-environment env all-keys)
  )


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(sys:define-declaration ,decl-name ,lambda-list ,@forms)
  )


(defun parse-macro (name lambda-list body &optional env)
  (error "No implementation for CLtL2 PARSE-MACRO in Allegro CL.")
  )


(defun enclose (lambda-expression &optional env)
  (error "No implementation for CLtL2 ENCLOSE in Allegro CL.")
  )


;;;; end of file -- clast-allegro.lisp --
