;;;; -*- Mode: Lisp -*-

;;;; clast-lispworks.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

(in-package "CLAST")

;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.

(defun variable-information (variable &optional env)
  (hcl:variable-information variable env)
  )


(defun function-information (f &optional env)
  (hcl:function-information f env)
  )


(defun declaration-information (decl-name &optional env)
  (hcl:function-information decl-name env)
  )


(defun augment-environment (env &rest keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                declare)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'hcl:augment-environment env keys)
  )


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(hcl:define-declaration ,decl-name ,lambda-list ,@forms)
  )


(defun parse-macro (name lambda-list body &optional env)
  (hcl:parse-macro name lambda-list body env)
  )


(defun enclose (lambda-expression &optional env)
  (hcl:enclose lambda-expression env)
  )


;;;; end of file -- clast-lispworks.lisp --
