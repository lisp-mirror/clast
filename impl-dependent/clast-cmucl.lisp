;;;; -*- Mode: Lisp -*-

;;;; clast-cmucl.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

(in-package "CLAST")

;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.

(defun variable-information (variable &optional env)
  (ext:variable-information variable env)
  )


(defun function-information (f &optional env)
  (ext:function-information f env)
  )


(defun declaration-information (decl-name &optional env)
  (ext:function-information decl-name env)
  )


(defun block-information (block-name
                          &optional
                          (env *lispworks-parsing-env*))
  (typecase env
    (parsing-environment
     (env-find-block block-name env))
    (t (values nil nil nil))) ; Just to reiterate...
  )


(defun tag-information (tag-name
                        &optional
                        (env *lispworks-parsing-env*))
  (typecase env
    (parsing-environment
     (env-find-tag tag-name env))
    (t (values nil nil nil))) ; Just to reiterate...
  )


(defun augment-environment (env &rest keys ; Just a utility variable.
                                &key
				variable
				symbol-macro
                                function
                                macro
                                declare
			        &allow-other-keys)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'ext:augment-environment env keys)
  )


(defmacro define-declaration (decl-name lambda-list &body forms)
  `(ext:define-declaration ,decl-name ,lambda-list ,@forms)
  )


(defun parse-macro (name lambda-list body &optional env)
  (ext:parse-macro name lambda-list body env)
  )


(defun enclose (lambda-expression &optional env)
  (ext:enclose lambda-expression env)
  )


;;;; end of file -- clast-cmucl.lisp --
