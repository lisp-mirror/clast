;;;; -*- Mode: Lisp -*-

;;;; parse-defs-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-defs)

(test defvar
  ;; GIVEN: a DEFVAR definition
  (let ((input '(defvar foo 42 "documentation")))
    ;; WHEN: the definition is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: name, value and documentation of the variable are
      ;; recorded correctly and the environment is augmented with
      ;; appropriate information
      (is (eql 'foo
	       (clast::defvar-form-name element)))
      (is (eql 'clast:constant-form
	       (type-of (clast::defvar-form-value element))))
      (is (eql "documentation"
	       (clast::defvar-form-doc-string element)))
      (is (eql :special
	       (clast:variable-information 'foo environment)))
      )))


(test defparameter
  ;; GIVEN: a DEFPARAMETER definition
  (let ((input '(defparameter foo 42 "documentation")))
    ;; WHEN: the definition is parsed
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: name, value and documentation of the variable are
      ;; recorded correctly and the environment is augmented with
      ;; appropriate information
      (is (eql 'foo
	       (clast::defparameter-form-name element)))
      (is (eql 'clast:constant-form
	       (type-of (clast::defparameter-form-value element))))
      (is (eql "documentation"
	       (clast::defparameter-form-doc-string element)))
      (is (eql :special
	       (clast:variable-information 'foo environment)))
      )))


(test defun
  ;; GIVEN: the identity function
  (let ((input
	 '(defun id (x) x)))
    ;; WHEN: the function is parsed
    (multiple-value-bind (element env)
	(clast:parse input)
      ;; THEN: parsing returns a CLAST-ELEMENT instance of the
      ;; appropriate type ...
      (is (eql 'defun-form (type-of element)))
      ;; ... the environment is correctly augmented
      (is (clast:function-information 'id env))
      (let ((name
	     (clast::defun-form-name element))
	    (lambda-list
	     (clast::defun-form-lambda-list element))
	    (body-env
	     (clast::form-body-env element))
	    (progn-forms
	     (clast::form-progn element)))
	;; ... the function name is correctly noted
	(is (eql 'id name))
	;; ... as its lambda list
	(is (eql 'clast::ordinary-lambda-list (type-of lambda-list)))
	;; ... its body environment
	(is (eql :lexical (clast:variable-information 'x body-env)))
	(is (eql :function (clast:function-information 'id body-env)))
	(print (function-information 'id body-env))
	;; ... and subforms
	(is (eql 'clast::block-form (type-of progn-forms)))
	;; TODO: Related types are not recorded correctly on SBCL. The
	;; correct code is in place but an internal crash prevents this
	;; from working. The test defun-api
	))))


(test defun-types
  ;; GIVEN: a function with a type declarations
  (let* ((input
	  '(defun fixnum-id (id)
	    (declare (type fixnum x))
	    x))
	 ;; WHEN: the function is parsed
	 (output
	  (clast:parse input)))
    ;; THEN: types are correctly recorded
    (declare (ignore output))
    (fiveam:fail)))


(test defmacro
  ;; GIVEN: the identity macro
  (let ((input
	 '(defmacro id (x) x)))
    ;; WHEN: the macro is parsed
    (multiple-value-bind (element env)
	(clast:parse input)
      ;; THEN: parsing returns a CLAST-ELEMENT instance of the
      ;; appropriate type ...
      (is (eql 'clast::defmacro-form (type-of element)))
      ;; ... the environment is correctly augmented
      (is (eql :macro (clast:function-information 'id env)))
      ;; ... as its lambda list, body environment and subforms
      (let ((name
	     (clast::defmacro-form-name element))
	    (lambda-list
	     (clast::defmacro-form-lambda-list element))
	    (body-env
	     (clast::form-body-env element))
	    (progn-forms
	     (clast::form-progn element)))
	;; ... the macro name is correctly noted
	(is (eql 'id name))
	;; ... as its lambda list
	(is (eql 'clast::macro-lambda-list (type-of lambda-list)))
	;; ... its body environment
	(is (eql :lexical (clast:variable-information 'x body-env)))
	(is (eql :macro (clast:function-information 'id body-env)))
	;; ... and subforms
	(is (eql 'clast::block-form (type-of progn-forms)))
	))))


;;;; end of file -- parse-defs-tests.lisp --
