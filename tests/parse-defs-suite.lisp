;;;; -*- Mode: Lisp -*-

;;;; parse-defs-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-defs)

(test defun
  ;; GIVEN: the identity function
  (let ((input
	 '(defun id (x) x)))
    ;; WHEN: the function is parsed
    (multiple-value-bind (element env)
	(clast:parse input)
      ;; THEN: parsing returns a CLAST-ELEMENT instance of the
      ;; appropriate type and the environment is correctly augmented
      (is (eql 'defun-form (type-of element)))
      (is (clast:function-information 'id env))
      )))


(test defun-name
  ;; GIVEN: the identity function
  (let* ((input
	  '(defun id (x) x))
	 ;; WHEN: the function is parsed
	 (output
	  (clast:parse input)))
    ;; THEN: the function name is correctly noted
    (is (eql 'id (clast::defun-form-name output)))
    ))


(test defun-lambda-list
  ;; GIVEN: the identity function
  (let* ((input
	  '(defun id (x) x))
	 ;; WHEN: the function is parsed
	 (output
	  (clast:parse input))
	 ;; THEN: its lambda list is correctly parsed
	 (lambda-list
	  (clast::defun-form-lambda-list output)))
    (is (eql 'clast::ordinary-lambda-list (type-of lambda-list)))
    ))


(test defun-body-env
  ;; GIVEN: the identity function
  (let* ((input
	  '(defun id (x) x))
	 ;; WHEN: the function is parsed
	 (output
	  (clast:parse input))
	 ;; THEN: the body environement of the function is correctly
	 ;; augmented
	 (body-env
	  (clast::form-body-env output)))
    (is (eql :lexical (clast:variable-information 'x body-env)))
    ))


(test defun-progn
  ;; GIVEN: the identity function
  (let* ((input
	  '(defun id (x) x))
	 ;; WHEN: the function is parsed
	 (output
	  (clast:parse input))
	 (progn-forms
	  (clast::form-progn output)))
    ;; THEN: the implicitly progned forms are correctly recorded
    (is (eql 'clast::block-form (type-of progn-forms)))
    ))


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


;;;; end of file -- parse-defs-tests.lisp --
