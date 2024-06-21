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
      (is (string= "documentation"
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
      (is (string= "documentation"
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

      (is (eq 'clast::defun-form (type-of element)))
      ;; ... the (global) environment is correctly augmented.

      (is-true (clast:function-information 'id env))
      (let ((name
	     (clast::defun-form-name element))
	    (lambda-list
	     (clast::defun-form-lambda-list element))
	    (body-env
	     (clast::form-body-env element))
	    (progn-forms
	     (clast::form-progn element)))

	;; ... the function name is correctly noted
	(is (eq 'id name))

	;; ... as lambda list
	(is (eq 'clast::ordinary-lambda-list (type-of lambda-list)))
	;; ... its body environment

	(is (eq :lexical (clast:variable-information 'x body-env)))
	(is (eq :function (clast:function-information 'id body-env)))

	;; ... and subforms
	(is (typep progn-forms 'clast::block-form))

	;; TODO: Related types are not recorded correctly on SBCL. The
	;; correct code is in place but an internal crash prevents this
	;; from working.
	))))


(test defun-types

  ;; GIVEN: a function with a type declarations.

  (let* ((input
	  '(defun fixnum-id (x)
             (declare (type fixnum x))
             x))
	 ;; WHEN: the function is parsed
	 (output
	  (clast:parse input))
         )

    ;; THEN: types are correctly recorded

    (declare (ignore output))

    ;; This needs to be fixed.

    (fiveam:fail "Test not yet implemented."))
  )


(test defmacro
  ;; GIVEN: the identity macro

  (let ((input
	 '(defmacro id (x) x)))

    ;; WHEN: the macro is parsed

    (multiple-value-bind (element env)
	(clast:parse input)

      ;; THEN: parsing returns a CLAST-ELEMENT instance of the
      ;; appropriate type ...

      (is (typep element 'clast::defmacro-form))

      ;; ... the environment is correctly augmented
      (is (eq :macro (clast:function-information 'id env)))

      (let ((name
	     (clast::defmacro-form-name element))
	    (lambda-list
	     (clast::defmacro-form-lambda-list element))
	    (body-env
	     (clast::form-body-env element))
	    (progn-forms
	     (clast::form-progn element))
            )
	;; ... the macro name is correctly noted
	(is (eq 'id name))

	;; ... as the lambda list
	(is (typep lambda-list 'clast::macro-lambda-list))

	;; ... its body environment
	(is (eq :lexical (clast:variable-information 'x body-env)))
	(is (eq :macro (clast:function-information 'id body-env)))

	;; ... and subforms
	(is (typep progn-forms 'clast::block-form))
	))))


(test defgeneric
  ;; GIVEN: a generic function declartion
  (let ((input
	 '(defgeneric id (x)
	   (:method ((x fixnum)) x))))
    ;; WHEN: the declaration is parsed
    (multiple-value-bind (element env)
	(clast:parse input)
      ;; THEN: parsing returns a CLAST-ELEMENT instance of the
      ;; appropriate type ...
      (is (eq 'clast::defgeneric-form (type-of element)))
      ;; ... the environment is correctly augmented
      (is-true (clast:function-information 'id env))
      (let ((name
	     (clast::defgeneric-form-name element))
	    (lambda-list
	     (clast::defgeneric-form-lambda-list element))
	    (methods
	     (clast::defgeneric-form-methods element)))
	;; ... the function name is correctly noted
	(is (eq 'id name))
        ;; ... as its lambda list
	(is (eq 'clast::specialized-lambda-list (type-of
                                                 lambda-list)))
	;;... its methods
        (is (= 1 (length methods)))
	;; TODO: the body environment and implicit progned forms are
	;; not added to the element during parsing at the moment.
	))))


(test define-compile-macro-form

  ;; GIVEN: a compiler macro that does nothing.

  (let ((input
	 '(define-compiler-macro id (x) x)))

    ;; WHEN: the macro is parsed.

    (multiple-value-bind (element env)
	(clast:parse input)

      ;; THEN: parsing returns a CLAST-ELEMENT instance of the
      ;; appropriate type ...

      (is (typep element 'clast::define-compiler-macro-form))

      ;; ... the environment is correctly augmented...
      (is (eq :macro (clast:function-information 'id env)))

      (let ((name
	     (clast::define-compiler-macro-form-name element))
	    (lambda-list
	     (clast::defmacro-form-lambda-list element))
	    (body-env
	     (clast::form-body-env element))
	    (progn-forms
	     (clast::form-progn element)))

	;; ... the macro name is correctly noted
	(is (eq 'id name))
	;; ... as the lambda list ...
	(is (typep lambda-list 'clast::macro-lambda-list))
	;; ... its body environment
	(is (eq :lexical (clast:variable-information 'x body-env)))
	(is (eq :macro (clast:function-information 'id body-env)))
	;; ... and subforms
	(is (typep progn-forms 'clast::block-form))
	))))


;;;; end of file -- parse-defs-tests.lisp --
