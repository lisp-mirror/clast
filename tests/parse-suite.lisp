;;;; -*- Mode: Lisp -*-

;;;; parse-tests.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package :clast-tests)

(in-suite :parse-base)

(test number
  ;; GIVEN: a number
  (let* ((input 9)
	 ;; WHEN: the number is parsed
	 (output
	  (clast:parse input))
	 (top
	  (clast::form-top output))
	 (source
	  (clast::form-source output))
	 (type
	  (first (clast::form-type output))))
    ;; THEN: a CLAST-ELEMENT of appropriate type is returned and the
    ;; string value is recorded correctly
    (is (eql 'clast:constant-form (type-of output)))
    (is (eql 9 (clast::form-value output)))
    ;; .. as is top form, its source and its type
    (is (eql nil top))
    (is (eql 9 source))
    (is (eql 'integer type))
    ))


(test string
  ;; GIVEN: a string
  (let* ((input "something")
	 ;; WHEN: the string is parsed
	 (output
	  (clast:parse input))
	 (top
	  (clast::form-top output))
	 (source
	  (clast::form-source output))
	 (type
	  (first (clast::form-type output))))
    ;; THEN: a CLAST-ELEMENT of appropriate type is returned and the
    ;; string value is recorded correctly
    (is (eql 'clast:constant-form (type-of output)))
    (is (string= "something" (clast::form-value output)))
    ;; .. as is top form, its source and its type
    (is (eql nil top))
    (is (string= "something" source))
    (is (eql 'simple-base-string type))
    ))


(test lambda-application
  ;; GIVEN:
  (let* ((input '((lambda (x) x) 9))
	 ;; WHEN:
	 (output
	  (clast:parse input))
	 (operator
	  (clast::form-operator output))
	 (args
	  (clast::form-args output))
	 (arg
	  (first args)))
    ;; THEN: 
    (is (eql 'clast::lambda-application (type-of output)))
    (is (eql 1 (length args)))
    (is (eql 'clast::constant-form (type-of arg)))
    (is (eql 'clast::lambda-form (type-of operator)))
    ))

;; TODO: Add FUNCTIONAL-OPERATOR-APPLICATION-FORM test. Here is a test
;; case:
;;
;; ((block name (+ 1 1)))

(test block-form
  ;; GIVEN:
  (let* ((input
	  '(block name (+ 1 1)))
	 ;; WHEN:
	 (output
	  (clast:parse input))
	 (name
	  (clast::block-name output))
	 (body
	  (clast::form-body output)))
    ;; THEN: 
    (is (eql 'clast::block-form (type-of output)))
    (is (eql 'name name))
    (is (eql 1 (length body)))
    ;; TODO: Once the code for adding a lexical tag to the environment
    ;; you should also test that the tag is added correctly.
    ))


(test return-from
  ;; GIVEN:
  (let* ((input
	  '(block stop (return-from stop 9)))
	 ;; WHEN:
	 (output
	  (clast:parse input))
	 (body
	  (clast::form-body output))
	 (return-from-form
	  (first body))
	 (return-from-name
	  (clast::form-name return-from-form))
	 (return-from-result-form
	  (clast::form-result return-from-form))
	 (return-from-result-value
	  (clast::form-value return-from-result-form))
	 ;; (return-from-enclosing-block
	 ;;  (clast::form-enclosing-block return-from-form))
	 )
    ;; THEN:
    (is (eql 'clast::return-from-form (type-of return-from-form)))
    (is (eql 'stop return-from-name))
    (is (eql 'clast:constant-form (type-of return-from-result-form)))
    (is (eql 9 return-from-result-value))
    ))


;; TODO: Add TAGBODY (form-body form-tags) and GO (form-name
;; enclosing-tagbody) tests after tags information handling in
;; environments is fixed. Here is a test case:
;;
;; (tagbody (go second)
;;  first (print "error")
;;  second (print "ok")))


;; TODO: Once parsing of THROW forms is added, define a test case for
;; both CATCH (form-catch-tag) and THROW. Here is a test a case:
;;
;; (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4)


;; TODO: DECLARE is really heavy to test because it requires testing
;; of all possible declaration specifiers (SYMBOL TYPE FTYPE IGNORE
;; IGNORABLE OPTIMIZE INLINE NOT-INLINE SPECIAL DYNAMIC-EXTENT
;; DECLARATION)

(test progn
  ;; GIVEN:
  (let ((input '(progn
		 (defun id (x) x)
		 (id 9))))
    ;; WHEN:
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: 
      (is (eql 'clast::progn-form (type-of element)))
      ;; FIXME: Both the environment that gets returned from a call to
      ;; PARSE and the body environment associated with the element
      ;; that gets returned do not get augmented with the definitions
      ;; that occur within the PROGN. This is wrong and should be
      ;; fixed.
      (is (eql :function (function-information 'id environment)))
      ;; ...
      (let ((body (clast::form-body element))
	    (body-env (clast::form-body-env element)))
	(is (eql 2 (length body)))
	(is (eql :function (function-information 'id body-env)))
	))))


;; TODO: Add PROGV when its implementation is complete


;; TODO: Add PROGV when its implementation is complete


;; TODO: Deferring testing of PROG and PROG* because they
;; significantly complex.


(test eval-when
  ;; GIVEN:
  (let* ((input
	  '(eval-when (:compile-toplevel :load-toplevel :execute)
	    (defun id (x) x)
	    (print "something")))
	 ;; WHEN:
	 (output
	  (clast:parse input))
	 (situations
	  (clast::form-situations output))
	 (body
	  (clast::form-body output))
	 (body-env
	  (clast::form-body-env output)))
    ;; THEN: 
    (is (eql 'eval-when-form (type-of output)))
    (is (equal situations
     	       (list :compile-toplevel :load-toplevel :execute)))
    ;; ...
    (is (eql 2 (length body)))
    ;; FIXME: The current implementation does not augment the body
    ;; environment while parsing its body forms.
    (is (eql :function (function-information 'id body-env)))
    ;; TODO: when a strategy for adding elements to the returned
    ;; environment is decided, update this test to reflect that.
    ))


(test declaim
  ;; GIVEN: 
  (let ((input
	 '(declaim (special first) (special second))))
					;: WHEN: 
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: 
      (is (eql :special
	       (variable-information 'var environment)))
      (let* ((declarations
	      (clast::declaration-form-declarations element))
	     (declaration
	      (first declarations))
	     (specifier-identifier
	      (clast::declaration-specifier-identifier declaration))
	     ;; FIXME: The resulting environment slot is never
	     ;; initialized nor considered during parsing. This causes
	     ;; this test failure.
	     (resulting-environment
	      (clast::declaration-form-resulting-environment element)))
	;; ...
	(is (eql 2
		 (length declarations)))
	(is (eql 'special
		 specifier-identifier))
	(is (eql :special
		 (variable-information 'var resulting-environment)))
	))))


(test flet
  ;; GIVEN: 
  (let ((input
	 '(flet ((local-id (x) x))
	   (local-id 9)
	   (defun id (x) x))))
    ;; WHEN: 
    (multiple-value-bind (element environment)
	(clast:parse input)
      ;; THEN: an element of the appropriate type should be returned
      (is (eql 'clast::flet-form (type-of element)))
      ;; ... the functions that were defined locally should not be in the
      ;; return environment (FIXME)
      (is (eql nil (function-information 'local-id environment)))
      ;; ... but progned definitions should
      (is (eql :function (function-information 'id environment)))
      (let ((binds
	     (clast::form-binds element))
	    (body
	     (clast::form-body element))
	    (body-env
	     (clast::form-body-env element)))
	;; ... body forms should be recorded correctly
	(is (eql 2 (length body)))
	;; ... as bindings, that should also be added to the body
	;; environment
	(is (eql :function (function-information 'local-id body-env)))
	))))

;; LABELS is equivalent to FLET except that the scope of the defined
;; function names for LABELS encompasses the function definitions
;; themselves as well as the body. Since this difference is not
;; recorded by the library fttb the test is the exact same.
(test labels
  (let ((input
	 '(labels ((local-id (x) x))
	   (local-id 9)
	   (defun id (x) x))))
    (multiple-value-bind (element environment)
	(clast:parse input)
      (is (eql 'clast::labels-form (type-of element)))
      ;; ... the functions that were defined locally should not be in the
      ;; return environment (FIXME)
      (is (eql nil (function-information 'local-id environment)))
      (is (eql :function (function-information 'id environment)))
      (let ((binds (clast::form-binds element))
	    (body (clast::form-body element))
	    (body-env (clast::form-body-env element)))
	(is (eql 2 (length body)))
	(is (eql :function (function-information 'local-id body-env)))
	))))


;; TODO: Add FUNCTION parsing tests once its implementation is
;; complete.

;; LAMBDA-FORM (form-lambda-list form-function body body-env)

;; FIXME: the PARSE method that specializes on LAMBDA delegates work
;; to PARSE-LAMBDA-FORM, which does not return a tuple (CLAST-ELEMENT,
;; ENVIRONMENT) but just a CLAST-ELEMENT.

;;;; end of file -- parse-tests.lisp --
