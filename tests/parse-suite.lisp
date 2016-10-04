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
	  
;; TODO: Add FUNCTIONAL-OPERATOR-APPLICATION-FORM test
;;
;; '((block name (+ 1 1)))

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
	 ; (return-from-enclosing-block
	 ;  (clast::form-enclosing-block return-from-form))
	 )
    ;; THEN:
    (is (eql 'clast::return-from-form (type-of return-from-form)))
    (is (eql 'stop return-from-name))
    (is (eql 'clast:constant-form (type-of return-from-result-form)))
    (is (eql 9 return-from-result-value))
    ; (is (eql  ))
    ))


;; TODO: TAGBODY form-body form-tags GO form-name enclosing-tagbody

(test tagbody-go
  ;; GIVEN: 
  (let* ((input '(tagbody (go second)
		  first (print "error")
		  second (print "ok")))
	 ;; WHEN: 
	 (output (clast:parse input)))
    ;; THEN
    ))

;; CATCH form-catch-tag
;; DECLARATION
;; PROGN
;; PROGV
;; PROG
;; PROG*
;; EVAL-WHEN
;; DECLAIM
;; FLET
;; LABELS
;; FUNCTION
;; LAMBDA
;; IF
;; COND
;; CASE CCASE ECASE TYPECASE ETYPECASE CTYPECASE
;; LET
;; LET*
;; MACROLET
;; MULTIPLE-VALUE-BIND
;; QUOTE
;; THE
;; SETQ
;; SETF
;; DOLIST
;; DOTIMES
;; DO
;; DECLARATION: SYMBOL TYPE FTYPE IGNORE IGNORABLE OPTIMIZE INLINE
;;              NOT-INLINE SPECIAL DYNAMIC-EXTENT DECLARATION


;; (test 
;;   ;; GIVEN:
;;   (let* ((input '())
;; 	 ;; WHEN:
;; 	 (output (clast:parse input))
;; 	 ( (clast:: output)))
;; 	 )
;;     ;; THEN: 
;;     (is (eql  ))
;;     ))


;; (test 
;;   ;; GIVEN:
;;   (let* ((input '(())
;; 	 ;; WHEN:
;; 	 (output (clast:parse input))
;; 	 ( (clast:: output)))
;; 	 )
;;     ;; THEN: 
;;     (is (eql  ))
;;     ))

;;;; end of file -- parse-tests.lisp --
