;;;; -*- Mode: Lisp -*-

;;;; clast-parse-protocol.lisp --
;;;; CLAST exports one main function that "parses" CL code into an AST
;;;; tree.  The "analysis functions" "traverse" (or "walk, or "visit")
;;;; the resulting AST.
;;;;
;;;; This files contains the main generic functions mentioned above.
;;;;
;;;; Parsing is CLOS based.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Protocol

;;;;---------------------------------------------------------------------------
;;;; PARSE, PARSE-FORM

(defgeneric parse (form &rest keys
                        &key
                        enclosing-form
                        macroexpand
                        environment
                        &allow-other-keys)
  (:documentation "Parses a form in a given 'environment'.

The methods of this generic function return a AST 'node' (a CLAST-ELEMENT) 
and the - possibly modified - ENVIRONMENT.

The methods of PARSE dispatch on 'atomic' and on 'compound' (i.e.,
CONS) forms.  Atomic forms - numbers, string, arrays, and symbols -
are dealt with directly.  Compound forms are dispatched to PARSE-FORM.

Arguments and Values:

form : the form to be parsed.
keys : the collection of key-value pairs passed to the call.
enclosing-form : the form that \"contains\" the form beling parsed.
environment : the environment in which the form is being parsed; it
              defaults to *CL-GLOBAL-ENV*
element : a CLAST-ELEMENT representing the AST node just parsed.
environment1 : the environment resulting from parsing the FORM.

See Also:

*CL-GLOBAL-ENV*")
  )


(defgeneric parse-form (op form &rest keys
                        &key
                        enclosing-form
                        macroexpand
                        environment
                        &allow-other-keys)
  (:documentation "Parses a form in a given 'ENVIRONMENT' given its 'op'.

The methods of PARSE-FORM descend recursively in a form, by
dispatching on the form 'operator'.  Each sub-form is passed,
recursively to PARSE.

Arguments and Values:

form : the form to be parsed.
keys : the collection of key-value pairs passed to the call.
enclosing-form : the form that \"contains\" the form beling parsed.
environment : the environment in which the form is being parsed; it
              defaults to *CL-GLOBAL-ENV*
element : a CLAST-ELEMENT representing the AST node just parsed.
environment1 : the environment resulting from parsing the FORM.

See Also:

*CL-GLOBAL-ENV*, PARSE")
  )


;;;;---------------------------------------------------------------------------
;;;; Conditions

(define-condition ast-parse-error (parse-error simple-error)
  ()

  (:default-initargs
   :format-control ""
   :format-arguments ())

  (:report
   (lambda (ape stream)
     (format stream
             "CLAST: ~?"
             (simple-condition-format-control ape)
             (simple-condition-format-arguments ape))))
  )


(define-condition unknown-operator-error (ast-parse-error)
  ((op :reader unknown-operator-name
       :initarg :name))
  (:report
   (lambda (usoe stream)
     (format stream "CLAST: Unknownw operator ~S is not handled."
             (unknown-operator-name usoe))))
  )


(define-condition unknown-special-operator-error (unknown-operator-error)
  ()
  (:report
   (lambda (usoe stream)
     (format stream "CLAST: Special form ~S not handled."
             (unknown-operator-name usoe))))
  )

;;;; end of file -- clast-parse-protocol.lisp --
