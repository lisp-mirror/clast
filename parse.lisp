;;;; -*- Mode: Lisp -*-

;;;; parse.lisp --
;;;; CLAST exports one main function that "parses" CL code into an AST
;;;; tree.  The "analysis functions" "traverse" (or "walk, or "visit")
;;;; the resulting AST.
;;;;
;;;; Parsing is CLOS based.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")

;;;;===========================================================================
;;;; Interface

;;;;---------------------------------------------------------------------------
;;;; PARSE, PARSE-FORM

(defgeneric parse (form &rest keys
                        &key
                        enclosing-form
                        macroexpand
                        environment
                        &allow-other-keys)
  (:documentation "Parses a form with respect to a given 'environment'.

The methods of this generic function return a AST 'node' (a CLAST-ELEMENT) 
and the - possibly modified - ENVIRONMENT.")
  )


(defgeneric parse-form (op form &rest keys
                        &key
                        enclosing-form
                        macroexpand
                        environment
                        &allow-other-keys)
  )


;;;;---------------------------------------------------------------------------
;;;; Conditions

(define-condition ast-parse-error (parse-error)
  ()
  )


(define-condition unknown-operator-error (ast-parse-error)
  ((op :reader unknown-operator-name
       :initarg :name))
  (:report
   (lambda (usoe stream)
     (format stream "Unknownw operator ~S is not handled."
             (unknown-operator-name usoe))))
  )


(define-condition unknown-special-operator-error (unknown-operator-error)
  ()
  (:report
   (lambda (usoe stream)
     (format stream "Special form ~S not handled."
             (unknown-operator-name usoe))))
  )


;;;;===========================================================================
;;;; Implementation

(declaim (inline operator arguments)
         (ftype (function (cons) t) operator)
         (ftype (function (cons) cons) arguments)
         )


(defun operator (form)
  (declare (type cons form))
  (first form))


(defun arguments (form)
  (declare (type cons form))
  (rest form))


(eval-when (:load-toplevel :compile-toplevel :execute)

(defmacro def-parse-self-evaluating-method (self-evaluating-type)
  `(defmethod parse ((form ,self-evaluating-type)
                     &rest keys
                     &key
                     environment
                     macroexpand
                     enclosing-form
                     &allow-other-keys)
     (declare (ignore keys))
     (values (make-instance 'constant-form
                            :type (type-of form)
                            :source form
                            :top enclosing-form
                            :value form)
             environment)))
)


#|
(defmethod parse ((form number)
                  &key
                  environment
                  enclosing-form
                  &allow-other-keys)
  (values (make-instance 'constant-form
                         :type (type-of form)
                         :source form
                         :top enclosing-form
                         :value form)
          environment))
|#

(def-parse-self-evaluating-method number)
(def-parse-self-evaluating-method string)
(def-parse-self-evaluating-method array)


(defun build-variable-reference (v kind local-p decls
                                   &optional
                                   enclosing-form
                                   environment)
  (declare (type symbol v)
           (type boolean local-p)
           (type (member :special :lexical nil) kind)
           )
  (let* ((var-decl-type (assoc 'type decls :test #'eq))
         (var-type (if var-decl-type
                       (cdr var-decl-type)
                       t))
         )
    (values
     ;; Value 1
     (ecase kind
       (:special (make-instance 'special-variable-ref
                                :symbol v
                                :local local-p
                                :source v
                                :top enclosing-form
                                :type var-type))
       (:lexical (make-instance 'variable-ref
                                :symbol v
                                :local local-p
                                :source v
                                :top enclosing-form
                                :type var-type))
       ((nil) (make-instance 'free-variable-ref
                             :symbol v
                             :local (assert (null local-p)) ; This'd better be NIL.
                             :source v
                             :top enclosing-form
                             :type var-type))
       )

     ;; Value 2
     environment)
    ))


(defun build-constant-reference (v kind local-p decls
                                   &optional
                                   enclosing-form
                                   environment)
  (declare (type symbol v)
           (type boolean local-p)
           (type (member :constant) kind)
           )
  (let* ((var-decl-type (assoc 'type decls :test #'eq))
         (var-type (if var-decl-type
                       (cdr var-decl-type)
                       (type-of (symbol-value v))))
         )
    (values
     (make-instance 'constant-ref
                    :symbol v
                    :value (symbol-value v)
                    :local local-p ; Also this'd better be NIL.
                    :source v
                    :top enclosing-form
                    :type var-type)
     environment)
    ))


(defmethod parse ((s symbol)
                  &rest keys
                  &key
                  environment
                  macroexpand
                  enclosing-form
                  &allow-other-keys)
  (multiple-value-bind (kind local-p decls)
      (variable-information s environment)
    (case kind
      ((:special :lexical nil)
        (build-variable-reference s kind local-p decls enclosing-form environment))
      
      (:constant
       (build-constant-reference s kind local-p decls enclosing-form environment))
      
      (:symbol-macro
       (if macroexpand ; CHANGE THIS to keep on macroexpanding and
                       ; save the macroexpansion
           (apply #'parse (macroexpand-1 s) keys)
           (values
            (make-instance 'symbol-macro-ref
                           :symbol s
                           :local local-p
                           :source s
                           :top enclosing-form
                           :type t)
            environment)))
      (:otherwise
       (error "Unrecognized (implementation dependent) variable kind ~S."
              kind)
       )
      )))


(defmethod parse ((form cons)
                  &rest keys
                  &key
                  environment
                  enclosing-form
                  &allow-other-keys)
  (if (constantp form environment)
      (values (make-instance 'constant-form
                             :type (type-of form)
                             :source form
                             :top enclosing-form
                             :value form)
              environment)
      (apply #'parse-form (operator form) form keys)
      ))


;;;; parse-form --

(defmethod parse-form ((op symbol) form ; FORM is (OP . ARGS)
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (let ((args (arguments form)))
    (multiple-value-bind (kind local-p info)
        (function-information op environment)

      (multiple-value-bind (parsed-args resulting-env)
          (parse-args args environment)

        (values
         ;; value 1
         (ecase kind
           ((nil)
            (make-instance 'application
                           :operator (make-instance 'function-name-ref
                                                    :symbol op
                                                    :local local-p
                                                    :source op
                                                    :top enclosing-form
                                                    :type 'function
                                                    )
                           :arguments parsed-args))

           (:function
            (let* ((f-type-decl (assoc 'ftype info :test #'eq))
                   (f-type (if f-type-decl
                               (cdr f-type-decl)
                               'function))
                   )
                           
              (make-instance 'function-application
                             :operator (make-instance 'function-name-ref
                                                      :symbol op
                                                      :local local-p
                                                      :source op
                                                      :top enclosing-form
                                                      :type f-type
                                                      )
                             :arguments parsed-args)))

           (:macro
            (multiple-value-bind (expanded-form expanded-p)
                (if macroexpand
                    (macroexpand-1 form environment)
                    (values form nil))
              (make-instance 'macro-application
                             :operator (make-instance 'macro-name-ref
                                                      :symbol op
                                                      :local local-p
                                                      :source op
                                                      :top enclosing-form
                                                      :type 'macro)
                             :arguments parsed-args
                             :expansion (if expanded-p
                                            (apply #'parse expanded-form keys)
                                            nil)))
            )

           (:special-form
            (error 'unknown-special-operator-error :name op))
           )

         ;; value 2
         resulting-env)
        ))))


(defun parse-args (args env)
  ;; This is tricky.  I "should" be able to "just parse" the list of
  ;; arguments, but, in reality, I may have to deal with possible side
  ;; effects on the environment.
  ;;
  ;; Also, this is still incorrect for macro arguments, which may be
  ;; arbitrarily destructured.

  (loop with acc-env = (augment-environment env)
        for a in args
        for (a-form a-env)
        = (multiple-value-list (parse a :environment acc-env))
        then (multiple-value-list (parse a :environment a-env))
        collect a-form into a-forms
        do (setf acc-env a-env)
        finally (return (values a-forms acc-env))))


;;; parse-form --
;;; Parsing of forms like ((lambda ....) . args) and similar things.

(defmethod parse-form ((op cons) form ; FORM is (OP . ARGS)
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (let ((args (arguments form)))
    (if (is-lambda-form op)
        (parse-lambda-form op enclosing-form environment macroexpand)
        )))


;;;;===========================================================================
;;;; Auxiliary parsing functions.

(defun parse-lambda-form (lf enclosing-form environment macroexpand)
  )

;;;; end of file -- parse.lisp --
