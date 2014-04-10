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
  (:documentation "Parses a form in a given 'environment'.

The methods of this generic function return a AST 'node' (a CLAST-ELEMENT) 
and the - possibly modified - ENVIRONMENT.

Arguments and Values:

form : the form to be parsed.
keys : the collection of key-value pairs passed to the call.
enclosing-form : the form that \"contains\" the form beling parsed.
environment : the environment in which the form is being parsed.
")
  )


(defgeneric parse-form (op form &rest keys
                        &key
                        enclosing-form
                        macroexpand
                        environment
                        &allow-other-keys)
  (:documentation "Parses a form in a given 'environment' given its 'op'.

Arguments and Values:

form : the form to be parsed.
keys : the collection of key-value pairs passed to the call.
enclosing-form : the form that \"contains\" the form beling parsed.
environment : the environment in which the form is being parsed.
")
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

(declaim (ftype (function (t) boolean) is-lambda-form)
         (inline is-lambda-form)
         )

(declaim (ftype (function (cons) t) operator)
         (ftype (function (cons) cons) arguments)
         (inline operator arguments)
         )


(defun operator (form)
  (declare (type cons form))
  (first form))


(defun arguments (form)
  (declare (type cons form))
  (rest form))


(defun is-lambda-form (form)
  (and (listp form) (eq (first form) 'lambda)))


;;;;---------------------------------------------------------------------------
;;;; PARSE and PARSE-FORM methods.

(eval-when (:load-toplevel :compile-toplevel :execute)

(defmacro def-parse-self-evaluating-method (self-evaluating-type)
  `(defmethod parse ((form ,self-evaluating-type)
                     &rest keys
                     &key
                     environment
                     macroexpand
                     enclosing-form
                     &allow-other-keys)
     (declare (ignore keys macroexpand))
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
           ;; (type (member :constant) kind)
           (ignore kind)
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
          (apply #'parse-args args keys)

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


(defun parse-form-seq (forms
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  ;; This is tricky.  I "should" be able to "just parse" the list of
  ;; forms or arguments, but, in reality, I may have to deal with
  ;; possible side effects on the environment.
  ;;
  ;; Also, this is still incorrect for macro arguments, which may be
  ;; arbitrarily destructured.

  (declare (type list forms)
           (ignore enclosing-form macroexpand))

  (loop with acc-env = (augment-environment environment)
        for a in forms
        for (a-form a-env)
        = (multiple-value-list (apply #'parse a :environment acc-env keys))
        then (multiple-value-list (apply #'parse a :environment a-env keys))
        collect a-form into a-forms
        do (setf acc-env a-env)
        finally (return (values a-forms acc-env))))


(defun parse-args (args &rest keys
                        &key
                        enclosing-form
                        environment
                        macroexpand
                        &allow-other-keys)
  (declare (ignore enclosing-form environment macroexpand))
  (apply #'parse-form-seq args keys))


(defun parse-binding-seq (bindings
                          &rest keys
                          &key
                          environment
                          enclosing-form
                          macroexpand
                          &allow-other-keys)
  ;; Similar to PARSE-FORM-SEQ but I need to update the environment
  ;; while going...

  (declare (ignore enclosing-form macroexpand))

  (loop with result-env = (augment-environment environment)
        for (var value) in bindings
        for (bind-form bind-env)
        = (multiple-value-list (apply #'parse value :environment result-env keys))
        collect (list var bind-form) into resulting-bindings
        do (setf result-env
                 (augment-environment bind-env
                                      :variable (list var)))
        finally (return (values resulting-bindings result-env))))


;;; parse-form --
;;; Parsing of forms like ((lambda ....) . args) and similar things.

(defmethod parse-form ((op cons) form ; FORM is (OP . ARGS)
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore environment macroexpand))

  (multiple-value-bind (parsed-args env)
      (apply #'parse-args (arguments form) keys)
  
    (values
     ;; Value 1
     (if (is-lambda-form op)
         (make-instance 'lambda-application
                        :operator (apply #'parse-lambda-form op
                                         :environment env
                                         keys)
                        :arguments parsed-args
                        :top enclosing-form
                        :source form
                        )
         (make-instance 'functional-operator-application
                        :operator (apply #'parse op :environment env keys)
                        :arguments parsed-args
                        :top enclosing-form
                        :source form))
     ;; Value 2
     env)))


;;;---------------------------------------------------------------------------
;;; Specialized PARSE-FORMs.
;;; At a minimum, we need all the special operators of Figure 3.2 of ANSI
;;; Section 3.1.2.1.2.1

(defmethod parse-form ((op (eql 'block)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (let ((block-name (second form)) ; Shoould add this a "lexical tag"
                                   ; in the environment if not nil.
        )
    (values
     (make-instance 'block-form
                    :name block-name
                    :body-env environment
                    :top enclosing-form
                    :source form
                    ;; The use of PARSE-FORM-SEQ must be reviewed.
                    :progn (apply #'parse-form-seq (cddr form) keys)
                    )
     environment)))


(defmethod parse-form ((op (eql 'catch)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)

  (declare (ignore macroexpand))

  (values
   (make-instance 'catch-form
                  :tag (apply #'parse (second form) keys)
                  :body-env environment
                  :top enclosing-form
                  :source form
                  ;; The use of PARSE-FORM-SEQ must be reviewed.
                  :progn (apply #'parse-form-seq (cddr form) keys)
                  )
   environment))


(defmethod parse-form ((op (eql 'declare)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  ;; This is a complicated one as it must be able to change the
  ;; environmment.

  (declare (ignore keys))

  (multiple-value-bind (decls-forms new-env)
      (parse-declarations (rest form)
                          environment
                          enclosing-form
                          macroexpand)
    (values
     (make-instance 'declaration-form
                    :declarations decls-forms
                    :resulting-environment new-env
                    :top enclosing-form
                    :source form)
     new-env)
    ))


(defmethod parse-form ((op (eql 'progn)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (values
   (make-instance 'progn-form
                  :top enclosing-form
                  :source form
                  :body-env environment
                  :progn (apply #'parse-form-seq (rest form) keys)
                  )
   environment)
  )


(defmethod parse-form ((op (eql 'eval-when)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (values
   (make-instance 'eval-when-form
                  :situations (second form)
                  :top enclosing-form
                  :source form
                  :body-env environment
                  :progn (apply #'parse `(progn ,@(cddr form)) keys)
                  )
   environment)
  )


(defmethod parse-form ((op (eql 'flet)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (let* ((functions (second form))
         (body (cddr form))
         (binds (mapcar (lambda (f)
                          (apply #'parse-local-function f keys))
                        functions))
         (ne (augment-environment environment
                                  :function (mapcar #'first functions)))
         )
    (values
     (make-instance 'flet-form
                    :binds binds
                    :top enclosing-form
                    :source form
                    :body-env ne
                    :progn (apply #'parse `(progn ,@body)
                                  :environment ne
                                  keys))
     environment)
    ))


(defmethod parse-form ((op (eql 'labels)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (let* ((functions (second form))
         (body (cddr form))
         (ne (augment-environment environment
                                  :function (mapcar #'first functions)))
         (binds (mapcar (lambda (f)
                          (apply #'parse-local-function
                                 f
                                 :environment ne
                                 keys))
                        functions))
         )
    (values
     (make-instance 'labels-form
                    :binds binds
                    :top enclosing-form
                    :source form
                    :body-env ne
                    :progn (apply #'parse-form-seq body
                                  :environment ne
                                  keys))
     environment)
    ))


(defmethod parse-form ((op (eql 'function)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (values
   (make-instance 'function-form
                  :function (apply #'parse (second form) keys)
                  ;; Fix the above (now it returns a free variable refs).

                  :top enclosing-form
                  :source form)
   environment)
  )


(defmethod parse-form ((op (eql 'lambda)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore enclosing-form environment macroexpand))
  (apply #'parse-lambda-form form keys)
  )


(defmethod parse-form ((op (eql 'go)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore keys macroexpand))
  
  (values (make-instance 'go-form
                         :tag (second form)
                         :source form
                         :top enclosing-form)
          environment)
  )


(defmethod parse-form ((op (eql 'if)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore enclosing-form macroexpand))
  (values (make-instance 'if-form
                         :condition (apply #'parse (second form) keys)
                         :then  (apply #'parse (third form) keys)
                         :else  (apply #'parse (fourth form) keys)
                         )
          environment))


(defmethod parse-form ((op (eql 'let)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (let* ((normalized-bindings (listify (second form)))
         (parsed-bindings (mapcar (lambda (b)
                                    (list (first b)
                                          (apply #'parse (second b)
                                                 keys)))
                                  normalized-bindings))
         (ne (augment-environment environment
                                  :variable (mapcar #'first normalized-bindings)))
         )
    (values (make-instance 'let-form
                           :binds parsed-bindings
                           :progn (apply #'parse-form-seq (cddr form)
                                         :environment ne
                                         keys)
                           :body-env ne
                           :top enclosing-form
                           :source form
                           )
            environment)))


(defmethod parse-form ((op (eql 'let*)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (let ((normalized-bindings (listify (second form))))
    (multiple-value-bind (parsed-bindings ne)
        (apply #'parse-binding-seq normalized-bindings keys)
    
      (values (make-instance 'let-form
                             :binds parsed-bindings
                             :progn (apply #'parse-form-seq
                                           (cddr form)
                                           :environment ne
                                           :body-env ne
                                           :top enclosing-form
                                           :source form
                                           ))
              environment)
      )))


(defmethod parse-form ((op (eql 'macrolet)) form ; The big one!
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)

  (declare (ignore macroexpand))

  (flet ((parse-local-macro-def (macro-def
                                 env
                                 &aux
                                 (m-name (first macro-def))
                                 (m-def (rest macro-def))
                                 )
           ;; ... and here is the magic of PARSE-MACRO and ENCLOSE.
           (list m-name
                 (enclose
                  (parse-macro m-name
                               (first m-def)
                               (cdr m-def)
                               env)
                  env)))
         )
    (let* ((macros (second form))
           (body (cddr form))
           (ne (augment-environment
                environment
                :macro (mapcar #'(lambda (m)
                                   (parse-local-macro-def m environment))
                               macros)))
           (binds (mapcar (lambda (f)
                            (apply #'parse-local-macro
                                   f
                                   :environment ne
                                   keys))
                          macros))
           )
      (values (make-instance 'macrolet-form
                             :binds binds
                             :top enclosing-form
                             :source form
                             :body-env ne
                             :progn (apply #'parse-form-seq body
                                           :environment ne
                                           keys))
              environment)
      ))
  )


(defmethod parse-form ((op (eql 'multiple-value-bind)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (let ((body-env (augment-environment environment
                                       :variable
                                       (copy-list (second form))))
        )
    (values (make-instance 'mvb-form
                           :binds (second form)
                           :top enclosing-form
                           :source form
                           :values-form (apply #'parse (third form) keys)
                           :body-env body-env
                           :progn (apply #'parse-form-seq (cdddr form)
                                         :environment body-env
                                         keys))
            environment)
    ))
                       

;;;;===========================================================================
;;;; Auxiliary parsing functions.

(defun parse-lambda-form (lf &rest keys
                             &key
                             enclosing-form
                             environment
                             macroexpand
                             &allow-other-keys)
  (declare (ignore macroexpand))

  (let* ((parsed-ll (parse-ll :ordinary (second lf)))
         (llvs (ll-vars parsed-ll))
         (body-env
          (augment-environment environment :variable llvs))
         )
    (make-instance 'lambda-form
                   :lambda-list parsed-ll
                   :progn (apply #'parse-form-seq (cddr lf)
                                 :environment body-env
                                 keys)
                   :source lf
                   :top enclosing-form
                   )
    ))


(defun parse-local-function (f &rest keys
                               &key
                               enclosing-form
                               environment
                               macroexpand
                               &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (f-name args &rest body)
      f
    (let* ((ll (parse-ll :ordinary args)) ; FTTB. Note that init forms are not expanded.
           (var-names (ll-vars ll))
           (f-body-env
            (augment-environment environment
                                 :variable var-names))
           )
      (make-instance 'function-definition-form
                     :name f-name
                     :lambda-list ll
                     :top enclosing-form
                     :source f
                     :body-env f-body-env
                     :progn (apply #'parse `(progn ,@body)
                                   :environment f-body-env
                                   keys))
      )))


(defun parse-local-macro (m &rest keys
                            &key
                            enclosing-form
                            environment
                            macroexpand
                            &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (m-name args &rest body)
      m
    (let* ((ll (parse-ll :macro args)) ; FTTB. Note that init forms are not expanded.
           (var-names (ll-vars ll))
           (m-body-env
            (augment-environment environment
                                 :variable var-names))
           )
      (make-instance 'macro-definition-form
                     :name m-name
                     :lambda-list ll
                     :top enclosing-form
                     :source m
                     :body-env m-body-env
                     :progn (apply #'parse `(progn ,@body)
                                   :environment m-body-env
                                   keys))
      )))


(defgeneric parse-declaration (decl-identifier
                               declaration
                               &rest keys
                               &key
                               environment
                               enclosing-form
                               macroexpand)
  )


(defun parse-declarations (decls env enclosing-form macroexpand)
  (declare (type list decls))

  (loop with result-env = env
        for d in decls
        for (parsed-decl new-env)
        = (multiple-value-list (parse-declaration (first d)
                                                  d
                                                  :environment env
                                                  :enclosing-form enclosing-form
                                                  :macroexpand macroexpand))
        then (multiple-value-list (parse-declaration (first d)
                                                     d
                                                     :environment new-env
                                                     :enclosing-form enclosing-form
                                                     :macroexpand macroexpand))
        collect parsed-decl into parsed-decls
        do (setf result-env new-env)
        finally (return (values parsed-decls result-env))))


(defmethod parse-declaration ((di symbol) ; Catch-all method.
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)

  (declare (ignore keys macroexpand))

  (let ((dsf (make-instance 'id-declaration-specifier-form
                            :id di
                            :top enclosing-form
                            :source d))
        )
    (values dsf environment)
    ))


(defmethod parse-declaration ((di (eql 'type))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let* ((vars (cddr d))
         (df (make-instance 'type-declaration-specifier-form
                            :spec (second d)
                            :symbols vars
                            :top enclosing-form
                            :source d
                            ))
         )
    ;; Now we should change 'env'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'ftype))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let* ((function-names (cddr d))
         (df (make-instance 'ftype-declaration-specifier-form
                            :spec (second d)
                            :symbols function-names
                            :top enclosing-form
                            :source d
                            ))
         )
    ;; Now we should change 'env'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'ignore))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'ignorable))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'optimize))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'inline))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'notinline))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'special))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


(defmethod parse-declaration ((di (eql 'dynamic-extent))
                              d
                              &rest keys
                              &key
                              environment
                              enclosing-form
                              macroexpand)
  (declare (ignore keys macroexpand))
  (let ((vars (rest d))
        (df (make-instance 'id-declaration-specifier-form
                           :id di
                           :top enclosing-form
                           :source d
                           ))
        )
    (declare (ignore vars))
    ;; Now we should change 'environment'.
    (values df environment)
    ))


;;;;===========================================================================
;;;; Utilities.

(defun listify (l)
  (declare (type list l))
  (mapcar (lambda (e) (if (consp e) e (list e))) l))
           


;;;; end of file -- parse.lisp --
