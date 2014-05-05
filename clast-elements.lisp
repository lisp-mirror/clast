;;;; -*- Mode: Lisp -*-

;;;; clast-elements.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package "CLAST")


(defclass form ()
  ((type :accessor form-type
         :initarg :type
         :initform t ; The declared or (possibly) inferred type of the form.
         )
   (top :accessor form-top
        :initarg :top
        :initform nil
        )
   (source :accessor form-source
           :initarg :source
           )
   ))


(defgeneric clast-element-subforms (ce))


(defclass expansion-component ()
  ((expansion :accessor form-expansion
              :initarg :expansion))
  )


(defclass constant-form (form)
  ((value :accessor form-value
          :initarg :value
          )
   ))


(defclass binding-form (form)
  ((binds :accessor form-binds
          :initarg :binds
          :initform ()
          )
   ))


(defclass vbinding-form (binding-form) ())


(defclass fbinding-form (binding-form) ())


(defclass implicit-progn ()
  ((iprogn-forms :accessor form-progn
                 :initarg :progn
                 :initform ())
   (body-env :accessor form-body-env
             :initarg :body-env
             :initform () #| (make-env) |#
             )
   ))


(defclass symbol-ref (form)
  ((symbol :accessor form-symbol :initarg :symbol)
   (local :accessor form-local
          :initarg :local
          :initform nil
          )
   )
  )


(defclass variable-ref (symbol-ref) ())


(defclass constant-ref (variable-ref constant-form) ())


(defclass free-variable-ref (variable-ref) ())


(defclass special-variable-ref (variable-ref) ())


(defclass symbol-macro-ref (symbol-ref expansion-component) ())


(defclass application (form)
  ((operator :accessor form-operator :initarg :operator)
   (args     :accessor form-args     :initarg :arguments)))


(defclass function-name-ref (symbol-ref) ())


(defclass macro-name-ref (symbol-ref) ())


(defclass function-application (application) ())


(defclass lambda-application (function-application) ())


(defclass functional-operator-application (function-application)
  ()
  (:documentation "The Functional Operator Application CLass.

This class represents functional applications where the operator is
non-standard with respect to the Common Lisp Standard; i.e.,
applications where the operator is not a symbol or a lambda expression."))


(defclass local-function-application (function-application) ())


(defclass macro-application (application expansion-component) ())


(defclass local-macro-application (macro-application) ())


(defclass declaration-form (form)
  ((decls :accessor declaration-form-declarations
          :initarg :declarations
          )
   (new-env :accessor declaration-form-resulting-environment
            :accessor form-resulting-environment
            :initarg :resulting-environment)
   )
  )


(defclass declaration-specifier-form (form)
  ((identifier :accessor declaration-specifier-form-identifier
               :accessor declaration-specifier-identifier
               )
   )
  )


(defclass tf-declaration-specifier-form (declaration-specifier-form)
  ((type-spec :accessor declaration-type-spec
              :initarg :spec)
   (symbol-refs :accessor declaration-type-spec-symbols
                :initarg :symbols)
   )
  )


(defclass type-declaration-specifier-form (tf-declaration-specifier-form)
  ((identifier :initform 'type))
  )


(defclass ftype-declaration-specifier-form (tf-declaration-specifier-form)
  ((identifier :initform 'ftype))
  )


(defclass id-declaration-specifier-form (declaration-specifier-form)
  ((identifier :initarg :id)
   )
  )


;;;; "composite" forms.

(defclass block-form (form implicit-progn)
  ((name :accessor form-name
         :accessor block-name
         :initarg :name)
   )
  )


(defclass catch-form (form implicit-progn)
  ((tag :accessor form-catch-tag
        :initarg :tag)
   )
  )


(defclass throw-form (form)
  ((tag :accessor form-throw-tag
        :initarg :tag)
   (result :accessor form-result
           :initarg :result)
   )
  )


(defclass eval-when-form (form implicit-progn)
  ((situations :accessor form-situations
               :initarg :situations)
   )
  )


(defclass flet-form (fbinding-form implicit-progn)
  ())


(defclass labels-form (fbinding-form implicit-progn)
  ())


(defclass macrolet-form (fbinding-form implicit-progn)
  ())


(defclass symbol-macrolet-form (vbinding-form implicit-progn)
  ())


(defclass function-form (form)
  ((funct :accessor form-function
          :initarg :function
          :initarg :name)
   (type :initform 'function)
   )
  )


(defclass lambda-form (function-form implicit-progn)
  ((lambda-list :accessor form-args
                :accessor form-lambda-list
                :initarg :args
                :initarg :lambda-list)
   (funct :initform 'lambda)
   )
  )


(defclass function-definition-form (lambda-form)
  ()
  )


(defclass macro-definition-form (function-definition-form)
  ((type :initform t))
  )





(defclass go-form (form)
  ((name :accessor form-name
         :initarg :name
         :initarg :tag)
   (enclosing-tagbody :accessor form-tagbody
                      :initarg :enclosing-tagbody)
   )
  )


(defclass if-form (form)
  ((condition :accessor form-condition
              :initarg :condition)
   (then :accessor form-then
         :initarg :then)
   (else :accessor form-else
         :initarg :else)
   )
  )


(defclass selection-form (form)
  ((clauses :accessor form-clauses
            :initarg :clauses
            )
   )
  )


(defclass clause-form (form implicit-progn)
  ((selector-form :accessor form-selector
                  :initarg :selector
                  )
   )
  )


(defclass selector-form (selection-form)
  ((selector :initarg :selector
             :accessor selector-form-selection)
   )
  )



(defclass cond-form (selection-form) ())


(defclass case-form (selector-form) ())


(defclass ccase-form (selector-form) ())


(defclass ecase-form (selector-form) ())


(defclass typecase-form (selector-form) ())


(defclass etypecase-form (selector-form) ())


(defclass let-form (vbinding-form implicit-progn) ())


(defclass let*-form (vbinding-form implicit-progn) ())


(defclass mvb-form (vbinding-form implicit-progn)
  ((values-form :accessor form-values-form
                :initarg :values-form)
   )
  )


(defclass load-time-value-form (form)
  ((ltv-form :accessor form-load-time-form
             :initarg :load-time-form)
   (read-only :accessor is-read-only
              :accessor read-only-p
              :initarg :read-only-p
              :initarg :is-read-only)
   )
  )


(defclass locally-form (form implicit-progn)
  ((decls :accessor form-declarations
          :initarg :declarations)
   )
  )


(defclass multiple-value-call-form (form implicit-progn)
  ;; Misnomer. 'implicit-progn' should be 'forms-sequence'.
  ((funct :accessor form-function
          :initarg :function)
   )
  )


(defclass multiple-value-prog1-form (form implicit-progn)
  ()
  )


(defclass progn-form (form implicit-progn)
  ()
  )


(defclass progv-form (form implicit-progn)
  ((symbols :accessor form-symbols
            :initarg :symbols)
   (vals :accessor form-values
         :initarg :values)
   )
  )

(defclass quote-form (constant-form) ())


(defclass return-from-form (form)
  ((name :accessor form-name
         :initarg :name)
   (result :accessor form-result
           :initarg :result)
   (enclosing-block :accessor form-enclosing-block
                    :accessor form-block
                    :initarg :block)
   )
  )


(defclass assignment-form (form)
  ((places :accessor form-places
           :initarg :places)
   (vals :accessor form-values
         :initarg :values)
   )
  )


(defclass set-form (assignment-form) ())

(defclass setq-form (assignment-form)
  ((places :initarg :symbols)))

(defclass setf-form (assignment-form) ())


(defclass tagbody-form (form)
  ((tagbody :accessor form-body
            :initarg :body
            )
   (tags :accessor form-tags
         :initarg :tags
         :initform ()
         )
   )
  )


(defclass prog-form (vbinding-form tagbody-form)
  ((body-env :accessor form-body-env
             :initarg :body-env
             :initform () #| (make-env) |#
             )
   )
  )


(defclass prog*-form (prog-form)
  ()
  )


(defclass the-form (form)
  ((type-cast :accessor form-type-cast
              :initarg :type)
   (form :accessor form-expr
         :initarg :expr)
   )
  )


(defclass unwind-protect-form (form)
  ((protected-form :accessor form-protected-form
                   :initarg :protected-form
                   )
   (cleanup-forms :accessor form-cleanup-forms
                  :initarg :cleanup-forms)
   )
  )


(defclass error-clause-form (form implicit-progn)
  ((datum :accessor form-datum
          :initarg :datum)

   (lambda-list :accessor form-lambda-list
                :accessor form-args
                :initarg :lambda-list
                :initarg :args)
   )
  )


(defclass condition-case-form (form)
  ((handled-form :accessor form-handled-form
                 :initarg :handled-form
                 )
   (clauses :accessor form-clauses
            :initarg :clauses
            )
   )
  )


(defclass handler-case-form (condition-case-form) ())

(defclass restart-case-form (condition-case-form) ())
  


(defclass handler-bind-form (fbinding-form implicit-progn) ())

(defclass restart-bind-form (fbinding-form implicit-progn) ())


;;;;---------------------------------------------------------------------------
;;;; Definition forms.

(defclass definition-form (form)
  ((name :accessor definition-form-name
         :initarg :name)
   ))


(defclass definition-lambda-list-form (definition-form implicit-progn)
  ((lambda-list :accessor lambda-definition-form-lambda-list
                :initarg :lambda-list
                :initform ())
   ))


(defclass defvar-form (definition-form)
  ((name :accessor defvar-form-name)
   (value :accessor defvar-form-value
          :initarg :value)
   (doc-string :accessor defvar-form-doc-string
               :initarg :doc-string
               :initform "")
   ))


(defclass defparameter-form (defvar-form)
  ((name :accessor defparameter-form-name)
   (value :accessor defparameter-form-value
          :initform (error "No initial value provided to DEFPARAMETER."))
   (doc-string :accessor defparameter-form-doc-string)
   ))


(defclass defconstant-form (definition-form)
  ((name :accessor defconstant-form-name)
   (value :accessor defconstant-form-value
          :initform (error "No initial value provided to DEFCONSTANT."))
   (doc-string :accessor defconstant-form-doc-string)
   ))


(defclass defun-form (definition-lambda-list-form)
  ((name :accessor defun-form-name)
   (lambda-list :accessor defun-form-lambda-list)
   ))


(defclass defmacro-form (definition-lambda-list-form)
  ((name :accessor defmacro-form-name)
   (lambda-list :accessor defmacro-form-lambda-list)
   ))


(defclass defegeneric-form (definition-lambda-list-form)
  ((name :accessor defgeneric-form-name)
   (lambda-list :accessor defgeneric-form-lambda-list)
   ))


(defclass defmethod-form (definition-lambda-list-form)
  ((name :accessor defgeneric-form-name)
   (lambda-list :accessor defmethod-form-lambda-list)
   (qualifiers :accessor defmethod-form-qualifiers
               :initarg :qualifiers
               :initform ())
   ))


(defclass define-compiler-macro-form (defmacro-form)
  ((name :accessor define-compiler-macro-form-name)))


(defclass define-modifier-macro-form (defmacro-form)
  ((name :accessor define-compiler-macro-form-name)))


(defclass defstruct-form (definition-form)
  ((name :accessor defstruct-form-name)
   (options :accessor defstruct-form-options
            :initarg :options)
   (slots :accessor defstruct-options-slots
          :initarg :slots)
   ))


(defclass defclass-form (definition-form)
  ((name :accessor defclass-form-name)
   (superclasses :accessor defclass-form-superclasses
                 :initarg :superclasses
                 :initform ())
   (options :accessor defclass-form-options
            :initarg :options)
   (slots :accessor defclass-options-slots
          :initarg :slots)
   ))


(defclass define-method-combination-form (definition-form)
  ((name :accessor define-method-combination-form-name)))


(defclass define-symbol-macro-form (definition-form)
  ((name :accessor define-symbol-macro-form-name)))


(defclass define-setf-expander-form (definition-form)
  ((name :accessor define-setf-expander-form-name)))


(defclass defsetf-form (definition-form)
  ((name :accessor defsetf-form-name)))


(defclass defpackage-form (definition-form)
  ((name :accessor defpackage-form-name)))


;;;;---------------------------------------------------------------------------
;;;; Subform extraction.

(defmethod clast-element-subforms ((ce constant-form)) ())


(defmethod clast-element-subforms ((ce symbol-ref)) ())


(defmethod clast-element-subforms ((a application))
  (cons (form-operator a)
        (form-args a)))


(defmethod clast-element-subforms ((a macro-application))
  (let ((expansion (form-expansion a)))
    (if expansion
        (list expansion)
        (cons (form-operator a)
              (form-args a)))))


(defmethod clast-element-subforms ((l let-form))
  (list* (form-binds l)
         (form-progn l)))


(defmethod clast-element-subforms ((d declaration-form))
  (declaration-form-declarations d))


(defmethod clast-element-subforms ((b block-form))
  (list* (block-name b)
         (form-progn b)))


(defmethod clast-element-subforms ((b progn-form))
   (form-progn b))


(defmethod clast-element-subforms ((f binding-form))
  (list (form-binds f)
        (form-progn f)))


(defmethod clast-element-subforms ((fdf function-definition-form))
  (list (form-function fdf)
        (form-lambda-list fdf)
        (form-progn fdf)))


(defmethod clast-element-subforms ((df defun-form))
  (list (defun-form-name df)
        (defun-form-lambda-list df)
        (form-progn df)))


(defmethod clast-element-subforms ((ce list)) ce) ; Catch all method.


(defmethod clast-element-subforms ((ce t)) ()) ; Catch all method.


;;;; end of file -- clast-elements.lisp --
