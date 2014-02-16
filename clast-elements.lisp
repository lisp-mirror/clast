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


(defclass local-function-application (function-application) ())


(defclass macro-application (application expansion-component) ())


(defclass local-macro-application (marco-application) ())


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
               :initarg :sitiations)
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
          :initarg :function)
   )
  )


(defclass lambda-form (function-form implicit-progn)
  ((lambda-list :accessor form-args
                :accessor form-lambda-list
                :initarg :args
                :initarg :lambda-list)
   )
  )


(defclass go-form (form)
  ((name :accessor form-name
         :initarg :name)
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


(defclass select-form (form)
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


(defclass cond-form (selector-form) ())


(defclass case-form (selector-form) ())


(defclass ccase-form (selector-form) ())


(defclass ecase-form (selector-form) ())


(defclass typecase-form (selector-form) ())


(defclass etypecase-form (selector-form) ())


(defclass let-form (vbinding-form implicit-progn) ())


(defclass let*-form (vbinding-form implicit-progn) ())


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
  ((place :accessor form-place
          :initarg :place)
   (value :accessor form-value
          :initarg :value)
   )
  )


(defclass set-form (assignment-form) ())

(defclass setq-form (assignment-form) ())

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
  ()
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


                
  


;;;; end of file -- clast-elements.lisp --
