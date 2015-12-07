;;;; -*- Mode: Lisp -*-

;;;; parse-defs.lisp --
;;;; Parsing of "definition" constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.

;;;----------------------------------------------------------------------------
;;; defvar

(defmethod parse-form ((op (eql 'defvar)) form
                       &rest keys
                       &key
                       enclosing-form ; An EVAL-WHEN or some LET.
                       macroexpand
                       environment
                       &allow-other-keys)
  "Parsing of DEFVAR forms.

The return values include the augmented environment.
"
  (declare (ignore macroexpand))

  (destructuring-bind (defvar-kwd var-name
                                  &optional init-value doc-string)
      form
    (declare (ignore defvar-kwd))
    (values
     (make-instance 'defvar-form
                    :name var-name
                    :value (apply #'parse init-value keys)
                    :doc-string doc-string
                    
                    :top enclosing-form
                    :source form)
     (augment-environment environment
                          :variable (list var-name)
                          :declare (list `(special ,var-name))
                          )))
  )


;;;----------------------------------------------------------------------------
;;; defparameter

(defmethod parse-form ((op (eql 'defparameter)) form
                       &rest keys
                       &key
                       enclosing-form ; An EVAL-WHEN or some LET.
                       macroexpand
                       environment
                       &allow-other-keys)
  "Parsing of DEFPARAMETER forms.

The return values include the augmented environment.
"

  (declare (ignore macroexpand))

  (destructuring-bind (defpar-kwd var-name init-value
                                  &optional doc-string)
      form
    (declare (ignore defpar-kwd))
    (values
     (make-instance 'defparameter-form
                    :name var-name
                    :value (apply #'parse init-value keys)
                    :doc-string doc-string
                    
                    :top enclosing-form
                    :source form)
     (augment-environment environment
                          :variable (list var-name)
                          :declare (list `(special ,var-name))
                          )))
  )


;;;----------------------------------------------------------------------------
;;; defun

(defmethod parse-form ((op (eql 'defun)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (defun-kwd f-name ll &body f-body)
      form
    (declare (ignore defun-kwd))
    (let* ((parsed-ll (parse-ll :ordinary ll))
           (ll-vars (ll-vars parsed-ll))
           (new-env
            (augment-environment environment
                                 :function (list f-name)
                                 ))
           (f-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (values
       (make-instance 'defun-form
                      :name f-name
                      :lambda-list parsed-ll
                      :top enclosing-form
                      :source form
                      :body-env f-body-env
                      :progn (apply #'parse `(block ,f-name
                                               ,@f-body)
                                    :environment f-body-env
                                    keys)
                      )
       new-env))))


;;;----------------------------------------------------------------------------
;;; defmacro

(defmethod parse-form ((op (eql 'defmacro)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (defmacro-kwd m-name ll &body m-body)
      form
    (declare (ignore defmacro-kwd))
    (let* ((parsed-ll (parse-ll :macro ll))
           (ll-vars (ll-vars parsed-ll))
           (new-env
            (augment-environment environment
                                 :function (list m-name)
                                 ))
           (m-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (values
       (make-instance 'defmacro-form
                      :name m-name
                      :lambda-list parsed-ll
                      :top enclosing-form
                      :source form
                      :body-env environment
                      :progn (apply #'parse `(block ,m-name
                                               ,@m-body)
                                    :environment m-body-env
                                    keys)
                      )
       new-env))))


;;;----------------------------------------------------------------------------
;;; defgeneric

(defmethod parse-form ((op (eql 'defgeneric)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (destructuring-bind (defgeneric-kwd gf-name ll &body opts-and-meths)
      form
    (declare (ignore defgeneric-kwd))
    (let* ((parsed-ll (parse-ll :generic-function ll))
           ;; (ll-vars (ll-vars parsed-ll))
           (gf-env
            (augment-environment environment
                                 :function (list gf-name)
                                 ))
           (options (remove :method opts-and-meths
                            :test #'eq
                            :key #'first))
           (methods (remove :method opts-and-meths
                            :test (complement #'eq)
                            :key #'first))
           (parsed-meths
            (mapcar (lambda (m)
                      (apply #'parse 'defmethod (cons 'defmethod (rest m))
                             :enclosing-form form
                             :environment gf-env
                             :macroexpand macroexpand
                             keys))
                    methods))
           )
      (values
       (make-instance 'defgeneric-form
                      :name gf-name
                      :lambda-list parsed-ll
                      :options options ; Unparsed.
                      :methods parsed-meths

                      :top enclosing-form
                      :source form
                      )
       gf-env))))


;;;----------------------------------------------------------------------------
;;; defmethod

(defmethod parse-form ((op (eql 'defmethod)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))

  (destructuring-bind (defmethod-kwd gf-name &rest method-spec)
      form
    (declare (ignore defmethod-kwd))
    (let* ((quals (loop for q in method-spec
                        when (listp q) do (loop-finish)
                        collect q ; Assuming only symbols as qualifiers
                        ))
           (ll-body (member-if #'listp method-spec))
           (ll (first ll-body))
           (meth-body (rest ll-body))

           (parsed-ll (parse-ll :specialized ll))
           ;; (ll-vars (ll-vars parsed-ll))
           (gf-env
            (if (function-information gf-name environment) ; The GF may already be there.
                environment
                (augment-environment environment
                                     :function (list gf-name)
                                     )))
           (m-body-env
            (augment-environment gf-env
                                 :variable (ll-vars ll)))
           )
      (values
       (make-instance 'defmethod-form
                      :name gf-name
                      :lambda-list parsed-ll
                      :qualifiers quals
                      :body-env m-body-env
                      :progn (apply #'parse `(block ,gf-name
                                               ,@meth-body)
                                    :environment m-body-env
                                    keys)

                      :top enclosing-form
                      :source form
                      )
       gf-env))))


;;;----------------------------------------------------------------------------
;;; define-compiler-macro-form
;;; Similar, with one difference to the DEFMACRO method.

(defmethod parse-form ((op (eql 'define-compiler-macro)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (dcm-kwd m-name ll &body m-body)
      form
    (declare (ignore dcm-kwd))
    (let* ((parsed-ll (parse-ll :macro ll))
           (ll-vars (ll-vars parsed-ll))
           (new-env
            (if (function-information m-name environment)
                environment
                (augment-environment environment
                                     :function (list m-name)
                                     )))
           (m-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (values
       (make-instance 'define-compiler-macro-form
                      :name m-name
                      :lambda-list parsed-ll
                      :top enclosing-form
                      :source form
                      :body-env environment
                      :progn (apply #'parse `(block ,m-name
                                               ,@m-body)
                                    :environment m-body-env
                                    keys)
                      )
       new-env))))


;;;----------------------------------------------------------------------------
;;; define-modify-macro

(defmethod parse-form ((op (eql 'define-modify-macro)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (declare (ignore macroexpand))
  (destructuring-bind (dmm-kwd dmm-name ll fun &optional docstring)
      form
    (declare (ignore dmm-kwd))
    (let* ((parsed-ll (parse-ll :define-modify-macro ll))
           (ll-vars (ll-vars parsed-ll))
           (new-env
            (augment-environment environment
                                 :function (list dmm-name)
                                 ))
           (m-body-env
            (augment-environment new-env
                                 :variable ll-vars
                                 ))
           )
      (declare (ignore m-body-env))
      (values
       (make-instance 'define-modifier-macro-form
                      :name dmm-name
                      :lambda-list parsed-ll
                      :function fun
                      :documentation docstring

                      :top enclosing-form
                      :source form
                      )
       new-env
       ))))


;;;----------------------------------------------------------------------------
;;; defstruct-form
;;; The really really big one, as it defines a lot of stuff automagically.
;;;
;;; Note. Parsing of DEFSTRUCT options is done "one at a time".  One
;;; could have collected, say, all the constructor options and parsed
;;; them at once, but then the overall parsed DEFSTRUCT would have had
;;; the parsed options reordered w.r.t. the source code.  This is
;;; judged undesirable.  Alas, this requires some more
;;; "post-processing" to deal with "defstruct wide" options effect.
;;; E.g., whether or not declare a default constructor.

#|
(defparameter *defstruct-options*
  '(:conc-name
    :constructor
    :copier
    :predicate
    :include
    :print-object
    :print-function
    :type
    :named))


(defun default-structure-fname (struct-name &rest names)
  (declare (type symbol struct-name))
  (intern
   (apply #'format nil "~A-~A" names)
   (symbol-package struct-name)))


(defun build-cons-key-arglist-types (parsed-slots)
  (loop for (slot-name nil . slot-keys) in parsed-slots
        collect
        (destructuring-bind (&key (type t) &allow-other-keys)
            slot-keys
          `(,(intern (string slot-name) "KEYWORD") ,type))
        into kvs
        finally (return (cons '&key kvs))
        )) ; THESE ARE TYPES!


(defun build-cons-key-arglist (parsed-slots)
  (loop for (slot-name initform) in parsed-slots
        collect `(,slot-name ,initform) ; ASSUME INITFORM UNPARSED (IT AIN'T SO).
        into kvs
        finally (return (cons '&key kvs))
        ))


(defun add-function-to-env (env fname args-decl return-type)
  (augment-environment
   env
   :function (list fname)
   :declare `((ftype (function ,args-decl ,return-type) ,fname))
   ))


(defgeneric parse-struct-option (opt-name
                                 opt
                                 struct-name
                                 env
                                 parsed-slots
                                 &key
                                 default-constructor-name
                                 default-copier-name
                                 default-predicate-name
                                 )
  (:method ((opt-name symbol)
            opt
            struct-name
            env
            parsed-slots
            &key
            default-constructor-name
            default-copier-name
            default-predicate-name
            )
   (declare (ignore parsed-slots
                    default-constructor-name
                    default-copier-name
                    default-predicate-name))
   (warn "Unrecognized or unimplemented parser for struct option ~S in structure ~S."
         opt-name
         struct-name)
   (values opt env)))


(defmethod parse-struct-option ((cons-kwd (eql :constructor))
                                cons-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                (default-constructor-name
                                 (default-structure-fname
                                  struct-name
                                  'make
                                  struct-name))
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-copier-name
                   default-predicate-name))

  (destructuring-bind (&optional (cname nil cname-supplied) arglist)
      cons-option
    (cond ((and cname-supplied (null cname))
           (values (list :constructor (parse nil :environment env))
                   env)
           )

          ((and cname (null arglist))
           (values `(:constructor ,cname)
                   (add-function-to-env env
                                        cname
                                        (build-cons-key-arglist parsed-slots)
                                        struct-name))
           )

          ((and cname arglist)
           (values `(:constructor ,cname ,arglist)
                   (add-function-to-env env
                                        cname
                                        arglist
                                        struct-name))
           )

          ((null cname-supplied)
           (values (list :constructor)
                   (add-function-to-env env
                                        default-constructor-name
                                        (build-cons-key-arglist parsed-slots)
                                        struct-name))
           )
          ))
  )


(defmethod parse-struct-option ((copier-kwd (eql :copier))
                                copier-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                (default-copier-name
                                 (default-structure-fname
                                  struct-name
                                  'copy
                                  struct-name))
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-predicate-name))

  (destructuring-bind (&optional (copier-fname nil copier-supplied))
      copier-option
    (cond ((and copier-supplied copier-fname)
           (values (list :copier copier-fname)
                   (add-function-to-env env
                                        copier-fname
                                        (list struct-name)
                                        struct-name))

           )
           
          ((and copier-supplied (null copier-fname))
           (values (list :copier (parse nil :environment env))
                   env)
           )
          
          ((null copier-supplied)
           (values (list :copier)
                   (add-function-to-env env
                                        default-copier-name
                                        (list struct-name)
                                        struct-name))
           )
          )))


(defmethod parse-struct-option ((predicate-kwd (eql :predicate))
                                predicate-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                (default-predicate-name
                                 (default-structure-fname
                                  struct-name
                                  struct-name
                                  'p))
                                )
  ;; Practically a duplicate of the :COPIER method.

  (declare (ignore default-constructor-name
                   default-copier-name))

  (destructuring-bind (&optional (predicate-fname nil predicate-supplied))
      predicate-option
    (cond ((and predicate-supplied predicate-fname)
           (values (list :predicate predicate-fname)
                   (add-function-to-env env
                                        predicate-fname
                                        (list struct-name)
                                        'boolean))
           )

          ((and predicate-supplied (null predicate-fname))
           (values (list :predicate (parse nil :environment env))
                   env)
           )

          ((null predicate-supplied)
           (values (list :predicate default-predicate-name)
                   (add-function-to-env env
                                        default-predicate-name
                                        (list struct-name)
                                        'boolean))
           )
          )))


(defmethod parse-struct-option ((printer-opt-kwd (eql :print-function))
                                printer-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))

  (destructuring-bind (&optional (printer-fname nil printer-supplied-p))
      printer-option
    (cond ((and printer-supplied-p printer-fname)
           (multiple-value-bind (fkind is-local f-decls)
               (function-information printer-fname env)
             (declare (ignore is-local f-decls))
             (if fkind
                 (values `(:print-function ,printer-fname) env)
                 (values `(:print-function ,printer-fname)
                         (add-function-to-env env
                                              printer-fname
                                              (list struct-name 'stream '(integer 0))
                                              'null))))
           )
          ((and printer-supplied-p (null printer-fname))
           (values `(:printer (parse nil :environment env))
                   env)
           )

          ((null printer-supplied-p)
           (values `(:print-function) env))
          )
    ))


(defmethod parse-struct-option ((printer-opt-kwd (eql :print-object))
                                printer-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))

  (destructuring-bind (&optional (printer-fname nil printer-supplied-p))
      printer-option
    (cond ((and printer-supplied-p printer-fname)
           (multiple-value-bind (fkind is-local f-decls)
               (function-information printer-fname env)
             (declare (ignore is-local f-decls))
             (if fkind
                 (values `(:print-object ,printer-fname) env)
                 (values `(:print-object ,printer-fname)
                         (add-function-to-env env
                                              printer-fname
                                              (list struct-name 'stream)
                                              'null))))
           )
          ((and printer-supplied-p (null printer-fname))
           (values `(:printer (parse nil :environment env))
                   env)
           )

          ((null printer-supplied-p)
           (values `(:print-object) env))
          )
    ))


(defmethod parse-struct-option ((include-kwd (eql :include))
                                include-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))

  (destructuring-bind (superstruct &rest slot-descriptions)
      include-option
    #|
    (warn "CLAST: uninplemented and incomplete parsing of DEFSTRUCT ~
                          :include options: slot modifiers still unimplemented.")
    |#
    (multiple-value-bind (parsed-slots parsed-slots-env)
        (parse-struct-slots struct-name struct-name slot-descriptions env ()) ; The () is wrong.
      (declare (ignore parsed-slots-env))
      (values `(:include ,superstruct ,@parsed-slots) env)
      )))


(defmethod parse-struct-option ((type-kwd (eql :type))
                                type-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (values (list :type (first type-option)) env))


(defmethod parse-struct-option ((initial-offset-kwd (eql :initial-offset))
                                init-offset-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (values (list :initial-offset (parse (first init-offset-option) env))
          env))


(defmethod parse-struct-option ((named-kwd (eql :named))
                                named-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (values (list :named) env))


(defmethod parse-struct-option ((named-kwd (eql :conc-name))
                                conc-name-option
                                struct-name
                                env
                                parsed-slots
                                &key
                                default-constructor-name
                                default-copier-name
                                default-predicate-name
                                )
  (declare (ignore default-constructor-name
                   default-copier-name
                   default-predicate-name))
  (destructuring-bind (&optional
                       (cn
                        (intern (format nil "~A-" struct-name)
                                (symbol-package struct-name)) ; A bit pointless...
                        conc-name-supplied)
                       )
      conc-name-option
    (cond ((and conc-name-supplied cn)
           (values (list :conc-name cn) env))
          ((and conc-name-supplied (null cn))
           (values (list :conc-name (parse nil :environment env)) env))
          )))


(defun parse-struct-slots (struct-name conc-name slots env keys)
  (let ((new-env env)
        (parsed-slots ())
        )
    (dolist (slot slots
                  (values (nreverse parsed-slots)
                          new-env))
      (multiple-value-bind (parsed-slot parsed-slot-env)
          (parse-struct-slot struct-name slot new-env keys)
        (push parsed-slot parsed-slots)
        
        ;; Add the slot reader.
        (setf new-env
              (add-function-to-env parsed-slot-env
                                   (default-structure-fname struct-name
                                                            conc-name
                                                            (first parsed-slot))
                                   (list struct-name)
                                   (if (getf (cddr parsed-slot) :type)
                                       (type-specifier-form-spec
                                        (getf (cddr parsed-slot) :type))
                                       T
                                       )))
        ))))


(defun parse-struct-slot (struct-name slot env keys)
  (declare (type list slot)
           (ignore struct-name))
  (destructuring-bind (slot-name
                       &optional
                       (slot-initform nil slot-initform-supplied)
                       &rest slot-keys
                       &key
                       (type t type-key-supplied)
                       (read-only nil read-only-key-supplied)
                       &allow-other-keys ; Funky code...
                       )
      slot

    ;; Note the environment that parses initforms.
    ;; It is necessary beacuse of verbiage in the CLHS
    ;; about initforms as lambda list keyword
    ;; initializers.

    (let ((new-slot-keys (copy-list slot-keys))
          (parsed-initform nil)
          )
      (when slot-initform-supplied
        (setf parsed-initform
              (apply #'parse slot-initform
                     :environment env
                     keys))
        )
      (when type-key-supplied
        (setf (getf new-slot-keys :type)
              (make-instance 'type-specifier-form
                             :spec type
                             :source type
                             ;; :top ...
                             ))
        )
      (when read-only-key-supplied
        (setf (getf new-slot-keys :read-only)
              (apply #'parse read-only
                     :environment env
                     keys))
        )
      (values `(,slot-name
                ,.(when slot-initform-supplied
                    (list parsed-initform))
                ,.new-slot-keys)
              env)))
  )


#| Moved to separate file...
(defmethod parse-form ((op (eql 'defstruct)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)

  (declare (ignore macroexpand))

  (destructuring-bind (ds-kwd name-n-options &rest spec)
      form
    (declare (ignore ds-kwd))
    (let* ((name-is-symbol (symbolp name-n-options))

           (name (typecase name-n-options
                   (symbol name-n-options)
                   (cons (first name-n-options))
                   (t (error 'ast-parse-error
                             :format-control "Illegal name and options in DEFSTRUCT: ~A."
                             :format-arguments (list name-n-options)))))

           (struct-name name)

           (options (typecase name-n-options
                      (cons (ensure-lists (rest name-n-options)))))

           (docstring (when (stringp (first spec))
                        (parse (first spec) :environment environment)))

           (slots (if docstring
                      (ensure-lists (rest spec))
                      (ensure-lists spec)))

           ;; Options unpacking.
           (default-conc-name
            (default-structure-fname struct-name
                                     struct-name
                                     ""))

           (conc-name (if name-is-symbol
                          default-conc-name
                          (let ((conc-name-opt
                                 (find :conc-name options :test #'eq :key #'first)))
                            (cond ((and conc-name-opt (second conc-name-opt))
                                   (second conc-name-opt))
                                  (conc-name-opt '||)
                                  (t default-conc-name))))
                      )

           (default-constructor-name
            (default-structure-fname struct-name
                                     'make
                                     struct-name))

           (default-copier-name
            (default-structure-fname struct-name
                                     'copy
                                     struct-name))

           (default-predicate-name
            (default-structure-fname struct-name
                                     struct-name
                                     'p))

  
           (parsed-opts ())
           (parsed-slots ())
           )

      (declare (type symbol
                     default-conc-name
                     default-constructor-name
                     default-copier-name
                     default-predicate-name
                     conc-name)
               )

      (labels ((parse-slot (slot env
                                 &aux
                                 (slot-name (first slot))
                                 (spec (rest slot)))
                 (declare (type list slot))
                 (destructuring-bind (slot-initform
                                      &rest slot-keys
                                      &key
                                      (type t)
                                      (read-only nil)
                                      &allow-other-keys ; Funky code...
                                      )
                     (or spec '(nil))
                   
                   (declare (ignore read-only))

                   ;; Note the environment that parses initforms.
                   ;; It is necessary beacuse of verbiage in the CLHS
                   ;; about initforms as lambda list keyword
                   ;; initializers.

                   (let* ((slot-accessor-name
                           (intern (format nil "~A~A" conc-name slot-name)
                                   (symbol-package slot-name)))
                          (parsed-initform
                           (apply #'parse slot-initform
                                  :environment env
                                  keys))
                          (new-env
                           (augment-environment
                            env
                            :function (list slot-accessor-name)
                            :declare (list `(ftype (function (,name) ,type)
                                                   ,slot-accessor-name))))
                          )
                     (values (list* slot-name parsed-initform slot-keys)
                             new-env)))
                 )

               (parse-slots (slots env &aux (new-env env))
                 (dolist (o slots new-env)
                   (multiple-value-bind (parsed-slot parsed-slot-env)
                       (parse-slot o new-env)
                     (push parsed-slot parsed-slots)
                     (setf new-env parsed-slot-env))))

               (select-opts (opt-kwd options)
                 (remove opt-kwd options :key #'first :test-not #'eq))

               (parse-options (options env)
                 (let ((cons-opts (select-opts :constructor options))
                       (copier-opts (select-opts :copier options))
                       (predicate-opts (select-opts :predicate options))
                       (type-opts (select-opts :type options))
                       (include-opts (select-opts :include options))
                       (print-function-opts (select-opts :print-function options))
                       (print-object-opts (select-opts :print-object options))
                       (init-offset-opts (select-opts :initial-offset options))
                       (new-env env)
                       )
                   (declare (ignore type-opts
                                    include-opts
                                    print-function-opts
                                    print-object-opts
                                    init-offset-opts
                                    ))

                   ;; Parse options in order.
                   (dolist (opt options)
                     (multiple-value-bind (parsed-opt parsed-opt-env)
                         (parse-struct-option (first opt)
                                              (rest opt)
                                              struct-name
                                              new-env
                                              parsed-slots
                                              :default-constructor-name default-constructor-name
                                              :default-copier-name default-copier-name
                                              :default-predicate-name default-predicate-name
                                              )
                       (push parsed-opt parsed-opts)
                       (setf new-env parsed-opt-env))
                     )

                   ;; Need to post-process options here, e.g., to
                   ;; declare default constructors.  Each call to the
                   ;; PARSE-STRUCT-OPTION function takes care of
                   ;; "present" options, but if the option is not
                   ;; present the appropriate default action (i.e.,
                   ;; the declaration of a function) must be performed.

                   (unless cons-opts
                     (setf new-env
                           (add-function-to-env new-env
                                                default-constructor-name
                                                (build-cons-key-arglist-types parsed-slots)
                                                struct-name))
                     )

                   (unless copier-opts
                     (setf new-env
                           (add-function-to-env new-env
                                                default-copier-name
                                                (list struct-name)
                                                struct-name))
                     )

                   (unless predicate-opts
                     (setf new-env
                           (add-function-to-env new-env
                                                default-predicate-name
                                                (list struct-name)
                                                'boolean))
                     )
                   
                   ;; PARSE-OPTIONS return values.
                   (values (nreverse parsed-opts) new-env))
                 )
               ) ; local functions.

        (multiple-value-bind (parsed-slots parsed-slot-env)
            (parse-struct-slots struct-name conc-name slots environment keys)
          (multiple-value-bind (parsed-opts opts-env)
              (parse-options options parsed-slot-env)

            (let* ((def-form
                    (make-instance 'defstruct-form
                                   :name name
                                   :options parsed-opts
                                   :slots (nreverse parsed-slots)

                                   :source form
                                   :top enclosing-form
                                   ;; :type T
                                   ))
                   )
              
              (values def-form opts-env)
              )))
        ))
    ))
|#


#| #| Old version not yet refactored by extracting the parsing of the
      separate options. |#
(defmethod parse-form ((op (eql 'defstruct)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)
  (destructuring-bind (ds-kwd name-n-options &rest spec)
      form
    (let* ((name-is-symbol (symbolp name-n-options))
           (name (typecase name-n-options
                   (symbol name-n-options)
                   (cons (first name-n-options))
                   (t (error 'ast-parse-error
                             :format-control "Illegal name and options in DEFSTRUCT: ~A."
                             :format-arguments (list name-n-options)))))
           (options (typecase name-n-options
                      (cons (ensure-lists (rest name-n-options)))))
           (docstring (if (stringp (first spec)) (first spec)))
           (slots (if docstring
                      (ensure-lists (rest spec))
                      (ensure-lists spec)))

           (slot-names (mapcar #'first slots))

           ;; Options unpacking.
           (default-conc-name (format nil "~A-" name))
           (conc-name (if name-is-symbol
                          default-conc-name
                          (let ((conc-name-opt
                                 (find :conc-name options :test #'eq :key #'first)))
                            (cond ((and conc-name-opt (second conc-name-opt))
                                   (string (second conc-name-opt)))
                                  (conc-name-opt "")
                                  (t default-conc-name))))
                      )
           (default-constructor-name (format nil "~A-~A" 'make name))
           (parsed-opts ())
           (parsed-slots ())
           )
      (declare (type string default-conc-name conc-name))

      (labels ((parse-slot (slot env
                                 &aux
                                 (slot-name (first slot))
                                 (spec (rest slot)))
                 (declare (type list slot))
                 (destructuring-bind (slot-initform
                                      &rest slot-keys
                                      &key
                                      (type t)
                                      (read-only nil)
                                      &allow-other-keys ; Funky code...
                                      )
                     (or spec '(nil))

                   ;; Note the environment that parses initforms.
                   ;; It is necessary beacuse of verbiage in the CLHS
                   ;; about initforms as lambda list keyword
                   ;; initializers.

                   (let* ((slot-accessor-name
                           (intern (format nil "~A~A" conc-name slot-name)
                                   (symbol-package slot-name)))
                          (parsed-initform
                           (apply #'parse slot-initform
                                  :environment env
                                  keys))
                          (new-env
                           (augment-environment
                            env
                            :function (list slot-accessor-name)
                            :declare (list `(ftype (function (,name) ,type)
                                                   ,slot-accessor-name))))
                          )
                     (values (list* slot-name parsed-initform slot-keys)
                             new-env)))
                 )

               (parse-slots (slots env &aux (new-env env))
                 (dolist (o slots new-env)
                   (multiple-value-bind (parsed-slot parsed-slot-env)
                       (parse-slot o new-env)
                     (push parsed-slot parsed-slots)
                     (setf new-env parsed-slot-env))))

               (build-cons-key-arglist (parsed-slots)
                 (loop for (slot-name initform) in parsed-slots
                       collect `(,slot-name ,initform))) ; ASSUME INITFORM UNPARSED (IT AIN'T SO)

               (build-cons-key-arglist-types (parsed-slots)
                 (loop for (slot-name nil . slot-keys) in parsed-slots
                       collect
                       (destructuring-bind (&key (type t) &allow-other-keys)
                           slot-keys
                         `(,slot-name ,type)))) ; THESE ARE TYPES!



               (parse-options (options env)
                 (dolist (opt options (values parsed-opts env))
                   (parse-struct-option (first opt) opt env parsed-slots)
                   ))
               
               (parse-constructor-opts (cons-options
                                        env
                                        parsed-slots
                                        &aux
                                        (new-env env)
                                        (default-cons-name
                                         (intern default-constructor-name
                                                 (symbol-package name)))
                                        (do-build-default-cons t)
                                        (parsed-cons-opts ())
                                        )
                 
                 (dolist (cons-opt cons-options new-env)
                   (destructuring-bind (cons-kwd
                                        &optional
                                        (cname nil cname-supplied)
                                        arglist)
                       cons-opt
                     (declare (ignore cons-kwd))
                     (cond ((and cname-supplied (null cname))
                            (setf do-build-default-cons nil)
                            (push (copy-list cons-opt) parsed-cons-opts))
                           ((and cname (null arglist))
                            (push `(:constructor
                                    ,cname
                                    ,(build-cons-key-arglist parsed-slots))
                                  parsed-cons-opts))
                           ((and cname arglist)
                            (push `(:constructor
                                    ,cname
                                    ,arglist)
                                  parsed-cons-opts))
                           ((null cname-supplied)
                            (push (copy-list cons-opt) parsed-cons-opts))
                           )))
                 (when do-build-default-cons
                   (setf new-env
                         (augment-environment
                          new-env
                          :function (list cons-name)
                          :declare `((ftype (function
                                             ,(build-cons-key-arglist parsed-slots)
                                             ,name)
                                            ,cons-name)))
                         ))
                 (values parsed-cons-opts
                         new-env)
                 )

               (parse-copier-option (copier-option env)
                 (destructuring-bind (copier-kwd
                                      &optional 
                                      (copier-fname
                                       nil
                                       copier-supplied-p))
                     copier-option
                   (cond ((and copier-supplied
                               copier-fname)
                          (setf copier-name copier-name) ; Redundant but symmetric.
                          )
                         ((and copier-supplied
                               (null copier-fname))
                          (setf copier-name nil) ; Redundant, but symmetric.
                          )
                         ((null copier-supplied)
                          (setf copier-fname
                                (intern (format nil "~A-~A" 'copy name) ; Wrongish... case.
                                        (symbol-package name)))
                          ))
                   (if copier-name
                       (values
                        `(:copier ,copier-name)
                        (augment-environment env
                                             :function (list copier-name)
                                             :declare (list `(ftype (function (,name) ,name)
                                                                    ,copier-fname))))
                       (values
                        `(:copier ,(parse nil))
                        env))
                   ))


               (parse-predicate-option (predicate-option env) ; Like COPIER.
                 (destructuring-bind (predicate-kwd
                                      &optional 
                                      (predicate-fname
                                       nil
                                       predicate-supplied-p))
                     predicate-option
                   (cond ((and predicate-supplied
                               predicate-fname)
                          (setf predicate-name predicate-name) ; Redundant but symmetric.
                          )
                         ((and predicate-supplied
                               (null predicate-fname))
                          (setf predicate-name nil) ; Redundant, but symmetric.
                          )
                         ((null predicate-supplied)
                          (setf predicate-fname
                                (intern (format nil "~A-P" name) ; Wrongish... case.
                                        (symbol-package name)))
                          ))
                   (if predicate-name
                       (values
                        `(:predicate ,predicate-name)
                        (augment-environment env
                                             :function (list predicate-name)
                                             :declare (list `(ftype (function (,name) boolean)
                                                                    ,predicate-fname))))
                       (values
                        `(:predicate ,(parse nil))
                        env))
                   ))

               (parse-include-opt (include-opt env)
                 (destructuring-bind (include-kwd superstruct &rest slot-descriptions)
                     include-opt
                   (declare (ignore include-kwd))
                   (warn "CLAST: uninplemented and incomplete parsing of DEFSTRUCT ~
                          :include options: slot modifiers still unimplemented.")
                   env
                   ))
               

               (parse-printer-option (printer-option env)
                 (destructuring-bind (printer-kwd
                                      &optional 
                                      (printer-fname
                                       nil
                                       printer-supplied-p))
                     printer-option
                   (if printer-supplied-p
                       (multiple-value-bind (fkind is-local f-decls)
                           (function-information printer-fname env)
                         (if fkind
                             (values `(,printer-option ,printer-name)
                                     env)
                             (let* ((ftype
                                     (ecase printer-option
                                       (:print-function
                                        `(ftype (function (,name stream (integer 0)) null)
                                                ,printer-name))
                                       (:print-object
                                        `(ftype (function (,name stream) null)
                                                ,printer-name))))
                                    
                                    (new-env
                                     (augment-environment
                                      env
                                      :function (list printer-name)
                                      :declare (list ftype)))
                                    )
                               (values `(,printer-option , printer-name)
                                       new-env))))
                       (values `(,printer-option) env))
                   ))


               (parse-type-option (type-option env)
                 (values (list :type (second type-option)) env))

               (parse-initial-offset (init-offset-option env)
                 (values (list :initial-offset (parse (second init-offset-option) env))
                         env))


               ) ; local functions.

        (let ((slots-env (parse-slots slots environment)))
          (multiple-value-bind (parsed-opts opts-env)
              (parse-options options environment)

            (let* ((def-form
                    (make-instance 'defstruct-form
                                   :name name
                                   :options parsed-opts
                                   :slots parsed-slots

                                   :source form
                                   ;; :top ...
                                   ;; :type T
                                   ))
                   )
              
              (values def-form
                      environment)
              ))
          )))
    ))
|#


(defmethod clast-element-subforms ((df defstruct-form))
  (list (defstruct-form-name df)
        (defstruct-form-options df)
        (defstruct-form-slots df)))
|#

;;;; end of file -- parse-def.lisp
