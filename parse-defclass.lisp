;;;; -*- Mode: Lisp -*-

;;;; parse-defclass.lisp --
;;;; Parsing of "defclass" constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.
;;;;
;;;; Parsing DEFCLASS is not as complicated as parsing DEFSTRUCT but
;;;; it still requires some hairy code.
;;;;
;;;; As per LOOP two kinds of sub-forms are defined to deal with slots
;;;; and options.


;;;----------------------------------------------------------------------------
;;; defclass-subform
;;; class-option-subform, class-slot-subform, class-slot-option-subform

(defclass defclass-subform (form) ())


(defclass class-slot-subform (defclass-subform)
  ((name :reader class-slot-subform-name
         :initarg :slot-name) ; Inherited; new reader and initarg added.
   (options :reader class-slot-subform-options
                  :initarg :slot-options)
   )
  )


(defun is-class-slot-subform (x)
  (typep x 'class-slot-subform))


(defun class-slot-subform-p (x)
  (is-class-slot-subform x))


(defun make-class-slot-form (slot-name options)
  (make-instance 'class-slot-subform
         :slot-name slot-name
         :slot-options options
         ))


(defun make-class-slot-form* (slot-name &rest options)
  (make-class-slot-form slot-name options))


(defmethod clast-element-subforms ((sos class-slot-subform))
  (list (class-slot-subform-name sos)
        (class-slot-subform-options sos)
        ))


(defclass class-option-subform (defclass-subform)
  ((name :reader class-option-subform-name
         :initarg :option-name) ; Inherited; new reader and initarg added.
   (spec :reader class-option-subform-spec
         :initarg :option-spec)
   )
  )


(defun is-class-option-subform (x)
  (typep x 'class-option-subform))


(defun class-option-subform-p (x)
  (is-class-option-subform x))


(defun make-class-option-form (option-name &rest spec)
  (make-instance 'class-option-subform
                 :option-name option-name
                 :option-spec spec))


(defmethod clast-element-subforms ((sos class-option-subform))
  (list (class-option-subform-name sos)
        (class-option-subform-spec sos)))


;;;----------------------------------------------------------------------------
;;; defclass-form

(defparameter *defclass-options*
  '(:default-initargs
    :documentation
    :metaclass))


(defparameter *defclass-slot-options*
  '(:reader
    :writer
    :accessor
    :allocation
    :initarg
    :initform
    :type
    :documentation
    ))

#|
(defun add-function-to-env (env fname args-decl return-type)
  (augment-environment
   env
   :function (list fname)
   :declare `((ftype (function ,args-decl ,return-type) ,fname))
   ))
|#

;;;---------------------------------------------------------------------------
;;; parse-class-option, parse-class-options

(defgeneric parse-class-option (opt-name
                                opt
                                class-name
                                enclosing-env
                                class-env
                                parsed-slots
                                keys
                                )
  (:documentation "This (generic) function parses a class option.

The method of this generic function dispatch on the OPT-NAME (which is
usually an EQL specializer).  OPT is the full class option form.  The
function keeps track of two environments and per ANSI: the
ENCLOSING-ENV, which is the environment in which the DEFCLASS form is
parsed (and/or evaluated) and the CLASS-ENV, which is the environment
which 'accumulates' the effects of parsing.")


  (:method ((opt-name symbol)
            opt
            class-name
            enclosing-env
            class-env
            parsed-slots
            keys
            )
   (declare (ignore class-env parsed-slots keys))
   (warn "Unrecognized or unimplemented parser for class option ~S in class ~S."
         opt-name
         class-name)
   (values (apply #'make-class-option-form opt-name opt)
           class-env)))


(defmethod parse-class-option ((di-kwd (eql :default-initargs))
                                di-option
                                class-name
                                enclosing-env
                                class-env
                                parsed-slots
                                keys
                                )
  (declare (ignore parsed-slots))
  (values (make-class-option-form
           di-kwd
           (loop for (initarg initform) on di-option by #'cddr
                 collect (list initarg
                               (apply #'parse initform :environment env keys))))
          env)
  )


(defmethod parse-class-option ((doc-kwd (eql :documentation))
                                doc-option
                                class-name
                                enclosing-env
                                class-env
                                parsed-slots
                                keys
                                )
  (declare (ignore parsed-slots))
  (values (make-class-option-form
           doc-kwd
           (list (apply #'parse (first doc-option)
                        :environment env
                        keys)))
          env)
  )


(defmethod parse-class-option ((metac-kwd (eql :metaclass))
                                metac-option
                                class-name
                                enclosing-env
                                class-env
                                parsed-slots
                                keys
                                )
  (declare (ignore parsed-slots))
  (values (make-class-option-form
           metac-kwd
           (list (apply #'parse (first metac-option)
                        :environment env
                        keys)))
          env)
  )


(defun parse-class-options (class-name options env keys
                                       &aux
                                       parsed-class-options
                                       (new-env env)
                                       )
  (dolist (opt options
               (values (nreverse parsed-class-options)
                       new-env))
    (multiple-value-bind (parsed-opt parsed-opt-env)
        (parse-class-option (first opt)
                            (rest opt)
                            class-name
                            env
                            new-env
                            ()
                            keys)
      (push parsed-opt parsed-class-options)
      (setf new-env
            parsed-opt-env))))


;;;---------------------------------------------------------------------------
;;; parse-class-slots, parse-class-slot, parse-class-slot-option

(defgeneric parse-class-slot-option (option
                                     option-value
                                     class-name
                                     slot-name
                                     enclosing-env
                                     class-env
                                     keys)
  (:documentation "Parses the options of a class slot definition.")
  (:method ((option symbol)
            option-value
            class-name
            slot-name
            enclosing-env
            class-env
            keys)
   (warn "Unrecognized or unimplemented class slot option ~A for ~
          slot ~A in class ~A."
         option
         slot-name
         class-name)
   (values (list option option-value)
           class-env))
  )


(defmethod parse-class-slot-option ((option (eql :reader))
                                    option-value
                                    class-name
                                    slot-name
                                    enclosing-env
                                    class-env
                                    keys)
  (values (list :reader option-value)
          class-env))


(defmethod parse-class-slot-option ((option (eql :writer))
                                    option-value
                                    class-name
                                    slot-name
                                    enclosing-env
                                    class-env
                                    keys)
  (values (list :writer option-value)
          class-env))


(defmethod parse-class-slot-option ((option (eql :accessor))
                                    option-value
                                    class-name
                                    slot-name
                                    enclosing-env
                                    class-env
                                    keys)
  (values (list :accessor option-value)
          class-env))


(defmethod parse-class-slot-option ((option (eql :initarg))
                                    option-value
                                    class-name
                                    slot-name
                                    enclosing-env
                                    class-env
                                    keys)
  (values (list :initarg option-value)
          env))


(defmethod parse-class-slot-option ((option (eql :allocation))
                                    option-value
                                    class-name
                                    slot-name
                                    enclosing-env
                                    class-env
                                    keys)
  (values (list :allocation option-value)
          env))
                


(defun parse-class-slots (class-name slots env keys)
  (let ((new-env env)
        (parsed-slots ())
        )
    (flet ((slot-declared-type (cs-opts)
             (let ((type-opt (find :type cs-opts :key #'first)))
               (if type-opt
                   (second type-opt)
                   t)))
           )
      (dolist (slot slots)
        (multiple-value-bind (parsed-slot parsed-slot-env)
            (parse-class-slot class-name slot env keys) ; Check the ENV argument!
                                                      ; It is so as
                                                      ; per ANSI.
          (push parsed-slot parsed-slots)))
        

      ;; Now we have done a first pass over the slot options.
      ;; Add the slot readers, writers, accessors...
    
      (dolist (ps parsed-slots)
        (loop with cs-opt = (class-slot-subform-options ps)
              for (so so-value) in cs-opt
              do (case so
                   ((:reader :accessor)
                    (setf new-env
                          (augment-environment
                           new-env
                           :function (list so-value)
                           :declare `((ftype (function (,class-name)
                                                       ,(slot-declared-type cs-opt))
                                             ,so-value)))))
                   (:writer
                    (setf new-env
                          (augment-environment
                           new-env
                           :function (list `(setf ,so-value))
                           :declare `((ftype
                                       (function (,(slot-declared-type cs-opt)
                                                  ,class-name)
                                                 ,(slot-declared-type cs-opt))
                                       (setf ,so-value))))))
                   ))
        )
        
      ;; Finally return the values.
      (values (nreverse parsed-slots)
              new-env))
    ))


#| No PARSE-CLASS-SLOT-OPTION
(defun parse-class-slot (class-name slot env keys)
  (declare (type list slot)
           (ignore class-name))

  (destructuring-bind (slot-name
                       &rest slot-options
                       &key
                       allocation
                       initform
                       type
                       documentation
                       &allow-other-keys
                       ;; readers, writers, accessors and initargs
                       )
      slot

    ;; Note: the options are not processed in the order received.

    (flet ((collect-options (opt)
             (loop for (kwd opt-value) on slot-options by #'cddr
                   when (eq opt kwd) collect opt-value))

           (remove-options (opt)
             (loop while (remf slot-options opt))) ; Daring code! Ain't it?
           )
      (let ((readers   (collect-options :reader))
            (writers   (collect-options :writer))
            (accessors (collect-options :accessor))
            (initargs  (collect-options :initarg))
            (other-options
             (progn (remove-options :reader)
               (remove-options :writer)
               (remove-options :accessor)
               (remove-options :initarg)

               (remove-options :allocation)
               (remove-options :initform)
               (remove-options :type)
               (remove-options :documentation)
               slot-options
               ))
            )
        
        (values (make-class-slot-form
                 slot-name
                 `(,@(when allocation
                       (list :allocation
                             (apply #'parse allocation keys))) ; A bit of an overkill
                                                               ; But symmetric FTTB.
                   ,@(when initform
                       (list :initform
                             (apply #'parse initform keys)))

                   ,@(when type
                       (list :type
                             (apply #'parse type keys)))

                   ,@(when documentation
                       (list :documentation
                             (apply #'parse documentation keys)))
                   
                   ,@(mapcan (lambda (r)
                               (list :reader (apply #'parse r keys)))
                             readers)

                   ,@(mapcan (lambda (w)
                               (list :writer (apply #'parse w keys)))
                             writers)

                   ,@(mapcan (lambda (a)
                               (list :accessor (apply #'parse a keys)))
                             accessors)

                   ,@(mapcan (lambda (i)
                               (list :initarg (apply #'parse i keys)))
                             initargs)

                   ,@(when slot-options ; Remaining ones; unparsed FTTB.
                       (copy-list slot-options))
                   ))
                env)))
    ))
|#


(defun parse-class-slot (class-name slot env keys
                                    &aux
                                    parsed-slot-options
                                    (new-env env)
                                    )
  (declare (type list slot))

  (destructuring-bind (slot-name &rest slot-options)
      slot
    (loop for (opt opt-value) on slot-options by #'cddr
          do (multiple-value-bind (parsed-slot-option
                                   parsed-slot-env)
                 (parse-class-slot-option opt opt-value
                                          class-name
                                          slot-name
                                          env
                                          new-env
                                          keys)
               (push parsed-slot-option
                     parsed-slot-options)
               (setf new-env ; This is a NO-OP FTTB. See PARSE-CLASS-SLOT-OPTION.
                     parsed-slot-env)
               )
          )

    (values (make-class-slot-form slot-name (nreverse parsed-slot-options))
            new-env)
    ))


;;;---------------------------------------------------------------------------
;;; parse-form defclass

(defmethod parse-form ((op (eql 'defclass)) form
                       &rest keys
                       &key
                       enclosing-form
                       environment
                       macroexpand
                       &allow-other-keys)

  (declare (ignore macroexpand enclosing-form))

  (destructuring-bind (dc-kwd name superclasses slots &rest options)
      form
    (declare (ignore dc-kwd))

    (multiple-value-bind (parsed-slots parsed-slots-env)
        (parse-class-slots name slots environment keys)
      (multiple-value-bind (parsed-opts parsed-opts-env)
          (parse-class-options name options parsed-slots-env keys)
        (values (make-instance 'defclass-form
                               :name name
                               :superclasses superclasses
                               :slots parsed-slots
                               :options parsed-opts)
                parsed-opts-env)
        ))))


(defmethod clast-element-subforms ((df defclass-form))
  (list* (defclass-form-name df)
         (defclass-form-options df)
         (defclass-form-slots df)))


;;;; end of file -- parse-defclass.lisp --
