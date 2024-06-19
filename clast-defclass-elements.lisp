;;;; -*- Mode: Lisp -*-

;;;; clast-defclass-elements.lisp --
;;;; Definitions of the DEFCLASS "subforms" necessary for parsing of
;;;; "defclass" constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Notes:
;;;; 2024-06-19 MA:
;;;; Factored out of 'parse-defclass' to improve code dependencies.


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


(defmethod clast-element-subforms ((df defclass-form))
  (list* (defclass-form-name df)
         (defclass-form-options df)
         (defclass-form-slots df)))


;;;; end of file -- clast-defclass-elements.lisp --
