;;;; -*- Mode: Lisp -*-

;;;; clast-defstruct-elements.lisp --
;;;; Definitions of the DEFSTRUCT "subforms" necessary for parsing of
;;;; "defstruct" constructs.

;;;; See the file COPYING for copyright and license information.


;;;; Notes:
;;;; 2024-06-19 MA:
;;;; Factored out of 'parse-defclass' to improve code dependencies.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.
;;;;
;;;; Parsing DEFSTRUCT is more complicated than parsing other
;;;; definition forms (DEFCLASS included); this is due to the fact
;;;; that DEFSTRUCT defines a lot of stuff automagically.
;;;;
;;;; As per LOOP two kinds of sub-forms are defined to deal with slots
;;;; and options.

;;;----------------------------------------------------------------------------
;;; defstruct-subform, struct-option-subform, struct-slot-subform

(defclass defstruct-subform (form) ()
  (:documentation "The Defstruct Subform Class.

This is the (sub) root of all the 'defstruct' parts'."))


(defclass struct-slot-subform (defstruct-subform)
  ((name :reader struct-slot-subform-name
         :initarg :slot-name) ; Inherited; new reader and initarg added.
   (initform :reader struct-slot-subform-initform
             :initarg :slot-initform)
   (slot-type :reader struct-slot-subform-type
              :initarg :slot-type)
   (read-only :reader struct-slot-subform-read-only
              :initarg :slot-read-only)
   (other-options :reader struct-slot-subform-other-options
                  :initarg :other-options)
   )
  (:documentation "The Defstruct Slot Subform Class.

The forms representing the slots in a 'defstruct'.")
  )


(defun is-struct-slot-subform (x)
  (typep x 'struct-slot-subform))


(defun struct-slot-subform-p (x)
  (is-struct-slot-subform x))


(defun make-struct-slot-form (slot-name
                              &optional
                              initform
                              &rest keys
                              &key
			      (type (make-instance
				     'type-specifier-form
				     :spec t))
			      (read-only nil)
                              &allow-other-keys
                              )
  (remf keys :type)
  (remf keys :read-only)
  (make-instance 'struct-slot-subform
         :slot-name slot-name
         :slot-initform initform
         :slot-read-only read-only
         :slot-type type
         :other-options keys))


(defmethod clast-element-subforms ((sos struct-slot-subform))
  (list (struct-slot-subform-name sos)
        (struct-slot-subform-initform sos)
        (struct-slot-subform-type sos)
        (struct-slot-subform-other-options sos)
        ))


(defclass struct-option-subform (defstruct-subform)
  ((name :reader struct-option-subform-name
         :initarg :option-name) ; Inherited; new reader and initarg added.
   (spec :reader struct-option-subform-spec
         :initarg :option-spec)
   )
  (:documentation "The Defstruct Option Subform Class.

The forms representing the options in a 'defstruct'.")
  )


(defun is-struct-option-subform (x)
  (typep x 'struct-option-subform))


(defun struct-option-subform-p (x)
  (is-struct-option-subform x))


(defun make-struct-option-form (option-name &rest spec)
  (make-instance 'struct-option-subform
                 :option-name option-name
                 :option-spec spec))


(defmethod struct-option-subform-arg1 ((sos struct-option-subform))
  (first (struct-option-subform-spec sos)))


(defmethod struct-option-subform-args ((sos struct-option-subform))
  (rest (struct-option-subform-spec sos)))


(defgeneric struct-option-subform-bare-p (sos)
  (:method ((sos struct-option-subform))
   (null (struct-option-subform-spec sos)))
  (:documentation "Returns true if the option was 'bare'.

I.e, when the option did not have a 'main' argument; this is the case
for :CONSTRUCTOR, :COPIER and :PREDICATE.")
  )


(defmethod clast-element-subforms ((sos struct-option-subform))
  (if (struct-option-subform-bare-p sos)
      (list (struct-option-subform-name sos))
      (list* (struct-option-subform-name sos)
             (struct-option-subform-spec sos))))


(defparameter *defstruct-options*
  '(:conc-name
    :constructor
    :copier
    :predicate
    :include
    :print-object
    :print-function
    :type
    :named)
  "The set, as a list, of (known and standard) 'defstruct' options.")


;;;----------------------------------------------------------------------------
;;; defstruct-form

(defmethod clast-element-subforms ((df defstruct-form))
  (list* (defstruct-form-name df)
         (defstruct-form-options df)
         (defstruct-form-slots df)))


;;;; end of file -- clast-defstruct-elements.lisp
