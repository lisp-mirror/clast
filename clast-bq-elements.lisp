;;;; -*- Mode: Lisp -*-

;;;; clast-bq-elements.lisp --
;;;;
;;;; Definitons related to backquotes; which are very implementation
;;;; dependent.

;;;; Some code lifted from IT.BESE.ARNESI and CL-WALKER.

;;;; See file COPYING in main folder for licensing and copyright information.

(in-package "CLAST")


;;; bq-form

(defclass bq-form (form)
  ((expr :initarg :expr
         :reader bq-form-expr
         )
   )
  (:documentation "The BQ Form Class.

The forms that represent \"backquote\" expressions.

Notes:

The actual forms are very implementation dependent.")
  )


(defun bq-form-p (x) (typep x 'bq-form))


;;; bq-comma

(defclass bq-comma (form)
  ((kind :initarg :kind
         :reader bq-comma-kind
         :type (member :value  ; Expressions like ,EXPR.
                       :splice ; Expressions like ,@EXPR
                       :nconc  ; Expressions like ,.EXPR
                       )
         )
   (expr :initarg :expr
         :reader bq-comma-expr)
   )
  (:default-initargs :kind :value)
  (:documentation "The BQ Comma Class.

The \"comma\" expressions in a backquote structure.

Notes:

These elements are patterned after SBCL.  In fact, the introduction of
these classes was made necessary to handle SBCL backquote constructs.
Cfr., the implementation dependent code.")
  )

(defun bq-comma-p (x) (typep x 'bq-comma))


;;;;---------------------------------------------------------------------------
;;;; Subform extraction.

(defmethod clast-element-subforms ((bqf bq-form))
  (bq-form-expr bqf)
  )


(defmethod clast-element-subforms ((bqc bq-comma))
  (bq-comma-expr bqc)
  )


;;;; end of file -- clast-bq-elements.lisp --
