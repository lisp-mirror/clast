;;;; -*- Mode: Lisp -*-

;;;; clast-loop-elements.lisp --
;;;; Parsing of iteration constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.

(defparameter *loop-keywords*
  #(:named 

    :with
    :initially :finally
    :for :as

    :being
    :the

    :hash-key :hash-keys :hash-value :hash-values
    :using

    :do :doing
    :return

    :collect :collecting
    :nconc :nconcing

    :sum :summing
    :count :counting
    :maximize :maximizing
    :minimize :minimizing

    :when :if :unless
    :else
    :end

    :while :until :repeat
    
    :always :thereis :never

    :and

    :of-type
    :into

    ;; ... and some extra ones...

    :record :records
    :tuple :tuples

    :over

    )
  "Set (as a vector) of all the LOOP keywords, plus some extra ones.

The semi-standard SQL querying keywords (RECORD, RECORDS, TUPLE and
TUPLES) and other ones are included in the set."
  )


(defparameter *loop-arithmetic-clause-keywords*
  #(:from :downfrom
    :to :upto :below :above :downto
    :by
    ))


(defparameter *loop-accumulation-clause-keywords*
  #(:collect :collecting
    :nconc :nconcing

    :sum :summing
    :count :counting 
    :maximize :maximizing
    :minimize :minimizing))


(declaim (ftype (function (t) boolean)
                is-loop-keyword
                is-loop-arithmetic-keyword)
         (inline is-loop-keyword
                 is-loop-arithmetic-keyword))

(defun is-loop-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-keywords*
                        :test #'string-equal)))))


(defun is-loop-arithmetic-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-arithmetic-clause-keywords*
                        :test #'string-equal)))))


(defun is-loop-accumulation-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-accumulation-clause-keywords*
                        :test #'string-equal)))))


(declaim (ftype (function (t (or symbol string)) boolean) loop-kwd=)
         (inline loop-kwd=))

(defun loop-kwd= (loop-kwd kwd)
  (declare (type (or symbol string) kwd))
  (and (symbolp loop-kwd)
       (string-equal loop-kwd kwd)))


(declaim (ftype (function ((or symbol string)) keyword) as-loop-kwd)
         (inline as-loop-kwd))

(defun as-loop-kwd (kwd)
  (declare (type (or symbol string) kwd))
  (intern (string kwd) "KEYWORD"))


;;;---------------------------------------------------------------------------
;;; loop-clause

(defclass loop-clause (form) ; named, for, as, when, etc.
  ((name :reader loop-clause-name
         :reader form-name
         :type (or symbol string)
         :initarg :name
         )
   (subclauses :reader loop-clause-subclauses
               :accessor subclauses
               :type list
               :initarg :subclauses
               :initform ()
               )
   )

  (:documentation "The Loop Clause Class.

The class of all the LOOP clause forms.")
  )


(defmethod print-object ((lc loop-clause) stream)
  (print-unreadable-object (lc stream :identity t)
    (format stream "LOOP CLAUSE: ~A ~S"
            (loop-clause-name lc)
            (subclauses lc))))


(defun is-loop-clause (x)
  (typep x 'loop-clause))


(defun make-loop-clause (clause-name subclauses &optional source top)
  (make-instance 'loop-clause
                 :name clause-name
                 :subclauses subclauses
                 :top top
                 :source source
                 ))


(defmethod clast-element-subforms ((lc loop-clause))
  (cons (loop-clause-name lc) (subclauses lc)))


(defclass loop-subclause (loop-clause) ()) ; of-type, using, by etc.


(defun is-loop-subclause (x)
  (typep x 'loop-subclause))


(defun make-loop-subclause (clause-name subclauses &optional source top)
  (make-instance 'loop-subclause
                 :name clause-name
                 :subclauses subclauses
                 :top top
                 :source source))


(defun reverse-subclauses (lc)
  (declare (type loop-clause lc))
  (setf (slot-value lc 'subclauses)
        (nreverse  (slot-value lc 'subclauses)))
  lc)


;;; loop-var-subclause --
;;; With this class I deal with all "variables" that are
;;; introduced in a LOOP form.

(defclass loop-var-subclause (loop-subclause)
  ((name :initarg :var
         :reader var-form
         :type (or symbol cons)
         )
   (of-type :initarg :of-type
            :initform t
            :reader loop-var-of-type-qualifier)
   )
  )


(defun is-loop-var-subclause (x)
  (typep x 'loop-var-subclause))


(defun make-loop-var-subclause (name of-type subclauses &optional top source)
  (make-instance 'loop-var-subclause
                 :var name
                 :of-type of-type
                 :subclauses subclauses
                 :top top
                 :source source))


(defmethod print-object ((lvc loop-var-subclause) stream)
  (print-unreadable-object (lvc stream :identity t)
    (format stream "LOOP VAR SUBCLAUSE: ~A ~S"
            (var-form lvc)
            (subclauses lvc))))


;;;; end of file -- clast-loop-elements.lisp --
