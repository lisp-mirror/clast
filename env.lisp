;;;; -*- Mode: Lisp -*-

;;;; env.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.

;;;; See file COPYING in main folder for licensing and copyright information.

;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

(in-package "CLAST")

;;;; env --
;;;; As correctly noted in CL-WALKER you need (at least) two
;;;; environments: one relative to the "walking" procedure and the other that
;;;; is the standard CL "environment" (as in the &environment macro keyword).
;;;;
;;;; The "walking environment" is essentially a wrapper for the CLtL2
;;;; "environments" with a few extra bells and whistles.

(defstruct (env-wrapper (:conc-name nil)
                        (:constructor wrap-environment (&optional environment))
                        (:constructor make-env (&optional environment)) 
                        )
  (environment nil))


;;;; Environment queries.

(defun special-variable-p (v env)
  (declare (type symbol v))
  (eq :special (variable-information v env)))


(defun constant-or-keyword-p (s env)
  (declare (type symbol s))
  (eq :constant (variable-information s env)))


(defun symbol-macro-p (s env)
  (declare (type symbol s))
  (eq :symbol-macro (variable-information s env)))


;;;; end of file -- env.lisp --
