;;;; -*- Mode: Lisp -*-

;;;; env.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.

;;;; See file COPYING in main folder for licensing and copyright information.

;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

(in-package "CLAST")

;;;; env --
;;;; As correctly noted in CL-WALKER you need to have a hairy
;;;; environment structure to pan over incompatibilities.  You need (at least) two
;;;; environments: one relative to the "walking" procedure and the other that
;;;; is the standard CL "environment" (as in the &environment macro keyword).
;;;;
;;;; The "walking environment" is essentially a wrapper for the CLtL2
;;;; "environments" with a few extra bells and whistles.

;;;; env-wrapper --
;;;; Note: This is the "opaque" structure par excellence.
;;;; The actual 'environment' API is in the "impl-dependent" files.
;;;;
;;;; It would be nice to :include the implementation dependent
;;;; environment.  Alas, we cannot assume that the actual
;;;; implementation dependent environment is a structure.

(defstruct (env-wrapper (:conc-name %env-wrapper-)
                        (:constructor %make-env-wrapper (&optional environment)) 
                        )
  (environment nil) ; The &environment CL environment.
  )


;;;; Convenience accessor.

(declaim (inline inplementation-env)
         (ftype (function (env-wrapper) t) inplementation-env))

(defun implementation-env (ew)
  (declare (type env-wrapper ew))
  (%env-wrapper-environment ew))


;;;; is-environment --

(defgeneric is-environment (e)
  (:method ((ew env-wrapper)) nil) ; To be explicit.
  (:method ((ew t)) nil)
  )


;;;; environmentp --

(defun environmentp (e) (is-environment e))


;;;; ensure-parsing-environment --
;;;; This is the only kruftiness in the environment handling API, in
;;;; the sense that is is a function that *must* get redefined by the build
;;;; process when starting from scratch.

(defun ensure-parsing-environment (&optional env)
  (declare (ignore env))
  (error "CLAST: you are running~2%~A ~A~2%~
          Your implementation is not supported by CLAST because ~
          it lacks the basic CLtL2 environment handling functions. ~
          Please lobby your vendor/implementor to support and distribute ~
          them, and CLAST will support that implementation as well."
         (lisp-implementation-type)
         (lisp-implementation-version)
         ))


;;;; end of file -- env.lisp --
