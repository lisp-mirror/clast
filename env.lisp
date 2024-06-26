;;;; -*- Mode: Lisp -*-

;;;; env.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.

;;;; See file COPYING in main folder for licensing and copyright information.

;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; The sub-library builds an API to handle "environment trees".
;;;; This is needed because the parsing/ast machinery needs at least a "global" environment plus

(in-package "CLAST")


;;;; *cl-global-env*
;;;; 
;;;; Notes:
;;;;
;;;; 20161014 MA: NIL usually suffices.  However it may be better to
;;;; wrap this is in an indirection layer in order to avoid clobbering
;;;; the actual CL implementation global environment.
;;;;
;;;; A way to achieve this is to redefine *cl-global-env* in each of
;;;; the implementation dependent code bases (in 'impl-dependent').

(defvar *cl-global-env* nil
  "This variable contains a designator for the 'global environment'.

The designator may be different from the actual implementation's
global environment designator (although NIL is valid), in order to
avoid clobbering it during the the parsing process.

This variable is special and it is necessary to handle the
situation of definitions (e.g., via DEFUN) done in a non empty lexical
environment.

See also:

Hyperspec 3.1.1.1")


;;;; environment --
;;;; As correctly noted in CL-WALKER you need to have a hairy
;;;; environment structure to pan over incompatibilities.  You need (at least) two
;;;; environments: one relative to the "walking" procedure and the other that
;;;; is the standard CL "environment" (as in the &environment macro keyword).
;;;;
;;;; The "walking environment" is essentially a wrapper for the CLtL2
;;;; "environments" with a few extra bells and whistles.
;;;;
;;;; Note: This is the "opaque" structure par excellence.
;;;; The actual 'environment' API is in the "impl-dependent" files.
;;;;
;;;; It would be nice to :include the implementation dependent
;;;; environment.  Alas, we cannot assume that the actual
;;;; implementation dependent environment is a structure.
;;;;
;;;; Notes:
;;;;
;;;; 2024-06-11 MA:
;;;; It turns out that the CLTL2 environment functions, mostly
;;;; AUGMENT-ENVIRONMENT, cannot really do much with the "global
;;;; environment". In particular, they cannot directly install a name
;;;; in it.  Therefore, in order to "parse" definitions that would
;;;; modify the global environment, I must keep track of a parallel
;;;; one.
;;;;
;;;; 2024-06-11 MA:
;;;; Decision taken: renamed ENV-WRAPPER to ENVIRONMENT.
;;;;
;;;; 2024-06-23 MA:
;;;; Maybe make this an "abstract" structure (with no constructor).

(defstruct (environment (:constructor %make-environment
                         (&optional env
                                    (global-extensions env)))
                        )
  "The CLAST Environment Structure.

A wrapper around the underlying enviroment data structures."
  (env nil) ; The &environment CL environment.
  (global-extensions nil)
  )


;;;; Environment factory function.

(defgeneric make-env (env-type env &key &allow-other-keys)
  (:documentation "Create an enviroment of type ENV-TYPE starting from ENV.

The environment ENV can be NIL, representing the \"null environment\",
as per the Hyperspec."))


(defmethod make-env ((env-type (eql 'environment))
                     (env null)
                     &key
                     &allow-other-keys)
  (%make-environment))


(defmethod make-env ((env-type (eql 'environment))
                     (env environment)
                     &key
                     &allow-other-keys)
  (%make-environment (environment-env env)
                     (environment-global-extensions env))
  )

;;;; Convenience accessor.

(declaim (inline inplementation-env)
         (ftype (function (environment) t) inplementation-env))

(defun implementation-env (ew)
  (declare (type environment ew))
  (environment-env ew))


;;;; is-environment --

(defgeneric is-environment (e)
  (:method ((ew environment)) nil) ; To be explicit.
  (:method ((ew null)) t) ; NIL is the "null" environment.
  (:method ((ew t)) nil)
  )


;;;; ensure-parsing-environment --
;;;; This is one kruftiness in the environment handling API, in
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


;;;; The Magnificent (yet neglected) 7; plus 2.
;;;; CLtL2 environment manipulation manipulation functions.
;;;;
;;;; Declarations and documentation.
;;;; These are "forward declarations" as the actual definitions are
;;;; implementation dependent.

;;; environment-query-fn
;;; Can be made more precise.

(deftype environment-query-fn ()
  '(function (symbol &optional t) (values symbol t t)))


;;; Forward declarations.

(declaim (ftype environment-query-fn variable-information))
(declaim (ftype environment-query-fn function-information))
(declaim (ftype environment-query-fn declaration-information))
(declaim (ftype environment-query-fn block-information))
(declaim (ftype environment-query-fn tag-information))

(declaim (ftype (function (t &key &allow-other-keys) t)
                augment-environment))

(declaim (ftype function parse-macro)) ; FTTB.
(declaim (ftype function enclose))     ; FTTB.


(eval-when (:load-toplevel :execute)
  (setf (documentation 'variable-information 'function)
        "Returns information about variable VAR.

KIND is NIL if no information can be found in ENV.

Arguments and Values:

VAR --- a symbol.
ENV --- an environment.
KIND  --- one of :lexical, :special, :constant or NIL.
LOCAL --- true if VAR is \"local\".
DECLS --- declarations regarding VAR in ENV.


See Also:

CLtL2 8.5 Environments.")

  (setf (documentation 'function-information 'function)
        "Returns information about function FN.

NIL is returned if no information can be found in ENV.

See Also:

CLtL2 8.5 Environments.")

  (setf (documentation 'declaration-information 'function)
        "Returns information about declaration DECL.

See Also:

CLtL2 8.5 Environments.")

  (setf (documentation 'block-information 'function)
        "Returns information about block named BLOCK-NAME.

See Also:

CLtL2 8.5 Environments, although BLOCK-INFORMATION is not in it.")

  (setf (documentation 'tag-information 'function)
        "Returns information about tag named TAG-NAME.

See Also:

CLtL2 8.5 Environments, although TAG-INFORMATION is not in it.")
  )
                

;;;; end of file -- env.lisp --
