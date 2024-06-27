;;;; -*- Mode: Lisp -*-

;;;; clast-allegro.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.
;;;;
;;;; See file COPYING for copyright and license information.


(in-package "CLAST")

;;;; parsing-environment --
;;;; This is probably general enough to be moved "up" in 'env.lisp',
;;;; but, at the cost of having to maintain duplicated code in several
;;;; files, I prefer to keep it here, in order to keep all the code for a
;;;; given implementation together.

(defstruct (parsing-environment
            (:include environment)
            (:constructor
             %make-parsing-environment (&optional
                                        env
                                        (global-extensions env)
                                        enclosing-env))
            )
  ;; The following are not needed for Allegro.
  ;; (tags () :type list)
  ;; (blocks () :type list)
  
  enclosing-env
  )


(defmethod print-object ((pe parsing-environment) stream)
  (print-unreadable-object (pe stream :identity t)
    (write-string "CLAST Allegro Parsing Environment" stream)))


(defmethod is-environment ((e parsing-environment)) t)
;; (defmethod is-environment ((e sys::augmented-environment)) t)
(defmethod is-environment ((e sys::augmentable-environment)) t)


;;; make-env methods.

(defmethod make-env ((env-type (eql 'parsing-environment))
                     (env null)
                     &key
                     (global-extensions
                      (sys:ensure-portable-walking-environment env))
                     &allow-other-keys)
  (%make-parsing-environment 
   (sys:ensure-portable-walking-environment env)
   global-extensions))


(defmethod make-env ((env-type (eql 'parsing-environment))
                     (env environment)
                     &key
                     &allow-other-keys)
  (%make-parsing-environment (environment-env env)
                             (environment-global-extensions env)))


(defmethod make-env ((env-type (eql 'parsing-environment))
                     (env parsing-environment)
                     &key
                     &allow-other-keys)
  (copy-parsing-environment env))


(defmethod make-env ((env-type (eql 'parsing-environment))
                     (env sys::augmentable-environment)
                     &key
                     (global-extensions
                      (sys:ensure-portable-walking-environment env))
                     &allow-other-keys)
  (%make-parsing-environment env global-extensions))


;;; ensure-parsing-environment

(defun ensure-parsing-environment (&optional (env *cl-global-env*))
  (make-env 'parsing-environment env))


;;; get-implementation-env

(defun get-implementation-env (env) ; This is needed for the internal API.
  (declare (type (or ; null ; In Allegro this should never be NIL.
                     sys::augmentable-environment
                     parsing-environment
                     )))
  (etypecase env
    ;; (null env)
    (sys::augmentable-environment env)
    (parsing-environment (implementation-env env))))


(eval-when (:load-toplevel :execute)
  (setq *cl-global-env* (ensure-parsing-environment nil))
  
  ;; This will work if above definitions are not changed.
  ;; It is necessary for Allegro, as it really does not like NIL as
  ;; `null` lexical environment as soon as things get just a tad
  ;; complicated.
  )


;;;; The Magnificent (yet neglected) 7.
;;;; CLtL2 environment manipulation manipulation functions.
;;;;
;;;; Franz unexplicably redefined the order of the return values.
;;;; Hence this implementation wrapper is needed.

(defmacro reorder-*information-values-portably (env-function-form)
  `(multiple-value-bind (binding-type
                         locative-cons
                         info
                         local-or-global-p)
       ,env-function-form
     (values (if (eq binding-type :special-operator)
                 :special-form
                 binding-type)
             local-or-global-p
             info
             locative-cons ; This will never be used in portable code,
                           ; but it is nice to provide it.
             )
     ))


;;; *allegro-portable-parsing-env*
;;; Morevoer, ACL needs a workaround to handle a "portable walking
;;; environment.
;;;
;;; Maybe just remove this and use *CL-GLOBAL-ENV*, which should be
;;; just fine (and tidier).

(defvar *allegro-portable-parsing-env*
  ;; (sys:ensure-portable-walking-environment nil)
  (or *cl-global-env* (ensure-parsing-environment nil))
  )


;;; env-item-info --
;;;
;;; Auxiliary function used in the *-information functions to access
;;; the "global-extensions'.

(defun env-item-info (accessor item env)
  (declare (type (function (symbol &optional t)
                           (values symbol t t))
                 accessor)
           (type symbol item)
           (type (or null
                     sys::augmentable-environment
                     parsing-environment)
                 env)
           )

  (multiple-value-bind (kind local decls)
      (reorder-*information-values-portably
       (funcall accessor item (get-implementation-env env)))
    (cond (kind
           (values kind local decls))
          ((parsing-environment-p env)
           (reorder-*information-values-portably
            (funcall accessor item
                     (parsing-environment-global-extensions env))))
          (t
           (values kind local decls) ; It should be NIL, NIL, NIL.
           ))
    ))


;;; variable-information

(defun variable-information (variable
                             &optional
                             (env *allegro-portable-parsing-env*))
  (declare (type symbol variable))
  (env-item-info #'(lambda (v e)
                     (sys:variable-information v e t))
                 variable
                 env)
  )


;;; function-information

(defun function-information (f
                             &optional
                             (env *allegro-portable-parsing-env*))
  (declare (type symbol f))
  (env-item-info #'(lambda (f e)
                     (sys:function-information f e t)
                     ;; Do not add T as last arg, otherwise Allegro returns a
                     ;; :special-operator result for macros implementing... special
                     ;; operators.
                     )
                 f
                 env)
  )


;;; declaration-information

(defun declaration-information (decl-name
                                &optional
                                (env *allegro-portable-parsing-env*))
  (declare (type symbol decl-name))

  (env-item-info #'(lambda (d e)
                     (sys:function-information d e t t))
                 decl-name
                 env)
  )


;;; tag-information

(defun tag-information (tag-name
                        &optional
                        (env *allegro-portable-parsing-env*))
  (declare (type symbol tag-name))

  (env-item-info #'sys:tag-information tag-name env)
  )


;;; block-information

(defun block-information (block-name
                          &optional
                          (env *allegro-portable-parsing-env*))
  (declare (type symbol block-name))

  (env-item-info #'sys:block-information block-name env)
  )


;;; augment-environment
;;;
;;; Notes:
;;;
;;; See comments in 'clast-lispworks.lisp' file.

#+nil
(defun augment-environment (env &rest all-keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                declare
                                &allow-other-keys)
  (declare (ignore variable symbol-macro function macro declare))
  (apply #'sys:augment-environment
         (or env *allegro-portable-parsing-env*)
         :allow-other-keys t
         all-keys)
  )


(defun augment-environment (env &rest keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                tag
                                block
                                declare
                                reset
                                global
                                )

  "Create a new environment based on ENV.

The new environment \"augmenting\" ENV according to the values of the
keyword arguments VARIABLE, SYMBOL-MACRO, FUNCTION, MACRO, TAG, BLOCK,
DECLARE, RESET, and GLOBAL.

The interpretation of the variables is the same of the CLtL2
AUGMENT-ENVIRONMENT version, except for TAG, BLOCK, RESET and GLOBAL.

TAG and BLOCK work similarly to VARIABLE, SYMBOL-MACRO, FUNCTION,
MACRO and DECLARE.

RESET is LW specific but is is unused.

GLOBAL is a boolean, that tells the the function to record the
information about VARIABLE, SYMBOL-MACRO, FUNCTION,
MACRO and DECLARE in the 'gloabl-extensions' slot of ENV, if this is a
CLAST ENVIRONMENT.  This is necessary because the parsing machinery
needs to keep track of non-top-level definitions by the usual 'def*'
forms.


Notes:

The present function is an extension of the CLtL2 AUGMENT-ENVIRONMENT
function: it accommodates the handling of \"global\" definitions and
of tags and blocks."

  ;; This function is hairy just because it wants to handle NULL
  ;; environments and the underlying implementation environments.
  ;; Moreover, it must also handle
  ;; It could be simplified by assuming that ENV is always a
  ;; PARSING-ENVIRONMENT.

  (declare

   ;; The arguments are effectively ignored.  They are listed in the
   ;; lambda list for documentation purposes.

   (ignore variable symbol-macro function macro declare reset block tag)
   (type (or null
             sys::augmentable-environment
             parsing-environment)
         env)
   (type boolean global reset)
   (type list variable symbol-macro function macro declare tag block)
   )

  (if global
      ;; In this case we need to do some shenanigans to record the
      ;; names in the fake global place.

      (apply #'augment-global-environment env :allow-other-keys t keys)

      ;; First we augment the underlying environment and then we create a
      ;; new parsing environment, which will eventually be returned.

      (let* ((new-lw-env
              (etypecase env
                ((or null sys::augmentable-environment)
                 (apply #'sys:augment-environment
                        env
                        :allow-other-keys t
                        keys))

                (parsing-environment
                 (apply #'sys:augment-environment
                        (get-implementation-env env)
                        :allow-other-keys t
                        keys))))

             (new-parsing-env
              (make-env 'parsing-environment new-lw-env))
             )

        ;; Copy over the fake global env if necessary.

        (when (parsing-environment-p env)
          (setf (parsing-environment-global-extensions new-parsing-env)
                (parsing-environment-global-extensions env)))

        ;; Nothing else is needed as Allegro already handles blocks
        ;; and tags in SYS:AUGMENT-ENVIRONMENT.
        new-parsing-env
        )))


;;; augment-global-environment --

(defun augment-global-environment (env &rest keys
                                       &key
                                       variable
                                       symbol-macro
                                       function
                                       macro
                                       declare)
  (declare (ignorable variable symbol-macro function macro declare)
           (type list variable symbol-macro function macro declare))

  (etypecase env
    (
     ;; (or null sys::augmentable-environment)
     sys::augmentable-environment ; The ENV should never be NIL in Allegro.

     ;; We just produce a parsing environment with a global extension.

     (make-env 'parsing-environment
               env
               :global-extensions
               (apply #'sys:augment-environment
                      env
                      keys))
     )
    (parsing-environment

     ;; First I create another fake global environment, possibly
     ;; extending the previous one.
     ;;
     ;; Next I record the new one (should I PUSH a new one?) and I
     ;; return a copy of the original environment.
     ;; The recordin is necessary to remember the global definitions
     ;; as they pile up while processing.

     (let ((extended-global-env
            (apply #'sys:augment-environment
                   (parsing-environment-global-extensions env)
                   keys))
           )
       (setf (parsing-environment-global-extensions env)
             extended-global-env)
       (copy-parsing-environment env)
       ))
    ))


;;; update-global-env --

(defun update-global-env (env global-env)
  "Returns a new environment with a new global extension environment.

The new environment is essentially a copy of ENV.  The function is
used to properly \"set\" the global environment while parsing certain
forms that must create subenvironments, e.g., FLET.

The protocol must be carefully followed as there are no programmatic
APIs/macros easing this sort of 'stack-faking'.

This is a destructive function."

  ;; Being paranoid.
  (check-type global-env
      (or ; null ; No NULL environments in Allegro.
       sys::augmentable-environment))

  (etypecase env
    (null
     (let ((ne (make-env 'parsing-environment env)))
       (setf (environment-global-extensions ne) global-env)
       ne))

    (parsing-environment
     (setf (environment-global-extensions env) global-env)
     env))
  )


;;; define-declaration

(defmacro define-declaration (decl-name lambda-list &body forms)
  `(sys:define-declaration ,decl-name ,lambda-list ,@forms)
  )


;;; PARSE-MACRO and ENCLOSE
;;;
;;; We assume (as it is implied and/or "ordered" in CLtL2) that
;;; PARSE-MACRO and ENCLOSE don't play shenanigans with the environments;
;;; in particular, we assume that two functions have access *only* to
;;; the implementation environment and not the the "extended" parsing
;;; environment defined above.

(defun parse-macro (name lambda-list body
                         &optional
                         (env *allegro-portable-parsing-env*))
  ;; (error "No implementation for CLtL2 PARSE-MACRO in Allegro CL.")

  (excl::defmacro-expander (list* name lambda-list body)
      (get-implementation-env env))
  )


(defun enclose (lambda-expression
                &optional
                (env *allegro-portable-parsing-env*))
  ;; (error "No implementation for CLtL2 ENCLOSE in Allegro CL.")

  (excl::make-lexical-closure lambda-expression
    nil
    (get-implementation-env env))
  )


;;;; end of file -- clast-allegro.lisp --
