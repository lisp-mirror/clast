;;;; -*- Mode: Lisp -*-

;;;; clast-lispworks.lisp --
;;;; Code lifted from IT.BESE.ARNESI and CL-WALKER.
;;;;
;;;; Environment functions.
;;;; Either you have CLtL2 environment functions or your
;;;; implementation sucks.

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
  (tags () :type list)
  (blocks () :type list)
  enclosing-env
  )


(defmethod print-object ((pe parsing-environment) stream)
  (print-unreadable-object (pe stream :identity t)
    (write-string "CLAST LW Parsing Environment" stream)))


(defmethod is-environment ((e parsing-environment)) t)
(defmethod is-environment ((e sys::augmented-environment)) t)


;;; make-env methods.

(defmethod make-env ((env-type (eql 'parsing-environment))
                     (env null)
                     &key
                     (global-extensions nil)
                     &allow-other-keys)
  (%make-parsing-environment env global-extensions))


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
                     (env sys::augmented-environment)
                     &key
                     (global-extensions nil)
                     &allow-other-keys)
  (%make-parsing-environment env global-extensions))


;;; ensure-parsing-environment

(defun ensure-parsing-environment (&optional (env *cl-global-env*))
  (make-env 'parsing-environment env))


(defvar *lispworks-parsing-env*
  (ensure-parsing-environment))


(declaim (inline get-implementation-env)
         (ftype (function ((or null
                               parsing-environment
                               sys::augmented-environment))
                          (or null sys::augmented-environment)
                          ;; T ; If we want to be less precise.
                          )
                get-implementation-env))


(defun get-implementation-env (env)
  (declare (type (or null
                     sys::augmented-environment
                     compiler::environment
                     parsing-environment
                     )))
  (etypecase env
    (null env)
    (sys::augmented-environment env)
    (compiler::environment env)
    (parsing-environment (implementation-env env))))


;;; env-find-block

(defun env-find-block (b-name env)
  (declare (type parsing-environment env))
  (labels ((env-find-block-1 (b-name env)
             (let ((b (member b-name (parsing-environment-blocks env)
                              :test #'eq)))
               (if b
                   b
                   (let ((next-pe (parsing-environment-enclosing-env env)))
                     (when next-pe
                       (env-find-block-1 b-name next-pe))))
               ))
           )
    (let ((block-info (env-find-block-1 b-name env)))
      (if block-info
          (values :block nil nil)
          (values nil nil nil)))))


;;; env-find-tag

(defun env-find-tag (t-name env)
  (declare (type parsing-environment env))
  (labels ((env-find-tag-1 (t-name env)
             (let ((tt (member t-name (parsing-environment-tags env)
                               :test #'eq)))
               (if tt
                   tt
                   (let ((next-pe (parsing-environment-enclosing-env env)))
                     (when next-pe
                       (env-find-tag-1 t-name next-pe))))
               ))
           )
    (let ((tag-info (env-find-tag-1 t-name env)))
      (if tag-info
          (values :tag nil nil)
          (values nil nil nil)))))

  
;;;; The Magnificent (yet neglected) 7; plus 2.
;;;; CLtL2 environment manipulation manipulation functions.

;;; env-item-info --
;;;
;;; Auxiliary function used in the *-information functions.

(defun env-item-info (accessor item env)
  (declare (type (function (symbol &optional t)
                           (values symbol t t))
                 accessor)
           (type symbol item)
           (type (or null
                     sys::augmented-environment
                     compiler::environment
                     parsing-environment)
                 env)
           )

  (multiple-value-bind (kind local decls)
      (funcall accessor item (get-implementation-env env))
    (cond (kind
           (values kind local decls))
          ((parsing-environment-p env)
           (funcall accessor item
                    (parsing-environment-global-extensions env)))
          (t
           (values kind local decls) ; It should be NIL, NIL, NIL.
           ))
    ))

;;; variable-information

(defun variable-information (variable
                             &optional
                             (env *lispworks-parsing-env*))
  (declare (type symbol variable))
  (env-item-info #'hcl:variable-information variable env))


;;; function-information

(defun function-information (f
                             &optional
                             (env *lispworks-parsing-env*))
  (declare (type symbol f))
  (env-item-info #'hcl:function-information f env))


;;; declaration-information

(defun declaration-information (decl-name
                                &optional
                                (env *lispworks-parsing-env*))
  (declare (type symbol decl-name))
  (env-item-info #'hcl:declaration-information decl-name env)
  )


;;; block-information

(defun block-information (block-name
                          &optional
                          (env *lispworks-parsing-env*))
  (typecase env
    (parsing-environment
     (env-find-block block-name env))
    (t (values nil nil nil))) ; Just to reiterate...
  )


;;; tag-information

(defun tag-information (tag-name
                        &optional
                        (env *lispworks-parsing-env*))
  (typecase env
    (parsing-environment
     (env-find-tag tag-name env))
    (t (values nil nil nil))) ; Just to reiterate...
  )


;;; augment-environment --
;;;
;;; Notes:
;;;
;;; 2024-06-12 MA: Maybe turn it into a generic function and declare
;;; at the library top level: it may become easier to deal with the
;;; underlying implementation bestiary.

(defun augment-environment (env &rest keys ; Just a utility variable.
                                &key
                                variable
                                symbol-macro
                                function
                                macro
                                (tag () tag-supplied-p)
                                (block () block-supplied-p)
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

   (ignore variable symbol-macro function macro declare reset)
   (type (or null
             sys::augmented-environment
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
                ((or null sys::augmented-environment)
                 (apply #'hcl:augment-environment
                        env
                        :allow-other-keys t
                        keys))

                (parsing-environment
                 (apply #'hcl:augment-environment
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


        ;; The next operations need to be refined...

        (when tag-supplied-p
          (setf (parsing-environment-tags new-parsing-env)
                (copy-list tag)))

        (when block-supplied-p
          (setf (parsing-environment-blocks new-parsing-env)
                (copy-list block)))

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
    ((or null sys::augmented-environment)

     ;; We just produce a parsing environment with a global extension.

     (make-env 'parsing-environment
               env
               :global-extensions
               (apply #'hcl:augment-environment
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
            (apply #'hcl:augment-environment
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
  (check-type global-env (or null sys::augmented-environment))

  (etypecase env
    (null
     (let ((ne (make-env 'parsing-environment env)))
       (setf (environment-global-extensions ne) global-env)
       ne))

    (parsing-environment
     (setf (environment-global-extensions env) global-env)
     env))
  )


;;; define-declaration --

(defmacro define-declaration (decl-name lambda-list &body forms)
  `(hcl:define-declaration ,decl-name ,lambda-list ,@forms)
  )


;;; PARSE-MACRO and ENCLOSE
;;;
;;; We assume (as it is implied and/or "ordered" in CLtL2) that
;;; PARSE-MACRO and ENCLOSE don't play shenanigans with the environments;
;;; in particular, we assume that two functions have access *only* to
;;; the implementation environment and not the the "extended" parsing
;;; environment defined above.

(defun parse-macro (name lambda-list body &optional (env *lispworks-parsing-env*))
  (hcl:parse-macro name lambda-list body env)
  )


(defun enclose (lambda-expression &optional (env *lispworks-parsing-env*))
  (hcl:enclose lambda-expression env)
  )


;;;; end of file -- clast-lispworks.lisp --
