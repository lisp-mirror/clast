;;;; -*- Mode: Lisp -*-

;;;; parse-iteration.lisp --
;;;; Parsing of iteration constructs.
;;;; No LOOP yet.  Be patient!!!!

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")

(defparameter *loop-keywords*
  '(:with
    :initially :finally
    :for :as

    :being
    :the

    :hash-key :hash-keys :hash-value :hash-values
    :using

    :do :doing
    :return

    :collect :collecting

    :when
    :if

    ;; more missing...

    ))


(declaim (ftype (function (t) boolean) is-loop-keyword)
         (inline is-loop-keyword))

(defun is-loop-keyword (form)
  (and (symbolp form)
       (not (null (find form *loop-keywords*
                        :test #'string-equal)))))

        
(defmethod parse-form ((op (eql 'loop)) form
                       &rest keys
                       &key
                       ;; enclosing-form
                       ;; macroexpand
                       ;; environment
                       &allow-other-keys)
  (if (is-loop-keyword (first form)) ; Should be enough...
      (apply #'parse-extended-loop form keys)
      (apply #'parse-simple-loop form keys)
      ))


(defun parse-simple-loop (loop-form
                          &rest keys
                          &key
                          enclosing-form
                          macroexpand
                          environment
                          &allow-other-keys)
  (declare (ignore macroexpand))
  (values
   (make-instance 'simple-loop-form
                  :top enclosing-form
                  :source loop-form
                  :body-env environment
                  :progn (apply #'parse-form-seq
                                (rest loop-form)
                                keys))
   environment)
  )


(defun parse-extended-loop (loop-form
                            &rest keys
                            &key
                            enclosing-form
                            macroexpand
                            environment
                            &allow-other-keys
                            )
  (declare (ignore enclosing-form macroexpand))
  (start-loop-parsing loop-form environment keys ()))


(defgeneric parse-loop-clause (clause-kwd form ; (first form) == clause-kwd
                                          clauses
                                          env
                                          &rest keys
                                          &key
                                          enclosing-form
                                          macroexpand
                                          environment
                                          &allow-other-keys)
  )


(declaim (ftype (function (t (or symbol string)) boolean)
                loop-kwd=)
         (inline loop-kwd=))

(defun loop-kwd= (loop-kwd kwd)
  (declare (type (or symbol string) kwd))
  (and (symbolp loop-kwd)
       (string-equal loop-kwd kwd)))


(defun start-loop-parsing (loop-form env keys clauses
                                     &aux
                                     (loop-kwd (first loop-form)))
  (cond ((loop-kwd= loop-kwd :named)
         (apply #'parse-loop-clause :named loop-form clauses env keys))
        (t
         (error 'ast-parse-error
                :format-control "Unrcognized LOOP keyword ~A." loop-kwd))
        ))


(defun continue-loop-parsing (loop-forms
                              clauses
                              env
                              keys)
  (if (null loop-forms)
      (finish-loop-parsing clauses env keys)
      (let ((loop-kwd (first loop-forms)))
        (unless (symbolp loop-kwd)
          (error 'ast-parse-error
                 :format-control "Incorrect LOOP keyword ~S."
                 :format-arguments (list loop-kwd)
                 ))
        (apply #'parse-loop-clause
               (intern (symbol-name loop-kwd) "KEYWORD")
               loop-forms
               clauses
               env
               keys)
        )))


(defun finish-loop-parsing (clauses env keys)
  (destructuring-bind (&key
                       environment
                       enclosing-form
                       macroexpand
                       &allow-other-keys)
      keys
    (declare (ignore macroexpand))
    (values
     (make-instance 'loop-form
                    :top enclosing-form
                    :loop-clauses (nreverse clauses)
                    :body-env env
                    )
     environment)))


;;;---------------------------------------------------------------------------
;;; name-clause --

(defmethod parse-loop-clause ((loop-kwd (eql :named))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form macroexpand))
  (let ((loop-name (second form)))
    (unless (symbolp loop-name)
      (error 'ast-parse-error
             :format-control "Incorrect name for LOOP NAMED clause ~S."
             :format-arguments (list loop-name)
             ))
    (continue-loop-parsing (cddr form)
                           (cons `(,(first form) ,loop-name) clauses)
                           (augment-environment environment
                                                :block (list loop-name))
                           keys)
    ))


;;;---------------------------------------------------------------------------
;;; variable-clause --

;;; with clause --

(defmethod parse-loop-clause ((loop-kwd (eql :with))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form macroexpand environment))
  (labels ((parse-var-spec (with-kwd rest-form with-clause vars)
             (let ((var (second rest-form))
                   (var-type t)
                   (var-form nil)
                   )
               (unless (symbolp var) ; Wrong.
                 (error 'ast-parse-error
                        :format-control "Incorrect variable name in LOOP ~S."
                        :format-arguments (list var)
                        ))
               (push with-kwd with-clause)
               (push var with-clause)
               (setf rest-form (cddr rest-form))
               (let ((next-token (first rest-form)))
                 (when (loop-kwd= next-token :of-type)
                   (setf var-type (second rest-form)
                         rest-form (cddr rest-form)
                         next-token (first rest-form))
                   (push :of-type with-clause)
                   (push var-type with-clause)
                   )
                 (when (loop-kwd= next-token '=)
                   (setf var-form (apply #'parse (second rest-form) keys)
                         rest-form (cddr rest-form)
                         next-token (first rest-form))
                   (push '= with-clause)
                   (push var-form with-clause)
                   )
                 (cond ((loop-kwd= next-token 'and)
                        (parse-var-spec :and rest-form with-clause (acons var var-type vars))) ; Wrong.
                       (t
                        (let* ((variables (mapcar #'first vars))
                               (declares (build-local-decls vars))
                               (ne (augment-environment env
                                                        :variable variables
                                                        :declare declares))
                               )
                          (continue-loop-parsing rest-form
                                                 (push (nreverse with-clause) clauses)
                                                 ne
                                                 keys)))
                       )
                 )))

           (build-local-decls (vars-n-types)
             (loop for (var . var-type) in vars-n-types
                   when (not (eq t var-type))
                   collect (list 'type var-type var)))
           )
    (parse-var-spec :with form () ())))


;;; intially/finally clause --

(defmethod parse-loop-clause ((loop-kwd (eql :initially))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form macroexpand))
  (labels ((parse-i/f-form (rest-forms i/f-forms)
             (if (and rest-forms
                      (not (is-loop-keyword (first rest-forms))))
                 (parse-i/f-form (rest rest-forms)
                                 (cons (apply #'parse (first rest-forms) keys)
                                       i/f-forms))
                 (continue-loop-parsing rest-forms
                                        (cons (cons :initially (nreverse i/f-forms))
                                              clauses)
                                        environment
                                        keys)))
           )
    (parse-i/f-form (rest form) ())))


(defmethod parse-loop-clause ((loop-kwd (eql :finally))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys)
  (declare (ignore enclosing-form macroexpand))
  (labels ((parse-i/f-form (rest-forms i/f-forms)
             (if (and rest-forms
                      (not (is-loop-keyword (first rest-forms))))
                 (parse-i/f-form (rest rest-forms)
                                 (cons (apply #'parse (first rest-forms) keys)
                                       i/f-forms))
                 (continue-loop-parsing rest-forms
                                        (cons (cons :finally (nreverse i/f-forms))
                                              clauses)
                                        environment
                                        keys)))
           )
    (parse-i/f-form (rest form) ())))


;;; for/as clause --

(defmethod parse-loop-clause ((loop-kwd (eql :as))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              ;; enclosing-form
                              ;; macroexpand
                              ;; environment
                              &allow-other-keys)
  (apply #'parse-loop-clause :for form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :for))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys
                              &aux
                              (subclause-vars ())
                              (subclause-var-types ())
                              )
  (declare (ignore enclosing-form macroexpand environment))
  (labels ((parse-var-spec (for-as-kwd rest-form for-as-clause vars)
             (let* ((var (second rest-form))
                    (var-type t)
                    )
               ;; Ensure subclause-vars and subclause-var-types are
               ;; empty after an 'and'.
               (setf subclause-vars ()
                     subclause-var-types ())
               (push for-as-kwd for-as-clause)
               (push var for-as-clause)
               (setf subclause-vars 
                     (nconc (flatten var)
                            subclause-vars))
               (setf rest-form (cddr rest-form))

               (let ((next-token (first rest-form)))
                 (when (loop-kwd= next-token :of-type)
                   (setf var-type (second rest-form)
                         rest-form (cddr rest-form)
                         next-token (first rest-form))
                   (setf subclause-var-types
                         (nconc (associate-var-types var var-type)
                                subclause-var-types)
                         )
                   (push :of-type for-as-clause)
                   (push var-type for-as-clause)
                   )
                 (cond ((or (loop-kwd= next-token :from)
                            (loop-kwd= next-token :downfrom))
                        (parse-arithmetic-subclause next-token
                                                    rest-form
                                                    for-as-clause))

                       ((or (loop-kwd= next-token :in)
                            (loop-kwd= next-token :on))
                        (parse-list-subclause next-token
                                              rest-form
                                              for-as-clause))

                       ((loop-kwd= next-token '=)
                        (parse-equals-subclause next-token
                                                rest-form
                                                for-as-clause))

                       ((loop-kwd= next-token :across)
                        (parse-across-subclause next-token
                                                rest-form
                                                for-as-clause))

                       ((loop-kwd= next-token :over) ; Yep.  This would be nice.
                        (parse-over-subclause next-token
                                              rest-form
                                              for-as-clause))

                       ((loop-kwd= next-token :being)
                        (parse-being-subclause next-token
                                               rest-form
                                               for-as-clause))
                       ))))

           (continue-parse-for-as-subclause (rest-form
                                             next-token
                                             for-as-clause)
             (cond ((loop-kwd= next-token :and)
                    (parse-var-spec :and rest-form for-as-clause subclause-vars))
                   (t
                    (let ((ne (augment-environment env
                                                   :variable subclause-vars
                                                   :declare subclause-var-types))
                          )
                      (continue-loop-parsing rest-form
                                             (cons (nreverse for-as-clause) clauses)
                                             ne
                                             keys)
                      ))))
           
           (parse-arithmetic-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form keys) for-as-clause)
               (setf rest-form (cddr rest-form)
                     next-token (first rest-form))
               (when (or (loop-kwd= next-token :to)
                         (loop-kwd= next-token :upto)
                         (loop-kwd= next-token :below)
                         (loop-kwd= next-token :downfrom)
                         (loop-kwd= next-token :downto)
                         (loop-kwd= next-token :above))
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) keys) for-as-clause)
                 (setf rest-form (cddr rest-form)
                       next-token (first rest-form))
                 )
               (when (loop-kwd= next-token :by)
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) keys) for-as-clause)
                 (setf rest-form (cddr rest-form)
                       next-token (first rest-form))
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-list-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form keys) for-as-clause)
               (setf rest-form (cddr rest-form)
                     next-token (first rest-form))
               (when (loop-kwd= next-token :by)
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) keys) for-as-clause)
                 (setf rest-form (cddr rest-form)
                       next-token (first rest-form))
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-equals-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form keys) for-as-clause)
               (setf rest-form (cddr rest-form)
                     next-token (first rest-form))
               (when (loop-kwd= next-token :then)
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) keys) for-as-clause)
                 (setf rest-form (cddr rest-form)
                       next-token (first rest-form))
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-across-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form keys) for-as-clause)
               (setf rest-form (cddr rest-form)
                     next-token (first rest-form))
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-over-subclause (next-token rest-form for-as-clause)
             (parse-across-subclause next-token rest-form for-as-clause)
             )

           (parse-being-subclause (next-token rest-form for-as-clause)
             (push next-token for-as-clause)
             (cond ((or (loop-kwd= (second rest-form) :each)
                        (loop-kwd= (second rest-form) :the))
                    (push (second rest-form) for-as-clause)
                    (setf rest-form (cddr rest-form)
                          next-token (first rest-form)))
                   (t
                    (setf rest-form (rest rest-form)
                          next-token (first rest-form)))
                   )
             (cond ((or (loop-kwd= next-token :hash-key)
                        (loop-kwd= next-token :hash-keys)
                        (loop-kwd= next-token :hash-value)
                        (loop-kwd= next-token :hash-values))
                    (parse-hash-subclause next-token rest-form for-as-clause)
                    )
                   ((or (loop-kwd= next-token :symbol)
                        (loop-kwd= next-token :symbols)
                        (loop-kwd= next-token :present-symbol)
                        (loop-kwd= next-token :present-symbols)
                        (loop-kwd= next-token :external-symbol)
                        (loop-kwd= next-token :external-symbols))
                    (parse-package-subclause next-token rest-form for-as-clause)
                    )
                   ((or (loop-kwd= next-token :record) ; LW (and CL-SQL?) extension.
                        (loop-kwd= next-token :records))
                    (parse-sql-query-subclause next-token rest-form for-as-clause)
                    )
                   (t
                    (error 'ast-parse-error
                           :format-control "Unrecognized LOOP keyword ~A."
                           :format-arguments (list next-token)))
                   ))

           (parse-hash-subclause (next-token rest-form for-as-clause)
             (push next-token for-as-clause)
             (cond ((or (loop-kwd= (second rest-form) :in)
                        (loop-kwd= (second rest-form) :of))
                    (push (second rest-form) for-as-clause)
                    (setf rest-form (cddr rest-form)
                          next-token (first rest-form)))
                   (t
                    (setf rest-form (rest rest-form)
                          next-token (first rest-form)))
                   )
             (push (apply #'parse next-token keys) for-as-clause)
             (setf rest-form (rest rest-form)
                   next-token (first rest-form))
             (when (loop-kwd= next-token :using)
               (push next-token for-as-clause)
               (setf rest-form (rest rest-form)
                     next-token (first rest-form))
               (handler-case
                   (destructuring-bind (htkv-kwd htkv-var)
                       next-token
                     (unless (or (loop-kwd= htkv-kwd :hash-value)
                                 (loop-kwd= htkv-kwd :hash-key))
                       (error 'ast-parse-error
                              :format-control "Wrong hash-table loop syntax ~S."
                              :format-arguments (list next-token)))
                     (push next-token for-as-clause)
                     (push htkv-var subclause-vars)
                     (setf rest-form (rest rest-form)
                           next-token (first rest-form))
                     )
                 (ast-parse-error (ape)
                   ;; Re-signal.
                   (error ape))
                 (error (e)
                   (format *error-output* "Error: CLAST:~%")
                   (error e)))
               )
             (continue-parse-for-as-subclause rest-form
                                              next-token
                                              for-as-clause)
             )

           (parse-package-subclause (next-token rest-form for-as-clause)
             (push next-token for-as-clause)
             (when (or (loop-kwd= (second rest-form) :in)
                       (loop-kwd= (second rest-form) :of))
               (push (second rest-form) for-as-clause)
               (setf rest-form (cddr rest-form)
                     next-token (first rest-form))
               (push (apply #'parse next-token keys) for-as-clause)
               )
             (setf rest-form (rest rest-form)
                   next-token (first rest-form))
             (continue-parse-for-as-subclause rest-form
                                              next-token
                                              for-as-clause)
             )

           (parse-sql-query-subclause (next-token rest-form for-as-clause)
             (push next-token for-as-clause)
             (cond ((or (loop-kwd= (second rest-form) :in)
                        (loop-kwd= (second rest-form) :of))
                    (push (second rest-form) for-as-clause)
                    (setf rest-form (cddr rest-form)
                          next-token (first rest-form)))
                   (t
                    (error 'ast-parse-error
                           :format-control "Wrong syntax for SQL clause ~A."
                           :format-arguments (list (second rest-form))))
                   )
             (push (apply #'parse next-token keys) for-as-clause) ; Query expr.
             (setf rest-form (rest rest-form)
                   next-token (first rest-form))

             (when (loop-kwd= next-token :not-inside-transaction)
               (push next-token for-as-clause)
               (push (apply #'parse (second rest-form) keys) for-as-clause)
               (setf rest-form (cddr for-as-clause)
                     next-token (first rest-form)))

             (when (loop-kwd= next-token :get-all)
               (push next-token for-as-clause)
               (push (apply #'parse (second rest-form) keys) for-as-clause)
               (setf rest-form (cddr rest-form)
                     next-token (first rest-form)))

             (continue-parse-for-as-subclause rest-form next-token for-as-clause)
             )
             
           )
    (parse-var-spec loop-kwd form () ())
    ))


;;; unconditional clause --

(defmethod parse-loop-clause ((loop-kwd (eql :do))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys
                              )
  (declare (ignore enclosing-form macroexpand))
  (parse-loop-do-clause loop-kwd form clauses env keys)
  )


(defmethod parse-loop-clause ((loop-kwd (eql :doing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys
                              )
  (declare (ignore enclosing-form macroexpand))
  (parse-loop-do-clause loop-kwd form clauses env keys)
  )


(defun parse-loop-do-clause (do/doing-kwd
                             form
                             clauses
                             env
                             keys)
  (labels ((parse-do/doing-form (rest-forms do/doing-forms)
             (if (and rest-forms
                      (not (is-loop-keyword (first rest-forms))))
                 (parse-do/doing-form (rest rest-forms)
                                      (cons (apply #'parse (first rest-forms) keys)
                                            do/doing-forms))
                 (continue-loop-parsing rest-forms
                                        (cons (cons do/doing-kwd (nreverse do/doing-forms))
                                              clauses)
                                        env
                                        keys)))
           )
    (parse-do/doing-form (rest form) ()))
  )


(defun parse-loop-compound-forms-clause (initial-kwd form clauses env keys)
  (labels ((parse-compound-forms (rest-forms compound-forms)
             (if (and rest-forms
                      (not (is-loop-keyword (first rest-forms))))
                 (parse-compound-forms (rest rest-forms)
                                       (cons (apply #'parse (first rest-forms) keys)
                                             compound-forms))
                 (continue-loop-parsing rest-forms
                                        (cons (cons initial-kwd
                                                    (nreverse compound-forms))
                                              clauses)
                                        env
                                        keys)))
           )
    (parse-compound-forms (rest form) ()))
  )


(defmethod parse-loop-clause ((loop-kwd (eql :return))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              enclosing-form
                              macroexpand
                              environment
                              &allow-other-keys
                              )
  (declare (ignore enclosing-form macroexpand environment))
  (if (is-loop-keyword (second form))
      (error 'ast-parse-error
             :format-control "LOOP keyword RETURN followed by ~
                              another LOOP keyword (~A)."
             :format-arguments (list (second form)))
      (let ((return-clause
             (list :return
                   (apply #'parse (second form) keys)))
            )
        (continue-loop-parsing (cddr form)
                               (cons return-clause clauses)
                               env
                               keys))
      ))

;;; accumulation clauses --

(defun parse-accumulation-clause (acc-kwd form clauses env keys)
  (let* ((acc-clause (list acc-kwd))
         (rest-form form)
         (next-token (first rest-form))
         )
    (if (null (rest form))
        (error 'ast-parse-error
               :format-control "Missing accumulation form after ~A."
               :format-arguments (list acc-kwd))
        (let ((acc-form (second rest-form)))
          (cond ((is-loop-keyword acc-form)
                 (error 'ast-parse-error
                        :format-control "LOOP keyword ~A found after ~A."
                        :format-arguments (list acc-form acc-kwd)))
                (t
                 (push (apply #'parse acc-form keys) acc-clause)
                 (setf rest-form (cddr rest-form)
                       next-token (first rest-form))
                 ))))
    (when (loop-kwd= next-token :into)
      ;; This makes things very tricky!
      ;; The variable should be available in the various environments
      ;; used for parsing the "stepping" functions.
      ;; Of course this calls for a different, delayed, parsing
      ;; routine; maybe based on saving "parsing thunks".
      (push next-token acc-clause)
      (push (parse (second rest-form) keys) acc-clause)
      (setf rest-form (cddr rest-form)
            next-token (first rest-form))

      (when (loop-kwd= next-token :of-type)
        (push :of-type acc-clause)
        (push (second rest-form) acc-clause)
        (setf rest-form (cddr rest-form)
              next-token (first rest-form))))

    (continue-loop-parsing rest-form
                           (cons (nreverse acc-clause)
                                 clauses)
                           env
                           keys)))
      

(defmethod parse-loop-clause ((loop-kwd (eql :collect))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :collect form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :collecting))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :collect form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :append))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :append form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :appending))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :appending form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :nconc))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :nconc form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :nconcing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :nconcing form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :count))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :count form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :counting))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :counting form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :sum))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :sum form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :summing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :summing form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :maximize))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :maximize form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :maximizing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :maximizing form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :minimize))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :minimize form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :minimizing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-accumulation-clause :minimizing form clauses env keys))


;;; conditional clause --
;;; COND-KWD is one of IF, WHEN, UNLESS

(defun parse-conditional-clause (cond-kwd cond-form clauses env keys)
  (labels ((parse-then-else-clauses (next-token
                                     rest-form
                                     then-else-clauses
                                     env
                                     keys)
             
             )
           )

    (let* ((cond-clause (list cond-kwd))
           (rest-form form)
           (next-token (first rest-form))
           )
      (if (null (rest rest-form))
          (error 'ast-parse-error
                 :format-control "Missing test form after ~A."
                 :format-arguments (list cond-kwd))
          (let ((test-form (second rest-form)))
            (cond ((is-loop-keyword test-form)
                   (error 'ast-parse-error
                          :format-control "LOOP keyword ~A found after ~A."
                          :format-arguments (list test-form cond-kwd)))
                  (t
                   (push (apply #'parse test-form keys) cond-clause)
                   (setf rest-form (cddr rest-form)
                         next-token (first rest-form)))
                  )))

      ;; We have parsed the TEST...
      ;; Now we parse the rest, but remembering that we are parsing a
      ;; context-free sub-lamguage.

      (multiple-value-bind (then-clauses next-token rest-form)
          (parse-then-else-clauses next-token rest-form () env keys)
        (declare (ignore next-token))
        (push then-else-clauses cond-clause)
        (continue-loop-parsing rest-form
                               (cons (nreverse cond-clause)
                                     clauses)
                               env
                               keys))
      )))
             



;;;;===========================================================================
;;;; Utilities.

;;; associate-var-type --
;;; No error checking...

(defun associate-var-types (var-tree type-decls-tree)
  (labels ((avt (var-tree type-decls-tree decls)
             (cond ((consp var-tree)
                    (cond ((null (first var-tree))
                           (avt (rest var-tree)
                                (rest type-decls-tree)
                                decls))
                          ((atom (first var-tree))
                           (avt (rest var-tree)
                                (rest type-decls-tree)
                                (cons (list (first type-decls-tree)
                                            (first var-tree))
                                      decls)))
                          (t
                           (avt (rest var-tree)
                                (rest type-decls-tree)
                                (avt (first var-tree)
                                     (first type-decls-tree)
                                     decls)))
                          ))
                   ((null var-tree) decls)
                   (t ; (atom var-tree)
                    (cons (list type-decls-tree var-tree) decls))))
           )
    (nreverse (avt var-tree type-decls-tree '() ))))


;;;; end of file -- parse-iteration.lisp --
