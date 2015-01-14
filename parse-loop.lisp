;;;; -*- Mode: Lisp -*-

;;;; parse-iteration.lisp --
;;;; Parsing of iteration constructs.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;;;===========================================================================
;;;; Prologue.

(defparameter *loop-keywords*
  #(:with
    :initially :finally
    :for :as

    :being
    :the

    :hash-key :hash-keys :hash-value :hash-values
    :using

    :do :doing
    :return

    :collect :collecting

    :when :if :unless
    :else
    :end

    :while :until :repeat
    
    :always :thereis :never

    :and

    ;; ... and some extra ones...

    :record :records
    :tuple :tuples

    :over

    )
  "Set of all the LOOP keywords, plus some extra ones.

The semi-standard SQL querying keywords (RECORD, RECORDS, TUPLE and
TUPLES) and other ones are included in the set."
  )


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
  "The main entry method for the parsing of LOOP forms."
  (if (is-loop-keyword (second form)) ; Should be enough...
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
  (start-loop-parsing (rest loop-form) environment keys ()))


(defgeneric parse-loop-clause (clause-kwd
                               form ; (first form) == clause-kwd
                               clauses
                               env
                               &rest keys
                               &key
                               enclosing-form
                               macroexpand
                               environment
                               &allow-other-keys)
  (:documentation "Parses a single LOOP 'clause'.

The methods of the generic function dispatch on the first argument,
which is EQL-specialized on various LOOP keywords.  The FORM is
actually the rmaining LOOP form to be parsed.  CLAUSES are the LOOP
clauses parsed so far and ENV is the resulting environment.

The methods return three values

Arguments and Values:

clause-kwd : the LOOP keyword to be dispatched on.
form       : the part of the LOOP form to be parsed; (first form) == clause-kwd.
clauses    : the clauses parsed so far.
env        : the environment to be used while parsing.
keys       : a plist of keyword arguments (the next ones)
enclosing-form : the form enclosing the LOOP.
macroexpand    : if and how to macroexpand the various (sub)forms.
environment    : the environment to be used while parsing (same as env).
parsed-clause       : the clause just parsed.
remaining-loop-form : the rest of the LOOP form to be parsed.
new-env             : a possibly augmented env.
"
  ))


(declaim (ftype (function (t (or symbol string)) boolean)
                loop-kwd=)
         (inline loop-kwd=))

(defun loop-kwd= (loop-kwd kwd)
  (declare (type (or symbol string) kwd))
  (and (symbolp loop-kwd)
       (string-equal loop-kwd kwd)))


(defmacro advance (forms-place &optional (n 1))
  `(setf ,forms-place (nthcdr ,n ,forms-place)))


(defmacro next-form (forms-place &optional next-token)
  (if (and next-token (symbolp next-token))
      `(setf ,next-token (first ,forms-place))
      `(first ,forms-place)))


(defun start-loop-parsing (loop-form env keys clauses
                                     &aux
                                     (loop-kwd (first loop-form)))
  (if (is-loop-keyword loop-kwd)
      (multiple-value-bind (clause rest-loop-form env)
          (apply #'parse-loop-clause loop-kwd loop-form clauses env keys)
        (continue-loop-parsing rest-loop-form
                               (cons clause clauses)
                               env
                               keys))
      (error 'ast-parse-error
             :format-control "Unrcognized LOOP keyword ~A."
             :format-arguments (list loop-kwd)))
  )


(defun continue-loop-parsing (loop-forms
                              clauses
                              env
                              keys)
  (if (null loop-forms)
      (finish-loop-parsing clauses env keys)
      (let ((loop-kwd (next-form loop-forms)))
        (unless (symbolp loop-kwd)
          (error 'ast-parse-error
                 :format-control "Incorrect LOOP keyword ~S."
                 :format-arguments (list loop-kwd)
                 ))
        (multiple-value-bind (loop-clause rest-loop-form env)
            (apply #'parse-loop-clause
                   (intern (symbol-name loop-kwd) "KEYWORD")
                   loop-forms
                   clauses
                   env
                   keys)
          (continue-loop-parsing rest-loop-form
                                 (cons loop-clause clauses)
                                 env
                                 keys)
          ))))


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


;;;===========================================================================
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
    (values (list loop-kwd loop-name)
            (cddr form)
            (augment-environment environment
                                 :block (list loop-name)))))


;;;===========================================================================
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
             (let ((var-spec (second rest-form))
                   (var-type t)
                   (var-form nil)
                   )
               (unless (or (symbolp var-spec)
                           (consp var-spec))
                 (error 'ast-parse-error
                        :format-control "Incorrect variable name in LOOP ~S."
                        :format-arguments (list var-spec)
                        ))
               (push with-kwd with-clause)
               (push var-spec with-clause)
               (advance rest-form 2)

               (let ((next-token (next-form rest-form)))
                 (when (loop-kwd= next-token :of-type)
                   (setf var-type (second rest-form))
                   (advance rest-form 2)
                   (next-form rest-form next-token)
                   (push :of-type with-clause)
                   (push var-type with-clause)

                   )

                 (setf vars
                       (nconc (associate-var-types var-spec var-type)
                              vars))

                 (when (loop-kwd= next-token '=)
                   (setf var-form (apply #'parse (second rest-form)
                                         :environment env
                                         keys))
                   (advance rest-form 2)
                   (next-form rest-form next-token)
                   (push '= with-clause)
                   (push var-form with-clause)
                   )
                 (if (loop-kwd= next-token 'and)
                     (parse-var-spec :and rest-form with-clause vars)
                     (let* ((variables (mapcan #'rest vars))
                            (declares (remove t vars :key #'first))
                            (ne (augment-environment env
                                                     :variable variables
                                                     :declare declares))
                            )
                       (values (nreverse with-clause) 
                               rest-form
                               ne)
                       )))
               ))
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
  (declare (ignore enclosing-form environment macroexpand))
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys))


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
  (declare (ignore enclosing-form environment macroexpand))
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys))


;;; for/as clause --
;;; The most complicated one...

(defmethod parse-loop-clause ((loop-kwd (eql :as))
                              form
                              clauses
                              env
                              &rest keys
                              &key
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
                              (rest-form form)
                              (next-token loop-kwd) ; EQL (first form)
                              (subclause-vars ())
                              (subclause-var-types ())
                              )
  (declare (ignore enclosing-form macroexpand environment))
  (labels ((parse-var-spec (for-as-kwd rest-form for-as-clause)
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
               (advance rest-form 2)
               (next-form rest-form next-token)

               (when (loop-kwd= next-token :of-type)
                 (setf var-type (second rest-form))
                 (advance rest-form 2)
                 (next-form rest-form next-token)
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
                     )))

           (continue-parse-for-as-subclause (rest-form
                                             next-token
                                             for-as-clause)
             (if (loop-kwd= next-token :and)
                 (parse-var-spec :and rest-form for-as-clause)
                 (let ((ne (augment-environment env
                                                :variable subclause-vars
                                                :declare subclause-var-types))
                       )
                   (values (nreverse for-as-clause)
                           rest-form
                           ne)
                   )))
           
           (parse-arithmetic-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form :environment env keys) for-as-clause)
               (advance rest-form 2)
               (next-form rest-form next-token)
               (when (or (loop-kwd= next-token :to)
                         (loop-kwd= next-token :upto)
                         (loop-kwd= next-token :below)
                         (loop-kwd= next-token :downfrom)
                         (loop-kwd= next-token :downto)
                         (loop-kwd= next-token :above))
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) :environment env keys) for-as-clause)
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )
               (when (loop-kwd= next-token :by)
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) :environment env keys) for-as-clause)
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-list-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form :environment env keys) for-as-clause)
               (advance rest-form 2)
               (next-form rest-form next-token)
               (when (loop-kwd= next-token :by)
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) :environment env keys) for-as-clause)
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-equals-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form :environment env keys) for-as-clause)
               (advance rest-form 2)
               (next-form rest-form next-token)
               (when (loop-kwd= next-token :then)
                 (push next-token for-as-clause)
                 (push (apply #'parse (second rest-form) :environment env keys) for-as-clause)
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 )
               (continue-parse-for-as-subclause rest-form
                                                next-token
                                                for-as-clause))
             )

           (parse-across-subclause (next-token rest-form for-as-clause)
             (let ((value-form (second rest-form)))
               (push next-token for-as-clause)
               (push (apply #'parse value-form :environment env keys) for-as-clause)
               (advance rest-form 2)
               (next-form rest-form next-token)
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
                    (advance rest-form 2)
                    (next-form rest-form next-token)
                    )
                   (t
                    (advance rest-form)
                    (next-form rest-form next-token)
                    ))
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
                    (advance rest-form 2)
                    (next-form rest-form next-token)
                    )
                   (t
                    (advance rest-form)
                    (next-form rest-form next-token)
                    ))
             (push (apply #'parse next-token :environment env keys) for-as-clause)
             (advance rest-form)
             (next-form rest-form next-token)

             (when (loop-kwd= next-token :using)
               (push next-token for-as-clause)
               (advance rest-form)
               (next-form rest-form next-token)
             
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
               (advance rest-form 2)
               (next-form rest-form next-token)
               (push (apply #'parse next-token :environment env keys) for-as-clause)
               )
             (advance rest-form)
             (next-form rest-form next-token)
             (continue-parse-for-as-subclause rest-form
                                              next-token
                                              for-as-clause)
             )

           (parse-sql-query-subclause (next-token rest-form for-as-clause)
             (push next-token for-as-clause)
             (cond ((or (loop-kwd= (second rest-form) :in)
                        (loop-kwd= (second rest-form) :of))
                    (push (second rest-form) for-as-clause)
                    (advance rest-form 2)
                    (next-form rest-form next-token))
                   (t
                    (error 'ast-parse-error
                           :format-control "Wrong syntax for SQL clause ~A."
                           :format-arguments (list (second rest-form))))
                   )
             (push (apply #'parse next-token :environment env keys) for-as-clause) ; Query expr.
             (advance rest-form)
             (next-form rest-form next-token)

             (when (loop-kwd= next-token :not-inside-transaction)
               (push next-token for-as-clause)
               (push (apply #'parse (second rest-form) :environment env keys) for-as-clause)
               (advance rest-form 2)
               (next-form rest-form next-token))

             (when (loop-kwd= next-token :get-all)
               (push next-token for-as-clause)
               (push (apply #'parse (second rest-form) :environment env keys) for-as-clause)
               (advance rest-form 2)
               (next-form rest-form next-token))

             (continue-parse-for-as-subclause rest-form next-token for-as-clause)
             )
             
           )
    (parse-var-spec loop-kwd rest-form ())
    ))


;;;---------------------------------------------------------------------------
;;; main clauses


(defun parse-loop-compound-forms-clause (cfc-kwd form clauses env keys)
  (declare (ignore clauses))
  (labels ((parse-compound-forms (rest-forms compound-forms)
             (if (and rest-forms
                      (not (is-loop-keyword (first rest-forms))))
                 (parse-compound-forms (rest rest-forms)
                                       (cons (apply #'parse (first rest-forms)
                                                    :environment env
                                                    keys)
                                             compound-forms))
                 (values (cons cfc-kwd (nreverse compound-forms))
                         rest-forms
                         env)))
           )
    (parse-compound-forms (rest form) ()))
  )


;;; unconditional clause --

(defmethod parse-loop-clause ((loop-kwd (eql :do))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys)
  )


(defmethod parse-loop-clause ((loop-kwd (eql :doing))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-loop-compound-forms-clause loop-kwd form clauses env keys)
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
  (declare (ignore clauses))
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
                 (push (apply #'parse acc-form :environment env keys) acc-clause)
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 ))))
    (when (loop-kwd= next-token :into)
      ;; This makes things very tricky!
      ;; The variable should be available in the various environments
      ;; used for parsing the "stepping" functions.
      ;; Of course this calls for a different, delayed, parsing
      ;; routine; maybe based on saving "parsing thunks".
      (push next-token acc-clause)
      (push (parse (second rest-form) :environment env keys) acc-clause)
      (advance rest-form 2)
      (next-form rest-form next-token)

      (when (loop-kwd= next-token :of-type)
        (push :of-type acc-clause)
        (push (second rest-form) acc-clause)
        (advance rest-form 2)
        (next-form rest-form next-token)))

    (values (nreverse acc-clause)
            rest-form
            env)))
      

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

(defun parse-selectable-clauses (sel-clause-forms sel-clauses env keys)
  (let ((rest-forms sel-clause-forms)
        (next-token nil)
        )
    (next-form rest-forms next-token)
    
    (multiple-value-bind (sel-clause rest-forms ne)
        (apply #'parse-loop-clause (first sel-clause-forms)
               sel-clause-forms
               ()
               env
               keys)
      (setf sel-clauses (cons sel-clause sel-clauses))
      (next-form rest-forms next-token)
      (cond ((loop-kwd= next-token :and)
             ;; Continue parsing selectable clauses.
             (advance rest-forms)
             ;; Here we could isert some error checking.
             (parse-selectable-clauses rest-forms
                                       sel-clauses
                                       ne
                                       keys))
            (t
             (values (nreverse sel-clauses)
                     rest-forms
                     ne))
            ))))


(defun parse-conditional-clause (cond-kwd cond-form clauses env keys)
  (declare (ignore clauses))
  (let* ((cond-clause (list cond-kwd))
         (rest-form cond-form)
         (next-token (first rest-form))
         (result-env env)
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
                 (push (apply #'parse test-form :environment env keys) cond-clause)
                 (advance rest-form 2)
                 (next-form rest-form next-token)
                 ))))

    ;; We have parsed the TEST...
    ;; Now we parse the rest, but remembering that we are parsing a
    ;; context-free sub-lamguage.

    (multiple-value-bind (then-clauses then-rest-form ne)
        (parse-selectable-clauses rest-form () env keys)
      (push then-clauses cond-clause)
      (setf rest-form then-rest-form
            result-env ne)
      (next-form rest-form next-token)
      (when (loop-kwd= next-token :else)
        (advance rest-form)
        (next-form rest-form next-token)
        (multiple-value-bind (else-clauses else-rest-form ne)
            (parse-selectable-clauses rest-form () ne keys)
          (push else-clauses cond-clause)
          (setf rest-form else-rest-form
                result-env ne)
          (next-form rest-form next-token)
          ))
      
      (when (loop-kwd= next-token :end)
        (advance rest-form)
        (next-form rest-form next-token))

      (values (nreverse cond-clause)
              rest-form
              result-env))
    ))
  

(defmethod parse-loop-clause ((loop-kwd (eql :if))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-conditional-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :when))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-conditional-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :unless))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-conditional-clause loop-kwd form clauses env keys))


;;; termination clause --
;;; TERM-KWD is one of WHILE, UNTIL, REPEAT, ALWAYS, NEVER, THEREIS.

(defun parse-termination-clause (term-kwd
                                 form
                                 clauses
                                 env
                                 keys)
  (declare (ignore clauses))
  (let ((rest-form form) ; Just for simmetry....
        (term-form nil)
        )
    (advance rest-form)
    (next-form rest-form term-form) ; Yeah! Yeah! Yeah! Don't be so
                                    ; fussy dear reader.
    (multiple-value-bind (parsed-form ne)
        (apply #'parse term-form :environment env keys)
      (advance rest-form)
      (values (list term-kwd parsed-form)
              rest-form
              ne)
      )))


(defmethod parse-loop-clause ((loop-kwd (eql :while))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :until))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :repeat))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :always))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :never))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))


(defmethod parse-loop-clause ((loop-kwd (eql :thereis))
                              form
                              clauses
                              env
                              &rest keys
                              &key
                              &allow-other-keys
                              )
  (parse-termination-clause loop-kwd form clauses env keys))



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
    (if (eq type-decls-tree t)
        (mapcar (lambda (v) (list t v)) (flatten var-tree))
        (nreverse (avt var-tree type-decls-tree '() )))))


;;;; end of file -- parse-iteration.lisp --
