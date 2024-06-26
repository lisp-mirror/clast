;;;; -*- Mode: Lisp -*-

;;;; clast.asd --

;;;; See file COPYING in main folder for licensing and copyright information.

(asdf:defsystem :clast
  :author "Marco Antoniotti"
  :license "BSD"
  :description "CLAST is a Common Lisp library that can produce an \"abstract syntax
tree\" of a \"form\".  Its main use is for source analysis and
transformation, e.g., extracting the \"free variables\" list from a
form."
  :components ((:file "clast-package")

               (:file "clast-elements"
                :depends-on ("clast-package"))

               (:file "clast-defclass-elements"
                :depends-on ("clast-elements"))

               (:file "clast-defstruct-elements"
                :depends-on ("clast-elements"))

               (:file "clast-loop-elements"
                :depends-on ("clast-elements"))

               (:file "clast-bq-elements"
                :depends-on ("clast-elements"))

               (:file "clast-parse-protocol"
                :depends-on ("clast-package"))

               (:file "clast-printing"
                :depends-on ("clast-defstruct-elements"
                             "clast-defclass-elements"
                             "clast-loop-elements"
                             ))

               (:module "utilities"
                :components ((:file "lambda-list-parsing")
                             (:file "kitchen-sink"))
                :depends-on ("clast-package"))

               (:file "env"
                :depends-on ("clast-package"))

               (:module "impl-dependent"
                :depends-on ("clast-elements"
                             "clast-defclass-elements"
                             "clast-defstruct-elements"
                             "clast-loop-elements"
                             "clast-printing"
                             "clast-parse-protocol"
                             "env")
                :components (
                             #+ccl
                             (:file "clast-clozure-cl")

                             #+lispworks
                             (:file "clast-lispworks")

                             #+lispworks
                             (:file "clast-capi-tree")

                             #+allegro
                             (:file "clast-allegro")

                             #+cmucl
                             (:file "clast-cmucl")

                             #+sbcl
                             (:file "clast-sbcl")

                             #-(or ccl lispworks allegro cmucl sbcl)
                             (:file "clast-missing-cltl2-envs")
                             )
                )

               (:file "env-queries"
                :depends-on ("impl-dependent"))
               
               (:file "parse-lambda-lists"
                :depends-on ("clast-elements" "env" "utilities"))

               (:file "parse"
                :depends-on ("clast-parse-protocol" "parse-lambda-lists"))

               (:file "parse-constants"
                :depends-on ("parse"))

               (:file "parse-defs"
                :depends-on ("parse"))

               (:file "parse-defstruct"
                :depends-on ("clast-defstruct-elements" "parse-defs"))

               (:file "parse-defclass"
                :depends-on ("clast-defclass-elements" "parse-defs"))

               (:file "parse-loop"
                :depends-on ("clast-loop-elements" "parse"))

               (:file "walk"
                :depends-on ("parse" "parse-loop"))
     
               (:file "tools"
                :depends-on ("parse"))
               )
  )

(asdf:defsystem :clast/tests
    :author "Marco Antoniotti"
    :license "BSD"
    :description "Tests for the CLAST library"
    :pathname "tests/"
    :depends-on (:clast #-5am :fiveam)
    :components ((:file "clast-tests-package")

		 (:file "suites"
		  :depends-on ("clast-tests-package"))

		 (:file "parse-defclass-suite"
		  :depends-on ("suites"))

		 (:file "parse-defs-suite"
		  :depends-on ("suites"))

		 (:file "parse-defstruct-suite"
		  :depends-on ("suites"))

		 (:file "parse-loop-suite"
		  :depends-on ("suites"))

		 (:file "parse-suite"
		  :depends-on ("suites"))
		 )
    )

;;;; end of file -- clast.asd --
