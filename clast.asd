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
               (:module "utilities"
                :components ((:file "lambda-list-parsing")
                             (:file "kitchen-sink"))
                :depends-on ("clast-package"))
               (:file "clast-printing"
                :depends-on ("clast-elements"))
               (:file "env"
                :depends-on ("clast-package"))
               (:module "impl-dependent"
                :depends-on ("clast-elements"
                             "clast-printing"
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
                             )
                )
               (:file "env-queries"
                :depends-on ("impl-dependent"))
               (:file "parse-lambda-lists"
                :depends-on ("clast-elements" "env" "utilities"))
               (:file "parse"
                :depends-on ("parse-lambda-lists"))
               (:file "parse-defs"
                :depends-on ("parse"))
               (:file "parse-loop"
                :depends-on ("parse"))
               (:file "walk"
                :depends-on ("parse" "parse-loop"))
               (:file "tools"
                :depends-on ("parse"))
               )
  )

;;;; end of file -- clast.asd --
