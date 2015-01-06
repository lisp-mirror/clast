;;;; -*- Mode: Lisp -*-

;;;; clast.asd --

;;;; See file COPYING in main folder for licensing and copyright information.

(asdf:defsystem :clast
  :components ((:file "clast-package")
               (:file "clast-elements"
                :depends-on ("clast-package"))
               (:module "utilities"
                :components ("lambda-list-parsing"
                             "kitchen-sink")
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
                             "clast-clozure-cl"

                             #+lispworks
                             "clast-lispworks"
                             #+lispworks
                             "clast-capi-tree"

                             #+allegro
                             "clast-allegro"

                             #+cmucl
                             "clast-cmucl"

                             #+sbcl
                             "clast-sbcl"
                             )
                )
               (:file "env-queries"
                :depends-on ("impl-dependent"))
               (:file "parse"
                :depends-on ("clast-elements" "env" "utilities"))
               (:file "parse-iteration"
                :depends-on ("parse"))
               (:file "tools"
                :depends-on ("parse"))
               )
  )

;;;; end of file -- clast.asd --
