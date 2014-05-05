;;;; -*- Mode: Lisp -*-

;;;; clast.asd --

;;;; See file COPYING in main folder for licensing and copyright information.

(asdf:defsystem "CLAST"
  :components ("clast-package"
               (:file "clast-elements"
                :depends-on ("clast-package"))
               (:module "utilities"
                :components ("lambda-list-parsing")
                :depends-on ("clast-package"))
               (:file "clast-printing"
                :depends-on ("clast-elements"))
               (:module "impl-dependent"
                :depends-on ("clast-elements"
                             "clast-printing")
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
               (:file "env"
                :depends-on ("impl-dependent"))
               (:file "parse"
                :depends-on ("clast-elements" "env" "utilities"))
               )
  )

;;;; end of file -- clast.asd --
