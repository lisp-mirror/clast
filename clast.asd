;;;; -*- Mode: Lisp -*-

;;;; clast.asd --

;;;; See file COPYING in main folder for licensing and copyright information.


(asdf:defsystem "CLAST"
  :components ("clast-package"
               "clast-elements"
               (:module "impl-dependent"
                :components (
                             #+lispworks
                             "clast-lispworks"

                             #+allegro
                             "clast-allegro"

                             #+sbcl
                             "clast-sbcl"
                             ))
               )
  )

;;;; end of file -- clast.asd --
