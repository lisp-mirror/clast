;;;; -*- Mode: Lisp -*-

;;;; clast-package.lisp --

;;;; See file COPYING in main folder for licensing and copyright information.

(defpackage "IT.UNIMIB.DISCO.MA.CL.UTIL.CLAST" (:use "CL")
  (:nicknames "CLAST" "CL.UTIL.CLAST")
  (:documentation "The CLAST package.

The top-level package API for the CLAST (CL Abstract Syntax Tree)
Library.

Notes:

The name is apt as its etymology goes back to 'clastic' rocks, made of
smaller and broken rocks.")

  (:shadow
   ;; The Magnificent (yet neglected) 7.

   "VARIABLE-INFORMATION"
   "FUNCTION-INFORMATION"
   "DECLARATION-INFORMATION"
   "AUGMENT-ENVIRONMENT"
   "DEFINE-DECLARATION"
   "PARSE-MACRO"
   "ENCLOSE"
   )

  (:export
   "PARSE"

   "WALK"

   )

  )

;;;; end of file -- clast-package.lisp --
