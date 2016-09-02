;;;; -*- Mode: Lisp -*-

;;;; clast-tests.asd --

;;;; See file COPYING in main folder for licensing and copyright information.

(asdf:defsystem :clast-tests
  :author "Marco Antoniotti"
  :license "BSD"
  :description "Tests for the CLAST library"
  :pathname "tests/"
  :depends-on ("CLAST" "FIVEAM")
  :components ((:file "clast-tests-package")
               )
  )

;;;; end of file -- clast-tests.asd --
