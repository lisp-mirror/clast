;;;; -*- Mode: Lisp -*-

;;;; parse-constants.lisp --
;;;; Useful "constants" built from basic parsing elements.

;;;; See the file COPYING for copyright and license information.

;;;; Portions of code lifted from IT.BESE.ARNESI and CL-WALKER.


(in-package "CLAST")


;;; +t-constant-ref
;;; +nil-constant-ref
;;;
;;; Not really "constants", though they should be.

(defvar +t-constant-ref+
  (build-constant-reference t :constant nil nil)
  "A representation of a reference to T.")


(defvar +nil-constant-ref+
  (build-constant-reference nil :constant nil nil)
  "A representation of a reference to NIL.")


;;;; end of file -- parse-constants.lisp
