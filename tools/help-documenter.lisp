;;;; -*- Mode: Lisp -*-

;;;; help-documenter.lisp
;;;;
;;;; HELambdaP `document` call to produce the CLAST main
;;;; documentation.
;;;;
;;;; See file COPYING in main folder for licensing and copyright information.

;;;; (in-package "CLAST")

(require :helambdap)


;;; Building the CLAST documentation (assume to be running on LW).

(hlp:document "../"
	      :documentation-title "CLAST"
              :exclude-directories
              (list "tools/"
                    "docs/"
                    ".git/"
                    "tests/"
                    "tmp/"
                    "utilities/"
                    "_clastbin/"
                    )

	      :exclude-files
              (list ".gitignore"
                    ;; "impl-dependent/clast-allegro.lisp"
                    "impl-dependent/clast-capi-tree.lisp"
                    ;; "impl-dependent/clast-clozure.lisp"
                    ;; "impl-dependent/clast-cmucl.lisp"
                    "impl-dependent/clast-lispworks.lisp"
                    ;; "impl-dependent/clast-sbcl.lisp"
                    ;; "impl-dependent/clast-missing-clt2-envs.lisp"
                    )
	      :only-exported t)

;;;; end of file -- clast-dot.lisp
