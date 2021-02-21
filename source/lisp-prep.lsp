;;;; File: world-sim/lisp-prep.lsp
;;; SCCS Version: %W%
;;; Contains: Lisp-implemmentation-specific prep code
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Mon Nov  7 17:21:34 1994 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).


(in-package :user)


;;; AKCL initialization

#+akcl (use-fast-links nil)
; #+akcl (proclaim '(optimize (safety 2)))
#+akcl (proclaim '(optimize (safety 0)))
#+akcl (setq si::*multiply-stacks* 4)

#+akcl (unintern 'lisp)			;/\/ where did LISP come from?

#+akcl (setq *load-verbose* nil)	;/\/ cannot otherwise suppress some
					;    output from loading .o files.


;;; Lucid CL (LCL) initialization

#+:lucid
(progn

  #+lucid
  (proclaim '(optimize (compilation-speed 0) 	;production mode
	               (speed 2)		;tail-merge off
		       (safety 0)))		;no arity or r/w arg checks

  #+lucid-development
  (proclaim '(optimize (compilation-speed 3)
	               (speed 0)
	               (safety 3)))

  (setq lcl:*redefinition-action* :warn) 	; or :query

  (push "lsp" lcl:*load-source-pathname-types*)

  ;; Avoid conflicts with, e.g., lcl:monitor.
  (setq lcl:*default-make-package-use-list*
	(list (find-package "LISP")))
  (unuse-package :lcl)

  ;; Arrange for some CL additions to work:

  (import '(lcl:defpackage lcl:restart-case lcl:continue
	    lcl:destructuring-bind)
	  :lisp)

  (export '(lcl:defpackage lcl:restart-case lcl:continue
	    lcl:destructuring-bind)
	  :lisp)

); end #+:lucid


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
