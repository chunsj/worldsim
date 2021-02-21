;;;; File: world-sim/world-prep.lsp
;;; SCCS Version: %W%
;;; Contains: Initial setup
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Tue Oct  3 22:05:46 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1993, 1994 AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; Load this file to prepare for compiling or loading the standalone
;;; World simulator.  It loads defsystem and, if needed, defpackage,
;;; and takes some other steps to prepare Lisp for what's to come.

;;; Assumes the current directory is "world-sim"

(in-package :user)

(defvar *world-version* "1.0")

(push :world *features*)

(load "world-release-date.lsp")		;defines *world-release-date*


;;; Lisp-implementation-specific prep

(load "lisp-prep.lsp")


;;; General prep

(unless (fboundp (find-symbol "DEFPACKAGE"))
  (load "support/defpackage"))

(unless (find-package :simple-defsystem)
  (load "support/defsys"))

(shadowing-import '(simple-defsystem:defsystem
                    simple-defsystem:compile-system
                    simple-defsystem:load-system
		    simple-defsystem:clean-system
		    simple-defsystem:print-system-tree))


;;; System definitions

(load "world-systems")


;;; OPLAN package

;;; /\/: This still seems to be needed, e.g. for loading util-package.o
;;; in AKCL 1.615.

(defpackage "OPLAN")


;;; End
