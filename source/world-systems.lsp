;;;; File: world-sim/world-systems.lsp
;;; SCCS Version: %W%
;;; Contains: The world simulator's system definitions
;;; Author: Jeff Dalton
;;; Created: November 1994
;;; Updated: Fri Jun 23 18:12:50 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; World-sim system definitions

;;; The usual procedure is to (compile-system 'world) and then build
;;; an image by starting a fresh Lisp, doing (load-system 'world),
;;; and saving a Lisp image.  The "Makefile" provides a convenient way
;;; to do this and other things.

;;; Note (if you're doing things by hand) that you have to load the
;;; "world-prep.lsp" file before loading this one.

;;; To print a description of the system structure, evaluate
;;;   (print-system-tree 'world)

;;; For a description of the defsystem used here, see support/defsys.lsp.

(in-package :user)


;;;; The World itself

(defsystem (world
	    :required-systems
	      (world-sim
	       simple-defsys))
  ;; Compile or load this system to compile or load the world simulator.
  )

(defsystem (world-sim :required-systems (support
					 initialization
					 sim-clock))
  (world-package)
  (world-parameters
    :requires (world-package))
  (world-toplevel
    :requires (world-package))
  (world-services
    :requires (world-package
	       world-toplevel		;for variable definitions
	       world-parameters))
  (world-loader
    :requires (world-package
	       world-parameters))
  (world-compiler
    :requires (world-package))
  )


;;;; Systems from O-Plan

;;; N.B. They are modified versions, not exactly what's in O-Plan.

;;; Simple-defsys

(defsystem (simple-defsys :directory "support")
  ;; Here just so it will be compiled when the world is.
  (defsys
    :loader null-loader))

(defun null-loader (pathname) nil)

;;; Initialization

(defsystem (initialization
	    :required-systems (support world-ipc parser-kit))
  (world-initsystem))

;;; World-ipc -- only what we need from O-Plan's ipc

(defsystem (world-ipc
	    :required-systems (support))
  (world-ipc)
  ("support/ipc-inter-agent"
    :requires (world-ipc)))

;;; Parser Kit

(defsystem (parser-kit
	    :directory "support/parser-kit")
  (parser-kit-pack)
  (descent
    :requires (parser-kit-pack))
  )

;;; Simulated time

(defsystem (sim-clock
	    :directory "support"
	    :required-systems (support))
  ;; Requires support for pseudo-processes, time-util, and defun-inline
  (sim-clock-package
    :defines (:package))
  (sim-clock
    :requires (sim-clock-package)))
	    

;;;; Support

(defsystem (support :directory "support"
		    :required-systems (base-support
				       dependent-support
				       external-support
				       common-support))
  ;; No files
  ;; /\/: Ok, one file. w/ conditionalized code within.
  ("save-image")
  )


;;;; KCL support

#+kcl
(defsystem (dependent-support :directory "support"
			      :required-systems (base-support))
  (kcl-extensions
    :defines (:macros))
  (kcl-conditions
    :defines (:functions :macros :structures)
    :requires (kcl-extensions))
  (kcl-util
    :defines (:functions)
    :requires (xfork))
  (xfork
    :defines (:functions)
    :compiler compile-with-C
    :loader load-with-C)
  (kcl-xwindowio
    :defines (:functions)
    :requires (xfork)
    :compiler compile-with-C
    :loader load-with-C)
  )

;;; KCL Compile/load routine for files that include definitions in C

#+kcl
(defun compile/load-file-with-C-fns (pathname library)
  ;; /\/ Wouldn't know :output-file for compile, because it's not
  ;; an arg to load and hence not given to load-with-C.  Maybe the
  ;; special compilers and loaders should be passed the file struct.
  (let ((source (merge-pathnames (make-pathname :type "lsp") pathname))
	(object (merge-pathnames (make-pathname :type "o") pathname)))
    (when (or (not (probe-file object))
	      (> (file-write-date source) (file-write-date object)))
      (safe-compile-file-with-C-fns pathname))
    (si:faslink object library)))

#+kcl
(defun safe-compile-file-with-C-fns (filename)
  (let ((*readtable* (copy-readtable nil))) ;for % char macro
    (compile-file filename)))

#+kcl
(defun compile-with-C (pathname &key output-file)
  ;; /\/ Ignores output file
  (safe-compile-file-with-C-fns pathname))

#+kcl
(defun load-with-C (pathname)
  (compile/load-file-with-C-fns pathname "-lc"))


;;;; Lucid support

#+lucid
(defsystem (dependent-support :directory "support"
			      :required-systems (base-support))
  (lucid-conditions)
  (lucid-util
    :defines (:functions))
  (lucid-xwindowio
    :defines (:functions))
  )


;;;; Allegro support

#+:allegro
(defsystem (dependent-support :directory "support"
			      :required-systems (base-support))
  (allegro-util
    :defines (:functions))
  (allegro-xwindowio
    :defines (:functions))
  )


;;;; Common Support

(defsystem (common-support :directory "support"
			   :required-systems (base-support
					      external-support))
  ;; For util package and macros, see system base-support.
  (util-functions
    :defines (:functions))
  (simple-matcher
    :defines (:functions))		;/\/ some exports too
  (time
    :defines (:package :functions))
  (pseudo-process)
  (developerlib)
  (xwindowio
    :requires (developerlib))
  (monitors				;requires xp
    :requires (developerlib))
  )


;;;; External support

(defsystem (external-support :directory "support")
  #+kcl
  (xp)
  #+kcl
  (loop)
  )


;;;; Base-support

;;; Base-support contains portable code needed at compile-time by both
;;; common-support and dependent-support (and then, indirectly, by pretty
;;; much everything else).  The main effect of this system is to define
;;; the UTIL package, which has some of its function definitions filled in
;;; by dependent-support and others filled in by parts of common-support.

(defsystem (base-support :directory "support")
  (util-package
    :defines (:package))
  (util-macros
    :defines (:macros)
    :requires (util-package))
  (arithmetic-dcls
    :defines (:macros :types)
    :requires (util-package)))

;;; End
