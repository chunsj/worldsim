;;;; File: world-compiler.lsp
;;; SCCS Version: %W%
;;; Contains: The means of compiling worlds.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 23 June 1995
;;; Updated: Mon Jul 10 19:29:14 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1995, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :world)

;;;; Compiling world definitions
;;;
;;; [Note that there are many similarities to how world definitions are
;;; loaded.  See world-loader.lsp.]
;;;
;;; Relevant parameters:
;;;
;;;  :world-dir -- the WorldSim root directory.
;;;  :oplan-dir -- the O-Plan root directory.
;;;
;;; Note that the separate world assigns the value of :world-dir to
;;; :oplan-dir if :oplan-dir isn't already defined.
;;;
;;; The default directory for world definitions is the "lib/worlds"
;;; subdirectory of :world-dir.  If no :world-dir has been specified,
;;; the "lib/worlds" subdirectory of :oplan-dir is used instead.
;;; If neither :world-dir nor :oplan-dir is defined, an error is signalled.
;;;
;;; A world definition is a directory containing the files that define
;;; the world.  The definitions directory (e.g. lib/worlds in :world-dir)
;;; is therefore a directory of directories.  Each world directory W
;;; should contain a Lisp source file named W-system.lsp, and that file
;;; should contain a defsystem for W-World in the WORLD package.
;;;
;;; A world is compiled by loading W-system.lsp and then evaluating
;;; (compile-system 'W-world).
;;;
;;; When W-system.lsp is loaded, simple-defsystem:*system-base-directory*
;;; refers to the W directory, so the system definions should specify
;;; directories relative to W and w/o mentioning W itself.
;;;

(defun compile-known-worlds ()
  (mapc #'compile-world (find-known-worlds)))

(defun compile-world (world-name)
  (check-type world-name string)
  (let* ((world-dir
	  (concat-string (world-definitions-dir) "/" world-name))
	 (system-file
	  (concat-name world-dir "/" world-name "-system.lsp"))
	 (stream
	  (open system-file :direction :input :if-does-not-exist nil)))
    (if (null stream)
	(warn "Can't compile the ~S world because it has no system definition."
	      world-name)
      (let ((simple-defsystem:*system-base-directory* world-dir)
	    (*package* (find-package :world)))
	(with-open-stream (s stream)
	  (load s))
	(compile-system
	  (read-from-string (concat-string world-name "-world")))))))

(defun find-known-worlds () ; -> list of system names as strings
  (let* ((definitions-dir (world-definitions-dir))
	 (files
	  (with-unix-process-io (io "/bin/ls" definitions-dir)
	    (stream->lines io)))
	 (directories
	  (remove-if-not
	    #'(lambda (f)
		(unix-directory-name-p (concat-string definitions-dir "/" f)))
	    files)))
    directories))

(defun unix-directory-name-p (namestring)
  (probe-file (concat-string namestring "/.")))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
