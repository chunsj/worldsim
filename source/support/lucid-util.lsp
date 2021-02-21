;;;; File: lucid-util.lsp
;;; SCCS Version: %W%
;;; Contains: Lucid-specific utilities
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Thu Jun  1 15:27:29 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; Some Lucid-specific utilities

;;; A version of this file should exist for each supported CL.

(in-package :util)


;;; Random Unix stuff

(defun argc ()
  (error "No argc"))

(defun argv (n) ; -> string or nil
  (lcl:command-line-argument n))

(defun exit-lisp (&optional (status 0))
  (lcl:quit status))

(defun getenv (string) ; -> string or nil
  (lcl:environment-variable string))	;settable in Lucid

(defun working-directory-pathname ()
  (lcl:working-directory))

(defun change-working-directory (namestring)
  (setf (lcl:working-directory) namestring))


;;; System -- runs a shell command given as a string.

(defun system (cmd)
  (lcl:run-program "sh" :arguments `("-c" ,cmd)))


;;; Interface to xmenu.

;;; It looks like LCL knows to wait (eventaully) for programs run
;;; with :wait nil.  Keeps a record of the child processes, perhaps?

(defvar *xmenu-pathname* nil)

(defun menu-request (xmenu-args &key (read-function #'read))
  (ensure-xmenu-pathname)
  (let ((process-stream
	 (lcl:run-program *xmenu-pathname*
			  :arguments (apply-xmenu-defaults xmenu-args)
			  :output :stream
			  :wait nil)))
    (unwind-protect
	 (funcall read-function process-stream)
      (close process-stream :abort t))))

(defun apply-xmenu-defaults (xmenu-args)
  (if (or (null (get-parameter :menu-geometry))
	  (member "-geometry" xmenu-args :test #'string=))
      xmenu-args
    (list*
      "-geometry" (get-parameter :menu-geometry)
      xmenu-args)))

(defun ensure-xmenu-pathname ()
  (or *xmenu-pathname*
      (setq *xmenu-pathname*
	    (concatenate 'string (get-parameter :oplan-dir) "/bin/xmenu"))))


;;; General function for getting an i/o stream for communication with
;;; a Unix process.

(defun unix-process-io (program-name &rest args)
  (multiple-value-bind (io-stream error-stream exit-status pid)
      (lcl:run-program program-name
		       :arguments args
		       :input :stream
		       :output :stream
		       :wait nil)
    (declare (ignore error-stream exit-status))
    (values
      io-stream
      pid)))

;;; Unix-process-finish is called when a child process should have
;;; terminated.  In some Lisps, it calls wait.  In Lucid CL, that's
;;; handled automatically.

(defun unix-process-finish (pid)
  (declare (ignore pid))
  nil)


;;; Structure operations

;;; These functions have to work for "general" structures, where all
;;; slots are of type T.  They needn't work for structures with more
;;; specialized slot types where slots might be stored in unusual ways.

(defun structurep (obj)
  (sys:structurep obj))

(defun map-structure (f s)
  (let ((new-s (sys:copy-structure s))
	(type (sys:structure-type s)))
    (dotimes (i (sys:structure-length new-s type))
      (setf (sys:structure-ref new-s i type)
	    (funcall f (sys:structure-ref s i type))))
    new-s))

;;; End
