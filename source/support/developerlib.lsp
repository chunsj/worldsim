;;;; File: developerlib.lsp
;;;; SCCS Version: %W%
;;;; Contains: Routines for development use.
;;;; Author: Richard Kirby (rbk)
;;;; Created: Thu Feb  1 13:59:27 1990
;;;; Updated: Thu Apr  6 19:45:02 1995 by Jeff Dalton
;;;; Release Version: %Y%
;;;; Copyright: (c) 1992, AIAI, University of Edinburgh
;;;; This material may be reproduced by or for the U.S. Government pursuant
;;;; to the copyright license under the clause at DFARS 252.227-7032
;;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;;; /\/: Much of this code is obsolete, though sometimes still used.

(in-package 'developerlib :nicknames '(dev))

(use-package :util)

(export '(*whoami*
	  dev-redirect-errors
	  dev-add-debug-level
	  dev-set-debug-level
	  *dev-debug-level*
	  dev-minimum-monitor-debug-level
	  dev-debug-msg
	  dev-debug
	  dev-error
	  dev-assert
	  dev-warn
	  *dev-warn-action*
	  dev-advise
	  dev-unadvise
	  dev-unwind-protect-on-error
	  dev-safe-eval
	  dev-setup-timer
	  dev-start-timing
	  dev-stop-timing
	  dev-dump-timing-data))

#+lucid
(import 'lcl:ignore-errors)		;/\/ it's not in the standard package

#+kcl
(import 'util::ignore-errors)

;;;; Global variables section.

;; This global variable is intended for use by the programmer, to aid in
;; identifying where the debug message has come from. I initialise it here to
;; a value, just so that any of the following code that uses it, will not barf.
;;
(defvar *whoami* :dev)

(eval-when (eval compile load)
  (defconstant *dev-debugp*
    t ; was: (read-from-string (si:getenv "OPLANDEBUG"))
    "Is true to include debug code."))

(defvar *dev-debugging-on-p* t
  "If *dev-debugp* is t then all the debugging code will be included in the
  users code. This flag provides a way of subsequently turning debugging off.")

(defvar *dev-debug-level* 10
  "A number representing the current debug level.")

;; The global constant *dev-debug-levels-list* is a hashtable in which debug
;; levels are stored. This can be added to by the application, but I have put
;; in some default values.
;;
(defvar *dev-debug-levels-list* (make-hash-table)
  "A hash table of debug level keywords vs. numbers.")

(defparameter dev-minimum-monitor-debug-level 1
  ;; /\/: Should this be a constant?  Follow the *name* convention?
  ;;      It started as a constant, hence the name...
  ;; /\/: 2 would make more sense as the value, but that would require
  ;;      changing the initial value, the control panel, etc.
  "Min dev-debug-level at which MON output should still occur.")

(eval-when (eval load)
  (dolist (entry
	    '((:all            10)
	      (:information     7)
	      (:warning         5)
	      (:non-fatal-error 5)
	      (:unwind-on-error 2)
	      (:fatal-error     1)
	      (:emergency       1)
	      (:user-request    1)
	      (:force-out       0)
	      (:none            0)))
    (let ((level (first entry))
	  (value (second entry)))
      (setf (gethash level *dev-debug-levels-list*) value))))

;;;; Initialisation section.

(defun dev-redirect-errors (stream)
  "Changes the various error, trace and output streams."
  
  (declare (type stream stream))
  
  (setq lisp:*debug-io* stream)
  (setq lisp:*trace-output* (make-synonym-stream 'lisp:*debug-io*))
  (setq lisp:*error-output* (make-synonym-stream 'lisp:*debug-io*)))

(defun dev-add-debug-level (identifier level)
  "Adds a debug level to the *dev-debug-levels-list* table.
   Used by the user to add an application relative level."
  
  (declare (type keyword identifier)
	   (type integer level))
  
  (setf (gethash identifier *dev-debug-levels-list*) level))


;;;; Utilities section.
;;;; Contains functions and macros used by the following sections.

(defmacro dev-get-level-num-from-level-name (name)
  "A macro that gets a level number from the keyword which is the level name."
  `(gethash ,name *dev-debug-levels-list*))


;;;; Debug section.
;;;; Contains useful functions and macros for debugging.

(defun dev-set-debug-level (level)
  (when (symbolp level)
    (setq level (dev-get-level-num-from-level-name level)))
  (check-type level integer)
  (setq *dev-debug-level* level))

(defmacro dev-debug-msg (msg identifier level)
  
  "A macro that may print a message out on *trace-output*, according to the
   current debug levels.
   Arguments:
     msg - A string which is the message to print out.
     identifier - The module that the message is associated with.
     level - Control level for inhibiting the printing of this message.
   Returns:
   Nothing.
   Side Effects:
     Possibly prints a message on *trace-output*"
  
  ;; This is why it is a macro, because when compiling, if *dev-debugp* is nil,
  ;; then this will be optimised out, and thus not clutter up the code.
  ;; Note too that the form that produces the message will be evaluated
  ;; only if the message will be printed.

  ;; /\/: Used to call force-output even when the *dev-debug-level* was
  ;;      too low. [jwd 1 mar 93]

  ;; /\/: :fatal-error is now a special case.  It was too easy for
  ;;      for such errors to go unnoticed, especially when it's some
  ;;      Lisp code that needs to notice.  :->  [jd 6 apr 95]

  (if (eq level :fatal-error)
      `(dev-fatal-error ,identifier ,msg)
    (when *dev-debugp*
      `(when *dev-debugging-on-p*
	 (when (<= (dev-get-level-num-from-level-name ,level)
		   *dev-debug-level*)
	   (format *trace-output* "~A: ~A~&" ,identifier ,msg)
	   (force-output *trace-output*))))))

;;; Dev-debug is a neater version of dev-debug-msg.  A call to dev-debug-msg
;;; always (or almost always) looks like this:
;;;
;;;   (dev-debug-msg (format nil <format string> <format arg>*)
;;;     *whoami* <level name>)
;;;
;;; It seems more reasonable to do this instead:
;;;
;;;   (dev-debug <level name> <format string> <format arg>*)
;;;
;;; [jd 25 Mar 94]
;;;
;;; /\/: :fatal-error is now a special case.  It was too easy for
;;;      for such errors to go unnoticed, especially when it's some
;;;      Lisp code that needs to notice.  :->  [jd 6 Apr 95]


(defmacro dev-debug (level message &rest format-args)
  (assert (keywordp level))
  (if (eq level :fatal-error)
      `(dev-fatal-error *whoami* ,message ,@format-args)
    (when *dev-debugp*
      `(when *dev-debugging-on-p*
	 (when (<= (dev-get-level-num-from-level-name ,level)
		   *dev-debug-level*)
	   (format *trace-output* "~A: ~@?~&" *whoami* ,message ,@format-args)
	   (force-output *trace-output*))))))


;;; Dev-fatal-error is always called for dev-debug messages when the
;;; level is :fatal-error.  It tries to make sure the user, or the code
;;; that's using O-Plan in subroutine mode, notices the error.

(defvar *direct-user-to-fatal-error* t)

(defun dev-fatal-error (module message &rest format-args)
  (cond ((get-parameter :interactive)
	 (format *trace-output* "~A: ~?~&" module message format-args)
	 (force-output *trace-output*)
	 (when *direct-user-to-fatal-error*
	   (ecase (menu-request
		    `("-heading"
		      ,(format nil "~A printed an error message" module)
		      "I've seen it=:ok"
		      "Don't bother me again=:get-lost"))
	     (:ok)
	     (:get-lost
	      (setq *direct-user-to-fatal-error* nil)))))
	(t
	 (error "Fatal error in ~A: ~?" module message format-args))))


;;; Assertions

(defmacro dev-assert (&rest args)
  "A macro that wraps up assert in a (if *dev-debugp* ).
   Arguments:
     args - the arguments to pass to assert."
  (if *dev-debugp*
      `(if *dev-debugging-on-p*
	(assert ,@args))))

(defun dev-error (ident msg)
  "Just some sugar for the call to error."
  (error "~A: ~A~&" ident msg))

(defvar *dev-warn-action* nil)

(defun dev-warn (message &rest format-args)
  (case *dev-warn-action*
    ((nil)
     (dev-debug :warning "~?" message format-args))
    ((:warn)
     (warn "~A: ~?" *whoami* message format-args))
    ((:break)
     (break "~A: ~?" *whoami* message format-args))))

;;;; Error handling section.
;;;; Contains functions and macros for handling all sorts of errors.

(defmacro dev-safe-eval (form)
  "Evals form, returning the result(s), unless an error is signalled,
   in which case it returns nil."
  (cond ((macro-function 'ignore-errors)
	 `(ignore-errors ,form))
	(t
	 ;; Sorry, no condition system in this Lisp
	 (warn "Dev-safe-eval can't ignore-errors in ~S." form) 
	 form)))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
