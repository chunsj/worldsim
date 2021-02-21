;;;; File: world-sim/world-toplevel.lsp
;;; SCCS Version: %W%
;;; Contains: The standalone world simulation top-level (main program)
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Thu Jun 22 18:26:26 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).
;;;
;;; See world-services.lsp for a description of the World process
;;; and how it works.
;;;
;;; This file defines the top-level for the standalone version of
;;; the World that runs separately from the O-Plan Exec agent.
;;;
;;; Contents:
;;;  * Overview
;;;  * Variable definitions
;;;  * Top-level
;;;  * Parameters and command-line arguments
;;;  * Process startup
;;;  * Connector pprocess for comms with the Exec
;;;  * Manual interface
;;;  * Main loop
;;;  * Saving an image
;;;  * "Last resort" condition handler
;;;

(in-package :world)


;;;; Overview

;;; The standalone World runs as a single Unix process containing
;;; several "pseudo-processes" (see support/pseudo-process.lsp),
;;; or "pprocesses".

;;; When in connect mode (selected by the "-connect" command-line
;;; argument), the World assumes the standard input and output (stdin
;;; and stdout) of the Unix process are used for communication with
;;; an Exec agent.

;;; When the World starts, the Lisp stream *terminal-io* is connected
;;; to stdin and stdout (or at least that's what we assume -- if some
;;; Common Lisp does things differently, we'll have to change how we
;;; handle this).

;;; In connect mode, *left-io* is given the initial value of *terminal-io*
;;; and *terminal-io* gets the World's user-interaction window as its
;;; value instead.  *left-io* can then be used for communications with
;;; the Exec while ordinary output goes to the interaction window.
;;; When not in connect mode, the value of *terminal-io* is still
;;; assigned to *left-io*, but *terminal-io* also keeps that value
;;; itself.

;;; The mode also affects what pprocesses are created, for which see
;;; below.

;;; Startup:

;;; World-top-level is the function that's called when a World image
;;; starts up.  It processes command-line arguments and parameters in
;;; a manner similar to O-Plan.

;;; World-startup is called to handle the rest of initialization.
;;; It handles the .config file, creates the pprocesses and windows,
;;; and then prints the "agent ready message" to stdout (via *left-io*).

;;; The next step is to call world-main-loop, in which control
;;; resides for the rest of the run.  At present, world-main-loop
;;; just calls pprocess-main-loop.

;;; Pseudo-processes:

;;;   :world -- the main simulator.

;;;   :lisp-listener -- a read-eval-print loop.  It exists only
;;;      when the World is *not* in connect mode and it reads and
;;;      writes *terminal-io* (usually the window in which the World
;;;      was run).

;;;   :left-io -- a connector pprocess that handles conversion between
;;;      text and message structs when communicating with an Exec.

;;; (Pseudo-) Sockets:

;;; Remember that :worldin and :worldout are named from the World's
;;; point of view.  The :world pprocess is the server of :worldin
;;; and a (indeed the) client of :worldout.  This means that it
;;; normally reads :worldin and writes :worldout.  Since :world is
;;; the :worldin server, messages sent to :worldin are added to
;;; :world's event-queue.

;;; The :left-io pprocess connects the :worldin and :worldout sockets
;;; to *left-io*.  It is described further in its own section, below.

;;; Manual interface (for testing without an exec):

;;; See separate section below.



;;;; Variable definitions

(setq *whoami* :world)			;/\/ for some O-Plan routines

(defvar *world-command-readtable* (copy-readtable nil)
  "The readtable for reading world commands.  Normally the
   standard Common Lisp readtable.")


;;; Streams

;;; The World has three main windows plus one for displaying the current
;;; simulated time.  The history window is used to display a trace of
;;; what is happening in the world; the agenda window shows the events
;;; that have been scheduled and have yet to occur; and the interaction
;;; window is used to receive commands from, and send messages to, the
;;; user.

;;; *left-io* can always be used to print messages to the Exec.
;;; In connect mode, it's actually connected to the Exec and
;;; *terminal-io* refers to the same stream as *interact*.
;;; When not in connect mode, it's *terminal-io* and *left-io*
;;; that are the same.

(defvar *history-window* nil)	;from (x-get-stream :world-history)
(defvar *agenda-window* nil)	;from (x-get-stream :world-agenda)
(defvar *interact* nil)		;from (x-get-stream :world-interact)

(defvar *left-io* nil)		;comms with the agent to the left


;;;; Top-level

;;; World-top-level is called when a save world image first starts up.

;;; One "world-init" file is loaded, if it exists.  We look first in
;;; the current directory, then in the user's home directory.

(defun world-top-level ()
  (setq *package* (find-package :world))
  (catch :world-exit
    (handler-bind ((condition #'last-resort-condition-handler))
      (command-line-arguments)
      (unwind-protect
	  (progn
	    (world-startup)
	    (world-main-loop))
        (terminate-xterms-if-necessary)
        (exit-lisp 0)))))

(defun exit-world ()
  (throw :world-exit nil))

(defmessage (:world :kill) ()
  ;; The exec wants us to exit.  Happens only when we're running as
  ;; a separate Unix process.
  (exit-world))


;;;; Parameters and command-line arguments

(defun command-line-arguments ()
  ;; Set up parameter defaults
  (is-init-parameter-defaults)
  ;; Process some values from environment variables as more defaults.
  (is-init-env-parameters)
  ;; Process argv
  (set-parameter :image-name (argv 0))
  (let ((init-file-p t)
	(i 1))
    (labels ((arg ()
	       (argv i))
	     (pop-arg ()
	       (prog1 (arg) (incf i)))
	     (arg->keyword (string)
	       (string->keyword (string-upcase (subseq string 1)))))
      (loop
        (when (null (arg))
	  (return))
	(case (intern (string-upcase (arg)) :world)
	  (-load    (pop-arg) (load-most-recent (pop-arg)))
	  (-break   (pop-arg) (break "~A" '-break))
	  (-eval    (pop-arg) (eval (read-from-string (pop-arg))))
	  (-noinit  (pop-arg) (setq init-file-p nil))
	  (-config  (pop-arg) (is-set-config-file (pop-arg)))
	  (-connect (pop-arg) (is-set-connect t))
	  ((-no -not)
	   ;; Used to set parameters to nil (false)
	   ;; /\/: Perhaps shouldn't require - or : on the parameter?
	   (let ((negator (pop-arg))
		 (parameter (arg)))
	     (if (and parameter (find (char parameter 0) "-:"))
		 (set-parameter (arg->keyword (pop-arg)) nil)
	       (error "~S was not followed by a parameter to negate"
		      negator))))
	  (t
	   ;; Other -name or :name args are given to set-parameter
	   ;; as keywords.  The parameter value will be a string.
	   (cond ((string= (arg) "")
		  (error "Skipping null argument."))
		 ((find (char (arg) 0) "-:")
		  (let* ((arg (pop-arg))
			 (value (pop-arg)))
		    (if value
			(set-parameter (arg->keyword arg) value)
		      (error "No value for ~A" arg))))
		 (t
		  (error "Skipping unknown argument: ~A" (pop-arg))))))))
    ;; Load init file if wanted.
    (when init-file-p (load-init-file "world-init" :world))
    ;; Define :oplan-dir in case any support code refers to it.
    (unless (get-parameter :oplan-dir)
      (set-parameter :oplan-dir (get-parameter :world-dir)))))


;;;; Process startup

(defun world-startup ()
  (make-world-pprocess)

  ;; is- and ipc- routines use *whoami* rather than *pprocess*
  (assert (eq *whoami* :world))

  (is-configure)			;handle .config file
  (unless (is-init *whoami*)
    (dev-error *whoami* "is-init failed."))

  ;; Arrange [for *whoami*] to be run if there's user input
  (ipc-register-listen-stream (x-get-stream :world-interact))

  (setq *history-window* (x-get-stream :world-history))
  (setq *agenda-window* (x-get-stream :world-agenda))
  (setq *interact* (x-get-stream :world-interact))

  ;; We now assume that all our main windows exist.

  ;; Make breaks, etc. go to a World-related window, indeed
  ;; to the one the user will use to send input to the World.
  (x-redirect-errors :world-interact)

  ;; Adjust *left-io* and *terminal-io*
  (setq *left-io* *terminal-io*)
  (when (is-connect-p)
    (setq *terminal-io* (x-get-stream :world-interact)))
  (connect-to-left-io)

  ;; Start a repl if not in connect mode.  To intervene in connect mode,
  ;; the user has to type "break" in the interaction window.
  (unless (is-connect-p)
    (new-repl))

  ;; A few last steps.
  (define-a-clock)
  (ensure-world-clock-window)
  (load-world-definition)

  (ipc-write-agent-ready *left-io*)

  )

(defun make-world-pprocess ()
  (new-pprocess :world
    :status :run-on-event
    :run-function 'world-event-handler))


;;;; Connector pprocess for comms with the Exec

;;; Always reads :worldout and prints the result on *left-io*.
;;; When in connect mode, it also reads *left-io* and sends the
;;; results to :worldin (and hence to the :world pprocess).

;;; The sockets are named from the World's point of view.
;;; So to send to the Exec, the World writes to :worldout.
;;; To send to the World, send to :worldin (using send-to-world, below).

(defun connect-to-left-io ()
  (new-connector :left-io
    :io-stream *left-io*
    :socket-to-read :worldout
    :socket-to-write (if (is-connect-p) :worldin nil)
    :input-guard 'world-input-guard))

(defun world-input-guard (self input)
  (declare (ignore self))
  (if (and (consp input) (symbolp (car input)))
      (values t input)
    (error "Invalid World input: ~S" input)))


;;;; Manual interface

;;; (SEND-TO-WORLD message-id message-arg...) can be used to send a
;;; message to the world as if from the Exec.  It's most often called
;;; from the read-evel-print loop when not in connect mode.

;;; (GO-FASTER) takes steps to make the system run faster.  At present,
;;; it has no effect except in [A]KCL.

(defun send-to-world (&rest message)
  (assert (repl-p *pprocess*))
  (ensure-client :worldin)
  (multiple-value-bind (ok-p transformed-message)
      (world-input-guard *pprocess* message)
    (when ok-p
      (ipc-write :worldin transformed-message))))

(defun go-faster ()
  #+kcl (use-fast-links t)
  nil)


;;;; Main loop

(defun world-main-loop ()
  (pprocess-main-loop))			;/\/ for now


;;;; Saving an image

(defun save-world (filename)
  (util:save-image filename #'world-top-level))


;;;; The "last resort" condition handler

(defvar *lrch-attempt* 0)

(defun last-resort-condition-handler (c)
  ;; When ...
  (when (and ;; We can't enter the debugger [because:]
	     (or ;; Not interactive [or]
	         (not (get-parameter :interactive))
		 ;; Connect mode before the windows are up
		 (and (is-connect-p)
		      (null *interact*)))
	     ;; And it's not a warning that wouldn't enter the
	     ;; debugger anyway.
	     (not (and (typep c 'warning)
		       (not *break-on-warnings*))))
    ;; ... then we'd better try to handle it.
    (incf *lrch-attempt*)
    (case *lrch-attempt*
      (1
       ;; Print a message and exit.  If a condition is signalled before
       ;; exit-lisp causes an unwind (e.g. if it happens while printing
       ;; the error message), then this handler will run again, but with
       ;; an attempt count of 2.
       (format *error-output*
	       "~&~%The following condition was not handled:~%~% ~A~%~%"
	       c)
       (exit-world))
      (2
       ;; This time we won't try printing a message.
       (exit-world))
      (t
       ;; Maybe this will work.
       (exit-lisp -1)))))


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
