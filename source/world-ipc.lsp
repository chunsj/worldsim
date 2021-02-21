;;;; File: world-ipc.lsp
;;; SCCS Version: %W%
;;; Contains: Inter-Process Communications interface, World-Sim version.
;;; Author: Jeff Dalton
;;; Created: November 1994
;;; Updated: Mon Nov 14 20:08:32 1994 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1993, 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package 'ipcinterface :nicknames '(ipc))

(export '(ipc-set-up-and-register-server
	  ipc-set-up-and-register-client
	  ipc-add-server
	  ipc-register-listen-stream
	  defmessage
	  ipc-get-handler
	  ipc-handle-message
	  ipc-with-own-copy
	  deep-copy
	  ipc-send
	  ipc-write
	  ipc-write-client
	  ipc-send-priority-message
	  ipc-run-program
	  ))

(use-package '(:dev :pseudo-process :util))


;;;; Socket and listen-stream operations

(defun ipc-set-up-and-register-server (soc-name)
  ;; Registers the current process as a server.
  (ensure-socket soc-name)
  (register-server soc-name (find-pprocess *whoami*))
  t)

(defun ipc-set-up-and-register-client (soc-name)
  ;; Registers the current process as a client.
  (ensure-socket soc-name)
  (register-client soc-name (find-pprocess *whoami*))
  t)

;;; Calls to the following procedure replace calls to add-server,
;;; but the function is rather different.

(defun ipc-add-server (soc-name)
  (ipc-set-up-and-register-server soc-name))

(defun ipc-register-listen-stream (stream)
  (let ((p (find-pprocess *whoami*)))
    (unless (member stream (pprocess-input-streams p))
      (setf (pprocess-input-streams p)
	    (nconc (pprocess-input-streams p) (list stream)))))
  stream)


;;;; Message handlers
;;;
;;; Define a handler like this:
;;;
;;;   (DEFMESSAGE (recipient message-id) (message-arg...) form...)
;;;
;;; Think of DEFMESSAGE as defining the meaning of a message, which
;;; includes how it is handled.  The syntax is similar to that of
;;; Flavors DEFMETHOD.  The recipient is either a <pprocess-name> or
;;; (<pprocess-name> <socket-name>).  The recipient can also be an id,
;;; such as :MCC-CLIENT, that isn't actually a pprocess name.  In that
;;; case, IPC-GET-HANDLER will still work, but not the IPC-HANDLE-
;;; functions.
;;;
;;; A pprocess handles a message from its event-queue by calling
;;;
;;;   (IPC-HANDLE-MESSAGE self message)
;;;
;;; Note that message is the entire message object, as defined by the
;;; pprocess message system, not just the message-contents.  However,
;;; the message-id and message-args in the DEFMESSAGE correspond to
;;; the contents, not the entire message.  (If you think about it,
;;; this is reasonable; the other stuff is handled behind the scenes.)
;;;
;;; IPC-HANDLE-MESSAGE does not handle the old-style MCC-messages, though
;;; we could make them work by seeing if the message-contents was of type
;;; MCC-MESSAGE.  Instead, we have a separate IPC-HANDLE-MCC-MESSAGE for
;;; this case.
;;;
;;; (IPC-GET-HANDLER pprocess-name message-id) is a lower-level way to
;;; look up a message handler without doing a route check or extrating
;;; the message-contents from a message.  It's used chiefly to handle
;;; DB-REQUESTs, which aren't packaged up as pprocess messages.

(proclaim '(inline ipc-get-handler))

(defmacro get-message-handler-record (pprocess-name message-id)
  ;; A handler record has the form (expected-route . handler-name)
  `(get ,message-id ,pprocess-name))

(defun ipc-get-handler (pprocess-name message-id)
  (cdr (or (get-message-handler-record pprocess-name message-id)
	   (error "No handler for ~S ~S." pprocess-name message-id))))

(defmacro defmessage ((recipient message-id) formals &body handler-body)
  ;; Recipient can be <pprocess-name> or (<pprocess-name> <socket-name>).
  ;; The socket-name can be nil, in which case the route is not checked.
  (when (symbolp recipient)
    (setq recipient `(,recipient)))
  (destructuring-bind (pprocess-name &optional expected-route)
      recipient
    (let ((handler-name (concat-name "%" pprocess-name "-" message-id
				     "-MESSAGE-HANDLER")))
      `(progn
	 (defun ,handler-name ,formals
	   ,@handler-body)
         (setf (get-message-handler-record ',pprocess-name ',message-id)
               (cons ',expected-route ',handler-name))
         ',handler-name))))

(defun ipc-handle-message (self message)
  ;; N.B. Assumes *whoami* = (pprocess-name self).
  ;; /\/: "self" is for the future.  We may use it when looking up
  ;; handlers or pass it to the handler as its 1st argument.
  (declare (ignore self))
  (let* ((contents (message-contents message))
	 (message-id (car contents))
	 (message-body (cdr contents)))
    (let* ((handler-rec (get-message-handler-record *whoami* message-id))
	   (expected-route (car handler-rec))
	   (handler (cdr handler-rec)))
      (unless handler
	(error "No handler in ~S for ~S." *whoami* message))
      (unless (or (null expected-route)
		  (eq expected-route (message-route message)))
	(cerror "Continue."
		"Wrong route to ~S for ~S.~%~
                 Route was ~S but should be ~S."
		*whoami* message (message-route message) expected-route))
      ;; Looks ok, so call the handler
      (apply handler message-body))))


;;;; Copying

;;; When a component needs to modify an object obtained from another
;;; component, it should make a copy of its own and use that instead.
;;; However, if ipc automatically provided the recipient with its own
;;; copy (as it would if each component was a separate Unix process
;;; communicating via sockets), then it shouldn't have to waste effort
;;; by making another copy.  Consequently, "own copies" should be
;;; made using a construct that can be redefined to avoid copying
;;; when such copies are unnecessary.  Hence the following macro.
;;; In the single-process O-Plan,
;;;
;;; (ipc-with-own-copy (<var> <init-form> <copy-form>) [<body>])
;;;
;;; returns the value of the last form in <body>, if a body is supplied,
;;; or else the value of <copy-form>.  In the body, <var> is bound to 
;;; the value of <copy-form>; in <copy-form> it is bound to the value
;;; of <init-form>.  (Let's hope this isn't too confusing.)
;;;
;;; <copy-form> should do nothing other than make a copy of the object.
;;; In a version of IPC that automatically supplied a recipient with its
;;; own copy, <copy-form> would be ignored and in effect replaced by
;;; (identity <var>>).

(defmacro ipc-with-own-copy ((var init-form copy-form) &body body)
  `(let ((,var ,init-form))
     (let ((,var ,copy-form))
       ,@(or body
	     `(,var)))))


;;;; Deep copy

;;; Deep-copy copies to all levels.  Some objects that are, strictly
;;; speaking, modifiable are treated as read-only and not copied.  The
;;; modifiable types treated as read-only are strings and pathnames.

(defun deep-copy (what)		;paranoia rules /\/
  (cond ((symbolp what) what)
	((consp what) (deep-copy-list what))
	((numberp what) what)
	((stringp what) what)
	((structurep what)
	 (map-structure #'deep-copy what))
	((pathnamep what) what)
	(t (copy-via-io what))))

(defun deep-copy-list (list)
  ;; The list must be a cons; it might not end in nil.
  (let ((head (list (deep-copy (car list)))))
    (do ((ptr head (cdr ptr))
	 (tail (cdr list) (cdr tail)))
	(nil)
      (when (atom tail)
	(setf (cdr ptr) (deep-copy tail))
	(return head))
      (setf (cdr ptr) (list (deep-copy (car tail)))))))

(defun copy-via-io (what)
  #-Allegro
  (break "Copy via I/O for ~S.~%Lose, lose." what)
  (values
   (read-from-string
      (write-to-string what :escape t :pretty nil :level nil :length nil)
      nil
      :eof)))

#|
;;; A version of deep-copy that preserves shared structure.

(defvar *copy-table* (make-hash-table :test #'eq))

(defvar *copy-root* nil)

(defun deep-copy (what)
  (clrhash *copy-table*)
  (let ((*copy-root* what))
    (deep-copy1 what)))

(defun deep-copy1 (what)
  (cond ((symbolp what) what)
	((consp what)
	 (let ((copy (gethash what *copy-table* *copy-table*)))
	   (if (not (eq copy *copy-table*))
	       copy
	     (setf (gethash what *copy-table*)
		   (deep-copy1-list what)))))
	((numberp what) what)
	((stringp what) what)
	((structurep what)
	 (let ((copy (gethash what *copy-table* *copy-table*)))
	   (if (not (eq copy *copy-table*))
	       copy
	     (setf (gethash what *copy-table*)
		   (map-structure #'deep-copy1 what)))))
	((pathnamep what) what)
	(t (copy-via-io what))))

(defun deep-copy1-list (list)
  ;; The list must be a cons; it might not end in nil.
  (let ((head (list (deep-copy1 (car list)))))
    (do ((ptr head (cdr ptr))
	 (tail (cdr list) (cdr tail)))
	(nil)
      (when (atom tail)
	(setf (cdr ptr) (deep-copy1 tail))
	(return head))
      (setf (cdr ptr) (list (deep-copy1 (car tail)))))))
|#


;;;; General message-sending

;;; The difference between ipc-send and ipc-write is intended to be
;;; merely syntactic.

(defun ipc-send (socket-name id &rest args)
  (send-to-socket socket-name (cons id args)))

(defun ipc-write (to what)
  "Stuffs what down a socket."
  (send-to-socket to what))		;/\/ used to deep-copy

(defun ipc-write-client (soc-name client what)
  ;; /\/ Should follow ipc-send in arg syntax.
  (send-to-client soc-name
		  (find-pprocess client)
		  what))		;/\/ deep-copy?

(defun ipc-listen (name)
  "Checks if data present on socket with symbolic name name."
  (break "ipc-listen ~S called" name)
  ())

;;; High-priority messages
;;;
;;; The message is placed at the front of the queue.  This should be
;;; done _only_ for user-initiated events that are allowed to occur at
;;; any time and that should take effect as soon as possible.  Setting
;;; the AM single-step mode or changing a component's debug-level are
;;; examples.

(defun ipc-send-priority-message (route recipient what)
  (send-immediate route (find-pprocess recipient) what))


;;;; Run a unix process and return an io stream

(defvar *stream->pid-table* '())	;for debugging, etc.

(defun ipc-run-program (name &rest args)
  (multiple-value-bind (stream pid)
      (apply #'util:unix-process-io name args)
    (setf (getf *stream->pid-table* stream) pid)
    stream))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
