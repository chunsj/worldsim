;;;; File: ipc-inter-agent.lsp
;;; SCCS Version: %W%
;;; Contains: Inter-agent ipc support
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Fri Sep 29 17:30:06 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :ipcinterface)

(use-package :pseudo-process)

(export '(ipc-run-agent
	  ipc-write-agent-ready
	  *1st-char-time-limit*
	  *char-time-limit*
	  *agent-ready-line*))


(defparameter *agent-ready-line* "<<<Ready>>>")

(defun ipc-write-agent-ready (stream)
  (terpri stream)
  (write-line *agent-ready-line* stream)
  (finish-output stream))


;;; Run-agent

(defparameter *1st-char-time-limit* 30)

(defparameter *char-time-limit* 15)

(define-condition read-char-timeout (condition))

;;; ipc-run-agent: Here the shell-command is just a pathname, without
;;; any arguments.

(defun ipc-run-agent (agent-name shell-command &rest args) ; -> io-stream
  (let ((io-stream (apply #'ipc-run-program shell-command args)))
    (look-for-ready-line agent-name io-stream)
    io-stream))

(defun look-for-ready-line (agent-name stream)

  ;; Wait for the 1st character.
  (wait-for-output agent-name stream *1st-char-time-limit*)
  
  ;; Look for the ready message.
  (let* ((record (make-string-output-stream))
	 (echo (make-echo-stream stream record)))
    (handler-case (scan-for-line *agent-ready-line* echo)
      (read-char-timeout ()
	(error "~S failed to complete startup in time.~%~
                Received so far:~%~S~%"
	       agent-name
	       (get-output-stream-string record))))

    ;; Found ready message.
    (let ((received (get-output-stream-string record)))
      (when (> (length received)
	       (+ 2 (length *agent-ready-line*)))
	(cerror "Continue anyway."
		"~S had problems but seems ok.  Received:~%~S~%"
		agent-name
		received)))))

(defun wait-for-output (source-name stream timeout)
  (until (select-input-p (list stream) timeout)
    (cerror "Try again"
	    "No response from ~S after ~D seconds."
	    source-name
	    timeout)))

(defun read-char-with-timeout (stream &optional (timeout *char-time-limit*))
  (cond ((select-input-p (list stream) timeout)
	 (read-char stream))
	(t
	 (signal 'read-char-timeout)
	 nil)))

(defparameter *newline-string* (string #\Newline))

(defun scan-for-line (target stream)
  ;; Look for the chars in target on a line of their own.
  (or (and (see-if-string target stream)
	   (see-if-string *newline-string* stream))
      (and (scan-for-char #\Newline stream)
	   (scan-for-line target stream))))

(defun scan-for-char (target stream)
  (loop
    (let ((c (read-char-with-timeout stream)))
      (cond ((null c)			  ;timed out
	     (return nil))		  ; so fail
	    ((char= c target)		  ;found target char
	     (return t))))))		  ; so return success

(defun see-if-string (target stream)
  ;; N.B. Does not back up if it sees only the 1st part of the target,
  ;; but does unread the 1st non-matching char.  E.g. if the target is
  ;; "abc" and it sees "abd", the "d" remains in the stream but the
  ;; "a" and "b" are gone.
  (let ((len (length target))
	(i 0))
    (loop
      (when (= i len)			  ;found it
	(return t))			  ; so return success
      (let ((c (read-char-with-timeout stream)))
	(cond ((null c)			  ;timed out
	       (return nil))		  ; so fail
	      ((char= c (schar target i)) ;found char in target
	       (incf i))		  ; so look for next target char
	      (t			  ;not what we're looking 
	       (unread-char c stream)	  ; so unread the last char read
	       (return nil)))))))	  ;    and return failure

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
