;;;; File: read-eem-script.lsp
;;; SCCS Version: %W%
;;; Contains: Reader for EEM scripts
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 24 November 1994
;;; Updated: Mon Jul 10 19:27:31 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :world)

(defvar *eem-script-readtable* (copy-readtable nil))

(defvar *eem-script-line-number* nil
  "The (1-origin) number of the line currently being read.")

(defvar *eem-script-name* nil)

(defun eem-script-newline-reader (stream char)
  ;; This will be called for newlines except when we're doing
  ;; read-char, peek-char, and the like, e.g. in time-specs.
  (declare (ignore stream char))
  (when *eem-script-line-number* (incf *eem-script-line-number*))
  (values))

(eval-when (load eval)
  (set-macro-character #\Newline #'eem-script-newline-reader
		       nil		;terminating
		       *eem-script-readtable*))

(defun read-eem-script (filename)
  (let ((*eem-script-name* filename)
	(*eem-script-line-number* 1)
	(*readtable* *eem-script-readtable*))
    (handler-case (read-eem-script-file filename)
      (error (condition)
	(error "Error at line ~D in ~A:~%~A"
	       *eem-script-line-number*
	       *eem-script-name*
	       condition))))
  (setq *EEM-on* nil))

(defun read-eem-script-file (filename)
  (with-open-file (in filename :direction :input)
    (loop for e = (read-event-rec in)
	  until (eq e :eof))))

(defun read-event-rec (in)
  (when (end-of-eem-script-p in)
    (return-from read-event-rec :eof))
  (let ((tspec (parse-time-spec in))
	(next nil)
	(hidden nil)
	(event nil))
    (unless tspec
      (error "Illegal or missing time spec."))
    (setq next (read in nil :eof))
    (when (eq next :hidden)
      (setq hidden t)
      (setq next (read in nil :eof)))
    (when (eq next :eof)
      (error "File ends too soon"))
    (setq event next)
    (schedule-world-event
      (make-world-event
        :due-time tspec
	:hidden-p hidden
	:description (format nil "EEM-Script: ~s" event)
	:action 'scripted
	:args (cdr event)
	:succeed-fun (car event)
	:fail-fun nil))))

;;; Parse-time-spec won't say whether it finds eof instead of a
;;; time-spec.  If we're at the end of the file, it will just
;;; return nil.  So before we try to parse a time-spec, we do
;;; this:

(defun end-of-eem-script-p (in)
  (let ((char nil))
    (loop
      (setq char (peek-char nil in nil :eof))
      (case char
	(:eof
	 (return t))
	(#\newline
	 (incf *eem-script-line-number*)
	 (assert (char= (read-char in) #\newline)))
	(#\space
	 (assert (char= (read-char in) #\space)))
	(#\tab
	 (assert (char= (read-char in) #\tab)))
	(#\;
	 ;; Throw away all chars to end of line, but leave the newline.
	 (loop
	   (case (read-char in nil :eof)
	     (#\newline (unread-char #\newline in)
			(return))
	     (:eof
	      (return-from end-of-eem-script-p t)))))
	(t
	 (return nil))))))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
