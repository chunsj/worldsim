;;;; File: simple-matcher.lsp
;;; SCCS Version: %W%
;;; Contains: A simple pattern-matcher
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 6 March 1995
;;; Updated: Mon Mar  6 17:34:59 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1995, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :util)

(export '(match match1))

;;; Trivial pattern matcher

;;; Patterns can contain atoms, lists, and variables.  Atoms are compared
;;; with EQL.  Variables have the form $<name> and match whatever single
;;; object appears at the corresponding position in the data.  If a variable
;;; appears more than once, it must match EQL objects.  The wildcard $
;;; matches anything and can match different objects each time.

;;; (Match pattern data) returns NIL if the match fails and a true value
;;; if it succeeds.  If there are no variables in the pattern, the true
;;; value is just T; otherwise it is an a-list with entries of the form
;;; (variable matching-object).

;;; The matcher uses "downward success continuations" so that it can
;;; be extended in various ways, but we don't really need to do this
;;; (so an even simpler matcher might be better).

#|
(defun match (pat dat)
  (match1 pat dat '() #'(lambda (env) (or env t))))

(defun match1 (pat dat env succeed)
  (cond ((eql pat '$)
	 (funcall succeed env))
	((item-var-p pat)
	 (if (assoc pat env)
	     ;; instantaited
	     (if (eql (lookup pat env) dat)
		 (funcall succeed env)
	       nil)
	   ;; not instantiated
	   (funcall succeed `((,pat . ,dat) . ,env))))
	((atom pat)
	 (if (eql pat dat) (funcall succeed env) nil))
	((atom dat)
	 nil)
	(t
	 (match1 (car pat)
		 (car dat)
		 env
		 #'(lambda (car-env)
		     (match1 (cdr pat) (cdr dat) car-env succeed))))))

(defun item-var-p (x)
  (and (symbolp x)
       (eql (char (string x) 0) #\$)
       (> (length (string x)) 1)))	;so that "$" isn't one
|#

;;; /\/: Matcher extended to handle ??, ?name and *.

(defun match (pat dat)
  (match1 pat dat '() #'(lambda (env) (or env t))))

(defun match1 (pat dat env succeed)
  (cond ((or (eq pat '$) (eq pat '??))
	 (funcall succeed env))
	((eq pat '*)
	 (error "In patterns, * must be in a list."))
	((item-var-p pat)
	 (if (assoc pat env)
	     ;; instantaited
	     (if (eql (lookup pat env) dat)
		 (funcall succeed env)
	       nil)
	   ;; not instantiated
	   (funcall succeed `((,pat . ,dat) . ,env))))
	((atom pat)
	 (if (eql pat dat) (funcall succeed env) nil))
	((eq (car pat) '*)
	 (or (match1 (cdr pat) dat env succeed)
	     (and (not (null dat))
		  (match1 pat (cdr dat) env succeed))))
	((atom dat)
	 nil)
	(t
	 (match1 (car pat)
		 (car dat)
		 env
		 #'(lambda (car-env)
		     (match1 (cdr pat) (cdr dat) car-env succeed))))))

(defun item-var-p (x)
  (and (symbolp x)
       (or (eql (char (string x) 0) #\$)
	   (eql (char (string x) 0) #\?))
       (> (length (string x)) 1)))	;so that "$" isn't one


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
