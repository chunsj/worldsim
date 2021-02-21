;;;; File: time.lsp
;;; SCCS Version: %W%
;;; Contains: Time and date operations.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: August 1991
;;; Updated: Sun Nov 13 03:36:01 1994 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1992, 1992, 1993, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; This file contains general-purpose code for manipulating times
;;; and dates.

(in-package :time-util)			;/\/: Put in the :util package?

(use-package :util)

(export '(encode-date decode-date decode-date-short))
(export '(day->int int->day next-day previous-day))
(export '(month->int int->month next-month previous-month))
(export '(time-and-date-string time-string))
(export '(plan-time-string seconds->time-string))
(export '(plan-minimal-time-string seconds->minimal-time-string))
(export '(seconds->description))
(export '(parse-time-string parse-time-spec))
(export '(abbreviation-position lookup-abbreviation invalid-abbreviation))


;;;; Dates

;;; Encode-date converts a date of the form (day month year [hour minute])
;;; into a universal time.  The day and month can be a number or a name.

(defun encode-date (date-list)
  (apply #'(lambda (date month year &optional (hour 0) (minute 0))
	     (encode-universal-time
	        0			;second
		minute
		hour
		date
		(month->int month)
		year))
	 date-list))

;;; Decode-date converts a universal time to a list of the form
;;; (day-name date month-name year) so that it can be more meaningfully
;;; printed.

(defun decode-date (utime)
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time utime)
    (declare (ignore second minute hour))
    (list (int->day day)
	  date
	  (int->month month)
	  year)))

(defun decode-date-short (utime) ; -> (date 3-char-month-name 2-digit-year)
  (let ((date (decode-date utime)))
    (list (second date)
	  (subseq (string (third date)) 0 3)
	  (rem (fourth date) 100))))


;;;; Day and month names

;;; We need conversions from symbolic names to integers and back.
;;; Unambiguous initial sequences of day or month names can be used
;;; as abbreviations.

;;; Note that in Common Lisp, Monday is day 0 but January is month 1.

(defvar *days* '#(monday tuesday wednesday thursday friday saturday sunday))

(defvar *months* '#(january february march april may june july
		    august september october november december))

(defun day->int (day)
  (check-type day (or symbol string integer))
  (if (integerp day)
      (check-day-int day)
    (or (abbreviation-position day *days*)
	(invalid-abbreviation day *days* "day name"))))

(defun int->day (i)
  (check-day-int i)
  (string (elt *days* i)))

(defun check-day-int (i)
  (if (<= 0 i 6)
      i
    (error "~S is not a valid number for a day." i)))

(defun next-day (day)
  (int->day (mod (1+ (day->int day)) 7)))

(defun previous-day (day)
  (int->day (mod (1- (day->int day)) 7)))

;;; Months

(defun month->int (month)
  (check-type month (or symbol string integer))
  (if (integerp month)
      (check-month-int month)
    (1+ (or (abbreviation-position month *months*)
	    (invalid-abbreviation month *months* "month name")))))

(defun int->month (i)
  (check-month-int i)
  (string (elt *months* (1- i))))

(defun check-month-int (i)
  (if (<= 1 i 12)
      i
    (error "~S is not a valid number for a month." i)))

(defun next-month (month)
  (int->month (1+ (mod (month->int month) 12))))

(defun previous-month (month)
  (int->month (1+ (mod (- (month->int month) 2) 12))))


;;;; Time and date formatting.
;;;
;;; time-and-date-string and time-string format universal times.
;;; plan-time-string and seconds->time-string just take seconds
;;;   and format the elapsed days, hours, etc since zero.
;;; plan-time-string can handle :infinity and the like as well as seconds.
;;; plan-minimal-time-string is like plan-time-string but suppresses
;;;   some leading zero fields.

(defun time-and-date-string (&optional (utime (get-universal-time)))
  ;; Similiar output to unix date command.
  (multiple-value-bind (sec min hr day month year day-of-week dst-p time-zone)
      (decode-universal-time utime)
    (declare (ignore dst-p time-zone))
    (format nil "~:(~A ~A~) ~2D ~2,'0D:~2,'0D:~2,'0D ~4D"
       (subseq (int->day day-of-week) 0 3)
       (subseq (int->month month) 0 3)
       day
       hr
       min
       sec
       year)))

(defun time-string (&optional (utime (get-universal-time)))
  ;; Similiar to TF <time_spec>
  (multiple-value-bind (sec min hr day month year day-of-week dst-p time-zone)
      (decode-universal-time utime)
    (declare (ignore month year day-of-week dst-p time-zone))
    (format nil "~2D~~~2,'0D:~2,'0D:~2,'0D"
       day
       hr
       min
       sec)))

(defun plan-time-string (secs)		;N.B. might be infinite
  (if (numberp secs)
      (seconds->time-string secs)
    secs))

(defun seconds->time-string (seconds)
  ;; Similiar to TF <time_spec>
  (let (days hrs mins secs)
    (multiple-value-setq (mins secs) (floor seconds 60))
    (multiple-value-setq (hrs mins) (floor mins 60))
    (multiple-value-setq (days hrs) (floor hrs 24))
    (format nil "~2D~~~2,'0D:~2,'0D:~2,'0D"
       days
       hrs
       mins
       secs)))

(defun plan-minimal-time-string (secs)		;N.B. might be infinite
  (if (numberp secs)
      (seconds->time-string secs)
    secs))

(defun seconds->minimal-time-string (seconds)
  ;; Similiar to TF <time_spec>
  ;; Suppresses leading fields that contain zero, but always
  ;; shows at least minutes and seconds.  Note that here 3:15
  ;; means 3 minutes 15 seconds, while if 3:15 were parsed as
  ;; a <time-spec> it would be 3 hours 15 minutes.
  (let (days hrs mins secs)
    (multiple-value-setq (mins secs) (floor seconds 60))
    (multiple-value-setq (hrs mins) (floor mins 60))
    (multiple-value-setq (days hrs) (floor hrs 24))
    ;; Rather than a super-complex format-string:
    (cond ((/= days 0)
	   (format nil "~2D~~~2,'0D:~2,'0D:~2,'0D"
		   days hrs mins secs))
	  ((/= hrs 0)
	   (format nil "~2D:~2,'0D:~2,'0D"
		   hrs mins secs))
	  (t
	   (format nil "~2D:~2,'0D"
		   mins secs)))))

(defun seconds->description (seconds)
  ;; Returns something like "3 days, 10 minutes".
  (let (days hrs mins secs items)
    (multiple-value-setq (mins secs) (floor seconds 60))
    (multiple-value-setq (hrs mins) (floor mins 60))
    (multiple-value-setq (days hrs) (floor hrs 24))
    (flet ((record (n singular plural)
	     (cond ((= n 1) (push `(1 " " ,singular ", ") items))
		   ((> n 1) (push `(,n " " ,plural ", ") items)))))
      (record days "day" "days")
      (record hrs "hour" "hours")
      (record mins "minute" "minutes")
      (record secs "second" "seconds")
      ;; /\/: You might think we'd do something with ~:{ and ~:^ --
      ;; but there are problems in at least AKCL and the XP format
      ;; that runs in AKCL.
      (format nil "~{~A~#,1^~}" (apply #'append (nreverse items))))))


;;; Time-spec parser
;;;
;;; The syntax is similar to that of the TF <time-spec>, and in principle
;;; someone who wanted to parse one of these could call the TF compiler;
;;; but in practice some modules don't want to depend on the TF compiler,
;;; deal with the TF language package, etc.  Hence this routine.
;;;
;;; <time_spec> ::= <integer> [<time_units>]
;;;              |  [<days> ~] <hours> : <minutes> [: <seconds>]
;;; <time_units> ::= seconds | minutes | hours | days
;;;
;;; <days>, <hours>, <minutes>, <seconds> ::= <integer>
;;;
;;; N.B. 3:15 is 3 hours 15 minutes, *not* 3 minutes 15 seconds.
;;;
;;; We allow a sign (+ or -) before the time-spec even though that does
;;; not appear in the syntax above.

(defun parse-time-string (string)
  (parse-time-spec (make-string-input-stream string)))

(defun parse-time-spec (stream) ; -> int or nil
  (let ((i 0) (sign 1) (days 0) (hours 0) (minutes 0) (seconds 0))
    (labels ((next () (read-char stream nil nil))
	     (quit () (return-from parse-time-spec nil))
	     (peek () (peek-char nil stream nil nil))
	     (peek-past-whitespace ()
	       (peek-char t stream nil nil))
	     (<int> ()
	       (let ((digit nil) (int 0))
		 (unless (find (peek) "0123456789")
		   (quit))		;must be at least one digit
		 (while (setq digit (and (peek) (digit-char-p (peek))))
		   (setq int (+ (* int 10) digit))
		   (next))
		 int))
	     (<word> ()
	       (let ((*package* (find-package :time-util)))
		 (read stream nil nil)))
	     (<minsec> ()
	       (setq minutes (<int>))
	       (when (eql (peek) #\:)
		 (next)
		 (setq seconds (<int>)))))
      (case (peek)
	((#\+) (next))
	((#\-) (setq sign (- sign))
	       (next)))
      (setq i (<int>))
      (case (peek-past-whitespace)
	;; /\/: Should just peek, not skip whitespace.
	((#\~) (setq days i)
	       (next)			;skip the ~
	       (setq hours (<int>))
	       (unless (eql (peek) #\:) ;a : is required after <hours>
		 (quit))
	       (next)			;skip the required :
	       (<minsec>))
	((#\:) (setq hours i)
	       (next)			;skip the :
	       (<minsec>))
	((#\s #\h #\m #\d)
	 (case (<word>)
	   ((s sec second seconds) (setq seconds i))
	   ((m min minute minutes) (setq minutes i))
	   ((h hour hours)         (setq hours i))
	   ((d day days)           (setq days i))
	   (t                      (quit))))
	;; /\/: Shouldn't let "1q" or something like that work.
	(t (setq seconds i)))
      (* sign (+ seconds (* 60 (+ minutes (* 60 (+ hours (* 24 days))))))))))


;;; Abbreviation-position is used to locate the full name that corresponds
;;; to an abbreviation.  The abbreviation is a symbol or string, and the
;;; full names are a sequence of symbols or strings.  If the abbreviation
;;; matches the beginning of exactly one of the full names, the position
;;; of that full name is returned.  Otherwise, NIL is returned.  The
;;; comparsion is case-insensitive.

(defun abbreviation-position (abbrev full-names)
  (when (symbolp abbrev)
    (setq abbrev (symbol-name abbrev)))
  (flet ((abbrev-match (abbrev full)
	   (eql 0 (search abbrev
			  (if (symbolp full) (symbol-name full) full)
			  :test #'char-equal))))
    (let ((first-pos
	   (position abbrev full-names :test #'abbrev-match)))
      (if (and first-pos
	       (null (position abbrev full-names :start (1+ first-pos)
			       :test #'abbrev-match)))
	  first-pos
	  nil))))

;;; Lookup-abbreviation is used to find a full name rather than its
;;; position.

(defun lookup-abbreviation (abbrev full-names)
  (let ((pos (abbreviation-position abbrev full-names)))
    (and pos (elt full-names pos))))

;;; Invalid-abbreviation signals an error that indicates whether an
;;; abbreviation is completely invalid or merely ambiguous.

;;; Note that the search, unlike that in abbreviation-position,
;;; may generate garbage by calling STRING.

(defun invalid-abbreviation (abbrev full-names description)
  (let ((ambiguities
	 (remove-if-not
	    #'(lambda (full)
		(eql 0 (search (string abbrev) (string full)
			       :test #'char-equal)))
	    full-names)))
    (if (eql (length ambiguities) 0)	;might be vector
	(error "~S in not a valid ~A." abbrev description)
	(error "~S is an ambiguous ~A.~%Possibilities: ~S."
	       abbrev description (map 'list #'string ambiguities)))))

;;; End
