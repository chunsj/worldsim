;;;; File: monitors.lsp
;;;; SCCS Version: %W%
;;;; Contains: Monitors - general purpose message outputter.
;;;; Author: Richard Kirby (rbk)
;;;; Created: Thu Aug 20 10:56:53 1992
;;;; Updated: Mon Jul 24 18:36:33 1995 by Jeff Dalton
;;;; Release Version: %Y%
;;;; Copyright: (c) 1992, AIAI, University of Edinburgh
;;;; This material may be reproduced by or for the U.S. Government pursuant
;;;; to the copyright license under the clause at DFARS 252.227-7032
;;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;;; N.B. Lucid CL has a package and macro named MONITOR.   /\/

(in-package :monitors :nicknames '(:mon))

(use-package :developerlib)
(use-package :util)

#+lucid
(import '(lcl:set-pprint-dispatch
	  lcl:*print-right-margin*))

(export '(monitor mon-init-db mon-add-user-def-print-fn mon-set-level
	  mon-format mon-current-levels *monitor-level*))

(eval-when (load eval compile)
  #+kcl
  (progn
    ;; Install Richard C. Waters' XP Pretty Printer.
    (xp::install :package (find-package :monitors))
    ;; Set mon-format to be the same as the xp format routine.
    (setf (symbol-function 'mon-format) #'xp::format))
  #-kcl
  (setf (symbol-function 'mon-format) #'format))

#+:x-window-support (use-package '(:x))

;; If t then monitors are included into the code, if nil then optimised out.
(defconstant *mon-include-monitors-p* t)

;; Default filename for Monitor defaults file. If a directory is not specified
;; then assumed to be the value of the environment variable OPLANDIR.
(defparameter *mon-defaults-file* "monitor.def")

;; A list of modules monitored.
(defvar *mon-modules* nil)

;; A list of levels ranked with lowest level first.
(defvar *mon-levels* '(:RAW-LISP :DEBUG :DETAILED :ONE-LINER :MINIMUM
		       :NONE :OFF))

;; Default settings.
(defvar *mon-defaults* nil)

(defvar *monitor-level*)		;bound by the MONITOR macro

;; A monitor structure to hold info on what level for a module, and where the
;; output should go, etc.
(defstruct monitor
  level
  output-device
  (print-array *print-array*)
  (print-base 10)			;was *print-base*
  (print-case :downcase)		;was *print-case*
  (print-circle *print-circle*)
  (print-escape *print-escape*)
  (print-length nil)			;was *print-length*
  (print-level nil)			;was *print-level*
  (print-pretty t)			;was *print-pretty*
  (print-radix *print-radix*))


;; A hashtable of module to monitor struct.
(defvar *mon-monitor-table* nil)

;; Takes an output-device spec and returns a stream open for writing.
(defun convert-to-stream (module output-device)
  (declare (special *standard-output*)
	   #-:x-window-support (ignore module))
  (case (car output-device)
    (:SCREEN
     (if (eql (cadr output-device) :DEBUG)
	 (make-synonym-stream '*debug-io*)
	 #+:x-window-support
	 (if (x-open-and-register-io-win module (cadr output-device))
	     (x-get-stream module)
	     (error "Unable to create x window"))
	 #-:x-window-support
	 (make-synonym-stream '*standard-output*)))
    (:FILE
     (open (concatenate 'string (get-parameter :oplan-tmp-dir) "/"
			(cadr output-device))
	   :direction :OUTPUT
	   :if-exists :SUPERSEDE
	   :if-does-not-exist :CREATE))))

;; This function needs to look at the stream and determine how many characters
;; can be printed on one line.
(defun mon-calculate-right-margin (stream)
  (declare (ignore stream))
  70)

;; Reads in from the Monitor defaults file the initial settings.
(defun mon-init-db (&optional (filename *mon-defaults-file*))
  (setq filename
	(merge-pathnames filename
			 (concatenate 'string
				      (get-parameter :oplan-dir)
				      "/lib/")))
  (setq *mon-monitor-table* (make-hash-table :size 10))
  (let (input file-levels)
    (with-open-file
	(in filename :direction :input)
      (setq *mon-modules* (read in))
      (setq file-levels (read in))
      (if (not (equal file-levels *mon-levels*))
	  (cerror "Will continue using *mon-levels*"
		  "Difference in levels between *mon-levels* and whats in ~A"
		  filename))
      (setq *mon-defaults* (read in))
      ;; Initialise all the modules to have a monitor structure filled with the
      ;; defaults.
      (dolist (module *mon-modules*)
	(setf (gethash module *mon-monitor-table*)
	      (make-monitor :level (first *mon-defaults*)
			    :output-device (convert-to-stream
					    :DEFAULT
					    (cdr *mon-defaults*))
			    :print-length *print-length*
			    :print-level *print-level*)))
      (loop
       (setq input (read in nil :eof))
       (if (eql input :eof) (return))
       (let ((module (first input))
	     (level (second input))
	     (device (cddr input))
	     mon)
	 (setq mon (gethash module *mon-monitor-table*))
	 (setf (monitor-level mon) level)
	 (setf (monitor-output-device mon)
	       (convert-to-stream module device)))))))

(defun mon-set-level (module level)
  (let ((mon (gethash module *mon-monitor-table*)))
    (if (and (monitor-p mon) (member level *mon-levels*))
	(setf (monitor-level mon) level))))

;; Returns t if first arg is of a lower level than second arg.
;; Inefficient but done at macro-expansion time which could be compile time
;; hence not a real problem.
(defun level-before-p (lvl1 lvl2)
  (let ((pos1 (position lvl1 *mon-levels*))
	(pos2 (position lvl2 *mon-levels*)))
    (if (null pos1)
	(error "~A is not a recognised level." lvl1))
    (if (null pos2)
	(error "~A is not a recognised level." lvl2))
    (< pos1 pos2)))

(defmacro monitor (module &rest msgs)
  ;; Check at expansion time whether to include monitor code or not.
  (when *mon-include-monitors-p*
    ;; Okay, we have to process the msgs to check that they are all valid,
    ;; and sort them into level order. Then we plant code to first check
    ;; that the monitor is turned on, and then to print out the appropriate
    ;; message.
    (let ((monitor-info-struct (gensym))
	  (output-stream (gensym))
	  level-pos levels raw-lisp-case case-body monitor-level
	  monitor-control format-str-plus-args)
      (setq msgs (sort msgs #'level-before-p :key #'car))
      ;; For each message, build up a case statement that will key on all
      ;; levels before and including the message level.
      ;; Chop off :RAW-LISP and :NONE levels.
      (setq levels (butlast (cdr *mon-levels*)))
      ;; If level = :RAW-LISP then use the lowest level statement as the
      ;; fmt/args to use.
      (dolist (msg msgs)
	(setq monitor-level (car msg))
	(setq monitor-control (cadr msg))
	(if (stringp monitor-control)
	    (setq monitor-control nil format-str-plus-args (cdr msg))
	    (setq format-str-plus-args (cddr msg)))
	(if (eql monitor-level :NONE)
	    (setq levels nil)
	    (progn
	      ;; First time through set the :RAW-LISP case.
	      (unless raw-lisp-case
		(setq raw-lisp-case (cdr msg)))
	      (unless (eql monitor-level :RAW-LISP)
		(setq level-pos
		      (1+ (position monitor-level levels)))
		(push (list (subseq levels 0 level-pos)
			    (unless (member :NO-MODULE-PREFIX monitor-control)
			      (list 'format output-stream "~A:" module))
			    (list* 'format output-stream format-str-plus-args)
			    (unless (member :NO-NEWLINE monitor-control)
			      `(format ,output-stream "~&")))
		      case-body)
		(setq levels (nthcdr level-pos levels))))))
      ;; If levels left over then add to last case (which is at the head
      ;; of the case-body).
      (if levels
	  (rplaca (car case-body) (append levels (caar case-body))))
      
      ;; The transplanted code.
      `(when (>= *dev-debug-level*
	         dev-minimum-monitor-debug-level)
	 (let* ((,monitor-info-struct (gethash ,module *mon-monitor-table*))
	        (*monitor-level* (monitor-level ,monitor-info-struct))
	        (,output-stream (monitor-output-device ,monitor-info-struct))
	        ; (*print-array* (monitor-print-array ,monitor-info-struct))
  	        ; (*print-base* (monitor-print-base ,monitor-info-struct))
	        (*print-case* (monitor-print-case ,monitor-info-struct))
	        ; (*print-circle* (monitor-print-circle ,monitor-info-struct))
	        ; (*print-escape* (monitor-print-escape ,monitor-info-struct))
	        ; (*print-length* (monitor-print-length ,monitor-info-struct))
	        ; (*print-level* (monitor-print-level ,monitor-info-struct))
	        (*print-pretty* (monitor-print-pretty ,monitor-info-struct))
	        ; (*print-radix* (monitor-print-radix ,monitor-info-struct))
	        ; (*print-right-margin* *print-right-margin*)
		)
	   ;; /\/ Commented out some of the less useful bindings in order to
	   ;; reduce overheads.  [jd 26 mar 93]
	   ;; /\/ Commented out most of the rest.  [jd 21 jul 95]
	   (declare (special *monitor-level*))
	   (if (member *monitor-level* '(:ONE-LINER :MINIMUM))
	       (setq *print-right-margin*
		     (mon-calculate-right-margin ,output-stream)))
	   (case *monitor-level*
	     (:OFF)
	     (:RAW-LISP
	      (let ((*print-escape* t)
		    (*print-pretty* nil)
		    (*print-array* t))
		,(unless (and (listp (car raw-lisp-case))
			      (member :NO-MODULE-PREFIX (car raw-lisp-case)))
		   (list 'format output-stream "~A:" module))
		(format ,output-stream ,@(if (listp (car raw-lisp-case))
					     (cdr raw-lisp-case)
					     raw-lisp-case))
		,(unless (and (listp (car raw-lisp-case))
			      (member :NO-NEWLINE (car raw-lisp-case)))
		   (list 'format output-stream "~&"))))
	     ,@(nreverse case-body)))))))

(defun mon-add-user-def-print-fn (type-specifier fn)
  (set-pprint-dispatch type-specifier fn))

;; Returns a list of the current settings. The list has the following form:
;; (... (monitor . level) ...)
(defun mon-current-levels ()
  (let (result)
    (maphash #'(lambda (k v)
		 (push (cons k (monitor-level v)) result))
	     *mon-monitor-table*)
    result))

;;;; ---------------------------- Change History ----------------------------
;;;; (Initials) (Date of change)           (Comment)
;;;;    rbk        30/10/92         Added the ability to specify no module
;;;;                                prefix and no automatic newline.
