;;;; File: defsys.lsp
;;; SCCS Version: %W%
;;; Contains: Simple defsystem
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: March 1992
;;; Updated: Mon Jul 10 16:09:33 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1991, 1992, 1993, 1994 AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :user)			;only initially

;;;; Operations on systems

;;; (COMPILE-SYSTEM system-name &KEY ...)
;;; (LOAD-SYSTEM system-name &KEY ...)
;;; (CLEAN-SYSTEM system-name &KEY ...)

;;; The operation is performed for the named system and for all
;;; systems it directly or indirectly :requires.  Required systems
;;; are processed before systems that require them.  "Clean" means
;;; to delete object files.

;;; System names should be symbols and are looked up using EQ.
;;; However, the top-level operations on systems (e.g. COMPILE-SYSTEM)
;;; will try STRING= if they find no EQ match, so that it matters less
;;; which package is current.  An error is signalled if more than one
;;; system name matched.  FIND-SYSTEM also follows these rules.

;;; Recompilation or reloading occur for a file F in system S if F,
;;; or a file that F requires, has changed since the last time F was
;;; compiled or loaded (respectively).  "A file that F requires"
;;; includes the files listed after :requires in the specification
;;; of F (in the definition of S) plus all files in all :required-
;;; systems of S.  However, a file that F requires counts only if
;;; it contains relevant definition types.  The definition types
;;; are given by the :defines slot of the file.  This is discussed
;;; further below.

;;; Keyword parameters, in terms of the opposites of the defaults:

;;;   :TEST T           -- report actions without performing them.
;;;   :RECURSIVE NIL    -- don't perform the operation on required systems.
;;;   :IGNORE-DEPENDS T -- ignore required files and systems when
;;;                        determining whether to recompile or reload.

;;; :RECURSIVE NIL does not mean that _no_ operation is performed.
;;; For instance (COMPILE-SYSTEM 'S :RECURSIVE NIL) will load S's
;;; required systems, so that S can be compiled, but won't compile them.

;;; The :requires relation between systems forms a directed acyclic graph.
;;; A textual description of the graph can be obtained by calling

;;;   (PRINT-SYSTEM-TREE root-system-name)


;;;; System definitions

;;; DEFSYSTEM can be used to define systems, modules, and other logical
;;; groupings of files.  Each such grouping is defined by a separate,
;;; top-level call to DEFSYSTEM, and DEFSYSTEM makes no distinction
;;; between different grouping types.

;;; Defsystem syntax: (DEFSYSTEM (system-name system-option*) file-spec*)

;;; System options:

;;;   :REQUIRED-SYSTEMS (system-name*)
;;;   :DIRECTORY string
;;;   :DO-AFTER-LOAD form

;;; File spec syntax: (name file-option*)

;;; File options:

;;;   :REQUIRES (file-name*)
;;;   :DEFINES (definition-type*)       ; defaults to (:EVERYTHING)
;;;   :COMPILER function-name           ; defaults to COMPILE-FILE
;;;   :LOADER function-name             ; defaults to LOAD
;;;   :DO-AFTER-LOAD form

;;; The :compiler can be nil for files that should not be compiled.

;;; File :SOURCE-NAME and :OBJECT-NAME can be specified expolicitly, but
;;; it's better to use a consistent naming system so it's not necessary.

;;; N.B.:

;;; System names should be symbols; file names should be symbols or strings.
;;; Strings are safer, because a symbol might conflict with a package export.

;;; Defsystem assumes a Unix filename syntax and that most names are lower
;;; case.  When symbols are used a file names, they're converted to lower
;;; case strings (see name-or-defsys), rather than keeping whatever case(s)
;;; they had internally.  Moreover, file names are often manipulated directly
;;; as strings.  In principle, this is less portable than using pathnames;
;;; but in practice it often wins because CL implementations still disagree
;;; about some pathname operations (eg, whether merge-pathnames does
;;; a relative merge).

;;; A file should be in at most one system.

;;; It is possible to indicate that a file contains (only) certain
;;; types of definitions by providing a list of definition types for
;;; its :defines slot.  Only definitions that make a difference to
;;; other files need to be considered.  (So, e.g., defvars for variables
;;; used only within the file that defines them don't matter.)

;;; At present, the only case that makes any difference to how
;;; defsystem behaves is when the :defines list contains only the
;;; single keyword :functions.  In this case it's assumed that when
;;; the file changes, files that :require it do not have to be
;;; recompiled.

;;; For checking purposes only, there is a list of acceptable definition
;;; types, *known-definition-types*.  Feel free to add to this list.
;;; Eventually it may become clear what types are actually useful, and
;;; then defsystem can go further in taking them into account.


(defpackage :simple-defsystem
  (:use #+:cltl2 :common-lisp
	#-:cltl2 :lisp)
  (:shadow :defsystem
	   :compile-system	:load-system		:clean-system
	   :find-system		:system-files		:system-load-date
	   :print-system-tree
	   :file-name
	   :*system-base-directory*
	   :*all-systems*	:*loaded-systems*
	   :*source-type*	:*object-type*)
  (:export :defsystem
	   :compile-system	:load-system		:clean-system
	   :find-system		:system-files		:system-load-date
	   :print-system-tree
	   :file-name
	   :*system-base-directory*
	   :*all-systems*	:*loaded-systems*
	   :*source-type*	:*object-type*))

(in-package :simple-defsystem)


;;; Parameters

;;; The *system-base-directory is prefixed to system :directory names.
;;; It should be nil or a string.  Nil is more or less equivalent to
;;; "." and to "./".

(defparameter *system-base-directory* nil)


;;; Implementation-dependent defintions

(defvar *source-type* "lsp")		;could be part of system definitions

(defvar *object-type*
  (or #+kcl                 "o"
      #+poplog              "null"
      #+excl                "fasl"
      #+(and :CMU :sparc)   "sparcf"
      #+(and Lucid solaris) "s2bin"
      #+(and Lucid sparc)   "sbin"
      #+:wcl                "o"
      (error "No *object-type* defined.")))

#+kcl
(defmacro with-compilation-unit (options &body forms)
  (declare (ignore options))
  `(progn . ,forms))

#+Lucid
(defmacro with-compilation-unit (options &body forms)
  `(lcl:with-compiler-options ,options
     (lcl:with-deferred-warnings . ,forms)))


;;; System definitions

;;; For a system S, we'll use Req(S) for the list of all systems directly
;;; required by S and Req*(S) for the list of all systems directly or
;;; indirectly required.  So Req* is the transitive closure of Req.

;;; When a top-level operation (such as load-system or compile-system)
;;; is performed on S, the following slots are given the correct values
;;; for S and Req*(S) by set-up-system:

;;;  * All-required-systems contains the structs of all systems required
;;;    directly or indirectly by this one, ordered so that a system always
;;;    appears before any systems that require it.  The order in which
;;;    the the top-level operation processes systems is that of
;;;       (system-all-required-systems S).

;;;  * The system-modification-date is the file-source-date of the most
;;;    recently modified file in the system, ie, the date of the most
;;;    recent edit of any file in the system.

;;;  * The system-redefiniton-date is like the system-modification-date
;;;    but considers only changes that imply that files that depend
;;;    on this system should be recompiled.

;;;  * System-dependent-files-p indicates whether any systems that
;;;    require this one (whether directly or indirectly) contain any
;;;    files.  If not, then this system needn't be loaded when we're
;;;    compiling.  [We could do better by seeing whether any files
;;;    need to be compiled, rather than seeing if there are no files,
;;;    but we don't (yet?) do so.]

(defvar *all-systems* '())

(defvar *loaded-systems* '())

(defstruct (mark (:conc-name nil))	;for topological sort (tsort)
  mark)

(defstruct (system (:include mark) (:print-function print-system))
  name				;as given to defsystem
  files				;file structs
  required-systems		;names, and only direct requirements
  all-required-systems		;structs, direct and indirect
  dependent-files-p		;true or false
  do-after-load			;a single form
  directory			;a lower-case, Unix-syntax string
  load-date
  compile-date
  modification-date
  redefinition-date)

(defun print-system (sys stream depth)
  (declare (ignore depth))
  (format stream "#<system ~S>" (system-name sys)))


;;; Flexible system lookup

(defun find-system (name &key (if-not-found nil))
  (or (find-exact-system name)
      (let ((possibles
	     (remove name *all-systems*
		     :key #'system-name
		     :test-not #'string=)))
	(cond ((null possibles)
	       (if (eq if-not-found :error)
		   (error "System ~S is not defined." name)
		 nil))
	      ((null (cdr possibles))
	       (car possibles))
	      (t (error "System name ~S is ambiguous among ~S."
			name possibles))))))

;;; Exact (EQ) system lookup

(defun find-exact-system (name)
  #+kcl					;member is faster in KCL
  (car (member name *all-systems* :key #'system-name :test #'eq))
  #-kcl
  (find name *all-systems* :key #'system-name :test #'eq))

;;; Exact lookup and error if not found

(defun find-system-else-error (name)
  (or (find-exact-system name)
      (error "System ~S is not defined." name)))


(defmacro defsystem ((system &key required-systems
			          do-after-load
			          directory)
		     &rest file-specs)
  "(DEFSYSTEM (system-name option*) file-spec*)."
  `(def-system ',system
     ',file-specs
     :required-systems ',required-systems
     :do-after-load ',do-after-load
     :directory ',directory))

(defun def-system (name file-specs
		   &key required-systems do-after-load directory)
  ;; Note that we do not call find-system for the systems we require,
  ;; because they may not be defined yet.
  (let ((sys (find-exact-system name)))
    ;; Ensure that the system exists if it doesn't already.
    (when (null sys)
      (setq sys (make-system :name name))
      (setq *all-systems* (nconc *all-systems* (list sys))))
    ;; Install parameter values.
    (setf (system-modification-date sys)
	  'not-a-system-modification-date)
    (setf (system-required-systems sys)
	  required-systems)
    (setf (system-do-after-load sys)
	  do-after-load)
    (when *system-base-directory*
      (setq directory
	    (concatenate 'string
	      *system-base-directory* "/" (or directory ""))))
    (when directory
      (setf (system-directory sys)
	    (concatenate 'string (name-for-defsys directory) "/")))
    ;; N.B. Must process files after setting directory.
    (setf (system-files sys)
	  (process-system-files sys file-specs))
    sys))

(defun name-for-defsys (name)		;puts name into std form
  (if (stringp name) name (string-downcase name)))


;;; Printing the system tree

;;; A required system is listed under the system that requires it,
;;; but indented a few spaces the the right.  This process is recursive.
;;; "Joins" are systems required by > 1 other system among the systems
;;; being described.  The recursive listing and indenting process stops
;;; at a join.  Instead of listing the subsystems of a join each time
;;; it occurs, the join's name is printed in angle brackets and it is
;;; given its own top-level description.

(defun print-system-tree (root-name)
  (let* ((root (find-system root-name :if-not-found :error))
	 (systems (cons root (reverse (find-all-required-systems root))))
	 (joins nil)
	 (*print-case* :downcase))
    (labels ((directly-required-p (server client)
	       (member (system-name server)
		       (system-required-systems client)
		       :test #'string=))
	     (directly-requiring-systems (server)
	       (remove server systems
		       :test-not #'directly-required-p))
	     (join-p (sys)
	       (> (length (directly-requiring-systems sys)) 1))
	     (walk (sys depth show-children-p)
	       (format t "~&~vT~:[~S~;<~S>~]~%"
		       (1+ (* 3 depth))
		       (and (> depth 0) (member sys joins))
		       (system-name sys))
	       (when show-children-p
		 (dolist (child (find-required-systems sys))
		   (walk child
			 (1+ depth)
			 (not (member child joins)))))
	       (when (= depth 0)
		 (format t "~%"))))
      (setq joins (remove-if-not #'join-p systems))
      (walk root 0 t)
      (dolist (j joins)
	(walk j 0 t))
      (values))))


;;; Generating file structs

(defparameter *known-definition-types*
  '(:functions :macros :variables :classes :structures :types
    :methods :messages :objects :package :everything))

(defstruct (file (:include mark) (:print-function print-file))
  name
  source-name
  object-name
  required-files
  (defines '(:everything))		;list of :functions, :macros, etc.
  source-date				;when last edited
  (compiler 'compile-file)
  (loader 'load)
  load-date
  loaded-version			;source or object name
  do-after-load
  system)

(defun print-file (file stream depth)
  (declare (ignore depth))
  (format stream "#<file ~S in system ~S>"
	  (file-name file)
	  (system-name (file-system file))))

(defun process-system-files (sys file-specs) ; -> list of file structs
  (let ((structs
	 (mapcar #'(lambda (spec) (apply #'process-file-spec spec))
		 file-specs)))
    ;; Post-processing
    (dolist (file structs)
      ;; The lists of required files must be changed to refer to the structs.
      (setf (file-required-files file)
	    (mapcar
	       #'(lambda (r)
		   (let ((rn (name-for-defsys r)))
		     (or (find rn structs :key #'file-name :test #'string=)
			 (error "File ~S requires ~S, but ~S is not ~
                                 a file in system ~S."
				(file-name file) rn rn (system-name sys)))))
	       (file-required-files file)))
      ;; The source and object names have to include the directory
      (when (system-directory sys)
	(setf (file-source-name file)
	      (relative-merge (file-source-name file)
			      (system-directory sys)))
	(setf (file-object-name file)
	      (relative-merge (file-object-name file)
			      (system-directory sys))))
      ;; The file should point back to the system
      (setf (file-system file) sys))
    structs))

(defun relative-merge (name directory)
  (concatenate 'string (namestring directory) (namestring name)))


(defun process-file-spec (name &key (source-name nil)
			            (object-name nil)
			            (requires '())
			            (defines '(:everything))
			            (compiler 'compile-file)
				    (loader 'load)
				    (do-after-load nil))
  ;; N.B. The keyword args to this function determine what keywords
  ;; can be specified for files in system definitions.
  (let ((unknown-types (set-difference defines *known-definition-types*)))
    (when unknown-types
      (cerror "Accept the definition types for this file only."
	      "File ~S had unknown definition types ~S."
	      (name-for-defsys name) unknown-types)))
  (let ((name (name-for-defsys name)))
    (make-file
      :name name
      :source-name
        (or source-name
	    (merge-pathnames (make-pathname :type *source-type*) name))
      :object-name
        (or object-name
	    (merge-pathnames (make-pathname :type *object-type*) name))
      :required-files requires
      :defines        defines
      :compiler       compiler
      :loader         loader
      :do-after-load  do-after-load)))


;;; Operations on systems

;;; A top-level operation, such as compile-system, that will look at
;;; file-source-dates should start by calling set-up-system.  This will
;;; make sure that all relevant file structs contain the current source
;;; (write) date and that all relevant systems have correct values in
;;; their all-required-systems, dependent-files-p, modification-date,
;;; and redefinition-date slots.

;;; Re Pass 2 in set-up-system:
;;; In (reverse req), a system r always appears before the (rr) systems
;;; it requires.  This lets us propagate dependent-files-p by looking
;;; at only the systems r directly requires.  We could have used the
;;; longer all-required-systems lists instead, like this:
;;;
;;;    (dolist (r req)
;;;      (when (system-files r)
;;;        (dolist (rr (system-all-required-systems r))
;;;          (setf (system-dependent-files-p rr) t))))
;;;
;;; That way we can process the systems in any order, but have to
;;; do more work.  The main advantage is that we don't call find-
;;; system-else-error.

(defvar *defsystem-test* nil)
(defvar *ignore-depends* nil)

(defun set-up-system (sys)
  (set-all-required-systems sys)
  (set-current-source-dates sys)
  (let ((req (system-all-required-systems sys)))
    (dolist (r req)				;Pass 1
      (setf (system-dependent-files-p r) nil)
      (set-all-required-systems r)
      (set-current-source-dates r))
    (dolist (r (cons sys (reverse req)))	;Pass 2
      (when (or (system-files r) (system-dependent-files-p r))
	(dolist (rr (system-required-systems r))
	  (setf (system-dependent-files-p (find-system-else-error rr))
		t)))))
  sys)

(defun set-all-required-systems (sys)
  (setf (system-all-required-systems sys)
        (find-all-required-systems sys)))

(defun set-current-source-dates (sys)
  (let ((mod-date 0)
	(redef-date 0))
    (dolist (file (system-files sys))
      (let ((source-date (get-file-source-date file)))
	(setf (file-source-date file) source-date)
	(setq mod-date (max mod-date source-date))
	(unless (equal (file-defines file) '(:functions))
	  (setq redef-date (max redef-date source-date)))))
    (setf (system-modification-date sys)
	  mod-date)
    (setf (system-redefinition-date sys)
	  redef-date)))

;;; Compile-system

(defun compile-system (name &key ((:test *defsystem-test*) *defsystem-test*)
			         ((:ignore-depends *ignore-depends*) nil)
			         (recursive t))
  (let ((sys (find-system name :if-not-found :error))
	(*load-verbose* nil))
    (set-up-system sys)
    (with-compilation-unit ()
      (mapc (if recursive #'compile/load-1-system #'load-1-system)
	    (system-all-required-systems sys))
      (compile-1-system sys))))

(defun compile/load-1-system (system)
  (compile-1-system system)
  (when (system-dependent-files-p system)
    (load-1-system system)))		;for systems that require this one

(defun compile-1-system (system)
  (format t "~&;;; Compiling system ~S.~%" (system-name system))
  (map-system-files system #'defsystem-compile-file)
  (setf (system-compile-date system) (get-universal-time))
  system)

(defun defsystem-compile-file (file)
  (when (file-needs-compilation-p file)
    (format t "~&;;; Compiling ~S.~%" (file-source-name file))
    (unless *defsystem-test*
      (mapc #'defsystem-load-file (find-all-required-files file))
      (unless 
	  (funcall (file-compiler file)
		   (file-source-name file)
		   ;; /\/: Confuses Lucid 4.0 because merge-pathnames does
		   ;; a directory-relative merge!
		   ; :output-file (file-object-name file)
		   )
	(error "Failed to compile ~S." (file-source-name file)))
      ;; Load right after compile.
      (funcall (file-loader file) (file-object-name file))
      (eval (file-do-after-load file))
      (setf (file-loaded-version file) (file-object-name file)
	    (file-load-date file) (get-universal-time)))))


;;; Load-system

(defun load-system (name &key ((:test *defsystem-test*) *defsystem-test*)
			      ((:ignore-depends *ignore-depends*) nil)
			      (recursive t))
  (let ((sys (find-system name :if-not-found :error))
	(*load-verbose* nil))
    (set-up-system sys)
    (when recursive
      (mapc #'load-1-system (system-all-required-systems sys)))
    (load-1-system sys)))

(defun load-1-system (system)
  (format t "~&;;; Loading system ~S.~%" (system-name system))
  (map-system-files system #'defsystem-load-file)
  (unless *defsystem-test*
    (setf (system-load-date system) (get-universal-time))
    (eval (system-do-after-load system))
    (unless (member system *loaded-systems*)
	    (setq *loaded-systems* (nconc *loaded-systems* (list system)))))
  system)

(defun defsystem-load-file (file)
  (when (file-needs-loading-p file)
    (let ((most-recent (most-recent-version file)))
      (format t "~&;;; Loading ~S.~%" (namestring most-recent))
      (unless *defsystem-test*
	(funcall (file-loader file) most-recent)
	(eval (file-do-after-load file))
	(setf (file-loaded-version file) most-recent
	      (file-load-date file) (get-universal-time))))))


;;; Clean-system

(defun clean-system (name &key ((:test *defsystem-test*) *defsystem-test*)
			       (recursive t))
  (let* ((sys (find-system name :if-not-found :error))
	 (req (find-all-required-systems sys)))
    (when recursive
      (mapc #'clean-1-system req))
    (clean-1-system sys)))

(defun clean-1-system (system)
  (format t "~&;;; Cleaning system ~S.~%" (system-name system))
  (map-system-files system #'defsystem-delete-file)
  system)

(defun defsystem-delete-file (file)
  (let ((object (file-object-name file)))
    (when (probe-file object)
      (format t "~&;;; Deleting ~S.~%" (namestring object))
      (unless *defsystem-test*
	(delete-file object)))))


;;; Topological sort and related routines.

(defun map-system-files (system fn)
  (mapc fn (required-file-order (system-files system)))
  nil)

(defun required-file-order (files)
  (tsort files #'file-required-files))

(defun find-all-required-files (file)
  (required-file-order (file-required-files file)))

(defun find-all-required-systems (sys) ; -> system structs
  (tsort (find-required-systems sys)
	 #'find-required-systems))

(defun find-required-systems (sys) ; -> system structs [not recursive]
  (mapcar #'(lambda (required-name)
	      (or (find-exact-system required-name)
		  (error "System ~S, required by ~S, is not defined."
			 required-name (system-name sys))))
	  (system-required-systems sys)))

;;; Topological sort, returning descendants before ancestors.

;;; Each call to tsort constructs unique marks, so that clearing
;;; old marks is not necessary.

(defun tsort (roots children-fn)
  (let ((start-mark (list :start))
	(finish-mark (list :finish))
	(result '()))
    (labels ((walk (items)
	       (dolist (at items)
		 (cond ((eq (mark at) start-mark)
			(error "Cycle involving ~S." at))
		       ((eq (mark at) finish-mark)
			;; already processed
			)
		       (t
			(setf (mark at) start-mark)
			(walk (funcall children-fn at))
			(push at result)
			(setf (mark at) finish-mark))))))
      (walk roots)
      (nreverse result))))


;;; Functions for examining file states, etc.

(defun most-recent-version (file)
  (if (source-newer-p file)
      (file-source-name file)
      (file-object-name file)))

(defun file-compilation-date (file)
  (if (not (probe-file (file-object-name file)))
      0
    (file-object-date file)))

(defun get-file-source-date (file)	;stored in file's source-date slot
  (or (file-write-date (file-source-name file))
      (error "No source ~S." (file-source-name file))))

(defun file-object-date (file)
  (file-write-date (file-object-name file)))

(defun source-newer-p (file)
  (or (not (probe-file (file-object-name file)))
      (> (file-source-date file) (file-object-date file))))

(defun file-needs-compilation-p (file)
  (and (file-compiler file)
       (or (source-newer-p file)
	   (and (not *ignore-depends*)
		(let ((compilation-date (file-compilation-date file)))
		  (or (some #'(lambda (required-system)
				(> (system-redefinition-date required-system)
				   compilation-date))
			    (system-all-required-systems (file-system file)))
		      (some #'(lambda (required)
				(and (not (equal (file-defines required)
						 '(:functions)))
				     (> (file-source-date required)
					compilation-date)))
			    (find-all-required-files file))))))))

(defun file-needs-loading-p (file)
  ;; /\/ If a required file or system has changed, perhaps we should
  ;; always load the source (perhaps a macro has been redefined)
  ;; /\/ Do we need to reload if a required file has merely
  ;; been reloaded?  If so, we can get away with checking only
  ;; the load-dates of the required files (and systems).
  (or (null (file-load-date file))
      (> (file-source-date file) (file-load-date file))
      (and (not *ignore-depends*)
	   (some #'(lambda (required-sys)
		     (> (system-modification-date required-sys)
			(file-load-date file)))
		 (system-all-required-systems (file-system file))))
      (and (not *ignore-depends*)
	   (some #'(lambda (required)
		     (> (file-source-date required) (file-load-date file)))
		 (find-all-required-files file)))))

;;; End
