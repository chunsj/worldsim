;;;; File: util-macros.lsp
;;; SCCS Version: %W%
;;; Contains: Useful macros that don't belong anywhere else
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Sun Oct  1 00:56:00 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1993
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :util)


;;; Inline functions

;;; Using a special macro is neater than writing a proclamation and a
;;; defun, and it makes it easier for us to exploit other ways (such as
;;; defsubst) to get a function call expanded inline when (as in some
;;; Common Lisps) inline proclamations don't suffice.

(defmacro defun-inline (name parameters &body body)
  `(progn
     (proclaim '(inline ,name))
     (defun ,name ,parameters
       .,body)))


;;; Auto-exporting function and macro definitions

(defmacro defun-export (name parameters &body body)
  `(progn
     (export ',name)
     (defun ,name ,parameters . ,body)))

(defmacro defmacro-export (name parameters &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,parameters . ,body)))


;;; LABEL defines a "named LET" that can be called as a local function.
;;;
;;; (LABEL fname ((var init) ...) form ...)
;;;   ==> (LABELS ((fname (var ...) form ...))
;;;         (fname init ...))

(defmacro label (name init-specs &body forms)
  `(labels ((,name ,(mapcar #'car init-specs)
	      ,@forms))
     (,name ,@(mapcar #'cadr init-specs))))


;;; While and until

(defmacro while (test &body forms)
  `(do () ((not ,test)) . ,forms))

(defmacro until (test &body forms)
  `(do () (,test) . ,forms))


;;; Working directory change

(defmacro with-working-directory (namestring &body forms)
  `(let ((.wd. (working-directory-pathname)))
     (unwind-protect
	  (progn (change-working-directory ,namestring)
		 ,@forms)
       (change-working-directory .wd.))))


;;; With-unix-process-io provides an io-stream to a process and
;;; arranges to call wait, or whatever's neccessary, at the end.

(defmacro with-unix-process-io ((stream-var program &rest args) &body forms)
  (let ((stream (gensym))
	(pid (gensym)))
    `(multiple-value-bind (,stream ,pid)
         (unix-process-io ,program ,@args)
       (unwind-protect
	    (with-open-stream (,stream-var ,stream)
	      ,@forms)
	 (unix-process-finish ,pid)))))


;;; Deletef

;;; Since we have the item first, rather than the place, we can't
;;; use define-modify-macro directly.  Hence the -from definitions.

(defmacro deletef (item place &rest other-args)
  `(deletef-from ,place ,item . ,other-args))

(define-modify-macro deletef-from (seq &rest other-args) delete-from)

(defmacro delete-from (place item &rest other-args)
  `(delete ,item ,place . ,other-args))

;;; Removef

(defmacro removef (item place &rest other-args)
  `(removef-from ,place ,item . ,other-args))

(define-modify-macro removef-from (seq &rest other-args) remove-from)

(defmacro remove-from (place item &rest other-args)
  `(remove ,item ,place . ,other-args))


;;; Ensuref is used to get memoized values.  E.g. (ensuref x (make-x))
;;; is equivalent to (or x (setf x (make-x))).  It uses setf methods
;;; to avoid multiple-evaluation, etc.

(defmacro ensuref (place init-form)
  (multiple-value-bind  (temp-vars val-forms store-vars
			 store-form access-form)
        (get-setf-method place)
      (unless (= (length store-vars) 1)
        (error "Ensure can't handle more than one store value in ~S" place))
      ;; could skip binding vars in some cases...
      `(let* ,(mapcar #'list temp-vars val-forms)
	 (or ,access-form
	     (let ((,(car store-vars) ,init-form))
	       ,store-form)))))


;;; letf* and letf from CMU CL.

;;; These macros waste time as opposed to space.  [It said.]

(defmacro letf* (bindings &body body &environment env)
  "Does what one might expect, saving the old values and setting the
  generalized variables to the new values in sequence.  Unwind-protects
  and get-setf-method are used to preserve the semantics one might expect
  in analogy to let*, and the once-only evaluation of subforms."
  (labels ((do-bindings
	    (bindings)
	    (cond ((null bindings) body)
		  (t (multiple-value-bind (dummies vals newval setter getter)
					  (get-setf-method (caar bindings) env)
		       (let ((save (gensym)))
			 `((let* (,@(mapcar #'list dummies vals)
				  (,(car newval) ,(cadar bindings))
				  (,save ,getter))
			     (unwind-protect
			       (progn ,setter
				      ,@(do-bindings (cdr bindings)))
			       (setq ,(car newval) ,save)
			       ,setter)))))))))
    (car (do-bindings bindings))))


(defmacro letf (bindings &body body &environment env)
  "Like letf*, but evaluates all the implicit subforms and new values of all
  the implied setfs before altering any values.  However, the store forms
  (see get-setf-method) must still be evaluated in sequence.  Uses unwind-
  protects to protect the environment."
  (let (temps)
    (labels
      ((do-bindings
	(bindings)
	(cond ((null bindings) body)
	      (t (let ((binding (car bindings)))
		   (multiple-value-bind (dummies vals newval setter getter)
					(get-setf-method (car binding) env)
		     (let ((save (gensym)))
		       (mapcar #'(lambda (a b) (push (list a b) temps))
			       dummies vals) 
		       (push (list save getter) temps)
		       (push (list (car newval) (cadr binding)) temps)
		       `((unwind-protect
			   (progn ,setter
				  ,@(do-bindings (cdr bindings)))
			   (setq ,(car newval) ,save)
			   ,setter)))))))))
      (let ((form (car (do-bindings bindings))))
	`(let* ,(nreverse temps)
	   ,form)))))


;;;; Reinitializing variables in groups

;;; Definit [an experiment /\/]

(defvar *definit-variables* '())

(defmacro get-group-initform (group-name var-name)
  `(getf (get ,var-name 'initforms) ,group-name))

(defmacro definit (group-name var-name initform &optional docstring)
  (check-type group-name symbol)
  (check-type var-name symbol)
  (when docstring
    (check-type docstring string))
  `(progn
     (pushnew ',var-name *definit-variables*)
     (setf (get-group-initform ',group-name ',var-name) ',initform)
     (defvar ,var-name ,initform ,@(when docstring (list docstring)))))

(defun initialize-variable-group (group-name)
  (dolist (v *definit-variables*)
    (let ((initform (get-group-initform group-name v)))
      (when initform
	(set v (eval initform))))))



;;;; Arithmetic with (positive) infinity

;;; /\/: Deprecated: use the version in inf-arithmetic.lsp instead.

(defconstant *infinity* 'infinity) ;/\/ s.b. :inf

(defmacro infinitep (n)
  `(symbolp ,n))	;/\/: should check for the right symbol

(defmacro +-or-bust (one two)
  "Adds one and two together, unless one or both of them are infinity,
   in which case this macro returns infinity."
  `(let ((t1 ,one) (t2 ,two))
     (or (and (or (infinitep t1) (infinitep t2)) *infinity*)
	 (+ t1 t2))))

(defmacro >-or-bust (one two)
  "t if one is greater than two, but checks for the infinity case."
  `(let ((t1 ,one) (t2 ,two))
     (or (and (infinitep t1) (numberp t2))
	 (and (numberp t1) (numberp t2) (> t1 t2)))))

(defmacro <-or-bust (one two)
  "t if one is less than two, but checks for the infinity case."
  `(let ((t1 ,one) (t2 ,two))
     (or (and (numberp t1) (infinitep t2))
         (and (numberp t1) (numberp t2) (< t1 t2)))))


;;;; Defining struct extensions (which contain extra slots)
;;;
;;; Example:
;;;
;;; (define-struct-extension (ne-exec-aux :accessor-prefix ne-
;;;                                       :extension-prefix nex-
;;;                                       :base-accessor ne-exec-slots)
;;;   ...
;;;   (effects '())
;;;   ...)
;;;
;;; This defines a struct called ne-exec-aux.  For that struct, the
;;; :conc-name is nex-.  However, names with that prefix will hardly
;;; ever be used.  Instead, for each slot we'll define accessors with
;;; a different prefix, ne-.
;;;
;;; The idea is that we already have a struct where accessors have
;;; ne- names, and we want to define some extra slots for that struct.
;;; Suppose ne is an instance of that struct.  The definition above
;;; will define (ne-effects ne) == (nex-effects (ne-exec-slots ne)).
;;;
;;; Ne-exect-slots is a function that goes from an instance of the
;;; base struct to an instance of the extension struct.  So it's
;;; an accessor on instances of the base struct, hence the name
;;; :base-accessor.  /\/: A better name can no doubt be found.
;;;

(defmacro define-struct-extension
            ((name &key accessor-prefix extension-prefix base-accessor)
	     &rest extra-slots)
  (unless (and accessor-prefix extension-prefix base-accessor)
    (error "Struct extension ~S is not fully specified." name))
  `(progn
     (defstruct (,name (:conc-name ,extension-prefix))
       ,@extra-slots)
     ,@(mapcan
	  #'(lambda (accessor-name)
	      (let ((accessor
		     (concat-name accessor-prefix accessor-name))
		    (extn-accessor
		     (concat-name extension-prefix accessor-name)))
		(list
		  `(defun ,accessor (obj)
		     (,extn-accessor (,base-accessor obj)))
		  `(defsetf ,accessor (obj) (new-value)
		     `(setf (,',extn-accessor (,',base-accessor ,obj))
		            ,new-value)))))
	  (mapcar #'car-if-consp extra-slots))))



;;;; Defstruct-export

;;; Defstruct-export is like defstruct but exports the type name, the
;;; constructors, the predicate (if any), the copier, all slot names
;;; (for use in :include options), and and all accessor names.

;;; At present there's no way to selectively export some names but
;;; not others.  Ways to do that would include a :private slot option
;;; that would prevent the slot and accessor names from being exported,
;;; and a :private struct option that would include a list of names
;;; to keep private.

;;; We also have import-struct which works like this:

(defmacro import-struct (struct-name &optional (package nil))
  ;; N.B. The args are evaluated, just as in IMPORT.
  `(import (get ,struct-name 'struct-exports)
           ,@(if package (list package) nil)))

;;; SLot-names are not normally imported, because they tend
;;; to use up common names (e.g. position, cost, x) and create
;;; name conflicts.

(defmacro import-slot-names (struct-name &optional (package nil))
  `(import (get ,struct-name 'struct-slot-names)
           ,@(if package (list package) nil)))

(defmacro defstruct-export (struct-spec &rest slot-specs &aux doc-string)

  ;; Minimal syntax check.
  (unless (or (symbolp struct-spec) (symbolp (car-if-consp struct-spec)))
    (error "Invalid export struct spec: ~S." struct-spec))

  ;; Separate doc string from slot specs if it exists.
  (when (stringp (car slot-specs))
    (setq doc-string (car slot-specs)
	  slot-specs (cdr slot-specs)))

  ;; Get various names and options
  (let* ((name (car-if-consp struct-spec))
	 (slot-names (mapcar #'car-if-consp slot-specs))
	 (options (if (consp struct-spec) (cdr struct-spec) nil))
	 (opts (make-defstruct-option-plist name options))
	 (include-name
	  (getf opts :include))
	 (conc-name
	  (if (find :conc-name options :key #'car-if-consp)
	      (getf opts :conc-name)	;may be nil
	    (concat-name name "-")))
	 (constructors
	  (cons (concat-name "MAKE-" name) (getf opts :constructors)))
	 (predicate
	  (or (getf opts :predicate)
	      (and (or (not (getf opts :type)) (getf opts :named))
		   (concat-name name "-P"))))
	 (copier
	  (or (getf opts :copier)
	      (concat-name "COPY-" name))))

    ;; Get inherited slots, if any.  The inherited slots are a
    ;; list of slot names.
    (let* ((inherited-slots
	    (if include-name
		(get include-name 'struct-slot-names)
	      '()))
	   (total-slots
	    (append slot-names inherited-slots))
	   (total-accessors
	    (if conc-name
		(mapcar #'(lambda (slot-name)
			    (concat-name conc-name slot-name))
			total-slots)
	      total-slots))
	   (exports
	    `(,name
	      ,@constructors
	      ,copier
	      ,@(if predicate (list predicate) nil)
	      ;; ,@slot-names
	      ,@(when conc-name
		  total-accessors))))

      ;; Produce macro expansion
      `(progn
	;; Record this struct's slots for any struct that :includes it,
	;; and record the exports so import-struct can work.
	(eval-when (eval compile load)
	   (setf (get ',name 'struct-slot-names) ',total-slots
		 (get ',name 'struct-exports) ',exports))
	(export ',exports)
	(defstruct ,struct-spec
	  ,@(if doc-string (list doc-string) nil)
	  ,@slot-specs)))))

;;; Make-defstruct-option-plist converts the options to an easily used
;;; canonical form.

(defun make-defstruct-option-plist (name options)
  (let ((plist nil))
    (dolist (opt options plist)
      (let ((opt-name (car-if-consp opt)))
	(case opt-name
	  (:constructor
	   (setf (getf plist :constructors)
		 (nconc (getf plist :constructors) (list (cadr opt)))))
	  (:named
	   (setf (getf plist :named) t))
	  (t
	   (if (getf plist opt-name)
	       (error "Two ~S options for export struct ~S" opt-name name)
	     (setf (getf plist opt-name)
		   (cadr opt)))))))))

;;; End
