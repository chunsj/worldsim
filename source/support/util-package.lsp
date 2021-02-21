;;;; File: util-package.lsp
;;; SCCS Version: %W%
;;; Contains: Definition of the UTIL package.
;;; Author: Jeff Dalton
;;; Created: February 1993
;;; Updated: Sun Oct  1 00:56:22 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1993
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; This file defines the UTIL package and (most of) its exports.

;;; Definitions are added to the UTIL package by a number of different
;;; files.  Anything that is not specific to planning and that does not
;;; have a natural home anywhere else tends to end up in UTIL.

;;; Most things in UTIL do not depend on anything from other packages,
;;; but that's not always so.  In some cases there are dependencies in
;;; both directions.  For instance, pop-gensym in is UTIL and uses
;;; context-layering; context-layering has its own package and uses
;;; some things from UTIL.

(in-package :util)

(shadowing-import '(simple-defsystem:*source-type*
		    simple-defsystem:*object-type*))

;; Macros and other compile-time defs
(export '(defun-inline defun-export defmacro-export))
(export '(label while until))
(export '(with-working-directory))
(export '(with-unix-process-io))
(export '(deletef removef ensuref))
(export '(letf* letf))
(export '(definit initialize-variable-group))
(export '(*infinity* infinity infinitep))
(export '(+-or-bust >-or-bust <-or-bust))
(export '(define-struct-extension))
(export '(defstruct-export import-struct import-slot-names))

;; Parameter DB
(export '(get-parameter set-parameter parameter-set-p parameter-alist))

;; Modes
(export '(flip-mode opposite-mode))

;; Strings
(export '(concat-name concat-string big-string-concat))
(export '(string->keyword string->int int->string))

;; Lists / sequences
(export '(car-if-consp cdr-if-consp length=1 length>1 list-beginning))
(export '(last-element last-elt))
(export '(replace-sublist))
(export '(take drop take-while drop-while take-until drop-until))
(export '(set-eql disjoint-sets-p stable-set-difference))
(export '(interleave transpose-list flatten-one-level))
(export '(for-adjacent-elements))
(export '(walk-tree))
(export '(fix-member fix-delete))
(export '(remove-1-eq delete-1-eq))
(export '(make-tconc tconc lconc tconc-contents))
(export '(recons remapcar))
(export '(list-lessp))
(export '(equivalence-classes group-by-numeric-key))
(export '(rotate-list chunk-list))
(export '(find-best max-value))
(export '(equal-with-1-1-renames))
(export '(canonical-description-order recursive-alphabetic-lessp))

;; Graphs
(export '(tsort find-longest-path-lengths))

;; Functions
(export '(partial1 compose2 wrap-1-arg-memoizer))

;; Hash-tables
(export '(hash-table-alist filtered-hash-table-alist))
(export '(hash-table-keys hash-table-values))
(export '(hash-table->function))

;; A-lists
(export '(alist->function lookup))

;; Files and I/O
(export '(find-all-files))
(export '(load-init-file))
(export '(load-most-recent find-most-recent))
(export '(with-working-directory))	;a macro
(export '(file->list stream->list stream->lines))
(export '(string->list))
(export '(make-null-input-stream make-null-output-stream))
(export '(make-null-io-stream))
(export '(temp-filename generate-unique-filename))
(export '(ask-for-line ask-if))
(export '(hit-return-to-continue))
(export '(big-menu-request))


;;; Implementation-specific exports

(export '(argc argv exit-lisp))
(export '(getenv system))
(export '(working-directory-pathname change-working-directory))
(export '(menu-request))
(export '(unix-process-io unix-process-finish))
(export '(run-xterm-for-io terminate-xterms-if-necessary))
(export '(still-running-p))
(export '(break-args))
(export '(structurep map-structure))


;;; Pop-gensym exports

(export '(pop-gensym make-pop-gensymtable clear-pop-gensymtable))


;;; End
