;;;; File: lucid-conditions.lsp
;;; SCCS Version: %W%
;;; Contains: Exports for some Lucid's condition system
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: June 1994
;;; Updated: Mon Jun 13 16:19:03 1994 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; At present, the exports match those of support/kcl-conditions.lsp,
;;; but not all of the exported names have have been defined by LCL.

(in-package :util)


;;;; Imports

;; Condition types
(import '(lcl:condition lcl:simple-condition lcl:serious-condition))
(import '(lisp:error lcl:simple-error))
(import '(lcl:warning lcl:simple-warning))

;; Defining form
(import '(lcl:define-condition))

;; Constructor
(import '(lcl:make-condition))

;; Operations on condition objects
; No condition-p, condition-format-string, or condition-format-args
; although such things as simple-condition-format-string exist.

(defun condition-p (x) (typep x 'condition))

;; Condition slots
; (import '(lcl:format-string lcl:format-arguments))

;; Signalling operations
(import '(lcl:signal))

(defun signal-error (datum &rest arguments)
  (apply #'error datum arguments))

;; Condition handling
(import '(lcl:handler-bind lcl:handler-case lcl:ignore-errors))

;; Restarts
; /\/: No restart support


;;;; Exports

;; Condition types
(export '(condition simple-condition serious-condition))
(export '(error simple-error))
(export '(warning simple-warning))

;; Defining form
(export '(define-condition))		;N.B.: a very limited version

;; Constructor
(export '(make-condition))

;; Operations on condition objects
(export '(condition-p condition-format-string condition-format-arguments))

;; Condition slots
; (export '(format-string format-arguments))

;; Signalling operations
(export '(signal signal-error))

;; Condition handling
(export '(handler-bind handler-case ignore-errors))

;; Restarts
; /\/: No restart support

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
