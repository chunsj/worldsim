;;;; File: parser-kit-pack.lsp
;;; SCCS Version: %W%
;;; Contains: Package definition for a recursive-descent parser kit
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 27 March 1993
;;; Updated: Sun Oct 16 02:01:43 1994 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; Parser-Kit -- an interface to the TF compiler parsing framework.

(in-package :oplan)

(defpackage #:parser-kit

  (:use #+:cltl2 :common-lisp
        #-:cltl2 :lisp)

  (:use #:util)

  (:export
   
    ;; Scanner interface
    ;; /\/ Maybe don't export *token*?
    #:token #:next-token #:*token* #:*end-token*
    #:with-token-generator #:*token-generator*
    #:push-token

    ;; Compiler-interface
    #:*syntax-error-reporter*
    #:*recovering*
    #:*error-count*
    #:test-compile
    #:list-tokens
    
    ;; Parsing procedures
    #:syntax-error
    #:token-mismatch-error
    #:token-set-error
    #:must-be      #:must-satisfy     #:must-be-member
    #:recover-at   #:recover-when
    #:skip-to      #:skip-until
    #:token-is     #:token-satisfies  #:token-case
    #:one-or-more  #:zero-or-more     #:*blocking-tokens*)

  )


