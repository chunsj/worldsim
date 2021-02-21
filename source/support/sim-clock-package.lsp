;;;; File: sim-clock-package.lsp
;;; SCCS Version: %W%
;;; Contains: Package definition for simulated-time clocks.
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: October 1994
;;; Updated: Sun May 14 00:20:22 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :oplan)

(defpackage :sim-clock

  (:nicknames :clock)

  (:use #+:cltl2 :common-lisp
        #-:cltl2 :lisp)
  (:use :util)
  (:use :time-util)
  (:use :pseudo-process)

  (:export

     ;; The clock
     #:*clock*				;binding provided by support/components

     ;; The clock struct
     #:clock
     #:clock-p
     #:make-clock
     #:clock-running-p
     #:clock-base-simulated-time
     #:clock-base-real-time
     #:clock-simulated-seconds-per-second
     #:clock-granularity
     ; #:clock-cached-time
     ; #:clock-cached-time-string

     #:define-a-clock

     #:the-clock-is-running

     #:start-clock
     #:set-clock-granularity
     #:stop-clock
     #:init-clock

     ;; Conversions
     #:simulated->real-seconds
     #:real->simulated-seconds
     #:simulated->universal-time
     #:simulated-time-string
     
     ;; Getting the simulated time
     #:get-simulated-time
     #:current-simulated-time

     ;; Setting the simulated time
     #:set-simulated-time

     ;; Re-exports from pseudo-precess
     #:get-primitive-real-time 
     #:primitive-time-units-per-second
     #:universal->primitive-real-time
     #:primitive-real->universal-time

     )

  )

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
