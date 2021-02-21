;;;; File: world-sim/world-package.lsp
;;; SCCS Version: %W%
;;; Contains: World package definition for the standalone world simulator
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Sun Oct 15 23:33:14 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; Package definition

(in-package :user)

(defpackage :world

  (:nicknames :world-sim)

  (:use #+:cltl2 :common-lisp
        #-:cltl2 :lisp
	#+lucid  :clos

	:simple-defsystem
	:util
	:time-util
	:dev
	:pseudo-process
	:sim-clock
;	:components
	:ipc
;	:mcc
	:xwindowio
	:initsystem)

  (:import-from :initsystem
     #:*parameter-defaults*
     #:*parameter-env-map*)

  (:export
     #:save-world
     #:compile-world
     #:compile-known-worlds))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
