;;;; File: world-sim/world-parameters.lsp
;;; SCCS Version: %W%
;;; Contains: WorldSim parameters that are global variables
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: 15 May 1995
;;; Updated: Mon Jul 10 19:19:47 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1995, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :world)

(defparameter *default-world-definition* "micro-exec")

(defparameter *show-hidden-events* nil
  "True to show scheduled events even if their hidden-p slot is true.")

;;; The Exogenous Event Manager is off by default.  It is automatically
;;; turned off if an EEM-script is loaded should it already be on.

(defparameter *EEM-on* nil) 

;;; Where an interval is 10 minutes (600 seconds).
(defparameter *EEM-probability-of-occurance-in-an-interval* 0.40)

(defparameter *exogenous-event-distribution*
  '(PROB-DIST
    (1.00 no-event)))

;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
