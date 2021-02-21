;;; -*- Mode: LISP; Package: USER; Syntax: Common-lisp;  -*-
;;; 
;;; File: Probability.lisp
;;; Contains: Probability code from Truckworld Simulator.
;;; Author:  Glen A. Reece <glenr@aisb.edinburgh.ac.uk>
;;; Created: Mon Dec 13 13:34:05 1993
;;; Updated: Mon Jul 10 18:58:20 1995 by Jeff Dalton
;;; Release Version: 1.0
;;; Copyright (c) 1993, 1994 by Glen A. Reece, All rights reserved.

(in-package :world)

;;; Randomize the seed when this file is loaded

(make-random-state t)

(defun process-probability (form &rest args)
  (cond
   
   ((not (listp form))	 (process-val form args))
   
   ((eq (car form) 'PROB-DIST)
    (process-probability-distribution (cdr form) args))
   
   (t
    (process-val form args))))

(defun process-probability-distribution (form args)
  (do* ((possible form (cdr possible))
	(cum-prob (first (first possible)) 
		  (+ cum-prob (if (first (first possible))
				  (first (first possible))
				0)))
	(r (/ (random 10000) 10000.0)))
      ((or (null possible) (< r cum-prob))
       (if possible
	   (process-probability (second (first possible)) args)
	 (progn
	   (cerror "Return NIL" "Probabilities in ~S don't sum to 1" form)
	   nil)))))

(let ((second-normal nil))

  (defun process-normal-distribution (form args)
    
    (if second-normal
	(prog1
	    (+ (first form) (* second-normal (sqrt (second form))))
	  (setf second-normal nil))
      
      (do* ((mean (first form))
	    (deviation (sqrt (second form)))
	    (fac)
	    (u1 (- (random 2.0) 1.0) (- (random 2.0) 1.0) )
	    (u2 (- (random 2.0) 1.0) (- (random 2.0) 1.0))
	    (r (+ (* u1 u1) (* u2 u2)) (+ (* u1 u1) (* u2 u2))))
	  ((and (< r 1.0) (> r 0.0))
	   
	   (setf fac (sqrt (* -2.0 (/ (log r) r))))
	   
	   (setf second-normal (* u1 fac))
	   (+ mean (* u2 fac deviation))))))
  )

(defun process-val (form args)
  (cond
   ((not (listp form)) (process-valu form args))

   ((eq (first form) 'PROB-VALUES)
    (mapcar #'(lambda (x) (process-valu x args)) (cdr form)))
   (t (process-valu form args))))

(defun process-valu (form args)
  (cond
   ((not (listp form)) form)
    
   ((eq (car form) 'DIST-MEAN-VAR)
    (process-normal-distribution (cdr form) args))
   
   (t form)))

;;; Given a probability (e.g., 0.20) the time-to-next function
;;; calculates the number of intervals it will be until the next
;;; occurance.

(defun time-to-next (probability)
  (let ((count 1))
    (loop (when (< (random 1.0) probability)
	    (return count))
	  (incf count))))

;;; End
