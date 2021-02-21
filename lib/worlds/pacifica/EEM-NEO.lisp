;;; -*- Mode: LISP; Package: WORLD; Syntax: Common-lisp;  -*-
;;;
;;; File    : Pacifica-Exogenous-Events.lisp
;;; Contains: Exogenous event definitions for Pacifica NEO Scenario.
;;; Created : Mon Dec 13 10:45:35 1993
;;; Updated : Tue Jul 19 13:11:16 1994
;;;
;;; Author  : Glen A. Reece <G.Reece@ed.ac.uk>
;;; Version : 1.00; 19/7/94
;;;
;;; Copyright (C) 1994, Glen A. Reece
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; CONTACT ADDRESS
;;;
;;;	Glen A. Reece
;;;	Department of Artificial Intelligence
;;;	University of Edinburgh
;;;	80 South Bridge (E17)
;;;	Edinburgh EH1 1HN
;;;	United Kingdom
;;;
;;; DESCRIPTION
;;;
;;; BUGS
;;;   Please notify Glen Reece (G.Reece@ed.ac.uk) of any problems or
;;;   suggestions regarding this package.
;;;
;;; ACKNOWLEDGEMENTS
;;;   Thanks to Jeff Dalton for implementation of the underlying
;;;   mechanisms of the simulator!
;;;
;;; HISTORY
;;;   Version 1.00: 19/7/94 (Glen A. Reece)
;;;    - added Simulation Script
;;;
;;;   Version 1.0b: 13/12/93 (Glen A. Reece)
;;;    - added CLOS object definitions
;;;    - added Exogenous Event Manager (EEM)
;;;    - added Snapshot function
;;;
;;;   Version 1.0a: 3/8/93 (Jeff Dalton)
;;;    - implemented underlying mechanisms
;;;
;;;   Original Design Notes: 9/7/93 (Glen A. Reece)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :world)

;;;************************************************
;;; EXOGENOUS-EVENT: Meteorological
;;;************************************************

(defun meteorological-event (&rest args)
  (let ((city (first (car args)))
	(new-climate (second (car args)))
	(world-objs-with-name nil)
	(city-obj nil))

    (if (null city)
	(setq city (choose-random-value *world-cities*)))
    (if (null new-climate)
	(setq new-climate (process-probability *weather-distribution*)))

    (setq world-objs-with-name (get-world-object city))
    (setq city-obj
	  (car
	   (remove nil
		   (mapcar #'(lambda (w-obj)
			       (if (eq (class-name (class-of w-obj))
				       'city)
				   w-obj))
			   world-objs-with-name))))

    (if (not (null city-obj))
	(schedule-world-event
	 (make-world-event
	  :due-time (if args
			(current-simulated-time)
			(+ (current-simulated-time)
			   (round
			    (process-probability
			     *time-to-next-exogenous-meteorological-event*))))
	  :description
	  (format nil "EEM: Meteorological Event")
	  :action 'Meteorological-Event
	  :args (list city-obj new-climate)
	  :succeed-fun 'meteorological-success
	  :fail-fun nil))
	(warn "No world-object found for city of ~s! Ignoring event...~%"
	      city))))
  
(defun meteorological-success (action)
  (let ((*print-case* :capitalize)
	(city-object (first (world-event-args action)))
	(new-climate (second (world-event-args action))))
    
    (display-history "(E) Climate in ~s is now ~s."
		     (name city-object) new-climate)
    
    (setf (climate city-object) new-climate)))

;;;************************************************
;;; EXOGENOUS-EVENT: Resource
;;;************************************************

(defun resource-event (&rest args)
  (let ((resource (first (car args)))
	(event (second (car args)))
	(event-value (third (car args)))
	(res-obj nil))

    (if (null resource)
	(setq resource (choose-random-value *world-resources*)))
    (if (null event)
	(setq event (process-probability *resource-distribution*)))
    
    (setq res-obj (car (get-world-object resource)))

    (if (not (null res-obj))
	(schedule-world-event
	 (make-world-event
	  :due-time (if args
			(current-simulated-time)
			(+ (current-simulated-time)
			   (round (process-probability
				   *time-to-next-exogenous-resource-event*))))
	  :description
	  (format nil "EEM: Resource Event on ~s" resource)
	  :action 'Resource-Event
	  :args (list res-obj event event-value)
	  :succeed-fun 'resource-success
	  :fail-fun nil))
	(warn "No world-object found for resource of ~s! Ignoring event...~%"
	      resource))))
    
(defun resource-success (action)
  (let* ((*print-case* :capitalize)
	 (res-object (first (world-event-args action)))
	 (res-obj (name res-object))
	 (event (second (world-event-args action))))

    (ecase event
      (FUEL
       (let ((event-type (third (world-event-args action)))
	     (current-fuel (fuel res-object)))

	 (if (null event-type)
	     (setq event-type
		   (process-probability *fuel-event-distribution*)))
	     
	 (if (in-country res-object)
	     (format *interact*
		     "(EEM) RESOURCE: Fuel level of ~s is now ~s~%"
		     (name res-object) event-type))
	 
	 (ecase event-type
	   (NO-CHANGE t)
	   (LESS-10-PERCENT
	    (if (in-country res-object)
		(progn
		  (let ((new-level (- current-fuel (* .10 current-fuel))))
		    (setf (fuel res-object) new-level)
		    (display-history "(E) ~s now has ~s gallons of fuel."
				     res-obj (fuel res-object))))))
	   (EMPTY
	    (if (in-country res-object)
		(progn
		  (setf (failure-reason res-object) 'whole-in-tank)
		  (setf (fuel res-object) 0)
		  (display-history "(E) ~s now has 0 gallons of fuel."
				   res-obj)))))))
      (MECHANICAL
       (let ((event-type (third (world-event-args action))))

       	 (if (null event-type)
	     (setq event-type
		   (process-probability *mechanical-event-distribution*)))

	 (if (in-country res-object)
	     (format
	      *interact*
	      "(EEM) RESOURCE: Mechanical status of ~s is now ~s~%"
	      (name res-object) event-type))
	 
	 (ecase event-type
	   (NO-CHANGE t)
	   (GOOD
	    (if (in-country res-object)
		(progn
		  (setf (mech-status res-object) 'good)
		  (display-history "(E) Mechanical status of ~s is now good."
				   res-obj))))
	   (POOR
	    (if (in-country res-object)
		(progn
		  (setf (mech-status res-object) 'poor)
		  (display-history "(E) Mechanical status of ~s is now poor."
				   res-obj))))
	   (BAD
	    (if (in-country res-object)
		(progn
		  (setf (mech-status res-object) 'bad)
		  (setf (failure-reason res-object) 'broken-fan-belt)	    
		  (display-history "(E) Mechanical status of ~s is now bad."
				   res-obj)))))))
      (TIRE
       (let ((event-type (third (world-event-args action))))

	 (if (null event-type)
	     (setq event-type
		   (process-probability *tire-event-distribution*)))

	 (if (in-country res-object)
	     (format *interact*
		     "(EEM) RESOURCE: Tire status of ~s is now ~s~%"
		     (name res-object)
		     (if (eq 'good event-type)
			 'okay
			 event-type)))
	 
	 (ecase event-type
	   (NO-CHANGE t)
	   (GOOD
	    (if (in-country res-object)
		(progn
		  (setf (tire-status res-object) 'okay)
		  (display-history "(E) Tire status of ~s is now okay."
				   res-obj))))
	   (BLOWN
	    (if (in-country res-object)
		(progn
		  (setf (tire-status res-object) 'blown)
		  (setf (failure-reason res-object) 'nail-in-tire)	     
		  (display-history "(E) Tire status of ~s is now blown."
				   res-obj))))))))))

;;;************************************************
;;; EXOGENOUS-EVENT: Terrorist
;;;************************************************
  
(defun terrorist-activity (&rest args)
  (let ((event (first (car args))))
    
    (if (null event)
	(setq event (process-probability *terrorist-distribution*)))
    
    (schedule-world-event
     (make-world-event
      :due-time (if args
		    (current-simulated-time)
		    (+ (current-simulated-time)
		       (round (process-probability
			       *time-to-next-exogenous-terrorist-event*))))
      :description
      (format nil "EEM: Terrorist Event")
      :action 'Terrorist-Event
      :args (list event)
      :succeed-fun 'terrorist-success
      :fail-fun nil))))

(defun terrorist-success (action)
  (let ((*print-case* :capitalize)
	(terrorist-group (choose-random-value *world-terrorist-groups*))
	(event (first (world-event-args action))))
    
    (ecase event
      (ATTACK
       (let* ((res-obj nil)
	      (terrorists-on-road (choose-random-value *world-roads*)))

	 (mapcar #'(lambda (gt)
		     (let ((robj (car (get-world-object gt))))
		       (if (equal terrorists-on-road (using-road robj))
			   (setq res-obj robj))))
		 *world-ground-transports*)

	 (if (and (not (null res-obj)) (in-country res-obj))
	     (ecase (process-probability *attack-event-distribution*)
	       (NO-CHANGE
		(display-history
		 "(E) ~s ATTACKED by ~s." (name res-obj) terrorist-group)
		(display-history
		 "(I) ~s escaped unharmed." (name res-obj)))
	       (POOR-MECH-STATUS
		(if (eq 'poor (mech-status res-obj))
		    (progn
		      (display-history
		       "(E) ~s ATTACKED by ~s~%~17,0TMechanical status of is now ~s." (name res-obj) terrorist-group 'bad)
		      (setf (failure-reason res-obj) 'bullet-in-radiator)
		      (setf (mech-status res-obj) 'bad))
		    (progn
		      (display-history
		       "(E) ~s ATTACKED by ~s~%~17,0TMechanical status of is now ~s." (name res-obj) terrorist-group 'poor)
		      (setf (mech-status res-obj) 'poor))))
	       (BLOWN-TIRE-STATUS
		(display-history
		 "(E) ~s ATTACKED by ~s~%~17,0TTire status is now ~s."
				 (name res-obj) terrorist-group 'blown)
		(setf (status res-obj) 'unavailable)
		(setf (failure-reason res-obj) 'bullet-in-tire)
		(setf (tire-status res-obj) 'blown))))))
      (BRIDGE
       (let* ((bridge (choose-random-value *world-bridges*))
	      (bridge-obj (car (get-world-object bridge))))
	 (display-history "(E) ~s ATTACKED by ~s~%~17,0TStatus is now ~s."
			  bridge terrorist-group 'closed)
	 (setf (status bridge-obj) 'closed)
	 (mapcar #'(lambda (road)
		     (setf (status (car (get-world-object road))) 'closed))
		 (roads bridge-obj))))
      (CAPTURE
       (let* ((res-obj nil)
	      (terrorists-on-road (choose-random-value *world-roads*)))
	 (mapcar #'(lambda (gt)
		     (let ((robj (car (get-world-object gt))))
		       (if (equal terrorists-on-road (using-road robj))
			   (setq res-obj robj))))
		 *world-ground-transports*)
	 (if (and (not (null res-obj)) (in-country res-obj))
	     (progn
	       (display-history	"(E) ~s CAPTURED by ~s!"
				(name res-obj) terrorist-group)
	       (setf (failure-reason res-obj) 'captured-by-terrorists)	       
	       (setf (status res-obj) 'captured)))))

      (ACCESS t))))
  
;;;************************************************
;;; EXOGENOUS-EVENT: Natural-Disaster
;;;************************************************

(defun natural-disaster (&rest args)
  (let ((event (first (car args))))

    (if (null event)
	(setq event (process-probability *disaster-distribution*)))
    
    (schedule-world-event
     (make-world-event
      :due-time (if args
		    (current-simulated-time)
		    (+ (current-simulated-time)
		       (round (process-probability
			       *time-to-next-exogenous-disaster-event*))))
      :description
      (format nil "EEM: Natural Disaster Event")
      :action 'Natural-Disaster-Event
      :args (list event)
      :succeed-fun 'disaster-success
      :fail-fun nil))))

(defun disaster-success (action)
  (let ((event (first (world-event-args action))))
    
    (format *interact* "(EEM) NATURAL DISASTER: Event (~s)~%" event)
    
    (ecase event
      (FLOOD
       (let ((road-object (car (get-world-object 'road-ae))))
	 
	 (setf (status road-object) 'closed)
	 (display-history
	  "(E) Road-AE washed away in flood... Status is now ~s." 'closed)
	 
	 ;;; Now schedule an event to reverse the effects of the flood
	 ;;; activity.  That is, schedule the re-opening of Road-AE...
	 
	 (if (null *reverse-flood-effects*)
	     (setq *reverse-flood-effects*
		   (schedule-world-event
		    (make-world-event
		     :due-time (+ (current-simulated-time)
				  (* 5 3600))   ; in 5 hours
		     :description
		     (format nil "EEM: Reverse flood activity effects")
		     :action 'reverse-effects
		     :args (list 'flood)
		     :succeed-fun 'reverse-effects-success
		     :fail-fun nil)))
	     (progn
	       (let ((orig-due-time nil))
		 
		 (extract-event
		  (world-event-number *reverse-flood-effects*))
		 
		 (setq orig-due-time (world-event-due-time
				      *reverse-flood-effects*))
		 
		 (setq *reverse-flood-effects*
		       (schedule-world-event
			(make-world-event
			 :due-time (+ orig-due-time
				      (* 2 3600))   ; Old due-time + 2 hours
			 :description
			 (format nil "EEM: Reverse flood activity effects")
			 :action 'reverse-effects
			 :args (list 'flood)
			 :succeed-fun 'reverse-effects-success
			 :fail-fun nil))))))))
      
      (VOLCANIC
       (let ((road-object (car (get-world-object 'road-bd)))
	     (on-road nil))
	 (setf (status road-object) 'closed)
	 (display-history "(E) Volcanic activity...Road-BD is now closed.")

	 (mapcar #'(lambda (resource)
		     (let ((res-obj (car (get-world-object resource))))
		       (if (equal (using-road res-obj) 'Road-BD)
			   (push res-obj on-road))))
		 *world-ground-transports*)

	 (if (not (null on-road))
	     (progn
	       (mapcar
		#'(lambda (res-object)
		    (let* ((event-struct
			    (world-event-struct res-object))
			   (res (first (world-event-args event-struct)))
			   (from (second (world-event-args event-struct)))
			   (to (third (world-event-args event-struct)))
			   (road (fourth (world-event-args event-struct)))
			   (speed (fifth (world-event-args event-struct)))
			   (present-speed (speed res-object))
			   (time (current-simulated-time))
			   (start-time
			    (world-event-start-time event-struct))
			   (dist-traveled
			    (round (* (/ (- time start-time) 3600)
				      present-speed))))
		      (if (or (< dist-traveled 35)
			      (> dist-traveled 40))
			  (progn
			    (display-history
			     "(I) Road-BD is blocked, so turning ~s around"
			     (name res-object))
			    (extract-event
			     (world-event-number event-struct))
			    (display-history
			     "Started: (Drive ~a from ~a ~%~22,0Tto ~a on ~a at ~a)" res to from road speed)
			    (schedule-world-event
			     (make-world-event
			      :due-time (+ (current-simulated-time)
					   (round
					    (* 3600
					       (/ dist-traveled
						  present-speed))))
			      :description
			      (format nil "Finish: Drive ~s"
				      (name res-object))
			      :action 'drive
			      :args (append
				     (world-event-args event-struct)
				     '(turned-around))
			      :succeed-fun
			      (world-event-succeed-fun event-struct)
			      :fail-fun
			      (world-event-fail-fun event-struct))))
			  (progn
			    (display-history
			     "(I) ~s was destroyed by lava!" (name res-object))
			    (setf (in-country res-object) nil)
			    (setf (failure-reason res-object)
				  'destroyed-by-lava)
			    (setf (status res-object) 'destroyed)))))
		on-road)))
	 
	 ;;; Now schedule an event to reverse the effects of the volcanic
	 ;;; activity.  That is, schedule the re-opening of Road-BD...

	 (if (null *reverse-volcanic-effects*)
	     (setq *reverse-volcanic-effects*
		   (schedule-world-event
		    (make-world-event
		     :due-time (+ (current-simulated-time)
				  (* 10 3600))   ; in 10 hours
		     :description
		     (format nil "EEM: Reverse volcanic activity effects")
		     :action 'reverse-effects
		     :args (list 'volcanic)
		     :succeed-fun 'reverse-effects-success
		     :fail-fun nil)))
	     (progn
	       (let ((orig-due-time nil))
		 
		 (extract-event
		  (world-event-number *reverse-volcanic-effects*))
		 
		 (setq orig-due-time (world-event-due-time
				      *reverse-volcanic-effects*))
		 
		 (setq *reverse-volcanic-effects*
		       (schedule-world-event
			(make-world-event
			 :due-time (+ orig-due-time
				      (* 3 3600))   ; Old due-time + 3 hours
			 :description
			 (format nil "EEM: Reverse volcanic activity effects")
			 :action 'reverse-effects
			 :args (list 'volcanic)
			 :succeed-fun 'reverse-effects-success
			 :fail-fun nil))))))))
      
      (ROCK-SLIDE
       (let ((road-object (car (get-world-object 'road-cd)))
	     (gt-res nil))
	 (display-history "(E) Rock slide on Road-CD...")
	 (mapcar #'(lambda (resource)
		     (let ((res-obj (car (get-world-object resource))))
		       (if (eq (using-road res-obj) 'road-cd)
			   (setq gt-res res-obj))))
		 *world-ground-transports*)

	 (if (and (not (null gt-res)) (> (speed gt-res) 0))
	     (progn
	       (display-history "(I) Speed of ~s has been reduced 10%."
				(name gt-res))
	       (let ((current-speed (speed gt-res)))
		 (adjust-speed-and-due-time
		  gt-res
		  road-object
		  (round (- current-speed (* 0.10 current-speed))))))))))))
	       
(defun reverse-effects-success (action)
  (declare (special *reverse-volcanic-effects* *reverse-flood-effects*))
  (let* ((args (world-event-args action))
	 (effects (first args)))
    (ecase effects
      (VOLCANIC
       
       ;;; Really what we should do here is see if there are any other
       ;;; volcanic activities on the world-agenda and if so then do
       ;;; NOT open the road!  But, for now we won't worry about this...
       
       (let ((road-object (car (get-world-object 'road-bd))))
	 (setq *reverse-volcanic-effects* nil)
	 (display-history "(I) Road-BD has been re-opened.")
	 (setf (status road-object) 'open)))
      (FLOOD
       (let ((road-object (car (get-world-object 'road-ae))))
	 (setq *reverse-flood-effects* nil)
	 (display-history "(I) Road-AE has been re-opened.")	 
	 (setf (status road-object) 'open))))))
