;;; -*- Mode: LISP; Package: USER; Syntax: Common-lisp;  -*-
;;;
;;; File    : Pacifica-NEO-Scenario.lisp
;;; Contains: Pacifica customization code for Pacifica SSNEO
;;; Created : Mon Dec 13 10:45:35 1993
;;; Updated : Tue Jul 19 13:13:42 1994
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

;;;=====================================================================
;;;                    Sensor Event Handlers
;;;=====================================================================

;;;******************************
;;; SENSOR: GT-sensor
;;;******************************

(defmessage (:world :gt-sensor) (args)
  (let* ((res (first args))
	 (res-object (car (get-world-object res)))
	 (result nil))
    
    ;;; Now send each of the object-slot values back to
    ;;; the REA via KS-WORLD...

    (if (not (null res-object))
	(progn
	  (display-history "(S) Sensing: ~s with GT-Sensor..." res)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name res-object)))
			  (setq result
				(cons (list slot-name res val) result))))
		  (class-slot-names res-object))))
    (send-to-exec :world
		  (list 'sensor
			'gt-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor gt-sensor ,res))
			nil))))

;;;******************************
;;; SENSOR: ACT-sensor
;;;******************************

(defmessage (:world :act-sensor) (args)
  (let* ((res (first args))
	 (res-object (car (get-world-object res)))
	 (result nil))

    ;;; Now send each of the object-slot values back to
    ;;; the REA via KS-WORLD...
    
    (if (not (null res-object))
	(progn
	  (display-history "(S) Sensing: ~s with ACT-Sensor..." res)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name res-object)))
			  (setq result
				(cons (list slot-name res val) result))))
		  (class-slot-names res-object))))
    
    (send-to-exec :world
		  (list 'sensor
			'act-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor act-sensor ,res))
			nil))))
    

;;;******************************
;;; SENSOR: APT-sensor
;;;******************************

(defmessage (:world :apt-sensor) (args)
  (let* ((res (first args))
	 (res-object (car (get-world-object res)))
	 (result nil))

    ;;; Now send each of the object-slot values back to
    ;;; the REA via KS-WORLD...
    
    (if (not (null res-object))
	(progn
	  (display-history "(S) Sensing: ~s with APT-Sensor..." res)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name res-object)))
			  (setq result
				(cons (list slot-name res val) result))))
		  (class-slot-names res-object))))
    (send-to-exec :world
		  (list 'sensor
			'apt-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor apt-sensor ,res))
			nil))))

;;;******************************
;;; SENSOR: AH-sensor
;;;******************************

(defmessage (:world :ah-sensor) (args)
  (let* ((res (first args))
	 (res-object (car (get-world-object res)))
	 (result nil))

    (if (not (null res-object))
	(progn
	  (display-history "(S) Sensing: ~s with AH-Sensor..." res)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name res-object)))
			  (setq result
				(cons (list slot-name res val) result))))
		  (class-slot-names res-object))))
    (send-to-exec :world
		  (list 'sensor
			'ah-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor ah-sensor ,res))
			nil))))

;;;******************************
;;; SENSOR: MH-sensor
;;;******************************

(defmessage (:world :mh-sensor) (args)
  (let* ((res (first args))
	 (res-object (car (get-world-object res)))
	 (result nil))

    (if (not (null res-object))
	(progn
	  (display-history "(S) Sensing: ~s with MH-Sensor..." res)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name res-object)))
			  (setq result
				(cons (list slot-name res val) result))))
		  (class-slot-names res-object))))
    (send-to-exec :world
		  (list 'sensor
			'mh-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor mh-sensor ,res))
			nil))))

;;;******************************
;;; SENSOR: FSS-sensor
;;;******************************

(defmessage (:world :fss-sensor) (args)
  (let* ((res (first args))
	 (res-object (car (get-world-object res)))
	 (result nil))

    (if (not (null res-object))
	(progn
	  (display-history "(S) Sensing: ~s with FSS-Sensor..." res)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name res-object)))
			  (setq result
				(cons (list slot-name res val) result))))
		  (class-slot-names res-object))))
    (send-to-exec :world
		  (list 'sensor
			'fss-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor fss-sensor ,res))
			nil))))


;;;******************************
;;; SENSOR: CITY-sensor
;;;******************************

(defmessage (:world :city-sensor) (args)
  (let* ((city-name (first args))
	 (city-object
	  (car
	   (remove nil
		   (mapcar #'(lambda (object)
			       (if (eq (class-name (class-of object)) 'city)
				   object))
			   (get-world-object city-name)))))
	 (result nil))

    ;;; Now send each of the object-slot values back to
    ;;; the REA via KS-WORLD...
    
    (if (not (null city-object))
	(progn
	  (display-history "(S) Sensing: ~s with City-Sensor..." city-name)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name city-object)))
			  (setq result
				(cons (list slot-name city-name val) result))))
		  (class-slot-names city-object))))
    
    (send-to-exec :world
		  (list 'sensor
			'city-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor city-sensor ,city-name))
			nil))))

;;;******************************
;;; SENSOR: ROAD-sensor
;;;******************************

(defmessage (:world :road-sensor) (args)
  (let* ((road-name (first args))
	 (road-object (car (get-world-object road-name)))
	 (result nil))

    ;;; Now send each of the object-slot values back to
    ;;; the REA via KS-WORLD...
    
    (if (not (null road-object))
	(progn
	  (display-history "(S) Sensing: ~s with Road-Sensor..." road-name)
	  (mapcar #'(lambda (slot-name)
		      (let ((val (funcall slot-name road-object)))
			  (setq result
				(cons (list slot-name road-name val) result))))
		  (class-slot-names road-object))))
    
    (send-to-exec :world
		  (list 'sensor
			'road-sensor
			(reverse result))
		  nil)
    (send-to-exec :world
		  (list 'effects
			`((sensor road-sensor ,road-name))
			nil))))


;;;=====================================================================
;;;                     World Event Handlers
;;;=====================================================================


;;;******************************
;;; ACTION: Taxi
;;;******************************

(defmessage (:world :taxi) (args)
  (let ((resource (first args)))
    (display-history "Started: (Taxi ~a)" resource)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 300 180))))     ; 2 to 8 minutes
      :description
      (format nil "Finish: Taxi ~s" resource)
      :action 'taxi
      :args (list resource)
      :succeed-fun 'taxi-success
      :fail-fun 'taxi-failure))))

(defun taxi-success (action)
 (let ((res (first (world-event-args action))))
  (display-history "Completed: (Taxi ~a)" res)
  (send-to-exec :world
              (list 'effects
		    `((parked-at-gate ,res no)
		      (res-status ,res runway-holding))
		    nil))))

;;;******************************
;;; ACTION: Tower-Clearance
;;;******************************

(defmessage (:world :tower-clearance) (args)
  (block tc-top
    (let* ((res (first args))
	   (res-obj (car (get-world-object res)))
	   (city-obj
	    (car (remove nil
			 (mapcar #'(lambda (object)
				     (if (eq (class-name (class-of object))
					     'city)
					 object))
				 (get-world-object (loc res-obj)))))))

      (if (null (airport city-obj))
	  (progn
	    (display-history "(I) No tower, ~s cleared for takeoff..." res)
	    (send-to-exec :world
			  (list 'effects
				`((clearance ,res yes))
				nil))
	    (return-from tc-top t)))
    
      (display-history "Started: (Tower-Clearance ~a)" res)
      (schedule-world-event
       (make-world-event
	:due-time (+ (current-simulated-time)
		     (round (process-probability
			     '(DIST-MEAN-VAR 120 60))))     ; 1 to 3 minutes
		   
	:description
	(format nil "Finished: Tower-Clearance ~s" res)
	:action 'tower-clearance
	:args (list res)
	:succeed-fun 'tower-clearance-success
	:fail-fun 'tower-clearance-failure)))))

(defun tower-clearance-success (action)
   (let* ((res (first (world-event-args action)))
	  (res-obj (car (get-world-object res))))
     (display-history "Completed: (Tower-Clearance ~a)" res)

     (setf (tower-clearance res-obj) 'yes)
     
     (send-to-exec :world
		   (list 'effects
			 `((clearance ,res yes))
			 nil))))

;;;******************************
;;; ACTION: Fly
;;;******************************

;;; NOTE: 
;;;       We also need to calculate fuel consumption here!
;;;

(defmessage (:world :fly) (args)
  (declare (special *world-ground-transports*))  
  (block fly-top
    (let* ((res (first args))
	   (from (second args))
	   (to (third args))
	   (route (get-world-object to))
	   (duration nil)
	   (res-object (car (get-world-object res))))

      ;;; Check for reasons why we couldn't possible fly and
      ;;; perform the appropriate action if such a reason is
      ;;; discovered...

      (if (eq 'blown (tire-status res-object))
	  (progn
	    (funcall 'fly-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (eq 'bad (mech-status res-object))
	  (progn
	    (funcall 'fly-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (eq 'unavailable (status res-object))
	  (progn
	    (funcall 'fly-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (<= (fuel res-object) 0)
	  (progn
	    (setf (failure-reason res-object) 'no-fuel)
	    (funcall 'fly-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (and
	   (< (fuel res-object) (* *air-fuel-threshold*
				   (fuel-capacity res-object)))
	   (> (fuel res-object) 0))
	  (progn
	    (setf (failure-reason res-object) 'low-fuel-gt)
	    (funcall 'fly-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

    (display-history "Started: (Fly ~a from ~a to ~a)" res from to)

    (mapcar #'(lambda (object)
		(if (eq (class-name (class-of object)) 'air-route)
		      (setf (using-route res-object) object)))
	    route)

    (if (null (using-route res-object))
	(progn
	  (setq route (get-world-object from))
	  (mapcar #'(lambda (object)
		      (if (eq (class-name (class-of object)) 'air-route)
			    (setf (using-route res-object) object)))
		  route)))

    ;;; Similar calculation to that of the Drive ACTION...
    
    (setq duration (round (* 3600 (/ (distance (using-route res-object))
				     (avg-speed res-object)))))
    
    (setf (world-event-struct res-object)
	  (schedule-world-event
	   (make-world-event
	    :due-time (+ (current-simulated-time) duration)
	    :start-time (current-simulated-time)
	    :description
	    (format nil "Finish: Fly ~s" res)
	    :action 'fly
	    :args (list res from to)
	    :succeed-fun 'fly-success
	    :fail-fun 'fly-failure)))

    (case (cargo-type res-object)
      (GTs-ONLY
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) 'ACT-cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-ground-transports*))
      (MHs-ONLY
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) 'ACT-cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-medical-helicopters*))
      (ALL-TRANSPORTS
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) 'ACT-cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-ground-transports*)
       
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) 'ACT-cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-medical-helicopters*)))
	  
    (send-to-exec :world
		  (list 'effects
			`((res-status ,res in-flight)
			  (at ,res geo-location)
			  (clearance ,res no))
			nil))

    (setf (status res-object) 'in-flight)
    (setf (loc res-object) 'geo-location)
    (setf (tower-clearance res-object) 'no))))


(defun fly-success (action)
  (declare (special *world-ground-transports*))  
  (let* ((args (world-event-args action))
	 (res (first args))
	 (from (second args))
	 (to (third args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Completed: (Fly ~a from ~a to ~a)"
		     res from to)

    (setf (world-event-struct res-obj) nil)
    (setf (loc res-obj) to)
    (send-to-exec :world
		  (list 'effects
			`((at ,res ,to))
			nil))

    (case (cargo-type res-obj)
      (PASSENGERS)
      (GTs-ONLY
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ,to))
					 nil))))
	       *world-ground-transports*))
      (MHs-ONLY
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ,to))
					 nil))))
	       *world-medical-helicopters*))
      (ALL-TRANSPORTS
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ,to))
					 nil))))
	       *world-ground-transports*)
       
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (send-to-exec :world
				   (list 'effects
					 `((at ,res-name ,to))
					 nil))))
	       *world-medical-helicopters*)))))




;;; A NO-WORLD-EVENT failure function is used for the case when some
;;; exogenous event is to take place in the World and the resource
;;; is not presently being used (which means that there is no
;;; world-event-structure to get information from).

(defun fly-failure-no-world-event (action-args)
  (let ((res (first action-args))
	(from (second action-args))
	(to (third action-args))
	(start-time (fourth action-args)))

    (fail-world-event (make-world-event
		       :action 'fly
		       :start-time start-time
		       :args (list res from to)
		       :fail-fun 'fly-failure))))
   
(defun fly-failure (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (from (second args))
	  (to (third args))
	  (*print-case* :capitalize)
	  (time-now (current-simulated-time))
	  (start-time (world-event-start-time action))
	  (res-object (car (get-world-object res)))
	  (speed (avg-speed res-object))
	  (location (loc res-object))
	  (distance-traveled nil)
	  (reason (failure-reason res-object)))

     (setf (world-event-struct res-object) nil)
     (setf (status res-object) 'unavailable)

     (setq distance-traveled (round
			      (* speed (/ (- time-now start-time) 3600))))
    
     (if (null reason)
	 (progn
	   (if (equal 'blown (tire-status res-object))
	       (setq reason 'burst-plane-tire))
	   (if (equal 'bad (mech-status res-object))
	       (setq reason 'hydraulics))
	   (if (equal 'unavailable (status res-object))
	       (setq reason 'resource-not-available))))
          
     (display-history "FAILURE: (Fly ~a from ~a to ~a)" res from to)
     (display-history "(I) The reason for the failure is ~s" reason)
     
     (case reason
       (burst-plane-tire
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (status-tires ,res blown))
			    'burst-plane-tire)))
       (captured-by-terrorists
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res captured)
			      (at ,res unknown))
			    'plane-captured-by-terrorists)))
       (hole-in-tank
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (fuel-level ,res 0))
			    'hole-in-tank)))
       (hydraulics
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (status-mech ,res bad))
			    'hydraulics)))
       (resource-not-available
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location))
			    'resource-not-available)))
       (no-fuel-plane
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (fuel-level ,res 0))
			    'no-fuel-plane)))
       (low-fuel-plane
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (fuel-level ,res ,(fuel res-object)))
			    'low-fuel-plane)))
       (otherwise
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location))
			    reason))))))

;;;******************************
;;; ACTION: Fly-Helicopter
;;;******************************

;;; NOTE: 
;;;       We also need to calculate fuel consumption here!
;;;

(defmessage (:world :fly-helicopter) (args)
  (declare (special *world-ground-transports*))  
  (block fly-top
    (let* ((res (first args))
	   (from (second args))
	   (to (third args))
	   (route (get-world-object to))
	   (duration nil)
	   (res-object (car (get-world-object res))))

      ;;; Check for reasons why we couldn't possibly fly and
      ;;; perform the appropriate action if such a reason is
      ;;; discovered...

      (if (eq 'bad (mech-status res-object))
	  (progn
	    (funcall 'fly-helicopter-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (eq 'unavailable (status res-object))
	  (progn
	    (funcall 'fly-helicopter-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (<= (fuel res-object) 0)
	  (progn
	    (setf (failure-reason res-object) 'no-fuel)
	    (funcall 'fly-helicopter-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

      (if (and
	   (< (fuel res-object) (* *air-fuel-threshold*
				   (fuel-capacity res-object)))
	   (> (fuel res-object) 0))
	  (progn
	    (setf (failure-reason res-object) 'low-fuel-gt)
	    (funcall 'fly-helicopter-failure-no-world-event
		     (list res from to (current-simulated-time)))
	    (return-from fly-top t)))

    (display-history "Started: (Fly-Helicopter ~a from ~a to ~a)" res from to)

    (mapcar #'(lambda (object)
                (if (and (eq (class-name (class-of object)) 'air-route)
			 (eq (route-type object) 'helicopter))
                      (setf (using-route res-object) object)))
            route)

    (if (null (using-route res-object))
        (progn
          (setq route (get-world-object from))
          (mapcar #'(lambda (object)
                      (if (and (eq (class-name (class-of object)) 'air-route)
			       (eq (route-type object) 'helicopter))
                            (setf (using-route res-object) object)))
                  route)))

    ;;; Similar calculation to that of the Drive ACTION...
    
    (setq duration (round (* 3600 (/ (distance (using-route res-object))
                                     (avg-speed res-object)))))


    (setf (world-event-struct res-object)
	  (schedule-world-event
	   (make-world-event
	    :due-time (+ (current-simulated-time) duration)
	    :start-time (current-simulated-time)
	    :description
	    (format nil "Finish: Fly-Helicopter ~s" res)
	    :action 'fly
	    :args (list res from to)
	    :succeed-fun 'fly-helicopter-success
	    :fail-fun 'fly-helicopter-failure)))

    (send-to-exec :world
		  (list 'effects
			`((res-status ,res in-flight)
			  (at ,res geo-location)
			  (clearance ,res no))
			nil))

    (setf (status res-object) 'in-flight)
    (setf (loc res-object) 'geo-location)
    (setf (tower-clearance res-object) 'no))))


(defun fly-helicopter-success (action)
  (declare (special *world-ground-transports*))  
  (let* ((args (world-event-args action))
	 (res (first args))
	 (from (second args))
	 (to (third args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Completed: (Fly-Helicopter ~a from ~a to ~a)"
		     res from to)

    (setf (world-event-struct res-obj) nil)
    (setf (loc res-obj) to)
    (send-to-exec :world
		  (list 'effects
			`((at ,res ,to))
			nil))))



;;; A NO-WORLD-EVENT failure function is used for the case when some
;;; exogenous event is to take place in the World and the resource
;;; is not presently being used (which means that there is no
;;; action-event-structure to get information from).

(defun fly-helicopter-failure-no-world-event (action-args)
  (let ((res (first action-args))
	(from (second action-args))
	(to (third action-args))
	(start-time (fourth action-args)))

    (fail-world-event (make-world-event
		       :action 'fly
		       :start-time start-time
		       :args (list res from to)
		       :fail-fun 'fly-failure))))
   
(defun fly-helicopter-failure (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (from (second args))
	  (to (third args))
	  (*print-case* :capitalize)
	  (time-now (current-simulated-time))
	  (start-time (world-event-start-time action))
	  (res-object (car (get-world-object res)))
	  (speed (avg-speed res-object))
	  (location (loc res-object))
	  (distance-traveled nil)
	  (reason (failure-reason res-object)))

     (setf (world-event-struct res-object) nil)
     (setf (status res-object) 'unavailable)

     (setq distance-traveled (round
			      (* speed (/ (- time-now start-time) 3600))))
    
     (if (null reason)
	 (progn
	   (if (equal 'bad (mech-status res-object))
	       (setq reason 'hydraulics))
	   (if (equal 'unavailable (status res-object))
	       (setq reason 'resource-not-available))))
          
     (display-history "FAILURE: (Fly-Helicopter ~a from ~a to ~a)" res from to)
     (display-history "(I) The reason for the failure is ~s" reason)
     
     (case reason
       (captured-by-terrorists
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res captured)
			      (at ,res unknown))
			    'plane-captured-by-terrorists)))
       (hole-in-tank
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (fuel-level ,res 0))
			    'hole-in-tank)))
       (roter-blade
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (status-mech ,res bad))
			    'roter-blade)))
       (resource-not-available
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location))
			    'resource-not-available)))
       (no-fuel-helicopter
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (fuel-level ,res 0))
			    'no-fuel-helicopter)))
       (low-fuel-helicopter
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location)
			      (fuel-level ,res ,(fuel res-object)))
			    'low-fuel-helicopter)))
       (otherwise
	(send-to-exec :failure
		      (list 'effects
			    `((res-status ,res unavailable)
			      (at ,res ,location))
			    reason))))))

;;;******************************
;;; ACTION: Land-Plane
;;;******************************

(defmessage (:world :land-plane) (args)
  (let ((res (first args))
	(to (second args)))
    
    (display-history "Started: (Land-Plane ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 480 180))))     ; 5 to 11 minutes
      :description
      (format nil "Finish: Land-Plane ~s" res)
      :action 'land-plane
      :args (list res to)
      :succeed-fun 'land-plane-success
      :fail-fun 'land-plane-failure))))

(defun land-plane-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (to (second args))
	  (res-obj (car (get-world-object res))))

     (send-to-exec :world
		   (list 'effects
			 `((res-status ,res on-ground))
			 nil))

     (setf (status res-obj) 'on-ground)
     (setf (in-country res-obj)
	   (if (or (eq to 'delta)
		   (eq to 'calypso))
	       t
	       nil))

     ;;; Update the amount of fuel the resource has remaining.

     (setf (fuel res-obj)
	   (round (- (fuel res-obj)
		     (/ (distance (using-route res-obj))
			(fuel-rate res-obj)))))
     
     (setf (using-route res-obj) nil)
     
     (display-history "Completed: (Land-Plane ~a at ~a)" res to)))

;;;******************************
;;; ACTION: Hover-and-Land
;;;******************************

(defmessage (:world :hover-and-land) (args)
  (let ((res (first args))
	(to (second args)))
    
    (display-history "Started: (Hover-and-Land ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 480 180))))     ; 5 to 11 minutes
      :description
      (format nil "Finish: Hover-and-Land ~s" res)
      :action 'hover-and-land
      :args (list res to)
      :succeed-fun 'hover-and-land-success
      :fail-fun 'hover-and-land-failure))))

(defun hover-and-land-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (to (second args))
	  (res-obj (car (get-world-object res))))

     (setf (status res-obj) 'on-ground)
     
     (send-to-exec :world
		   (list 'effects
			 `((res-status ,res on-ground))
			 nil))

     ;;; Update the amount of fuel the resource has remaining.

     (setf (fuel res-obj)
	   (round (- (fuel res-obj)
		     (/ (distance (using-route res-obj))
			(fuel-rate res-obj)))))
     
     (setf (using-route res-obj) nil)
     
     (display-history "Completed: (Hover-and-Land ~a at ~a)" res to)))

;;;******************************
;;; ACTION: Taxi-To-Gate
;;;******************************

(defmessage (:world :taxi-to-gate) (args)
  (let ((res (first args)))
    (display-history "Started: (Taxi-To-Gate ~a)" res)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 300 180))))     ; 2 to 8 minutes
		   
      :description
      (format nil "Finish: Taxi-To-Gate ~s" res)
      :action 'taxi-to-gate
      :args (list res)
      :succeed-fun 'taxi-to-gate-success
      :fail-fun 'taxi-to-gate-failure))))

(defun taxi-to-gate-success (action)
   (let* ((res (first (world-event-args action)))
	  (res-obj (car (get-world-object res))))
     (display-history "Completed: (Taxi-To-Gate ~a)" res)

     (setf (at-gate res-obj) 'yes)
     
     (send-to-exec :world
		   (list 'effects
			 `((parked-at-gate ,res yes))
			 nil))))

(defun taxi-to-gate-failure (action)
 (let ((res (first (world-event-args action))))
  (display-history "FAILURE: (Taxi-To-Gate ~a)" res)
  (display-history "Reason: Too much ground traffic.")
  (send-to-exec :world
              `((parked-at-gate ,res no))
              'ground-trafic)))

;;;******************************
;;; ACTION: Unload-Plane
;;;******************************

(defmessage (:world :unload-plane) (args)
  (let* ((res (first args))
	 (to (second args))
	 (res-obj (car (get-world-object res))))
	 
    (display-history "Started: (Unload-Plane ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (offload res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 300 300))))   ; 0 to 10 minutes
      :description
      (format nil "Finish: Unload-Plane ~s" res)
      :action 'unload-plane
      :args (list res to)
      :succeed-fun 'unload-plane-success
      :fail-fun 'unload-plane-failure))))

(defun unload-plane-success (action)
  (declare (special *world-ground-transports*))
  (let* ((args (world-event-args action))
	 (res (first args))
	 (to (second args))
	 (res-obj (car (get-world-object res)))
	 (city-obj
	  (car (remove nil
		       (mapcar #'(lambda (object)
				   (if (eq (class-name (class-of object))
					   'city)
				       object))
			       (get-world-object to))))))

    (case (cargo-type res-obj)
      (PASSENGERS
       (setf (status res-obj) 'available)
       (setf (status-load res-obj) 'empty)
       (setf (last-cargo res-obj) (cargo-type res-obj))
       (setf (cargo-type res-obj) nil)
       (setf (loc res-obj) to)

       (send-to-exec :world
		     (list 'effects
			   `((res-status ,res available)
			     (load-status ,res empty))
			   nil))
    
       (if (< 0 (passengers res-obj))
	   (progn
	     (let ((people (passengers res-obj)))
	       (setf (passengers res-obj) 0)
	       (setf (num-nationals city-obj) (+ (num-nationals city-obj)
						 people))
	       (send-to-exec :world
			     (list 'effects
				   `((nationals ,to ,(num-nationals city-obj)))
				   nil))))))
       
      (GTs-ONLY
       (setf (status res-obj) 'available)
       (setf (status-load res-obj) 'empty)
       (setf (last-cargo res-obj) (cargo-type res-obj))
       (setf (cargo-type res-obj) nil)
       (setf (loc res-obj) to)
	  
       (send-to-exec :world
		     (list 'effects
			   `((res-status ,res available)
			     (load-status ,res empty))
			   nil))
	  
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (setf (in-country obj)
			   (if (or (eq to 'delta)
				   (eq to 'calypso))
			       t
			       nil))
		     (setf (status obj) 'available)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name available))
					 nil))))
	       *world-ground-transports*))

      (MHs-ONLY
       (setf (status res-obj) 'available)
       (setf (status-load res-obj) 'empty)
       (setf (last-cargo res-obj) (cargo-type res-obj))
       (setf (cargo-type res-obj) nil)
       (setf (loc res-obj) to)

       (send-to-exec :world
		     (list 'effects
			   `((res-status ,res available)
			     (load-status ,res empty))
			   nil))
	  
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (setf (in-country obj)
			   (if (or (eq to 'delta)
				   (eq to 'calypso))
			       t
			       nil))
		     (setf (status obj) 'available)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name available))
					 nil))))
	       *world-medical-helicopters*))
       
      (ALL-TRANSPORTS
       (setf (status res-obj) 'available)
       (setf (status-load res-obj) 'empty)
       (setf (last-cargo res-obj) (cargo-type res-obj))
       (setf (cargo-type res-obj) nil)
       (setf (loc res-obj) to)

       (send-to-exec :world
		     (list 'effects
			   `((res-status ,res available)
			     (load-status ,res empty))
			   nil))
       
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (setf (in-country obj)
			   (if (or (eq to 'delta)
				   (eq to 'calypso))
			       t
			       nil))
		     (setf (status obj) 'available)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name available))
					 nil))))
	       *world-medical-helicopters*)
       
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (loc obj) to)
		     (setf (in-country obj)
			   (if (or (eq to 'delta)
				   (eq to 'calypso))
			       t
			       nil))
		     (setf (status obj) 'available)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name available))
					 nil))))
	       *world-ground-transports*)))
    
    (display-history "Completed: (Unload-Plane ~a at ~a)" res to)))


;;;******************************
;;; ACTION: Refuel
;;;******************************

;;; NOTE: Need to calculate the time it will take to fill the tank
;;;       of the resource based upon how empty it is and its refuel
;;;       time.  The success-fun will have to retun the correct level
;;;       for the resource.
;;;
;;;       Will this have to be split into ground-fill-up and air-fill-up?

(defmessage (:world :refuel) (args)
  (let* ((res (first args))
	 (res-obj (car (get-world-object res))))
    (display-history "Started: (Refuel ~a)" res)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (refuel res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 120 120))))     ; 0 to 4 minutes
		   
      :description
      (format nil "Finish:  Refuel ~s" res)
      :action 'refuel
      :args (list res)
      :succeed-fun 'refuel-success
      :fail-fun 'refuel-failure))))
    

(defun refuel-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (res-obj (car (get-world-object res)))
	  (max (fuel-capacity res-obj)))

     (setf (fuel res-obj) (fuel-capacity res-obj))
     (setf (status res-obj) 'available)
     
     (display-history "Completed: (Refuel ~a)" res)
     (display-history "(I) ~s now has ~s gallons of fuel."
		      res (fuel res-obj))
     
     (send-to-exec :world
		   (list 'effects
			 `((res-status ,res available)
			   (fuel-level ,res ,(fuel res-obj)))
			 nil))))

;;;******************************
;;; ACTION: Drive
;;;******************************

(defmessage (:world :drive) (args)
  (block drive-top
    (let* ((res (first args))
	   (from (second args))
	   (to (third args))
	   (road (fourth args))
	   (speed (fifth args))
	   (duration nil)
	   (res-obj (car (get-world-object res)))
	   (road-obj (car (get-world-object road)))
	   (bridge? (bridge-xing road-obj))
	   (bridge-status nil)
	   (actual-speed (calculate-ground-speed res road)))

      ;;; Check for reasons why we couldn't possible drive and
      ;;; perform the appropriate action if such a reason is
      ;;; discovered...

      (if (eq 'blown (tire-status res-obj))
	  (progn
	    (funcall 'drive-failure-no-world-event
		     (list res from to road actual-speed
			   (current-simulated-time)))
	    (return-from drive-top t)))

      (if (eq 'bad (mech-status res-obj))
	  (progn
	    (funcall 'drive-failure-no-world-event
		     (list res from to road actual-speed
			   (current-simulated-time)))
	    (return-from drive-top t)))

      (if (eq 'unavailable (status res-obj))
	  (progn
	    (funcall 'drive-failure-no-world-event
		     (list res from to road actual-speed
			   (current-simulated-time)))
	    (return-from drive-top t)))

      (if (<= (fuel res-obj) 0)
	  (progn
	    (setf (failure-reason res-obj) 'no-fuel-gt)
	    (funcall 'drive-failure-no-world-event
		     (list res from to road actual-speed
			   (current-simulated-time)))
	    (return-from drive-top t)))

      (if (not (null bridge?))
	  (setq bridge-status (status (car (get-world-object bridge?)))))
      
      (if (and (eq 'open (status road-obj))
	       (or (eq 'open bridge-status)
		   (null bridge-status)))
	  (progn
	    (display-history
	     "Started: (Drive ~a from ~a ~%~22,0Tto ~a on ~a at ~a)"
	     res from to road speed)
	    
    ;;;
    ;;;   distance to travel (miles)
    ;;; ------------------------------ * 3600 (sec/hour) = time (seconds)
    ;;;  speed of travel (miles/hour)
    ;;;
    
	    (setq duration
		  (round (* 3600 (/ (distance road-obj) actual-speed))))
		      
	    (if (not (eq speed actual-speed))
		(display-history
		 "(I) Road conditions warrant a speed of ~s m.p.h."
		 actual-speed))
    
	    (setf (world-event-struct res-obj)
		  (schedule-world-event
		   (make-world-event
		    :due-time (+ (current-simulated-time) duration)
		    :start-time (current-simulated-time)
		    :description
		    (format nil "Finish: Drive ~s" res)
		    :action 'drive
		    :args (list res from to road actual-speed)
		    :succeed-fun 'drive-success
		    :fail-fun 'drive-failure)))
	  
	    (send-to-exec :world
			  (list 'effects
				`((res-status ,res driving)
				  (at ,res geo-location))
				nil))

	    (setf (last-loc res-obj) from)
	    (setf (using-road res-obj) road)
	    (setf (status res-obj) 'driving)
	    (setf (loc res-obj) 'geo-location)
	    (setf (speed res-obj) actual-speed)))
      
      (if (eq 'closed bridge-status)
	  (send-to-exec :world
			(list 'effects
			      `((access ,(bridge-xing road-obj) closed))
			      nil)))

      (if (eq 'closed (status road-obj))	    
	  (send-to-exec :world
			(list 'effects
			      `((access ,road closed)))
			nil)))))

(defun drive-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (from (second args))
	  (to (third args))
	  (road (fourth args))
	  (drive-speed (fifth args))
	  (road-obj (car (get-world-object road)))
	  (res-obj (car (get-world-object res)))
	  (cities (end-points road-obj))
	  (end-cities nil))


     (display-history
      "Completed: (Drive ~a from ~a ~%~24,0Tto ~a on ~a at ~a)"
		      res from to road drive-speed)

     ;;; Inform the REA that the resource is in a new location...
     (send-to-exec :world
		   (list 'effects
			 `((at ,res new-location))
			 nil))

     ;;; Figure out where the resoure is...
     (if (< 5 (length args))
	 (setf (loc res-obj) (last-loc res-obj))
	 (progn
	   (setq end-cities (remove from cities))
	   (if (equal end-cities (end-points road-obj))
	       (setf (loc res-obj) (if (> (position from *world-cities*)
					  (position (car cities)
						    *world-cities*))
				       (car cities)
				       (cadr cities)))
	       (setf (loc res-obj) (car end-cities)))))

     (send-to-exec :world
		   (list 'effects
			 `((res-status ,res available)
			   (at ,res ,(loc res-obj)))
			 nil))
     
     ;;; Update the amount of fuel the resource has remaining.

     (setf (fuel res-obj)
	   (round (- (fuel res-obj)
		     (/ (distance road-obj) (fuel-rate res-obj)))))

     (setf (world-event-struct res-obj) nil)
     (setf (using-road res-obj) nil)
     (setf (last-loc res-obj) (loc res-obj))
     (setf (speed res-obj) 0)
     (setf (status res-obj) 'available)))


;;; A NO-WORLD-EVENT failure function is used for the case when some
;;; exogenous event is to take place in the World and the resource
;;; is not presently being used (which means that there is no
;;; action-event-structure to get information from).

(defun drive-failure-no-world-event (action-args)
  (let ((res (first action-args))
	(from (second action-args))
	(to (third action-args))
	(road (fourth action-args))
	(speed (fifth action-args))
	(start-time (sixth action-args)))

    (fail-world-event (make-world-event
		       :action 'drive
		       :start-time start-time
		       :args (list res from to road speed)
		       :fail-fun 'drive-failure))))
   
(defun drive-failure (action)
  (let* ((args (world-event-args action))
	 (*print-case* :capitalize)
	 (time-now (current-simulated-time))
	 (start-time (world-event-start-time action))
	 (res (first args))
	 (from (second args))
	 (to (third args))
	 (road (fourth args))
	 (road-object (car (get-world-object road)))
	 (speed (fifth args))
	 (distance-traveled nil)
	 (res-object (car (get-world-object res)))
	 (location (loc res-object))
	 (reason (failure-reason res-object)))

    (display-history
     "FAILURE: (Drive ~a from ~a ~%~22,0Tto ~a on ~a at ~a)"
     res from to road speed)
    
    (setq distance-traveled (round
			     (* speed (/ (- time-now start-time) 3600))))
    
    (if (null reason)
	(progn
	  (if (equal 'blown (tire-status res-object))
	      (setq reason 'nail-in-tire))
	  (if (equal 'bad (mech-status res-object))
	      (setq reason 'broken-fan-belt))
	  (if (equal 'unavailable (status res-object))
	      (setq reason 'resource-not-available))))

    (setf (speed res-object) 0)
    (setf (world-event-struct res-object) nil)
    (setf (using-road res-object) nil)
    
    (display-history
    "(I) ~s traveled ~s miles of ~s along ~s~%~16,0Tbefore the FAILURE: ~s!"
    res distance-traveled (distance road-object) road reason)
   
   (if (>= distance-traveled (* 0.50 (distance road-object)))
       (setf (closest-city res-object) to)
       (setf (closest-city res-object) from))

   (case reason
     (nail-in-tire
      (setf (status res-object) 'unavailable)      
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res ,(status res-object))
			    (at ,res ,location)
			    (status-tires ,res ,(tire-status res-object)))
			  'nail-in-tire)))
     (bullet-in-tire
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res ,(status res-object))
			    (at ,res ,location)
			    (status-tires ,res ,(tire-status res-object)))
			  'bullet-in-tire)))
     (captured-by-terrorists
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res captured)
			    (at ,res unknown))
			  'captured-by-terrorists)))
     (hole-in-tank
      (setf (status res-object) 'unavailable)      
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res unavailable)
			    (at ,res ,location)
			    (fuel-level ,res 0))
			  'hole-in-tank)))
     (broken-fan-belt
      (setf (status res-object) 'unavailable)      
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res unavailable)
			    (at ,res ,location)
			    (status-mech ,res bad))
			  'broken-fan-belt)))
     (resource-not-available
      (setf (status res-object) 'unavailable)      
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res unavailable)
			    (at ,res ,location))
			  'resource-not-available)))
     (no-fuel-gt
      (setf (status res-object) 'unavailable)
      (setf (failure-reason res-object) nil)      
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res unavailable)
			    (at ,res ,location)
			    (fuel-level ,res 0))
			  'no-fuel-gt)))
     (otherwise
      (send-to-exec :failure
		    (list 'effects
			  `((res-status ,res ,(status res-object))
			    (at ,res ,location))
			  reason))))))
      
    
;;;******************************
;;; ACTION: Load-vehicle
;;;******************************

(defmessage (:world :load-vehicle) (args)
  (let* ((res (first args))
	 (to (second args))
	 (cargo (third args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Started: (Load-Vehicle ~a at ~a with ~a)"
		     res to cargo)
    
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (onload res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 120 120))))    ; 0 to 4 minutes
      :description
      (format nil "Finish:  Load-Vehicle ~s" res)
      :action 'load-vehicle
      :args (list res to cargo)
      :succeed-fun 'load-vehicle-success
      :fail-fun 'load-vehicle-failure))))

    
(defun load-vehicle-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (to (second args))
	  (cargo (third args))
	  (res-obj (car (get-world-object res)))
	  (city-obj
	   (car (remove nil
			(mapcar #'(lambda (object)
				    (if (eq (class-name (class-of object))
					    'city)
					object))
				(get-world-object to))))))

     (case cargo
       (PASSENGERS
	(setf (status-load res-obj) 'loaded)
	(setf (cargo-type res-obj) cargo)
	(setf (passengers res-obj) (num-nationals city-obj))
	(setf (num-nationals city-obj) 0)
     
	(send-to-exec :world
		      (list 'effects
			    `((load-status ,res loaded)
			      (passengers ,res yes)
			      (cargo ,res no)
			      (nationals ,to 0))
			    nil)))
       (CARGO))

     (display-history "Completed: (Load-Vehicle ~a at ~a with ~a)"
		      res to cargo)))
     
;;;******************************
;;; ACTION: Load-Helicopter
;;;******************************

(defmessage (:world :load-helicopter) (args)
  (let* ((res (first args))
	 (to (second args))
	 (cargo (third args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Started: (Load-Helicopter ~a at ~a with ~a)"
		     res to cargo)
    
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (onload res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 120 120))))    ; 0 to 4 minutes
      :description
      (format nil "Finish:  Load-Helicopter ~s" res)
      :action 'load-helicopter
      :args (list res to cargo)
      :succeed-fun 'load-helicopter-success
      :fail-fun 'load-helicopter-failure))))

    
(defun load-helicopter-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (to (second args))
	  (cargo (third args))
	  (res-obj (car (get-world-object res)))
	  (city-obj
	   (car (remove nil
			(mapcar #'(lambda (object)
				    (if (eq (class-name (class-of object))
					    'city)
					object))
				(get-world-object to))))))

     (setf (status-load res-obj) 'loaded)
     (setf (cargo-type res-obj) cargo)

     (case cargo
       (PASSENGERS
	(setf (passengers res-obj) (num-nationals city-obj))
	(setf (num-nationals city-obj) 0)
     
	(send-to-exec :world
		      (list 'effects
			    `((load-status ,res loaded)
			      (cargo ,res passengers)
			      (nationals ,to 0))
			    nil)))
       (MEDICAL-SUPPLIES
	(send-to-exec :world
		      (list 'effects
			    `((load-status ,res loaded)
			      (cargo ,res medical-supplies))
			    nil)))
       (WEAPONS))

     (display-history "Completed: (Load-Helicopter ~a at ~a with ~a)"
		      res to cargo)))
     
;;;******************************
;;; ACTION: Load-cargo
;;;******************************

(defmessage (:world :load-cargo) (args)
  (let* ((res (first args))
	 (to (second args))
	 (cargo (third args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Started: (Load-Cargo ~a at ~a with ~a)"
		     res to cargo)
    
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (onload res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 120 120))))    ; 0 to 4 minutes
      :description
      (format nil "Finish:  Load-Cargo ~s" res)
      :action 'load-cargo
      :args (list res to cargo)
      :succeed-fun 'load-cargo-success
      :fail-fun 'load-cargo-failure))))

;;; NOTE:
;;;
;;;       We need to make sure that the transported resources have
;;;       fuel, good mech-status, and good tire-status.  Otherwise,
;;;       we cannot load them and a refuel or repair will have to
;;;       be initiated by the REA.

(defun load-cargo-success (action)
  (declare (special *world-ground-transports*
		    *world-medical-helicopters*
		    *world-attack-helicopters*))
  (let* ((args (world-event-args action))
	 (res (first args))
	 (to (second args))
	 (cargo (third args))
	 (res-obj (car (get-world-object res))))

    (case cargo
      (GTs-ONLY

       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (in-country obj) nil)
		     (setf (loc obj) 'cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name unavailable)
					   (at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-ground-transports*)
       
       (setf (cargo-type res-obj) cargo)
       (setf (status-load res-obj) 'loaded)
       (setf (status res-obj) 'available)

       (send-to-exec :world
		     (list 'effects
			   `((res-status ,res available)
			     (load-status ,res loaded))
			   nil)))
      (MHs-ONLY

       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (in-country obj) nil)
		     (setf (loc obj) 'cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name unavailable)
					   (at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-medical-helicopters*)

       (setf (cargo-type res-obj) cargo)
       (setf (status-load res-obj) 'loaded)
       (setf (status res-obj) 'available)
	 
	 (send-to-exec :world
		       (list 'effects
			     `((res-status ,res available)
			       (load-status ,res loaded))
			     nil)))
      (ALL-TRANSPORTS
       ;;; ONLY ground-transports and medical-helicopters
       
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (in-country obj) nil)
		     (setf (loc obj) 'cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name unavailable)
					   (at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-ground-transports*)
       
       (mapcar #'(lambda (res-name)
		   (let ((obj (car (get-world-object res-name))))
		     (setf (in-country obj) nil)
		     (setf (loc obj) 'cargo-bay)
		     (setf (status obj) 'unavailable)
		     (send-to-exec :world
				   (list 'effects
					 `((res-status ,res-name unavailable)
					   (at ,res-name ACT-cargo-bay))
					 nil))))
	       *world-medical-helicopters*)
       
       (setf (cargo-type res-obj) cargo)
       (setf (status-load res-obj) 'loaded)
       (setf (status res-obj) 'available)

       (send-to-exec :world
		     (list 'effects
			   `((res-status ,res available)
			     (load-status ,res loaded))
			   nil)))
      (Otherwise
       (error "FAILURE: UNKNOWN cargo type (~S) in load-cargo-success~%"
	      cargo)))
    
    (display-history "Completed: (Load-Cargo ~a at ~a with ~a)"
		     res to cargo)))


;;;******************************
;;; ACTION: Unload-vehicle
;;;******************************

(defmessage (:world :unload-vehicle) (args)
  (let* ((res (first args))
	 (to (second args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Started: (Unload-Vehicle ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (offload res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 120 120))))     ; 0 to 4 minutes
		   
      :description
      (format nil "Finish:  Unload-Vehicle ~s" res)
      :action 'unload-vehicle
      :args (list res to)
      :succeed-fun 'unload-vehicle-success
      :fail-fun 'unload-vehicle-failure))))

    
;;; NOTE: 
;;;
;;;       Need to put the people in the city and change their
;;;       status in the GT!  Yes, but we need to determine if
;;;       we are unloading the passengers in a city or on a
;;;       road.
;;; 

(defun unload-vehicle-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (to (second args))
	  (res-obj (car (get-world-object res)))
	  (city-obj
	   (car (remove nil
			(mapcar #'(lambda (object)
				    (if (eq (class-name (class-of object))
					    'city)
					object))
				(get-world-object to))))))
	  
     (case (cargo-type res-obj)
       (PASSENGERS
	(setf (status-load res-obj) 'empty)
	(setf (last-cargo res-obj) (cargo-type res-obj))
	(setf (cargo-type res-obj) nil)
	(setf (num-nationals city-obj) (+ (num-nationals city-obj)
					  (passengers res-obj)))
	(setf (passengers res-obj) 0)
	(setf (status res-obj) 'available)
     
	(send-to-exec :world
		      (list 'effects
			    `((res-status ,res available)
			      (load-status ,res empty)
			      (nationals ,to ,(num-nationals city-obj)))
			    nil)))
       (CARGO))
     
     (display-history "Completed: (Unload-Vehicle ~a at ~a)" res to)))
     
;;;******************************
;;; ACTION: Unload-Helicopter
;;;******************************

(defmessage (:world :unload-helicopter) (args)
  (let* ((res (first args))
	 (to (second args))
	 (res-obj (car (get-world-object res))))
    
    (display-history "Started: (Unload-Helicopter ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (offload res-obj)
		   (round (process-probability
			   '(DIST-MEAN-VAR 120 120))))     ; 0 to 4 minutes
		   
      :description
      (format nil "Finish:  Unload-Helicopter ~s" res)
      :action 'unload-helicopter
      :args (list res to)
      :succeed-fun 'unload-helicopter-success
      :fail-fun 'unload-helicopter-failure))))

    
;;; NOTE: 
;;;
;;;       Need to put the people in the city and change their
;;;       status in the GT!  Yes, but we need to determine if
;;;       we are unloading the passengers in a city or on a
;;;       road.
;;; 

(defun unload-helicopter-success (action)
   (let* ((args (world-event-args action))
	  (res (first args))
	  (to (second args))
	  (res-obj (car (get-world-object res)))
	  (city-obj
	   (car (remove nil
			(mapcar #'(lambda (object)
				    (if (eq (class-name (class-of object))
					    'city)
					object))
				(get-world-object to))))))

     (setf (status-load res-obj) 'empty)
     (setf (last-cargo res-obj) (cargo-type res-obj))
     (setf (status res-obj) 'available)

     (case (cargo-type res-obj)
       (PASSENGERS
	(setf (num-nationals city-obj) (+ (num-nationals city-obj)
					  (passengers res-obj)))
	(setf (passengers res-obj) 0)
     
	(send-to-exec :world
		      (list 'effects
			    `((res-status ,res available)
			      (load-status ,res empty)
			      (nationals ,to ,(num-nationals city-obj)))
			    nil)))
       (MEDICAL-SUPPLIES
	(send-to-exec :world
		      (list 'effects
			    `((res-status ,res available)
			      (load-status ,res empty))
			    nil)))
       (WEAPONS))
     
     (setf (cargo-type res-obj) nil)
     (display-history "Completed: (Unload-Helicopter ~a at ~a)" res to)))
     
;;;******************************
;;; ACTION: Pre-Flight
;;;******************************

(defmessage (:world :pre-flight) (args)
  (let ((res (first args)))
    (display-history "Started: (Pre-Flight of ~a)" res)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 300 180))))     ; 2 to 8 minutes
		   
      :description
      (format nil "Finish:  Pre-Flight ~s" res)
      :action 'pre-flight
      :args (list res)
      :succeed-fun 'pre-flight-success
      :fail-fun 'pre-flight-failure))))

    

(defun pre-flight-success (action)
   (let* ((args (world-event-args action))
          (res (first args))
	  (res-obj (car (get-world-object res)))
	  (city-obj
	   (car (remove nil
			(mapcar #'(lambda (object)
				    (if (eq (class-name (class-of object))
					    'city)
					object))
				(get-world-object (loc res-obj)))))))

     (setf (status res-obj) 'boarded)
     (setf (status-load res-obj) 'loaded)
     (setf (cargo-type res-obj) 'passengers)
     (setf (passengers res-obj) (num-nationals city-obj))
     (setf (num-nationals city-obj) 0)
     
     (send-to-exec :world
                   (list 'effects
			 `((res-status ,res boarded)
			   (load-status ,res loaded)
			   (nationals ,(loc res-obj) 0))
			 nil))
     
     (display-history "Completed: (Pre-Flight of ~a)" res)))
     
(defun pre-flight-failure (action)
 (let ((args (world-event-args action))
       (res (first args)))
  (display-history "FAILURE: (Pre-Flight of ~a)" res)
  (display-history "Reason:")
  (send-to-exec :world
		(list 'effects
		      `((res-status ,res available))
		      nil))))


;;;******************************
;;; ACTION: PLANE-TIRE-REPAIR
;;;******************************

(defmessage (:world :plane-tire-repair) (args)
  (let ((res (first args))
        (to (second args)))
    (display-history "Started: (Plane-Tire-Repair on ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 3600 1200))))     ; 40 to 80 minutes
      :description
      (format nil "Finish:  Plane-Tire-Repair ~s" res)
      :action 'plane-tire-repair
      :args (list res to)
      :succeed-fun 'plane-tire-repair-success
      :fail-fun 'plane-tire-repair-failure))))

(defun plane-tire-repair-success (action)
   (let* ((args (world-event-args action))
          (res (first args))
	  (res-obj (car (get-world-object res)))
          (to (second args)))
     (display-history "Completed: (Plane-Tire-Repair on ~a at ~a)" res to)
     (display-history "(I) Tire Status of ~s is now Okay." res)
     
     (setf (failure-reason res-obj) nil)
     (setf (tire-status res-obj) 'okay)
     (setf (status res-obj) 'available)
     
     (send-to-exec :world
		(list 'effects
		      `((res-status ,res available)
			(status-tires ,res okay))
		      nil))))

;;;******************************
;;; ACTION: GT-TIRE-REPAIR
;;;******************************

(defmessage (:world :gt-tire-repair) (args)
  (let ((res (first args))
        (to (second args)))
    (display-history "Started: (GT-Tire-Repair on ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 1200 1300))))     ; 15 to 25 minutes
      :description
      (format nil "Finish:  GT-Tire-Repair ~s" res)
      :action 'gt-tire-repair
      :args (list res to)
      :succeed-fun 'gt-tire-repair-success
      :fail-fun 'gt-tire-repair-failure))))

(defun gt-tire-repair-success (action)
   (let* ((args (world-event-args action))
          (res (first args))
	  (res-obj (car (get-world-object res)))
          (to (second args)))
     (display-history "Completed: (GT-Tire-Repair on ~a at ~a)" res to)
     (display-history "(I) Tire Status of ~s is now Okay." res)
     
     (setf (failure-reason res-obj) nil)
     (setf (tire-status res-obj) 'okay)
     (setf (status res-obj) 'available)
     
     (send-to-exec :world
		(list 'effects
		      `((res-status ,res available)
			(status-tires ,res okay))
		      nil))))

;;;******************************
;;; ACTION: GT-TIRE-TOW-and-REPAIR
;;;******************************

(defmessage (:world :gt-tire-tow-and-repair) (args)
  (let* ((res (first args))
	 (res-obj (car (get-world-object res))))
    (display-history "Started: (GT-Tire-Tow-and-Repair of ~a to ~a)"
		     res (closest-city res-obj))
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 3600 1200))))     ; 40 to 80 minutes
      :description
      (format nil "Finish:  GT-Tire-Tow-and-Repair ~s" res)
      :action 'gt-tire-tow-and-repair
      :args (list res)
      :succeed-fun 'gt-tire-tow-success
      :fail-fun 'gt-tire-tow-failure))))

(defun gt-tire-tow-success (action)
   (let* ((args (world-event-args action))
          (res (first args))
	  (res-obj (car (get-world-object res))))

     (display-history "Completed: (GT-Tire-Tow-and-Repair of ~a to ~a)"
		      res (closest-city res-obj))
     (display-history "(I) Tire Status of ~s is now Okay." res)
     
     (setf (tire-status res-obj) 'okay)
     (setf (status res-obj) 'available)
     (setf (loc res-obj) (closest-city res-obj))
     (setf (failure-reason res-obj) nil)
     (setf (closest-city res-obj) nil)
     
     (send-to-exec :world
		(list 'effects
		      `((res-status ,res available)
			(at ,res ,(loc res-obj))
			(status-tires ,res okay))
		      nil))))

;;;******************************
;;; ACTION: GT-MECH-REPAIR
;;;******************************

(defmessage (:world :gt-mech-repair) (args)
  (let ((res (first args))
        (to (second args)))
    (display-history "Started: (GT-Mech-Repair on ~a at ~a)" res to)
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 1800 600))))     ; 20 to 40 minutes
      :description
      (format nil "Finish:  GT-Mech-Repair ~s" res)
      :action 'gt-mech-repair
      :args (list res to)
      :succeed-fun 'gt-mech-repair-success
      :fail-fun 'gt-mech-repair-failure))))

(defun gt-mech-repair-success (action)
   (let* ((args (world-event-args action))
          (res (first args))
	  (res-obj (car (get-world-object res)))
          (to (second args)))
     (display-history "Completed: (GT-Mech-Repair on ~a at ~a)" res to)
     (display-history "(I) Mechanical Status of ~s is now Good." res)
     
     (setf (failure-reason res-obj) nil)
     (setf (mech-status res-obj) 'good)
     (setf (status res-obj) 'available)
     
     (send-to-exec :world
		(list 'effects
		      `((res-status ,res available)
			(status-mech ,res good))
		      nil))))

;;;******************************
;;; ACTION: GT-MECH-TOW-and-REPAIR
;;;******************************

(defmessage (:world :gt-mech-tow-and-repair) (args)
  (let* ((res (first args))
	 (res-obj (car (get-world-object res))))
    (display-history "Started: (GT-Mech-Tow-and-Repair of ~a to ~a)"
		     res (closest-city res-obj))
    (schedule-world-event
     (make-world-event
      :due-time (+ (current-simulated-time)
		   (round (process-probability
			   '(DIST-MEAN-VAR 2700 900))))     ; 30 to 60 minutes
      :description
      (format nil "Finish:  GT-Mech-Tow-and-Repair ~s" res)
      :action 'gt-mech-tow-and-repair
      :args (list res)
      :succeed-fun 'gt-mech-tow-success
      :fail-fun 'gt-mech-tow-failure))))

(defun gt-mech-tow-success (action)
   (let* ((args (world-event-args action))
          (res (first args))
	  (res-obj (car (get-world-object res))))

     (display-history "Completed: (GT-Mech-Tow-and-Repair of ~a to ~a)"
		      res (closest-city res-obj))
     (display-history "(I) Mechanical Status of ~s is now Good." res)
     
     (setf (mech-status res-obj) 'good)
     (setf (status res-obj) 'available)
     (setf (loc res-obj) (closest-city res-obj))
     (setf (failure-reason res-obj) nil)
     (setf (closest-city res-obj) nil)
     
     (send-to-exec :world
		(list 'effects
		      `((res-status ,res available)
			(at ,res ,(loc res-obj))
			(status-mech ,res good))
		      nil))))

;;;--------------------------------------------------------------------------
;;;                    END OF FILE: MSG-NEO.lisp
;;;--------------------------------------------------------------------------
