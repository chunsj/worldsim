;;; -*- Mode: LISP; Package: USER; Syntax: Common-lisp;  -*-
;;;
;;; File    : Pacifica-CLOS-Definitions.lisp
;;; Contains: Contains definitions of objects for the Pacifica NEO
;;;           Scenario.
;;; Created : Mon Dec 13 10:45:35 1993
;;; Updated :
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

(proclaim '(optimize (compilation-speed 0)))

;;;---------------------------------------------------------------------------
;;;       Common Lisp Object System (CLOS) Domain Descriptions
;;;---------------------------------------------------------------------------

;;;****************************************
;;; Class : WORLD
;;;****************************************

(defclass world ()
  ((name :initarg :name :accessor name))
  (:documentation "Generic definition of a world object."))

;;;****************************************
;;; Class : CITY
;;;****************************************

(defclass city (world)
  ((climate :initarg :climate :accessor climate)
   (num-nationals :initarg :num-nationals :initform 0 :accessor num-nationals)
   (status :initarg :status :initform 'open :accessor status)   

   (airport :initarg :airport :initform nil :accessor airport)
   (air-routes :initarg :air-routes :initform nil :accessor air-routes)
   (neighbors :initarg :neighbors :initform nil :accessor neighbors)
   (roads :initarg :roads :initform nil :accessor roads)   
   (seaport :initarg :seaport :initform nil :accessor seaport)) 
  (:documentation "Generic definition of a city object."))
  
;;;****************************************
;;; Class : ROAD
;;;****************************************

(defclass road (world)
  ((condition :initarg :condition :accessor condition)
   (status :initarg :status :accessor status)
   
   (bridge-xing :initarg :bridge-xing :initform nil :accessor bridge-xing)
   (distance :initarg :distance :accessor distance)   
   (end-points :initarg :end-points :initform nil :accessor end-points)
   (road-type :initarg :road-type :accessor road-type))
  (:documentation "Generic definition of a road object."))

;;;****************************************
;;; Class : AIR-ROUTE
;;;****************************************

(defclass air-route (world)
  ((status :initarg :status :accessor status)

   (distance :initarg :distance :accessor distance)
   (end-points :initarg :end-points :initform nil :accessor end-points)
   (route-type :initarg :route-type
	       :initform 'aircraft :accessor route-type))
  (:documentation "Generic definition of a road object."))

;;;****************************************
;;; Class : BRIDGE
;;;****************************************

(defclass bridge (world)
  ((status :initarg :status :accessor status)
   (roads :initarg :roads :accessor roads))
  (:documentation "Generic definition of a bridge object."))

;;;****************************************
;;; Class : AIRPORT
;;;****************************************

(defclass airport (world)
  ((status :initarg :status :accessor status)
   (loc :initarg :loc :accessor loc))
  (:documentation "Generic definition of a airport object."))

;;;****************************************
;;; Class : SEAPORT
;;;****************************************

(defclass seaport (world)
  ((status :initarg :status :accessor status))
  (:documentation "Generic definition of a seaport object."))

;;;****************************************
;;; Class : TERRORIST-GROUP
;;;****************************************

(defclass terrorist-group (world)
  ((loc :initarg :loc :accessor loc))
  (:documentation "Generic definition of a terrorist-group object."))
 
;;;****************************************
;;; Class : GROUND-TRANSPORT
;;;****************************************

(defclass ground-transport (world)
  ((cargo-type :initarg :cargo-type :initform nil :accessor cargo-type)
   (closest-city :initarg :closest-city :initform nil :accessor closest-city)
   (failure-reason :initarg :failure-reason :initform nil
		   :accessor failure-reason)
   (fuel :initarg :fuel :initform 55 :accessor fuel)
   (in-country :initarg :in-country :initform nil :accessor in-country)
   (last-cargo :initarg :last-cargo :initform nil :accessor last-cargo)
   (last-loc :initform nil :accessor last-loc)
   (loc :initarg :loc :initform nil :accessor loc)
   (mech-status :initarg :mech-status :initform 'good :accessor mech-status)
   (passengers :initarg :passengers :initform 0 :accessor passengers)
   (speed :initarg :speed :initform 0 :accessor speed)
   (status :initarg :status :accessor status)
   (status-load :initarg :status-load :initform 'empty
		:accessor status-load)
   (tire-status :initarg :tire-status :initform 'okay :accessor tire-status)
   (using-road :initarg :using-road :initform nil :accessor using-road)
   (world-event-struct :initarg :world-event-struct
		       :initform nil :accessor world-event-struct)
   
   (avg-speed :initarg :avg-speed :initform 40 :accessor avg-speed)
   (fuel-capacity :initarg :fuel-capacity :initform 55 :accessor fuel-capacity)
   (fuel-rate :initarg :fuel-rate :initform 6.3 :accessor fuel-rate)
   (offload :initarg :offload :initform 1200 :accessor offload)   
   (onload :initarg :onload :initform 1200 :accessor onload)
   (range :initarg :range :initform 348 :accessor range)
   (refuel :initarg :refuel :initform 900 :accessor refuel)
   (tires :initarg :tires :initform 'standard :accessor tires)
   (w-cargo :initarg :w-cargo :initform 0 :accessor w-cargo)   
   (wo-cargo :initarg :wo-cargo :initform 50 :accessor wo-cargo))
  (:documentation
   "Ground Transport specific characteristics.
    static and dynamic slots (note that all times are given
    in seconds)."))

;;;****************************************
;;; Class : AIR-CARGO-TRANSPORT
;;;****************************************

(defclass air-cargo-transport (world)
  ((at-gate :initarg :at-gate :initform 'no :accessor at-gate)
   (cargo-type :initarg :cargo-type :initform nil :accessor cargo-type)
   (failure-reason :initarg :failure-reason :initform nil
		   :accessor failure-reason)
   (fuel :initarg :fuel :initform 2013 :accessor fuel)
   (in-country :initarg :in-country :initform nil :accessor in-country)   
   (last-cargo :initarg :last-cargo :initform nil :accessor last-cargo)
   (last-loc :initform nil :accessor last-loc)   
   (loc :initarg :loc :initform nil :accessor loc)
   (mech-status :initarg :mech-status :initform 'good :accessor mech-status)
   
   (world-event-struct :initarg :world-event-struct
		       :initform nil :accessor world-event-struct)   
   (passengers :initarg :passengers :initform 0 :accessor passengers)   
   (status :initarg :status :initform 'available :accessor status)
   (status-load :initarg :status-load :initform 'empty
		:accessor status-load)
   (tire-status :initarg :tire-status :initform 'okay :accessor tire-status)   
   (tower-clearance :initarg :tower-clearance :initform 'no
		    :accessor tower-clearance)
   (using-route :initarg :using-route :initform nil :accessor using-route)
   
   (avg-speed :initarg :avg-speed :initform 436 :accessor avg-speed)
   (fuel-capacity :initarg :fuel-capacity
		  :initform 2013 :accessor fuel-capacity)
   (fuel-rate :initarg :fuel-rate :initform 3.1 :accessor fuel-rate)
   (offload :initarg :offload :initform 5400 :accessor offload)
   (onload :initarg :onload :initform 6300 :accessor onload)
   (range :initarg :range :initform 6238 :accessor range)
   (refuel :initarg :refuel :initform 4500 :accessor refuel)   
   (runway :initarg :runway :initform 9150 :accessor runway)
   (w-cargo :initarg :w-cargo :initform 30 :accessor w-cargo)
   (wo-cargo :initarg :wo-cargo :initform 329 :accessor wo-cargo))
  (:documentation
   "Air Cargo Transport specific characteristics.
    static and dynamic slots (note that all times are given
    in seconds)."))

;;;****************************************
;;; Class : AIR-PASSENGER-TRANSPORT
;;;****************************************

(defclass air-passenger-transport (world)
  ((at-gate :initarg :at-gate :initform 'no :accessor at-gate)
   (cargo-type :initarg :cargo-type :initform nil :accessor cargo-type)
   (failure-reason :initarg :failure-reason :initform nil
		   :accessor failure-reason)
   (fuel :initarg :fuel :initform 2500 :accessor fuel)
   (in-country :initarg :in-country :initform nil :accessor in-country)
   (last-cargo :initarg :last-cargo :initform nil :accessor last-cargo)
   (last-loc :initform nil :accessor last-loc)
   (loc :initarg :loc :initform nil :accessor loc)
   (mech-status :initarg :mech-status :initform 'good :accessor mech-status)
   (world-event-struct :initarg :world-event-struct
		       :initform nil :accessor world-event-struct)   
   (passengers :initarg :passengers :initform 0 :accessor passengers)   
   (status :initarg :status :initform 'available :accessor status)
   (status-load :initarg :status-load :initform 'empty
		:accessor status-load)
   (tire-status :initarg :tire-status :initform 'okay :accessor tire-status)   
   (tower-clearance :initarg :tower-clearance :initform 'no
		    :accessor tower-clearance)
   (using-route :initarg :using-route :initform nil :accessor using-route)   

   (avg-speed :initarg :avg-speed :initform 435 :accessor avg-speed)
   (fuel-capacity :initarg :fuel-capacity :initform 2809
		  :accessor fuel-capacity)
   (fuel-rate :initarg :fuel-rate :initform 3.0 :accessor fuel-rate)
   (offload :initarg :offload :initform 2700 :accessor offload)   
   (onload :initarg :onload :initform 1800 :accessor onload)
   (range :initarg :range :initform 8427 :accessor range)   
   (refuel :initarg :refuel :initform 3600 :accessor refuel)
   (runway :initarg :runway :initform 6831 :accessor runway)
   (w-cargo :initarg :w-cargo :initform 10 :accessor w-cargo)   
   (wo-cargo :initarg :wo-cargo :initform 311 :accessor wo-cargo))
  (:documentation
   "Air Passenger Transport specific characteristics.
    static and dynamic slots (note that all times are given
    in seconds)."))

;;;****************************************
;;; Class : ATTACK-HELICOPTER
;;;****************************************

(defclass attack-helicopter (world)
  ((cargo-type :initarg :cargo-type :initform nil :accessor cargo-type)
   (failure-reason :initarg :failure-reason :initform nil
		   :accessor failure-reason)
   (fuel :initarg :fuel :initform 80 :accessor fuel)
   (in-country :initarg :in-country :initform nil :accessor in-country)
   (last-cargo :initarg :last-cargo :initform nil :accessor last-cargo)
   (last-loc :initform nil :accessor last-loc)
   (loc :initarg :loc :initform nil :accessor loc)
   (mech-status :initarg :mech-status :initform 'good :accessor mech-status)
   (world-event-struct :initarg :world-event-struct
		       :initform nil :accessor world-event-struct)
   (status :initarg :status :initform 'available :accessor status)
   (status-load :initarg :status-load :initform 'empty
		:accessor status-load)
   (tower-clearance :initarg :tower-clearance :initform 'no
		    :accessor tower-clearance)
   (using-route :initarg :using-route :initform nil :accessor using-route)   

   (avg-speed :initarg :avg-speed :initform 80 :accessor avg-speed)
   (fuel-capacity :initarg :fuel-capacity :initform 80
		  :accessor fuel-capacity)
   (fuel-rate :initarg :fuel-rate :initform 5.0 :accessor fuel-rate)
   (offload :initarg :offload :initform 2700 :accessor offload)   
   (onload :initarg :onload :initform 1800 :accessor onload)
   (range :initarg :range :initform 400 :accessor range)   
   (refuel :initarg :refuel :initform 1200 :accessor refuel))
  (:documentation
   "Attack Helicopter specific characteristics.
    static and dynamic slots (note that all times are given
    in seconds)."))

;;;****************************************
;;; Class : MEDICAL-HELICOPTER
;;;****************************************

(defclass medical-helicopter (world)
  ((cargo-type :initarg :cargo-type :initform nil :accessor cargo-type)
   (failure-reason :initarg :failure-reason :initform nil
		   :accessor failure-reason)
   (fuel :initarg :fuel :initform 100 :accessor fuel)
   (in-country :initarg :in-country :initform nil :accessor in-country)
   (last-cargo :initarg :last-cargo :initform nil :accessor last-cargo)
   (last-loc :initform nil :accessor last-loc)
   (loc :initarg :loc :initform nil :accessor loc)
   (mech-status :initarg :mech-status :initform 'good :accessor mech-status)
   (world-event-struct :initarg :world-event-struct
		       :initform nil :accessor world-event-struct)   
   (passengers :initarg :passengers :initform 0 :accessor passengers)   
   (status :initarg :status :initform 'available :accessor status)
   (status-load :initarg :status-load :initform 'empty
		:accessor status-load)
   (tower-clearance :initarg :tower-clearance :initform 'no
		    :accessor tower-clearance)
   (using-route :initarg :using-route :initform nil :accessor using-route)   

   (avg-speed :initarg :avg-speed :initform 60 :accessor avg-speed)
   (fuel-capacity :initarg :fuel-capacity :initform 100
		  :accessor fuel-capacity)
   (fuel-rate :initarg :fuel-rate :initform 4.0 :accessor fuel-rate)
   (offload :initarg :offload :initform 600 :accessor offload)   
   (onload :initarg :onload :initform 600 :accessor onload)
   (range :initarg :range :initform 400 :accessor range)   
   (refuel :initarg :refuel :initform 1800 :accessor refuel)
   (w-cargo :initarg :w-cargo :initform 0 :accessor w-cargo)   
   (wo-cargo :initarg :wo-cargo :initform 10 :accessor wo-cargo))
  (:documentation
   "Medical Helicopter specific characteristics.
    static and dynamic slots (note that all times are given
    in seconds)."))

;;;****************************************
;;; Class : FAST-SEALIFT-SHIP
;;;****************************************

(defclass fast-sealift-ship (world)
  ((at-port :initarg :at-port :initform 'no :accessor at-port)
   (cargo-type :initarg :cargo-type :initform nil :accessor cargo-type)
   (failure-reason :initarg :failure-reason :initform nil
		   :accessor failure-reason)
   (fuel :initarg :fuel :initform 25000 :accessor fuel)
   (in-country :initarg :in-country :initform nil :accessor in-country)
   (last-cargo :initarg :last-cargo :initform nil :accessor last-cargo)
   (last-loc :initform nil :accessor last-loc)
   (loc :initarg :loc :initform nil :accessor loc)
   (mech-status :initarg :mech-status :initform 'good :accessor mech-status)
   (world-event-struct :initarg :world-event-struct
		       :initform nil :accessor world-event-struct)   
   (passengers :initarg :passengers :initform 0 :accessor passengers)   
   (status :initarg :status :initform 'available :accessor status)
   (status-load :initarg :status-load :initform 'empty
		:accessor status-load)
   (using-route :initarg :using-route :initform nil :accessor using-route)   

   (avg-speed :initarg :avg-speed :initform 30 :accessor avg-speed)
   (fuel-capacity :initarg :fuel-capacity :initform 25000
		  :accessor fuel-capacity)
   (fuel-rate :initarg :fuel-rate :initform 0.5 :accessor fuel-rate)
   (offload :initarg :offload :initform 14400 :accessor offload)   
   (onload :initarg :onload :initform 10800 :accessor onload)
   (range :initarg :range :initform 12500 :accessor range)   
   (refuel :initarg :refuel :initform 3600 :accessor refuel)
   (w-cargo :initarg :w-cargo :initform 1000 :accessor w-cargo)   
   (wo-cargo :initarg :wo-cargo :initform 3110 :accessor wo-cargo))
  (:documentation
   "Fast Sealift Ship specific characteristics.
    static and dynamic slots (note that all times are given
    in seconds)."))

;;;------------------------------------------------------------------------
;;;                      METHOD DEFINITIONS
;;;------------------------------------------------------------------------

;;;********************************
;;; METHOD: Initialize-instance
;;;
;;; Defines an after method on the initialize-instance for
;;; world objects. The effect is that each time a world
;;; object is created, that object is placed into a hash table
;;; which maintains all world objects keyed on the name slot
;;; of the object.
;;;
;;;********************************

(defmethod initialize-instance :after ((wobj world) &key)
           (declare (special *world-hash-table*))
           (let ((has-value? (gethash (name wobj) *world-hash-table*)))
           (if has-value?
               (setf (gethash (name wobj)
                              *world-hash-table*)
                     (cons wobj has-value?))
               (setf (gethash (name wobj) *world-hash-table*) (list wobj)))))


;;;********************************
;;; METHOD: Climate
;;;********************************

(defmethod (setf climate) :after (new-climate (cobj city))
	   (let ((connected-roads (roads cobj)))
	     (mapcar #'(lambda (road-name)
			 (let* ((road-obj
				 (car (get-world-object road-name)))
				(other-end
				 (car
				  (remove (name cobj) (end-points road-obj))))
				(other-city
				 (car
				  (remove
				   nil
				   (mapcar #'(lambda (obj)
					       (if (eq (class-name
							(class-of obj))
						       'city)
						   obj))
					   (get-world-object other-end)))))
				(other-climate (climate other-city)))
			   (adjust-road-conditions
			    road-obj new-climate other-climate)))
		     connected-roads)))


;;;********************************
;;; METHOD: Condition
;;;********************************

(defmethod (setf condition) :after (new-condition (robj road))
	   (declare (special *world-ground-transports*)
		    (ignore new-condition))
	   (mapcar #'(lambda (gt-resource)
		       (let ((gt-object (car (get-world-object gt-resource))))
			 (if (eq (name robj) (using-road gt-object))
			     (adjust-speed-and-due-time gt-object robj))))
		       *world-ground-transports*))


;;;********************************
;;; METHOD: Fuel
;;;********************************

(defmethod (setf fuel) :after (new-fuel-level (gt-obj ground-transport))
	   (if (in-country gt-obj)
	       (if (and (<= new-fuel-level 0)
			(not (null (using-road gt-obj))))
		   (progn
		     (let ((world-struct (world-event-struct gt-obj)))
		       (setf (speed gt-obj) 0)
		       (if (null (failure-reason gt-obj))
			   (setf (failure-reason gt-obj) 'no-fuel-gt))
		       (if (not (null world-struct))
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

(defmethod (setf fuel) :after (new-fuel-level (act-obj air-cargo-transport))
	   (if (in-country act-obj)	   
	       (if (<= new-fuel-level 0)
		   (progn
		     (let ((world-struct (world-event-struct act-obj)))
		       (if (null (failure-reason act-obj))
			   (setf (failure-reason act-obj) 'no-fuel-plane))
		       (if (not (null world-struct))		   
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

(defmethod (setf fuel)
    :after (new-fuel-level (apt-obj air-passenger-transport))
    	   (if (in-country apt-obj)
	       (if (<= new-fuel-level 0)
		   (progn
		     (let ((world-struct (world-event-struct apt-obj)))
		       (if (null (failure-reason apt-obj))
			   (setf (failure-reason apt-obj) 'no-fuel-plane))
		       (if (not (null world-struct))		   
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

;;;********************************
;;; METHOD: Status
;;;********************************

(defmethod (setf status) :after (new-status (gt-obj ground-transport))
	   (if (in-country gt-obj)	   
	       (if (or (equal new-status 'captured)
		       (equal new-status 'destroyed))
		   (progn
		     (let ((world-struct (world-event-struct gt-obj)))
		       (if (not (null world-struct))
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

(defmethod (setf status) :after (new-status (apt-obj air-passenger-transport))
	   (if (in-country apt-obj)	   
	       (if (or (equal new-status 'captured)
		       (equal new-status 'destroyed))
		   (progn
		     (let ((world-struct (world-event-struct apt-obj)))
		       (if (not (null world-struct))
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

(defmethod (setf status) :after (new-status (act-obj air-cargo-transport))
	   (if (in-country act-obj)	   
	       (if (or (equal new-status 'captured)
		       (equal new-status 'destroyed))
		   (progn
		     (let ((world-struct (world-event-struct act-obj)))
		       (if (not (null world-struct))
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

;;;********************************
;;; METHOD: Mech-status
;;;********************************

(defmethod (setf mech-status)
    :after (new-mech-stat (gt-obj ground-transport))
	   (if (in-country gt-obj)    
	       (ecase new-mech-stat
		 (GOOD)
		 (POOR
		  (let* ((current-fuel (fuel gt-obj))
			 (new-fuel-level (- current-fuel
					    (* .25 current-fuel))))
		    (setf (fuel gt-obj) new-fuel-level)))
		 (BAD
		  (let ((world-struct (world-event-struct gt-obj)))
		    (if (not (null world-struct))
			(fail-world-event
			 (extract-event
			  (world-event-number world-struct)))))))))

(defmethod (setf mech-status)
    :after (new-mech-stat (apt-obj air-passenger-transport))
	   (if (in-country apt-obj)    
	       (ecase new-mech-stat
		 (GOOD)	     
		 (POOR
		  (let* ((current-fuel (fuel apt-obj))
			 (new-fuel-level (- current-fuel
					    (* .25 current-fuel))))
		    (setf (fuel apt-obj) new-fuel-level)))
		 (BAD
		  (let ((world-struct (world-event-struct apt-obj)))
		    (if (not (null world-struct))
			(fail-world-event
			 (extract-event
			  (world-event-number world-struct)))))))))

(defmethod (setf mech-status)
    :after (new-mech-stat (act-obj air-cargo-transport))
	   (if (in-country act-obj)    
	       (ecase new-mech-stat
		 (GOOD)	     
		 (POOR
		  (let* ((current-fuel (fuel act-obj))
			 (new-fuel-level (- current-fuel
					    (* .25 current-fuel))))
		    (setf (fuel act-obj) new-fuel-level)))
		 (BAD
		  (let ((world-struct (world-event-struct act-obj)))
		    (if (not (null world-struct))
			(fail-world-event
			 (extract-event
			  (world-event-number world-struct)))))))))


;;;********************************
;;; METHOD: Tire-status 
;;;********************************

(defmethod (setf tire-status)
    :after (new-tire-stat (gt-obj ground-transport))
	   (if (in-country gt-obj)    
	       (if (eq new-tire-stat 'blown)
		   (progn
		     (let ((world-struct (world-event-struct gt-obj)))
		       (if (not (null world-struct))
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))
		   

(defmethod (setf tire-status)
    :after (new-tire-stat (act-obj air-cargo-transport))
	   (if (in-country act-obj)    
	       (if (eq new-tire-stat 'blown)
		   (progn
		     (let ((world-struct (world-event-struct act-obj)))
		       (if (not (null world-struct))
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

(defmethod (setf tire-status)
    :after (new-tire-stat (apt-obj air-passenger-transport))
	   (if (in-country apt-obj)    
	       (if (eq new-tire-stat 'blown)
		   (progn
		     (let ((world-struct (world-event-struct apt-obj)))
		       (if (not (null world-struct))	    
			   (fail-world-event
			    (extract-event
			     (world-event-number world-struct)))))))))

;;;********************************
;;; METHOD: Class-slot-names
;;;
;;; Defines a method which gets all of the slot-names associated with
;;; a particular object.
;;;
;;;********************************

(defmethod class-slot-names ((instance standard-object))
  "Given an INSTANCE, returns a list of the slots in the instance's class."
  (mapcar #'clos:slot-definition-name
          (clos:class-slots (class-of instance))))

;;;------------------------------------------------------------------------
;;;                       PRINT-OBJECT DEFINITIONS
;;;------------------------------------------------------------------------

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <GROUND-TRANSPORT>
;;;-----------------------------------------------------------------------

(defmethod print-object ((gt ground-transport) stream)
  (format stream "#<~S ~A>"
	  (type-of gt)
	  (if (slot-boundp gt 'name)
	      (name gt)
	      "(no name)"))
  gt)

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <AIR-CARGO-TRANSPORT>
;;;-----------------------------------------------------------------------

(defmethod print-object ((act air-cargo-transport) stream)
  (format stream "#<~S ~A>"
	  (type-of act)
	  (if (slot-boundp act 'name)
	      (name act)
	      "(no name)"))
  act)

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <AIR-PASSENGER-TRANSPORT>
;;;-----------------------------------------------------------------------

(defmethod print-object ((apt air-passenger-transport) stream)
  (format stream "#<~S ~A>"
	  (type-of apt)
	  (if (slot-boundp apt 'name)
	      (name apt)
	      "(no name)"))
  apt)

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <ATTACK-HELICOPTER>
;;;-----------------------------------------------------------------------

(defmethod print-object ((ah attack-helicopter) stream)
  (format stream "#<~S ~A>"
	  (type-of ah)
	  (if (slot-boundp ah 'name)
	      (name ah)
	      "(no name)"))
  ah)

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <MEDICAL-HELICOPTER>
;;;-----------------------------------------------------------------------

(defmethod print-object ((mh medical-helicopter) stream)
  (format stream "#<~S ~A>"
	  (type-of mh)
	  (if (slot-boundp mh 'name)
	      (name mh)
	      "(no name)"))
  mh)

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <FAST-SEALIFT-SHIP>
;;;-----------------------------------------------------------------------

(defmethod print-object ((fss fast-sealift-ship) stream)
  (format stream "#<~S ~A>"
	  (type-of fss)
	  (if (slot-boundp fss 'name)
	      (name fss)
	      "(no name)"))
  fss)

;;;-----------------------------------------------------------------------
;;; PRINT-OBJECT <ROAD>
;;;-----------------------------------------------------------------------

(defmethod print-object ((r road) stream)
  (format stream "#<~S ~A>"
	  (type-of r)
	  (if (slot-boundp r 'name)
	      (name r)
	      "(no name)"))
  r)


;;;------------------------------------------------------------------------
;;;                       FUNCTION DEFINITIONS
;;;------------------------------------------------------------------------

(defun world-snapshot ()
  (declare (special *world-hash-table*))
  (let* ((*print-pretty* t)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*package* (find-package :rea))
	 (bad-slots '(air-routes roads seaport airport neighbors runway
		      name failure-reason bridge-xing range avg-speed
		      onload offload refuel wo-cargo w-cargo fuel-capacity
		      fuel-rate last-loc world-event-struct closest-city
		      end-points tower-clearance at-gate distance))
	 (world-objects nil))

    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value world-objects))
	     *world-hash-table*)
    
    (with-open-file (fs "snapshot.tfin"
			:direction :output
			:if-exists :supersede)
      (format fs "(defun C:TFIN ()~%")
      (mapcar #'(lambda (wobj)
		  (let ((object nil)
			(object-pair nil))
		  
		    (if (> (list-length wobj) 1)
			(progn
			  (setq object-pair t)
			  (mapcar #'(lambda (wobject)
				      (mapcar #'(lambda (slot)
						  (if (not
						       (member slot bad-slots))
						      (format fs "(~s ~s ~s)~%"
							      slot
							      (name wobject)
							      (funcall
							       slot wobject))))
					      (class-slot-names wobject)))
				  wobj))
			(setq object (first wobj)))

		    (if (not object-pair)
			(mapcar #'(lambda (slot)
				    (if (not (member slot bad-slots))
					(format fs "(~s ~s ~s)~%"
						slot (name object)
						(funcall slot object))))
				(class-slot-names object)))))
	      world-objects)
      (format fs "(command redrawall)~%(princ))~%")))
  t)

;;;------------------------------------------------------------------------
;;; GET-WORLD-OBJECT arg: name of object
;;;
;;; Looks up the name of an object in the *world-hash-table*.
;;;
;;; Returns: world-object if name is found, nil otherwise.
;;;
;;;------------------------------------------------------------------------

(defun get-world-object (name)
  (gethash name *world-hash-table*))

;;;
;;; FOR DEMO PURPOSES ONLY!
;;;

(defun gwo (name)
  (car (get-world-object name)))

(defun swo (o a v)
  (eval `(setf (,a ,(gwo o)) ',v)))

(defun show (name)
  (describe (gwo name)))

(defun gt-blown-tire (gt-name)
  (swo gt-name 'tire-status 'blown))

(defun plane-bad-mech (plane-name)
  (swo plane-name 'status 'unavailable)
  (swo plane-name 'failure-reason 'hydraulics)
  (swo plane-name 'mech-status 'bad))


;;;------------------------------------------------------------------------
;;; ADJUST-SPEED-AND-DUE-TIME args: ground-transport-object road-object
;;;
;;; When the road conditions change for a particular road this function
;;; is called to determine how that new condition should effect the speed
;;; of resources travelling along it.
;;;
;;; 
;;;------------------------------------------------------------------------

(defun adjust-speed-and-due-time
    (gt-resource-object road-object &optional (calculated-speed nil))
  
  (let* ((time (current-simulated-time))
	 (present-speed (speed gt-resource-object))
	 (event-structure (world-event-struct gt-resource-object))
	 (event-args (world-event-args event-structure))
	 (start-time (world-event-start-time event-structure))
	 (present-due-time (world-event-due-time event-structure))
	 (dist-traveled (round (* (/ (- time start-time) 3600)
				  present-speed)))
	 (road-distance (distance road-object))
	 (new-speed nil))

    (if (null calculated-speed)
	(setq new-speed
	      (calculate-ground-speed
	       (first event-args) (fourth event-args)))
	(setq new-speed calculated-speed))
	   
    ;;; IF a speed change has not occurred then do NOT change the due time!
    
    (if (not (= new-speed present-speed))
	(progn
	  (setf (world-event-due-time event-structure)
		(+ time
		   (round (* 3600 (/ (- road-distance dist-traveled)
				     new-speed)))))
	  (extract-event (world-event-number event-structure))
	  (schedule-world-event event-structure)))))


;;;------------------------------------------------------------------------
;;; ADJUST-ROAD-CONDITIONS
;;;------------------------------------------------------------------------

(defun adjust-road-conditions (road-object end-1-climate end-2-climate)
  (declare (special *road-condition-table*))
  (let ((table-index (ecase end-1-climate
		       (SUNNY
			(ecase end-2-climate
			  (SUNNY 0)
			  (RAINY 1)
			  (STORMY 2)))
		       (RAINY
			(ecase end-2-climate
			  (SUNNY 1)
			  (RAINY 3)
			  (STORMY 4)))
		       (STORMY
			(ecase end-2-climate
			  (SUNNY 2)
			  (RAINY 4)
			  (STORMY 5)))))
	(cond-index (ecase (road-type road-object)
		      (ASPHALT 0)
		      (GRAVEL 1)
		      (DIRT 2))))
    (setf (condition road-object)
	  (nth cond-index (nth table-index *road-condition-table*)))))

;;;------------------------------------------------------------------------
;;; CALCULATE-GROUND-SPEED
;;;------------------------------------------------------------------------

(defun calculate-ground-speed (res road)
  (let* ((road-obj (car (get-world-object road)))
	 (cities (end-points road-obj))
	 (city-1-obj
	  (car (remove nil
		  (mapcar #'(lambda (obj)
			      (if (eq (class-name (class-of obj)) 'city)
				  obj))
			  (get-world-object (first cities))))))
	 (city-2-obj
	  (car (remove nil 
		  (mapcar #'(lambda (obj)
			      (if (eq (class-name (class-of obj)) 'city)
				  obj))
			  (get-world-object (second cities))))))
	 (res-obj (car (get-world-object res)))
	 (rc (condition road-obj))
	 (rt (road-type road-obj))
	 (ws (climate city-1-obj))
	 (wd (climate city-2-obj))
	 (sm (mech-status res-obj))
	 (sa (avg-speed res-obj))
	 (speed nil))
    (case rc
      (good (push 3 speed))
      (slippery (push -6 speed))
      (muddy (push -10 speed))
      (impassable (push -15 speed))
      (otherwise (push 0 speed)))
    (case rt
      (asphalt (push 3 speed))
      (dirt (push 0 speed))
      (gravel (push -5 speed))
      (otherwise (push 0 speed)))
    (case ws
      (sunny (push 3 speed))
      (rainy (push -5 speed))
      (stormy (push -10 speed))
      (otherwise (push 0 speed)))
    (case wd
      (sunny (push 3 speed))
      (rainy (push -5 speed))
      (stormy (push -10 speed))
      (otherwise (push 0 speed)))
    (case sm
      (good (push 2 speed))
      (fair (push -10 speed))
      (bad (push -20 speed))
      (otherwise (push 0 speed)))
    (push sa speed)
    (apply #'+ speed)))

;;;------------------------------------------------------------------------
;;;                     PRINT-OBJECT DEFINITIONS
;;;------------------------------------------------------------------------

;;;**************************
;;; City
;;;**************************

(defmethod print-object ((c city) stream)
  (format stream "#<~S ~A>"
	  (type-of c)
	  (if (slot-boundp c 'name)
	      (name c)
	      "(no name)"))
  c)

;;;**************************
;;; Airport
;;;**************************

(defmethod print-object ((a airport) stream)
  (format stream "#<~S ~A>"
	  (type-of a)
	  (if (slot-boundp a 'name)
	      (name a)
	      "(no name)"))
  a)

;;;---------------------------------------------------------------------------
;;;                        World Object Definitions
;;;---------------------------------------------------------------------------

;;;****************************************
;;; Cities
;;;****************************************

(make-instance 'city
	       :name 'Abyss
	       :climate 'rainy
	       :num-nationals 50
	       :roads '(road-ab road-ad road-ae)
	       :neighbors '(Barnacle Delta Exodus)
	       :status 'open)

(make-instance 'city
	       :name 'Barnacle
	       :climate 'sunny
	       :num-nationals 67
	       :roads '(road-ab road-bc road-bd)
	       :neighbors '(Abyss Calypso Delta)
	       :status 'open)

(make-instance 'city
	       :name 'Calypso
	       :climate 'sunny
	       :num-nationals 78
	       :roads '(road-bc road-cd)
	       :neighbors '(Barnacle Delta)
	       :status 'open
	       :air-routes '(City-K)
	       :airport t
	       :seaport t)

(make-instance 'city
	       :name 'Delta
	       :climate 'sunny
	       :num-nationals 20
	       :roads '(road-ad road-bd road-cd)
	       :neighbors '(Abyss Barnacle Calypso)
	       :status 'open
	       :air-routes '(City-K)	       
	       :airport t
	       :seaport t)

(make-instance 'city
	       :name 'Exodus
	       :climate 'stormy
	       :roads '(road-ae)
	       :neighbors '(Abyss)
	       :status 'open)

(make-instance 'city
	       :name 'City-K
	       :climate 'rainy
	       :status 'open
	       :air-routes '(Calypso Delta)	       
	       :airport t)

;;;****************************************
;;; Roads
;;;****************************************

(make-instance 'road
	       :name 'Road-AB
	       :road-type 'asphalt
	       :distance 40
	       :end-points '(Abyss Barnacle)
	       :status 'open
	       :condition 'slippery)

(make-instance 'road
	       :name 'Road-AD
	       :road-type 'asphalt
	       :distance 65
	       :end-points '(Abyss Delta)
	       :bridge-xing 'Bay-Bridge
	       :status 'open
	       :condition 'slippery)

(make-instance 'road
	       :name 'Road-AE
	       :road-type 'dirt
	       :distance 40
	       :end-points '(Abyss Exodus)	       
	       :status 'open
	       :condition 'impassable)

(make-instance 'road
	       :name 'Road-BC
	       :road-type 'asphalt
	       :distance 45
	       :end-points '(Barnacle Calypso)	       
	       :status 'open
	       :condition 'good)

(make-instance 'road
	       :name 'Road-BD
	       :road-type 'gravel
	       :distance 85
	       :end-points '(Barnacle Delta)
	       :bridge-xing 'Bay-Bridge
	       :status 'open
	       :condition 'good)

(make-instance 'road
	       :name 'Road-CD
	       :road-type 'asphalt
	       :distance 50
	       :end-points '(Calypso Delta)	       
	       :status 'open
	       :condition 'good)

;;;****************************************
;;; Air-Routes
;;;****************************************

(make-instance 'air-route
	       :name 'Delta
	       :status 'open
	       :distance 520
	       :end-points '(Delta City-K))

(make-instance 'air-route
	       :name 'Calypso
	       :status 'open
	       :distance 500
	       :end-points '(Calypso City-K))

(make-instance 'air-route
	       :name 'Abyss
	       :status 'open
	       :distance 38
	       :route-type 'helicopter
	       :end-points '(Abyss Barnacle))

(make-instance 'air-route
	       :name 'Abyss
	       :status 'open
	       :distance 74
	       :route-type 'helicopter
	       :end-points '(Abyss Calypso))

(make-instance 'air-route
	       :name 'Abyss
	       :status 'open
	       :distance 49
	       :route-type 'helicopter
	       :end-points '(Abyss Delta))

(make-instance 'air-route
	       :name 'Abyss
	       :status 'open
	       :distance 36
	       :route-type 'helicopter
	       :end-points '(Abyss Exodus))

(make-instance 'air-route
	       :name 'Barnacle
	       :status 'open
	       :distance 30
	       :route-type 'helicopter
	       :end-points '(Barnacle Calypso))

(make-instance 'air-route
	       :name 'Barnacle
	       :status 'open
	       :distance 60
	       :route-type 'helicopter
	       :end-points '(Barnacle Delta))

(make-instance 'air-route
	       :name 'Barnacle
	       :status 'open
	       :distance 80
	       :route-type 'helicopter
	       :end-points '(Barnacle Exodus))

(make-instance 'air-route
	       :name 'Calypso
	       :status 'open
	       :distance 40
	       :route-type 'helicopter
	       :end-points '(Calypso Delta))

(make-instance 'air-route
	       :name 'Calypso
	       :status 'open
	       :distance 83
	       :route-type 'helicopter
	       :end-points '(Calypso Exodus))

;;;****************************************
;;; Airports
;;;****************************************

(make-instance 'airport
	       :name 'Calypso-Airport
	       :loc 'Calypso
	       :status 'open)

(make-instance 'airport
	       :name 'Delta-Airport
	       :loc 'Delta
	       :status 'open)

(make-instance 'airport
	       :name 'City-K-Airport
	       :loc 'City-K
	       :status 'open)

;;;****************************************
;;; Bridges
;;;****************************************

(make-instance 'bridge
	       :name 'Bay-Bridge
	       :roads '(Road-AD Road-BD)
	       :status 'open)

;;;****************************************
;;; Terrortist-Groups
;;;****************************************

(make-instance 'terrorist-group
	       :name 'T-Fal-1
	       :loc 'Road-AD)

;;;****************************************
;;; Ground-Transports
;;;****************************************

(make-instance 'ground-transport
	       :name 'GT1
	       :status 'available
	       :loc 'City-K)

(make-instance 'ground-transport
	       :name 'GT2
	       :status 'available
	       :loc 'City-K)

;;;****************************************
;;; Air-Cargo-Transports
;;;****************************************

(make-instance 'air-cargo-transport
	       :name 'C5-1
	       :loc 'City-K
	       :status-load 'empty
	       :status 'available)

(make-instance 'air-cargo-transport
	       :name 'C5-2
	       :loc 'City-K
	       :status-load 'empty
	       :status 'available)

;;;****************************************
;;; Air-Passenger-Transports
;;;****************************************

(make-instance 'air-passenger-transport
	       :name 'B707
	       :in-country t
	       :loc 'Delta
	       :status 'available)

;;;****************************************
;;; Attack-Helicopters
;;;****************************************

(make-instance 'attack-helicopter
	       :name 'AH-1
	       :loc 'City-K
	       :status 'available)

;;;****************************************
;;; Medical-Helicopters
;;;****************************************

(make-instance 'medical-helicopter
	       :name 'MH-1
	       :loc 'City-K
	       :status 'available)

;;;****************************************
;;; Fast-Sealift-Ships
;;;****************************************

(make-instance 'fast-sealift-ship
	       :name 'FSS-1
	       :loc 'City-K
	       :status 'available)




