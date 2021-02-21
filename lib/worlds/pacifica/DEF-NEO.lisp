;;; -*- Mode: LISP; Package: USER; Syntax: Common-lisp;  -*-
;;;
;;; File    : Pacifica-Defaults.lisp
;;; Contains: Defaults for the Pacifica Small Scale NEO Simulation
;;; Created : Mon Dec 13 10:45:35 1993
;;; Updated : Thu Nov 17 15:54:35 1994
;;;
;;; Author  : Glen A. Reece <G.Reece@ed.ac.uk>
;;; Version : 1.00; 
;;;
;;; Copyright (C) 1994, 1995 Glen A. Reece
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

(defvar *world-hash-table* (make-hash-table :test 'equal :size 100))

(defvar *world-cities* '(Abyss Barnacle Calypso Delta Exodus City-K))
(defvar *world-roads* '(road-ab road-ae road-ad road-bc road-bd road-cd))
(defvar *world-bridges* '(bay-bridge))
(defvar *world-terrorist-groups* '(t-fal-1))
(defvar *world-resources* '(B707 C5-1 C5-2 GT1 GT2 AH-1 MH-1 FSS-1))
(defvar *world-helicopters* '(AH-1 MH-1))
(defvar *world-ships* '(FSS-1))
(defvar *world-ground-transports* '(GT1 GT2))
(defvar *world-cargo-planes* '(C5-1 C5-2))
(defvar *world-passenger-planes* '(B707))
(defvar *world-medical-helicopters* '(MH-1))
(defvar *world-attack-helicopters* '(AH-1))

(defparameter *air-fuel-threshold* 0.25)
(defparameter *helicopter-fuel-threshold* 0.40)
(defparameter *ground-fuel-threshold* 0.46)

(defparameter *reverse-bridge-effects* nil)
(defparameter *reverse-volcanic-effects* nil)
(defparameter *reverse-flood-effects* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Exogenous Event Manager Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Turn the Exogenous Event Manager Process on for the Pacifica World.
(defparameter *EEM-on* t)

(defparameter *exogenous-event-distribution*
  '(PROB-DIST
    (0.33 terrorist-activity)
    (0.30 resource-event)       
    (0.30 meteorological-event) 
    (0.07 natural-disaster)))

(defparameter *time-to-next-exogenous-meteorological-event*
  '(DIST-MEAN-VAR 1800 600))

(defparameter *time-to-next-exogenous-resource-event*
  '(DIST-MEAN-VAR 1200 600))

(defparameter *time-to-next-exogenous-terrorist-event*
  '(DIST-MEAN-VAR 1200 300))

(defparameter *time-to-next-exogenous-disaster-event*
  '(DIST-MEAN-VAR 1200 900))

(defparameter *weather-distribution*
  '(PROB-DIST
    (0.60 sunny)
    (0.30 rainy)
    (0.10 stormy)))

(defparameter *resource-distribution*
  '(PROB-DIST
    (0.50 fuel)
    (0.29 mechanical)
    (0.21 tire)))

(defparameter *fuel-event-distribution*
  '(PROB-DIST
    (0.52 less-10-percent)
    (0.38 no-change)    
    (0.10 empty)))

(defparameter *mechanical-event-distribution*
  '(PROB-DIST
    (0.33 no-change)
    (0.33 good)
    (0.21 poor)
    (0.13 bad)))

(defparameter *tire-event-distribution*
  '(PROB-DIST
    (0.40 no-change)
    (0.40 good)
    (0.20 blown)))

(defparameter *terrorist-distribution*
  '(PROB-DIST
    (0.75 attack)
    (0.15 bridge)
    (0.09 capture)
    (0.01 access)))

(defparameter *attack-event-distribution*
  '(PROB-DIST
    (0.60 poor-mech-status)
    (0.30 blown-tire-status)
    (0.10 no-change)))

(defparameter *disaster-distribution*
  '(PROB-DIST
    (0.50 volcanic)
    (0.35 rock-slide)
    (0.15 flood)))

;;;---------------------------------------------------------------------------
;;; ROAD-CONDITION-TABLE
;;;
;;; Table used to calculate road conditions given the climate at both
;;; ends of a road, and the type of road.
;;;
;;;          WEATHER     |  Asphalt  |  Gravel  |    Dirt    |
;;;       ----------------------------------------------------
;;;       Sunny, Sunny   |   Good    | Slippery |  Slippery  |
;;;       Sunny, Rainy   | Slippery  | Slippery |    Muddy   |
;;;       Sunny, Stormy  | Slippery  | Slippery |    Muddy   |
;;;       Rainy, Rainy   | Slippery  | Slippery | Impassable |
;;;       Rainy, Stormy  | Slippery  | Slippery | Impassable |
;;;       Stormy, Stormy | Slippery  | Slippery | Impassable |
;;;
;;;---------------------------------------------------------------------------

(defparameter *road-condition-table*
  '((good slippery slippery)
    (slippery slippery muddy)
    (slippery slippery muddy)
    (slippery slippery impassable)
    (slippery slippery impassable)
    (slippery slippery impassable)))
