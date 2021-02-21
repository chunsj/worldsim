(in-package :world)

(defun succeed-world-event (event)
  (if (equal 'scripted (world-event-action event))
      (funcall (world-event-succeed-fun event) (world-event-args event))
      (funcall (world-event-succeed-fun event) event)))

(defun schedule-eem-process ()
  (schedule-world-event
   (make-world-event
    :hidden-p t
    :due-time (+ (current-simulated-time)
		 (* 600 (time-to-next
			*EEM-probability-of-occurance-in-an-interval*)))
    :description (format nil "Exogenous Event Manager Process")
    :action 'EEM-Process
    :args nil
    :succeed-fun 'eem-success
    :fail-fun nil)))

(defun eem-success (action)
  (declare (ignore action))
  (funcall (process-probability *exogenous-event-distribution*))
  (if *EEM-on* (schedule-eem-process)))

(defun no-event (action)
  (declare (ignore action)))
