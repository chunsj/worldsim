;;;; File: pseudo-process.lsp
;;; SCCS Version: %W%
;;; Contains: A mechanism for simulated multi-tasking
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: February 1993
;;; Updated: Fri Sep 29 19:03:36 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1993, 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

;;; This file contains a "pseudo-process" mechanism plus all of the
;;; tools it uses, such a queues and "advice" functions (for a single-
;;; stepper).  It is self-contained and does not require any other parts
;;; of O-Plan (or indeed any other files).  There is an associated set
;;; of tests (pseudo-process-tests.lsp) which requires O-Plan's test
;;; framework.  The file ipc.lsp builds some additional mechanisms on
;;; top of the ones provided here.

;;; For more information see "doc/pseudo-processes" and "doc/ipc".

;;; Contents:
;;;  * Package definition.
;;;  * Time tools
;;;  * Queues.
;;;  * Pseudo-processes.
;;;     - Basic definitions.
;;;     - Main loop.
;;;     - P-process environment operations
;;;     - Event dequeuing
;;;     - Status changes, including sleep-for/until.
;;;  * Pseudo-select.
;;;  * Pseudo-sockets.
;;;  * Messages.
;;;  * Debugging tools.
;;;     - Describe functions.
;;;     - Single-stepper.
;;;     - Advice package.
;;;  * Read-eval-print-loop pprocesses.
;;;  * Connector pprocesses (between streams and sockets).


;;;; Package definition

(in-package :pseudo-process :nicknames '(:pprocess))

#+lucid
(import 'lcl:with-simple-restart)	;/\/ it's not in the standard package

;;; Exports for a type typically include the type name and some slot
;;; names, for use in the defstruct :include option.  Most slot accessors
;;; are exported, but some are regarded as too low-level.

;;; Note that find-pprocess and find-socket signal an error if the pprocess
;;; or socket doesn't exist.  To check existence, use exists-pprocess or
;;; exists-socket.

(export '(get-primitive-real-time primitive-time-units-per-second
          universal->primitive-real-time primitive-real->universal-time))
	  
(export '(make-queue queue-p reset-queue empty-queue-p
	  enqueue dequeue queue-contents insert-in-queue
	  enqueue-increasing enqueue-decreasing delqueue-if))

(export '(*all-pprocesses* *pprocess* print-pprocess
	  pprocess pprocess-p new-pprocess register-pprocess
	  find-pprocess exists-pprocess
	  pprocess-name pprocess-status
	  pprocess-run-function pprocess-event-queue
	  pprocess-input-streams pprocess-wakeup-time
	  pprocess-specials ;but not pprocess-specvals
	  pprocess-plist
	  clear-pprocesses clear-pprocess-system
	  set-pprocess-time-conversion-base
	  pprocess-main-loop run-pprocess
	  call-in-pprocess-env
	  pprocess-symbol-value add-pprocess-special
	  next-event-p next-event peek-next-event
	  set-pprocess-status terminate-pprocess suspend-pprocess
	  set-to-wait-for-event
	  set-to-sleep-for set-to-sleep-until
	  wait-until-wakeup wakeup))

; Export pprocess slot names for use in :include.
(export '(status run-function specials))

(export '(select-input select-input-p))
(export '(pseudo-select pseudo-select-p)) ;/\/ make internal

(export '(*all-sockets*
	  socket socket-p new-socket register-socket
	  find-socket exists-socket ensure-socket
	  socket-server socket-clients
	  register-server register-client ensure-client))

(export '(make-message message-p message-sender message-route message-contents
	  send-to-socket send-to-client send-reply send-to-pprocess
	  send-message))

(export '(send-immediate send-question send-answer))

; Debugging tools
(export '(describe-pprocesses describe-pprocess
	  describe-sockets describe-socket
	  step-pprocesses unstep-pprocesses
	  watch-pprocess unwatch-pprocess
	  watch-socket unwatch-socket
	  *step-io* *default-step-action* *default-filter-action*))

(export '(run-pprocess-filter call-in-pprocess-env-filter
	  send-message-filter send-immediate-filter
	  next-event-filter))

(export '(advice+ advice-))

; REPL
(export '(repl repl-p new-repl repl-prompt repl-io))
(export '(really-clear-whitespace))

; Connectors
(export '(connector new-connector connector-p
	  connector-io-stream
	  connector-socket-to-read connector-socket-to-write
	  connector-input-guard
	  connector-debug-io))

; For stepper
(proclaim '(notinline run-pprocess call-in-pprocess-env
	              send-message send-immediate next-event))

; For sim-clock.lsp
(export '(*clock*))


;;;; Time operations

;;; Primitive real time

;;; Primitive real time exists so that we can use different low-level
;;; time operations in different Lisps.  Ordinarily, we'll use internal
;;; real time, but in some Lisps this may be too expensive (since it's
;;; bignums).  Unfortunately, in some other Lisps (such as Lucid 4.1),
;;; internal real time is even larger bignums.  /\/

;;; It's assumed that primitive real time is in units smaller than
;;; seconds.

(defconstant primitive-time-units-per-second internal-time-units-per-second)

(defmacro get-primitive-real-time ()
  `(get-internal-real-time))


;;; Memoized primitive real time

;;; Get-time is a way to avoid repeated system calls when repeatedly
;;; checking the time within a short interval.  Forget-time should be
;;; called if a significant period of time may have elapsed since
;;; the last call to forget-time.

;;; Note that some Common Lisps (e.g. KCL) don't compile DEFUNs that
;;; are inside LETs, hence the global variables that "s.b. lexical".

(proclaim '(inline forget-time get-time checked-time-p))

(defvar the-time nil)			;/\/ s.b. lexical

(defun forget-time ()
  (setq the-time nil))

(defun get-time ()
  (or the-time (setq the-time (get-primitive-real-time))))

(defun checked-time-p ()
  the-time)


;;; Primitive-real <--> universal time conversions

;;; N.B. The functions minumum-timeout and set-to-sleep-for contain
;;; in-line conversion.

;;; It's assumed that primitive real time is in units smaller than
;;; seconds.  We get a second of real time after a second's worth
;;; of the smaller units have passed; hence the use of floor when
;;; converting from primitive-real to universal time.

(defvar primitive-real-base nil)	;/\/ s.b. lexical
(defvar universal-base nil)		;/\/ s.b. lexical

(defun ensure-pprocess-time-conversion-base ()
  (unless primitive-real-base
    (set-pprocess-time-conversion-base)))

(defun set-pprocess-time-conversion-base ()
  (setf primitive-real-base (get-primitive-real-time)
	universal-base (get-universal-time)))

(defun universal->primitive-real-time (universal-time)
  ;; Universal time is a number of seconds.
  (+ primitive-real-base
     (* primitive-time-units-per-second
	(- universal-time universal-base))))

(defun primitive-real->universal-time (primitive-real-time)
  (+ universal-base
     (floor (- primitive-real-time primitive-real-base)
	    primitive-time-units-per-second)))


;;;; Queues

(proclaim '(inline empty-queue-p))

(defstruct (queue (:print-function print-queue))
  (head (list nil))			;a headed list
  (tail head))				;ptr to the last cons in the list

(defun print-queue (q stream depth)
  (declare (ignore depth))
  (format stream "#<queue ~S>" (queue-contents q)))

(defun reset-queue (q)
  (setf (queue-head q) (list nil)
	(queue-tail q) (queue-head q))
  q)

(defun empty-queue-p (q)
  (eq (queue-head q) (queue-tail q)))

(defun enqueue (item q)
  (setf (cdr (queue-tail q)) (list item))
  (setf (queue-tail q) (cdr (queue-tail q)))
  item)

(defun dequeue (q)
  (if (eq (queue-head q) (queue-tail q))
      (error "Dequeue from empty queue ~S." q)
    (let ((new-head (cdr (queue-head q))))
      (prog1 (car new-head)
	(setf (car new-head) nil)		;erase obsolete pointer
	(setf (queue-head q) new-head)))))

(defun queue-contents (q)
  (cdr (queue-head q)))

(defun set-queue-contents (q new-contents)
  (setf (cdr (queue-head q)) new-contents)
  (setf (queue-tail q) (last (queue-head q))) 	;n.b. contents may be null
  new-contents)

(defsetf queue-contents set-queue-contents)


;;; Poor man's priority queues

(defun insert-in-queue (item q predicate key)
  ;; The item is inserted in front of the first element such that
  ;; (funcall predicate (funcall key item) (funcall key element))
  ;; is true.
  (let ((k (funcall key item)))
    (labels ((scan (ptr)
	       (cond ((null (cdr ptr))
		      (setf (cdr ptr)		;insert at end of queue
			    (setf (queue-tail q) (list item))))
		     ((funcall predicate k (funcall key (cadr ptr)))
		      (setf (cdr ptr)		;insert in front of (cadr ptr)
			    (cons item (cdr ptr))))
		     (t (scan (cdr ptr))))))
      (assert (queue-head q))
      (scan (queue-head q))
      item)))

(defun enqueue-increasing (item q &key (key #'identity))
  (insert-in-queue item q #'< key))

(defun enqueue-decreasing (item q &key (key #'identity))
  (insert-in-queue item q #'> key))


;;; Removing items from queues before their time.

;;; (Delqueue-if pred q) deletes the first item in the queue that
;;; satisfies the predicate.  The deleted item is returned.  If no
;;; such item exists, nil is returned instead.

(defun delqueue-if (pred q)
  (let ((item nil))
    (labels ((scan (ptr)
	       (cond ((null (cdr ptr)))		;end of queue
		     ((funcall pred (cadr ptr))	;found item to delete
		      (setq item (cadr ptr))
		      (when (null (setf (cdr ptr) (cddr ptr)))
			(setf (queue-tail q) ptr)))
		     (t (scan (cdr ptr))))))
      (assert (queue-head q))
      (scan (queue-head q))
      item)))


;;;; Pseudo-processes

(defvar *all-pprocesses* nil
  "A list of all pseudo-processes.")

(defvar *pprocess* nil
  "The pseudo-process now running, else nil.")

(defstruct (pprocess (:print-function print-pprocess)
		     (:constructor %make-pprocess))
  (name (error "No name for pprocess."))
  (status :new)				;see pprocess-main-loop
  (run-function 'not-a-run-function)	;called with 1 arg: the pprocess
  (event-queue (make-queue))		;incoming messages
  (input-streams '())			;sources of potential input
  (wakeup-time nil)			;primitive-real-time to sleep until
  (specials '())			;process-local special variables
  (specvals				; and their corresponding values
   (mapcar #'symbol-value-or-nil specials))
  (plist '())				;random additional information
  )

(defun clear-pprocesses ()
  (setq *all-pprocesses* nil))

(defun new-pprocess (name &rest other-args)
  (register-pprocess (apply #'%make-pprocess :name name other-args)))

(defun register-pprocess (p)
  ;; Can be called on instances of subtypes.
  (assert (not (exists-pprocess (pprocess-name p))))
  (setq *all-pprocesses* (nconc *all-pprocesses* (list p)))
  p)

(defun find-pprocess (name)
  (or (find name *all-pprocesses* :key #'pprocess-name :test #'eq)
      (error "There is no pprocess named ~S." name)))

(defun exists-pprocess (name)
  (find name *all-pprocesses* :key #'pprocess-name :test #'eq))

(defun print-pprocess (p stream depth)
  ;; Can be called on instances of subtypes.
  (declare (ignore depth))
  (format stream "#<~A ~S>" (type-of p) (pprocess-name p)))

(defun symbol-value-or-nil (sym)
  (if (boundp sym) (symbol-value sym) nil))


;;;; Some restarts, if possible

;;; /\/: We should allow exiting the currently running p-process.
;;; Fear of excessive overheads keeps us from setting up a restart
;;; in run-pprocess, though we could have a restart that throws to
;;; :run-pprocess-exit.  But it's better to have something simple
;;; now than wait until I can write the necessary restart-bind.
;;; So for now there's only the with-simple-restart which turns
;;; out to exit the current cycle.  (Look at the one place where
;;; is macro is used.)

#+:kcl
(defmacro with-pprocess-restarts (&body forms)
  `(progn
     ,@forms))

#-:kcl
(defmacro with-pprocess-restarts (&body forms)
  `(with-simple-restart (abort "Return to round-robin scheduler")
     ,@forms))


;;;; Main loop for running pseudo-processes
;;;
;;; At the end of a cycle it:
;;;  * Exits, returning :done, if no pprocess ran and no pprocess is
;;;    waiting for a wakeup or for input on a stream.
;;;  * Calls select-input-p if no pprocess ran but some are waiting
;;;    for streams; the timeout will be the next wakeup time, if any.
;;;  * Sleeps until the next wakeup time if no pprocess ran, none
;;;    are waiting for streams, and some have wakeups.
;;;
;;; Note that a pprocess that wants to wait for a stream should
;;; have status :run-on-event and include the stream in its
;;; input-streams slot.  Stream input is considered an event so
;;; far as this status is concerned, but does not involve the
;;; event-queue slot.

(defun pprocess-main-loop ()
  (ensure-pprocess-time-conversion-base)
  (catch :pprocess-main-loop-exit
    (loop				;loop to restore the restarts
      (catch :pprocess-cycle-exit
	(with-pprocess-restarts
	  (repeat-pprocess-cycle))))))

(defun repeat-pprocess-cycle ()
  (loop
    (let ((running nil))
      (forget-time)
      (dolist (p *all-pprocesses*)
	(ecase (pprocess-status p)
	  (:run-on-event
	   (when (or (not (empty-queue-p (pprocess-event-queue p)))
		     (and (pprocess-wakeup-time p)
			  (>= (get-time) (pprocess-wakeup-time p)))
		     (and (pprocess-input-streams p)
			  (some #'listen (pprocess-input-streams p))))
	     (setq running t)
	     (run-pprocess p)))
	  (:run
	   (setq running t)
	   (run-pprocess p))
	  (:suspended
	   (when (and (pprocess-wakeup-time p)
		      (>= (get-time) (pprocess-wakeup-time p)))
	     (setq running t)
	     (wakeup-and-run-pprocess p)))
	  (:finished)
	  (:new)))
      (when (not running)
	(let ((streams (all-monitored-streams *all-pprocesses*))
	      (timeout (minimum-timeout *all-pprocesses*)))
	  (cond (streams
		 (select-input-p streams timeout))
		(timeout
		 (sleep timeout))
		(t (throw :pprocess-main-loop-exit :done))))))))


(defun run-pprocess (p)
  (forget-time)				;who knows how long this will take?
  (setf (pprocess-wakeup-time p) nil)	;it's awake
  (progn				;/\/ was: catch :run-pprocess-exit
    (progv (pprocess-specials p) (pprocess-specvals p)
      (unwind-protect
	   (let ((*pprocess* p))
	     (funcall (pprocess-run-function p)
		      p))
	(record-pprocess-specvals p)))))

(defun record-pprocess-specvals (p)
  ;; We want a non-consing equivalent to something like this:
  ;;  (setf (pprocess-specvals p)
  ;;    (mapcar #'symbol-value (pprocess-specials p)))
  (do ((vars (pprocess-specials p) (cdr vars))
       (vals (pprocess-specvals p) (cdr vals)))
      ((null vars) nil)
    (setf (car vals) (symbol-value (car vars)))))

;;; /\/: When a :suspended pprocess is awakened after a sleep, it's
;;; given status :run.  It's therefore up to the pprocess's run-function
;;; to change to another status if that's what's required.  What we
;;; _should_ do is remember the status from before the suspension and
;;; restore it.  Indeed, we already have procedures that will do this.
;;; See wait-until-wakeup and wakeup.  --> Rename it to sleep-until-wakeup?

(defun wakeup-and-run-pprocess (p)
  (setf (pprocess-wakeup-time p) nil
	(pprocess-status p) :run)
  (run-pprocess p))

(defun all-monitored-streams (pprocesses)
  ;; /\/: REDUCE lacks :KEY.
  (mapcan #'(lambda (p)
	      (if (eq (pprocess-status p) :run-on-event)
		  (copy-list (pprocess-input-streams p))
		nil))
	  pprocesses))

;;; /\/: The sleep granularity is 1 second, which may be too coarse.
;;; Unfortunately, some Common Lisps (e.g. KCL) can't do any better,
;;; unless we write C code.  A problem with 1-second granularity is
;;; that when minimum-timeout says to sleep for 0 seconds, this means
;;; there's nothing to do yet; but maybe there's a wakeup time less than
;;; a second away -- not actually 0 seconds away.  So we'd like to sleep
;;; a little rather than spin our wheels waiting for the time to pass.
;;; A possible compromise may be to sleep for 1/10th of a second, in
;;; those Lisps that can do so, whenever we find ourselves wanting to
;;; sleep for 0 seconds.  It may also be a good idea to switch to
;;; primitive time units, at least in set-to-sleep-for.  Anyway, the use
;;; of floor indicates we'd rather spin than be too late.

(defun minimum-timeout (pprocesses) ; -> seconds
  ;; This procedure should be called only by pprocess-main-loop and
  ;; only if a cycle in which nothing ran has just been completed.
  ;; In that case, if (checked-time-p) is false, then no pprocess
  ;; had a non-null wakeup-time in that cycle and we can just return
  ;; nil.
  (when (checked-time-p)
    (let ((min-w nil))
      (dolist (p pprocesses)
	(let ((w (pprocess-wakeup-time p)))
	  (when w (setq min-w (if min-w (min w min-w) w)))))
      ;; So far, min-w is a primitive real time.  We want a delta in seconds.
      (if min-w
	  (floor (max 0 (- min-w (get-primitive-real-time)))
		 primitive-time-units-per-second)
	nil))))


;;;; Process env operations

;;; /\/: Also pass args to give to fn?  Call that funcall-in or apply-in...?

(defun call-in-pprocess-env (p fn)
  ;; Can't successfully get or set special values if the process is running.
  (when (eq p *pprocess*)
    (warn "Calling in env of active process."))
  (progv (pprocess-specials p) (pprocess-specvals p)
    (unwind-protect (funcall fn p)
      (record-pprocess-specvals p))))

(defun pprocess-symbol-value (p symbol)
  (call-in-pprocess-env p
    #'(lambda (p)
	(declare (ignore p))
	(symbol-value symbol))))

(defun set-pprocess-symbol-value (p symbol new-value)
  (call-in-pprocess-env p
    #'(lambda (p)
	(declare (ignore p))
	(setf (symbol-value symbol) new-value))))

(defsetf pprocess-symbol-value set-pprocess-symbol-value)

(defun add-pprocess-special (p symbol value)
  ;; /\/: Won't work if the pprocess is running.
  (push symbol (pprocess-specials p))	;add to end /\/
  (push value (pprocess-specvals p)))


;;;; Event dequeuing

(proclaim '(inline next-event-p))

(defun next-event-p (&optional (pprocess *pprocess*))
  (not (empty-queue-p (pprocess-event-queue pprocess))))

(defun next-event (&optional (pprocess *pprocess*))
  (dequeue (pprocess-event-queue pprocess)))

(defun peek-next-event (&optional (pprocess *pprocess*))
  (and (next-event-p pprocess)
       (car (queue-contents (pprocess-event-queue pprocess)))))


;;;; Status changes

;;; Note that a status change takes effect on the next cycle.
;;; Setting the status to :suspended, for instance, does not
;;; immediately suspend the pprocess.  This property should not
;;; be exploited too freely, because a true lightweight process
;;; mechanism would probably not work the same way.

(defun set-pprocess-status (new-status &optional (p *pprocess*))
  (setf (pprocess-status p) new-status)
  p)

(defun terminate-pprocess (&optional (p *pprocess*))
  ;; /\/: Throw to :run-pprocess-exit?
  (setf (pprocess-status p) :finished)		; never run again
  p)

(defun set-to-wait-for-event (&optional (p *pprocess*))
  (setf (pprocess-status p) :run-on-event)
  p)

;;; /\/: Suspend-pprocess should allow specification of a wait-function
;;; that is called on each cycle to see if the pprocess should be
;;; restarted.  It should return NIL or the new status to give the
;;; pprocess.  It should be an ordinary function, not one that has to
;;; be called in the pprocess-env of the process or that has to have
;;; *pprocess* set.

;;; At present, the only way for a pprocess to restart, when :suspended
;;; for a reason other than to wait for a wakeup-time, is for another
;;; pprocess to set its status slot.

(defun suspend-pprocess (&optional (p *pprocess*))
  (setf (pprocess-status p) :suspended)		; stop running for now
  p)


;;; The following two procedures can be used when one pprocess wants
;;; another to do something and wants to suspend operation until it does.

(defun wait-until-wakeup (&optional (reason :wait-until-wakeup) (p *pprocess*))
  (setf (getf (pprocess-plist p) :suspend-reason)
	reason)
  (shiftf (getf (pprocess-plist p) :suspended-status)
	  (pprocess-status p)
	  :suspended))

(defun wakeup (p)
  (shiftf (pprocess-status p)
	  (getf (pprocess-plist p) :suspended-status)
	  nil)
  (setf (getf (pprocess-plist p) :suspend-reason)
	nil))


;;;; Sleep-for/until

;;; Set-to-sleep-for/until -- set a pprocess's wakeup-time so that it
;;; will be awakened after a time even if there's no other reason for
;;; it to run.  If status = :suspended, this is "let me sleep no matter
;;; what"; if status = :run-on-event it's "wait for the next event but
;;; with a timeout".

(defun set-to-sleep-for (seconds &optional (p *pprocess*))
  (setf (pprocess-wakeup-time p)
	(+ (get-primitive-real-time)
	   (* primitive-time-units-per-second seconds))))

(defun set-to-sleep-until (universal-time &optional (p *pprocess*))
  ;; Note that, unlike set-to-sleep-for, we don't have to determine
  ;; the current time.
  (setf (pprocess-wakeup-time p)
	(universal->primitive-real-time universal-time)))


;;;; Pseudo-select -- "synchronous I/O multiplexing"
;;;
;;; Pseudo-select is similar to the Unix select procedure.  It waits
;;; for input to be ready on any of a list of streams, with an optional
;;; timeout in seconds, and then returns a list of the streams that have
;;; input.  When the timeout is not supplied or is NIL, pseudo-select
;;; will block until input is available.  To poll, specify a timeout
;;; of zero.
;;;
;;; Pseudo-select simulates a select by polling all the streams 
;;; (using listen), and then, if no input is available, sleeping for
;;; one second before polling all the streams again.  However, it
;;; could be replaced by a real select if one were available.
;;;
;;; Useful repl tests:
;;;
;;;  1 (progn (clear-input t) (pseudo-select '(t)))
;;;  2 (progn (clear-input t) (pseudo-select '(t nil)))
;;;  3 (progn (clear-input t) (pseudo-select '(t) 3))
;;;
;;; 1 should return (t) as soon as something is typed;
;;; 2 should return (t nil) after something is typed, but typically
;;;   after a short (< 1 sec) delay while a call to sleep finishes.
;;; 3 should return () after 3 seconds -- unless you type sooner,
;;;   in which case it will return (t).
;;;
;;; /\/: The tests may not work very well in an Emacs buffer.  (???)

(defun pseudo-select (streams &optional timeout) ; -> ready streams
  (cond ((null streams)
	 (error "No streams for pseudo-select."))
	((and (endp (cdr streams))	; only 1 stream
	      (null timeout))		; and no timeout
	 (peek-char nil (car streams))	; -- so wait for some input
	 streams)
	(t
	 (loop
	   (let ((ready-streams (remove-if-not #'listen streams)))
	     (when (or ready-streams (and timeout (<= timeout 0)))
	       (return ready-streams))
	     (when timeout (decf timeout))
	     (sleep 1))))))

(defun pseudo-select-p (streams &optional timeout) ; -> T/F
  (cond ((null streams)
	 (error "No streams for pseudo-select-p."))
	((and (null (cdr streams)) (null timeout))
	 (peek-char nil (car streams)))
	(t (loop (cond ((dolist (s streams nil)
			  (when (listen s) (return t)))
			(return t))
		       ((and timeout (<= timeout 0))
			(return nil))
		       (t (when timeout (decf timeout))
			  (sleep 1)))))))

(unless (fboundp 'select-input)
  (setf (symbol-function 'select-input) #'pseudo-select))
(unless (fboundp 'select-input-p)
  (setf (symbol-function 'select-input-p) #'pseudo-select-p))

;;;; Pseudo-sockets

;;; Ports?  Mailboxes?

;;; N.B. There's no ensure-server because we want servers to be more
;;; definite about whether they're a server yet or not.

(defvar *all-sockets* '()
  "A list of all pseudo-sockets.")

(defstruct (socket (:constructor %make-socket)
		   (:print-function print-socket))
  (name (error "No name for socket."))
  (server nil)
  (clients '()))

(defun new-socket (name)
  (register-socket (%make-socket :name name)))

(defun register-socket (s)
  ;; Can be called on instances of subtypes.
  (assert (not (exists-socket (socket-name s))))
  (setq *all-sockets* (nconc *all-sockets* (list s)))
  s)

(defun find-socket (name)
  (or (find name *all-sockets* :key #'socket-name :test #'eq)
      (error "There is no socket named ~S." name)))

(defun exists-socket (name)
  (find name *all-sockets* :key #'socket-name :test #'eq))

(defun ensure-socket (name)
  (or (exists-socket name)
      (new-socket name)))

(defun print-socket (sock stream depth)
  ;; Can be called on instances of subtypes.
  (declare (ignore depth))
  (format stream "#<~A ~S>" (type-of sock) (socket-name sock)))

(defun register-server (socket-name &optional (pprocess *pprocess*))
  (check-type pprocess pprocess)
  (let ((s (find-socket socket-name)))
    (when (socket-server s)
      (error "Socket ~S already has a server." (socket-name s)))
    (setf (socket-server s) pprocess)))

(defun register-client (socket-name &optional (pprocess *pprocess*))
  (check-type pprocess pprocess)
  (let ((s (find-socket socket-name)))
    (when (member pprocess (socket-clients s) :test #'eq)
      (error "Socket ~S already has client ~S." (socket-name s) pprocess))
    (setf (socket-clients s)
	  (nconc (socket-clients s) (list pprocess)))))

(defun ensure-client (socket-name &optional (pprocess *pprocess*))
  (let ((s (find-socket socket-name)))
    (unless (member pprocess (socket-clients s) :test #'eq)
      (register-client socket-name pprocess))))


;;;; Messages

;;; Send-to-socket sends to the server of a socket
;;; Send-to-client sends to a particular client of a socket
;;; Send-reply answers the sender of a message
;;; Send-to-pprocess sends direct to a process
;;; Send-message is a primitive used by all the above.

(defstruct message
  (sender (error "No sender."))		;a pseudo-process
  (route nil)				;socket-name or :direct
  (contents nil))			;arbitrary object

(defun send-to-socket (socket-name contents)
  (let* ((socket (find-socket socket-name))
	 (receiver (socket-server socket))
	 (sender *pprocess*))
    (unless (member sender (socket-clients socket) :test #'eq)
      (cerror "Send anyway."
	      "Sender ~S is not a client of ~S." sender socket-name))
    (send-message sender receiver socket-name contents)))

(defun send-to-client (socket-name client contents)
  (let* ((socket (find-socket socket-name))
	 (receiver client)
	 (sender *pprocess*))
    (unless (member receiver (socket-clients socket) :test #'eq)
      (error "Receiver ~S is not a client of ~S." receiver socket-name))
    (unless (eq sender (socket-server socket))
      (cerror "Send anyway."
	      "Sender ~S is not the server of ~S." sender socket-name))
    (send-message sender receiver socket-name contents)))

(defun send-reply (message-to-answer reply-contents)
  (send-message *pprocess*
		(message-sender message-to-answer)
		(message-route message-to-answer)
		reply-contents))

(defun send-to-pprocess (receiver contents)
  (send-message *pprocess* receiver :direct contents))

(defun send-message (sender receiver route contents)
  (check-type sender (or pprocess null))
  (check-type receiver pprocess)
  (enqueue (make-message :sender sender :route route :contents contents)
	   (pprocess-event-queue receiver)))


;;; Send-immediate puts a message at the front of the queue.

;;; Use with caution!

(defun send-immediate (route receiver contents)
  #-Allegro ; wants exists-socket defined at compile-time /\/
  (check-type route (or (member :direct) (satisfies exists-socket)))
  (check-type receiver pprocess)
  (push (make-message :sender *pprocess* :route route :contents contents)
	(queue-contents (pprocess-event-queue receiver))))
	

;;; Synchronous communication

(defun send-question (receiver contents)
  (etypecase receiver
    (pprocess (send-to-pprocess receiver contents))
    (symbol (send-to-socket receiver contents)))
  (wait-until-wakeup :send-question))

(defun send-answer (question answer-contents)
  (wakeup (message-sender question))
  (send-reply question answer-contents))


;;;; Debugging tools

;;; Descriptions of the state of things

;;; /\/: Combine these functions with the describe-X-for-stepper ones?

(defun describe-pprocesses ()
  (mapc #'describe-pprocess *all-pprocesses*)
  (values))

(defun describe-pprocess (&optional (p *pprocess*))
  (when (symbolp p) (setq p (find-pprocess p)))
  (format t "~&~S ~S, status ~S"
	  (type-of p) (pprocess-name p) (pprocess-status p))
  (unless (empty-queue-p (pprocess-event-queue p))
    (format t ", event queue:")
    (dolist (e (queue-contents (pprocess-event-queue p)))
      (if (message-p e)
	  (format t "~&  ~S from ~S."
		  (message-route e) (message-sender e))
	  (format t "~&  ~S." e))))
  (when (pprocess-input-streams p)
    (format t "~&   input streams: ~S" (pprocess-input-streams p)))
  (when (pprocess-wakeup-time p)
    (format t "~&   wakeup time: ~S" (pprocess-wakeup-time p)))
  (when (pprocess-plist p)
    (format t "~&   plist: ~S" (pprocess-plist p)))
  (format t "~%")
  (values))


(defun describe-sockets ()
  (mapc #'describe-socket *all-sockets*)
  (values))

(defun describe-socket (s)
  (when (symbolp s) (setq s (find-socket s)))
  (format t "~&~S ~S" (type-of s) (socket-name s))
  (when (socket-server s)
    (format t "~&  server: ~S" (pprocess-name (socket-server s))))
  (when (socket-clients s)
    (format t "~&  clients: ~{~S~^, ~}"
	    (mapcar #'pprocess-name (socket-clients s))))
  (format t "~%")
  (values))


;;; Single-stepper

(defvar *step-io* (make-synonym-stream '*terminal-io*))

(defvar *default-step-action* nil
  "Default step action for (not especially interesting) pprocess events.
   The value should be one of :step, :trace, or nil.")

(defvar *default-filter-action* nil
  "Default step action for interesting pprocess events.
   The value should be one of :step, :trace, or nil.")

(defmacro filter-action (action)
  `(case ,action
     ((:step)  :step)			;explicit instruction
     ((:trace) :trace)			;explicit instruction
     ((nil) *default-step-action*)	;not very interesting event
     (t     *default-filter-action*)))	;interesting event

(defun step-pprocesses (&optional (filter-action :step) (default-action nil))
  (check-type filter-action (member :step :trace nil))
  (check-type default-action (member :step :trace nil))

  (setq *default-filter-action* filter-action
	*default-step-action* default-action)

  (advice+ 'run-pprocess 'stepper
    #'(lambda (previous)
	#'(lambda (p)
	    (let ((action (filter-action (run-pprocess-filter p))))
	      (when action
		(describe-pprocess-for-stepper "Run" p)
		(wait-for-user action))
	      (funcall previous p)))))

  (advice+ 'call-in-pprocess-env 'stepper
    #'(lambda (previous)
	#'(lambda (p fn)
	    (let ((action (filter-action (call-in-pprocess-env-filter p fn))))
	      (when action
		(describe-pprocess-for-stepper "Call in" p)
		(wait-for-user action))
	      (let ((results (multiple-value-list (funcall previous p fn))))
		(when action
		  (let ((*print-level* 3) (*print-length* 5))
		    (format *step-io* "~&results: ~S~%" results))
		  (wait-for-user action))
		(values-list results))))))

  ;; Continued ...

  ;; ... continued.

  (advice+ 'send-message 'stepper
    #'(lambda (previous)
	#'(lambda (sender receiver route contents)
	    (let ((action
		   (filter-action
		    (send-message-filter sender receiver route contents))))
	      (when action
		(format *step-io* "~&--> ~S to ~S via ~S: ~S~%"
			(pprocess-name sender) (pprocess-name receiver)
			route contents)
		(wait-for-user action))
	      (funcall previous sender receiver route contents)))))

  (advice+ 'send-immediate 'stepper
    #'(lambda (previous)
	#'(lambda (route receiver contents)
	    (let ((action
		   (filter-action
		    (send-immediate-filter route receiver contents))))
	      (when action
		(format *step-io* "~&**> ~S to ~S via ~S: ~S~%"
			(pprocess-name *pprocess*) (pprocess-name receiver)
			route contents)
		(wait-for-user action))
	      (funcall previous route receiver contents)))))

  (advice+ 'next-event 'stepper
    #'(lambda (previous)
	#'(lambda (&optional (p *pprocess*))
	    (let ((action (filter-action (next-event-filter p))))
	      (when action
		(describe-event-for-stepper
		  (format nil "<-- ~S dequeues:" (pprocess-name p))
		  (car (queue-contents (pprocess-event-queue p))))
		(wait-for-user action))
	      (funcall previous p)))))
  t)


(defun unstep-pprocesses ()
  (advice- :all 'stepper))


;;; [Un]Watch-pprocess/socket

;;; To see the current list, call e.g. (watch-pprocess t).

;;; /\/: Should the names be plural (e.g. watch-pprocesses)?

(defvar *watched-pprocess-alist* nil)
(defvar *watched-socket-alist* nil)

(defun pprocess-watch-action (pprocess)
  (cdr (assoc (pprocess-name pprocess) *watched-pprocess-alist*)))
  
(defun socket-watch-action (route)	;route is typically a socket name
  (cdr (assoc route *watched-socket-alist*)))

(defun watch-pprocess (action &rest names)
  ;; The fist arg is supposed to be an action, but we try to adjust if not.
  (when (and (not (member action '(:step :trace t)))
	     (exists-pprocess action))
    (push action names)
    (setq action t))
  (check-type action (member :step :trace t))
  (mapc #'find-pprocess names)		;make sure the pprocess names are valid
  (apply #'unwatch-pprocess names)	;remove old settings
  (setq *watched-pprocess-alist*
	(append *watched-pprocess-alist*
		(mapcar #'(lambda (name) (cons name action))
			names))))

(defun unwatch-pprocess (&rest names)
  (setq *watched-pprocess-alist*
	(if (null names)
	    nil
	  (set-difference
	    *watched-pprocess-alist* (mapcar #'list names) :key #'car))))

(defun watch-socket (action &rest names)
  ;; The fist arg is supposed to be an action, but we try to adjust if not.
  ;; /\/: Can't watch route :direct
  (when (and (not (member action '(:step :trace t)))
	     (exists-socket action))
    (push action names)
    (setq action t))
  (check-type action (member :step :trace t))
  (mapc #'find-socket names)		;make sure the socket names are valid
  (apply #'unwatch-socket names)	;remove old settings
  (setq *watched-socket-alist*
	(union *watched-socket-alist*
	       (mapcar #'(lambda (name) (cons name action))
		       names))))

(defun unwatch-socket (&rest names)
  (setq *watched-socket-alist*
	(if (null names)
	    nil
	  (set-difference
	    *watched-socket-alist* (mapcar #'list names) :key #'car))))


;;; Step filters
;;;
;;; The filter can return one of the following:
;;;
;;;   :step  -- step regardless of the default
;;;   :trace -- trace regardless of the default
;;;   t      -- obey *default-filter-action* 
;;;   nil    -- obey *default-step-action*
;;;
;;; In effect, t indicates the event is of interest but doesn't say
;;; whether it should be traced or stepped.  Nil is for not particularly
;;; interesting events, but they still might be stepped or traced.

(defun run-pprocess-filter (p)
  (pprocess-watch-action p))

(defun call-in-pprocess-env-filter (p fn)
  (declare (ignore fn))
  (pprocess-watch-action p))

(defun send-message-filter (sender receiver route contents)
  (declare (ignore contents))
  (let ((sender-action   (pprocess-watch-action sender))
	(receiver-action (pprocess-watch-action receiver))
	(route-action    (socket-watch-action   route)))
    (cond ((or (eq sender-action :step)
	       (eq receiver-action :step)
	       (eq route-action :step))
	   :step)
	  ((or (eq sender-action :trace)
	       (eq receiver-action :trace)
	       (eq route-action :trace))
	   :trace)
	  (t
	   (or sender-action receiver-action route-action)))))

(defun send-immediate-filter (route receiver contents)
  (declare (ignore contents))
  (let ((receiver-action (pprocess-watch-action receiver))
	(route-action    (socket-watch-action   route)))
    (cond ((or (eq receiver-action :step)
	       (eq route-action :step))
	   :step)
	  ((or (eq receiver-action :trace)
	       (eq route-action :trace))
	   :trace)
	  (t
	   (or receiver-action route-action)))))

(defun next-event-filter (p)
  (let ((receiver-action (pprocess-watch-action p))
	(route-action
	 (socket-watch-action
	  (message-route (car (queue-contents (pprocess-event-queue p)))))))
    (cond ((or (eq receiver-action :step)
	       (eq route-action :step))
	   :step)
	  ((or (eq receiver-action :trace)
	       (eq route-action :trace))
	   :trace)
	  (t
	   (or receiver-action route-action)))))

;;; Wait-for-user is called after the stepper has printed a description
;;; of an event that should be stepped or traced.  Wait-for-user decides
;;; whether it's just trace.  The argument "filter-action" is the result
;;; of one of the filters defined above, as processed by the filter-action
;;; macro.  The value should be either :step or :trace.  Random non-nil
;;; values are treated as :step.  Nil is an error.

(defparameter *step-help-message*
  "~&<ret>  -- continue~%~
     b      -- break:   enter a Lisp break loop~%~
     t      -- trace:   change :step defaults to :trace and continue~%~
     s      -- step:    change :trace defaults to :step and continue~%~
     f[stn] -- filter:  set default for interesting events~%~
     d[stn] -- default: set default for uninteresting events~%~
     g      -- go:      turn off the stepper completely~%~
   ~%~
     The f and d commands are followed by a single letter:~%~
     s for step, t for trace, or n for nil (do nothing).~%")

(defun wait-for-user (filter-action)
  (assert (not (null filter-action)))
  (prog ()
     (when (eq filter-action :trace)
       (return))
   ASK					;/\/: rewrite using loop?
     (format *step-io* "~&step> ")
     (let ((in (read-line *step-io*)))
       (unless (string-equal in "")
	 (case (schar in 0)
	   (#\b (break "pprocess stepper")
		(go ASK))
	   (#\t (change-step-defaults :step :trace))
	   (#\s (change-step-defaults :trace :step))
	   (#\f (setq *default-filter-action* (command-action in))
		(go ASK))
	   (#\d (setq *default-step-action* (command-action in))
		(go ASK))
	   (#\g (unstep-pprocesses))
	   (t   (step-help)
		(go ASK)))))
     (return)))

(defun step-help ()
  (format *step-io* "~&Default for interesting events:   ~S.~%"
	  *default-filter-action*)
  (format *step-io* "~&Default for uninteresting events: ~S.~%"
	  *default-step-action*)
  (format *step-io* *step-help-message*))

(defun change-step-defaults (from to)
  (when (eq *default-step-action* from)
    (setq *default-step-action* to))
  (when (eq *default-filter-action* from)
    (setq *default-filter-action* to)))
  
(defun command-action (cmd)
  (let ((command (schar cmd 0))
	(letter (if (> (length cmd) 1) (schar cmd 1) "<cr>")))
    (case letter
      (#\s :step)
      (#\t :trace)
      (#\n nil)
      (t
       (format *step-io*
	 "~&\"~A\" should be followed by \"s\", \"t\", or \"n\", not \"~A\"~%"
	 command letter)))))


;;; The stepper has its own "describe-" functions that sometimes print
;;; different information than the ordinary ones do.  The main difference
;;; at present is that events are described in more detail.

(defun describe-pprocess-for-stepper (prefix-text p)
  (format *step-io* "~&~A ~S ~S, status ~S"
	  prefix-text (type-of p) (pprocess-name p) (pprocess-status p))
  (unless (empty-queue-p (pprocess-event-queue p))
    (format *step-io* ", event queue:")
    (dolist (e (queue-contents (pprocess-event-queue p)))
      (describe-event-for-stepper " " e)))
  (when (pprocess-input-streams p)
    (format *step-io* "~&   input streams: ~S~%" (pprocess-input-streams p)))
  (when (pprocess-wakeup-time p)
    (format *step-io* "~&   wakeup time: ~S~%" (pprocess-wakeup-time p)))
  (when (pprocess-plist p)
    (format t "~&   plist: ~S~%" (pprocess-plist p)))
  (format *step-io* "~&")
  (values))

(defun describe-event-for-stepper (prefix-text e)
  (if (message-p e)
      (format *step-io* "~&~A ~S from ~S: ~S.~%"
	      prefix-text
	      (message-route e)
	      (pprocess-name (message-sender e))
	      (message-contents e))
      (format *step-io* "~&~A ~S.~%" prefix-text e)))


;;;; Micro version of advise

;;; The names ADVICE+ and ADVICE- have been chosen, in part, to
;;; minimize the chance of conflicts with any built-in advice system.

;;; A piece of advice is defined by a "wrapping function" that takes
;;; an existing function and returns a new one.  The new function usually
;;; calls the old.  A typical wrapping function therefore looks something
;;; like this:
;;;   #'(lambda (next)
;;;       #'(lambda (x) ... (funcall next x) ...))

;;; Advice is named.  It is hence possible to redefine or remove a
;;; particular piece of advice.  New advice is wrapped around all
;;; existing advice; that is, it gets to run first.  Redefinition
;;; is accomplished by calling ADVICE+ with the name of some existing
;;; advice.  This does not change its position in the nested wrappings.
;;; On the other hand, if advice is removed and then reintroduced it
;;; is treated as new advice and runs first.

(defvar *all-advised-functions* '()
  "The names of all functions that have ever been advised.")

(defmacro advice-plist (fn-name) `(get ,fn-name 'advice-plist))

(defun advice+ (fn-name advice-name wrapping-fn)
  (unless (member fn-name *all-advised-functions* :test #'eq)
    (setq *all-advised-functions*
	  (nconc *all-advised-functions* (list fn-name))))
  (if (advice-plist fn-name)
      (assert (get fn-name 'original-definition))
    (setf (get fn-name 'original-definition)
	  (symbol-function fn-name)))
  (if (getf (advice-plist fn-name) advice-name)
      (setf (getf (advice-plist fn-name) advice-name)
	    wrapping-fn)
    (setf (advice-plist fn-name)
	  (list* advice-name wrapping-fn (advice-plist fn-name))))
  (rewrap-advice fn-name)
  fn-name)

(defun advice- (&optional (fn-name :all) (advice-name :all))
  (mapcan (if (eq advice-name :all)
	      #'(lambda (f)
		  (when (advice-plist f)
		    (setf (symbol-function f) (get f 'original-definition)
			  (advice-plist f)    nil)
		    (list f)))
	      #'(lambda (f)
		  (when (getf (advice-plist f) advice-name)
		    (simple-advice- f advice-name)
		    (list f))))
	  (if (eq fn-name :all)
	      *all-advised-functions*
	      (list fn-name))))

(defun simple-advice- (fn-name advice-name)
  (remf (advice-plist fn-name) advice-name)
  (rewrap-advice fn-name)
  fn-name)

(defun rewrap-advice (fn-name)
  (labels ((wrap (plist f)
	     (if (null plist)
		 f
	       (let ((wrapping-fn (cadr plist)))
		 (funcall wrapping-fn
			  (wrap (cddr plist) f))))))
    (assert (get fn-name 'original-definition))
    (setf (symbol-function fn-name)
	  (wrap (advice-plist fn-name)
		(get fn-name 'original-definition)))))


;;; Pico (and obsolete) version of advise:

#+:undef
(defun add-advice (fn-name wrapping-fn)
  (let ((current-definition (symbol-function fn-name)))
    (unless (get fn-name 'original-definition)
      (setf (get fn-name 'original-definition) current-definition))
    (setf (symbol-function fn-name)
	  (funcall wrapping-fn current-definition))))

#+:undef
(defun remove-advice (fn-name)
  (prog1 (setf (symbol-function fn-name) (get fn-name 'original-definition))
    (remprop fn-name 'original-definition)))


;;;; A read-eval-print-loop pprocess

;;; Note that we have to deal with an annoying [A]KCL bug.  If you
;;; type in a form that READ can find the end of without looking at
;;; the first character after the form (eg, if you type a list or
;;; string as opposed to, say, a number or symbol), then the newline
;;; after the form stays in the input buffer and makes LISTEN return
;;; true.  CLEAR-INPUT doesn't clear it.  So we have to do it by hand.

(defvar *repl-io*)

(defstruct (repl (:include pprocess)
		 (:constructor %make-repl)
		 (:print-function print-pprocess))
  (io t)				; an i/o stream
  (prompt "form> ")			; a string
  (start-fn nil))			; a function or nil

(defun new-repl (&key (name :lisp-listener)
		      (io t)
		      (prompt "form> ")
		      (start-fn nil))
  (when (eq io 't)			; format thinks t = standard-output
    (setq io (make-synonym-stream '*terminal-io*)))
  (let* ((*repl-io* io)
	 (*standard-input* (make-synonym-stream '*repl-io*))
	 (*standard-output* (make-synonym-stream '*repl-io*))
	 (*debug-io* (make-synonym-stream '*repl-io*))
	 (*query-io* (make-synonym-stream '*repl-io*))
	 (*trace-output* (make-synonym-stream '*repl-io*))
	 (*error-output* (make-synonym-stream '*repl-io*)))
    (register-pprocess
      (%make-repl
        :name name
	:status :run			; just to get started
	:io io
	:prompt prompt
	:start-fn start-fn
	:input-streams (list io)
	:specials '(*repl-io* *standard-input* *standard-output*
		    *debug-io* *query-io* *trace-output* *error-output*
		    - + ++ +++ / // /// * ** ***)
	:run-function
	  ;; Call the start-fn, output the first prompt, then change
	  ;; status and run-function so that we can wait for input
	  ;; and eval when it comes.
          #'(lambda (self)
	      (when (repl-start-fn self)
		(funcall (repl-start-fn self) self))
	      (format (repl-io self) "~%~A" prompt)
	      #+Allegro
	        (force-output (repl-io self))
	      #+kcl
	        (really-clear-whitespace (repl-io self))
	      (setf (pprocess-status self) :run-on-event
		    (pprocess-run-function self) 'repl-handler))))))

(defun repl-handler (self)
  (declare (special - + ++ +++ / // /// * ** ***))
  (shiftf +++ ++ + - (repl-read (repl-io self)))
  (shiftf /// // / (multiple-value-list (repl-eval -)))
  (shiftf *** ** * (first /))
  (format (repl-io self) "~{~S~^~%~}" /)
  (format (repl-io self) "~%~A"
	  (repl-prompt self))		; for next time
  #+Allegro
    (force-output (repl-io self))
  )



(defun repl-read (stream)
  (prog1 (read stream)
    #+kcl
     (really-clear-whitespace stream)))

;;; Repl-eval sets up a simple restart in those Lisps that have them.

(defun repl-eval (form)
  (if (macro-function 'with-simple-restart)
      (with-simple-restart (abort "Return to O-Plan")
	(eval form))
    (eval form)))

(defun really-clear-whitespace (stream)
  (loop
    (let ((c (read-char-no-hang stream nil nil nil)))
      (cond ((null c)
	     (return))
	    ((not (whitespace-p c))
	     (unread-char c stream)
	     (return))))))

(defun whitespace-p (c)
  (member c '(#\space #\newline #\tab #\return) :test #'char=))


;;;; Connectors
;;;
;;; A connector is a kind of p-process used to connect a Lisp io-stream
;;; to a (pseudo) socket or pair of (pseudo) sockets.  There can be a
;;; pair of sockets because some sockets (e.g. O-Plan's :leftin and :leftout)
;;; are used in only one direction.  A connector is created as follows:
;;;
;;;   (NEW-CONNECTOR
;;;     :IO-STREAM stream
;;;     :SOCKET-TO-READ socket-name
;;;     :SOCKET-TO-WRITE socket-name)
;;;
;;; When there's a pair of sockets, the connector works as follows.
;;; When something is written to the socket-to-read, the connector receives
;;; it and then prints it on the io-stream.  This implies that the connector
;;; is the server of socket-to-read.  When input appears on the io-stream, 
;;; as indicated by LISTEN, the connector reads it and then writes it to the
;;; socket-to-write.  This implies that the connector is a client of socket-
;;; to-write.
;;;
;;; Note that the connection will be one-way if either socket-to-read
;;; or socket-to-write is nil.  (This is sometimes desirable.)  The
;;; server / client convention described above is still followed.
;;;
;;; However, if a connector is given only a single socket (i.e. the
;;; same socket as both socket-to-read and socket-to-write), it doesn't
;;; make sense for the connector to be both server and client.  So in
;;; this case, it becomes the server.  Items written to the socket are
;;; printed on the stream as before, and input that appears on the
;;; stream is read and then sent to all clients of the socket.
;;;
;;; A connector can be given an input-guard function.  The guard is
;;; called on any input read from the io-stream and returns two values:
;;; a true/false indication of whether the input was acceptable, and
;;; the possibly transformed value that should actually be sent to the
;;; socket.  It is up to the guard to signal an error (if that's desirable)
;;; when the input is not acceptable.

(defparameter *default-connector-specials*
  '(*debug-io* *trace-output* *error-output*))

(defstruct (connector (:constructor %make-connector)
		      (:print-function print-pprocess)
		      (:include pprocess
			 (specials *default-connector-specials*)
			 (status :run-on-event)
			 (run-function
			   #'connector-event-handler)))
  io-stream		;stream
  socket-to-read	;socket name or nil
  socket-to-write	;socket name of nil
  (input-guard		;fn: connector, input --> ok-p, transformed-input
   'default-input-guard)
  (debug-io		;stream
   (make-synonym-stream '*terminal-io*))
  )

(defun default-input-guard (input)
  (values t input))
  
(defun new-connector (name &rest other-initargs)
  (let ((*debug-io* *debug-io*)
	(*trace-output* (make-synonym-stream '*debug-io*))
	(*error-output* (make-synonym-stream '*debug-io*)))
    (let ((c (apply #'%make-connector :name name other-initargs)))
      (register-pprocess c)
      ;; If we have somewhere to send input from the stream, arrange
      ;; to listen to the stream.
      (when (connector-socket-to-write c)
	(pushnew (connector-io-stream c)
		 (pprocess-input-streams c)))
      ;; The stream to use for *debug-io* is passed in via the
      ;; :debug-io initarg, and here we make it the pprocess-local
      ;; value of the variable.
      (call-in-pprocess-env c
	#'(lambda (c)
	    (setq *debug-io* (connector-debug-io c))))
      ;; Establish socket roles
      (let ((r (connector-socket-to-read c))
	    (w (connector-socket-to-write c)))
	(when r
	  (ensure-socket r)
	  (register-server r c))
	(when w
	  (unless (eq r w)
	    (ensure-socket w)
	    (register-client w c))))
      c)))

(defun connector-event-handler (self)
  (when (next-event-p self)
    (transmit-from-socket-to-stream self))
  (when (and (connector-socket-to-write self)
	     (listen (connector-io-stream self)))
    (transmit-from-stream-to-socket self)))

(defun transmit-from-socket-to-stream (self)
  (let ((out (connector-io-stream self))
	(e (next-event self)))
    (write (message-contents e) :stream out
	   ;; /\/ used to be :pretty t.
	   :escape t :pretty nil :length nil :level nil)
    (terpri out)
    (force-output out)))

(defun transmit-from-stream-to-socket (self)
  (let* ((in (connector-io-stream self))
	 (input (read in)))
    (really-clear-whitespace in)
    ;; The input-guard can check and transform the input.
    (multiple-value-bind (ok-p transformed-input)
	(funcall (connector-input-guard self) self input)
      (when ok-p
	(let ((r (connector-socket-to-read self))
	      (w (connector-socket-to-write self)))
	  (if (eq r w)
	      ;; Send to all clients of w
	      (dolist (c (socket-clients (find-socket w)))
		(send-message self c w transformed-input))
	    ;; Send to w
	    (send-to-socket
	      w
	      transformed-input)))))))

;;; End
