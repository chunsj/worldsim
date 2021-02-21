;;;; File: world-initsystem.lsp
;;; SCCS Version: %W%
;;; Contains: Window and IPC configuration and initialization
;;; Authors: Jeff Dalton and Richard Kirby
;;; Created: Mon Jan 22 12:17:30 1990
;;; Updated: Mon Oct  9 03:22:21 1995 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1992, 1993, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :initsystem :nicknames '(:is))

(use-package :util)
(use-package :xwindowio)
(use-package :ipcinterface)
(use-package :parser-kit)

(export '(is-init-parameter-defaults))
(export '(is-init-env-parameters))
(export '(is-connect-p is-set-connect))
(export '(is-exec-p is-set-exec-p))
(export '(is-subr-p))

(export '(is-init is-get-config-entry))
(export '(is-get-window-args is-get-window-width))
(export '(is-set-config-file is-configure))

(export '(is-patch-loader))


;;;; Parameter initialization.

(defparameter *parameter-defaults*
  ;;  Parameter-name   Default-value
  '( (:menu-geometry   "+100+150")
     (:interactive     t         )
     (:separate-world  t         )))

(defparameter *parameter-env-map*
  ;;  Paramater-name   Env-variable   Status (:optional or :required)
  '( (:world-dir       "WORLDDIR"     :required)
     (:world-tmp-dir   "WORLDTMPDIR"  :required) ))

(defun is-init-parameter-defaults ()
  (dolist (d *parameter-defaults*)
    (apply #'set-parameter d)))

(defun is-init-env-parameters ()
  ;; Initialize from the environment
  (dolist (m *parameter-env-map*)
    (apply #'init-env-parameter m))
  ;; Apply defaults for :optional env variables
  ;; None at present.  The format is:
  ;;   (default-world-directory :world-something "somedir/subdir")
  nil)

(defun init-env-parameter (p env-var status)
  (set-parameter p
    (ecase status
      (:required (require-env env-var))
      (:optional (getenv env-var)))))

(defun require-env (name)
  (or (getenv name)
      (ask-user-for-env name)))

(defun ask-user-for-env (name)
  (format t "Environment variable ~A is not defined.~%" name)
  (format t "Please enter a value: ")
  (read-line t))

(defun default-world-directory (parameter relative-default)
  ;; The relative-default is relative to :world-dir
  (set-parameter parameter
    (or (get-parameter parameter)
	(concat-string (get-parameter :world-dir) "/" relative-default))))


;;; Routines related to command-line arguments

(defun is-set-connect (value)
  (set-parameter :connect value))

(defun is-connect-p ()
  (get-parameter :connect))

(defun is-set-config-file (filename)
  (set-parameter :config filename))

(defun is-subr-p ()
  (get-parameter :subr))


;;;; .config file processing

;;; The user can specify the .config file by using the "-config"
;;; command-line argument.  If so, the specified filename is saved
;;; as the value of the :config parameter.  If not, this parameter
;;; will be nil, which means to use the default .config file.
;;; Note that using :config <name> on the command line would also
;;; work.

;;; A user-specified .config file name is first tried as-is and then
;;; relative to $WORLDDIR/lib.  If the file is not found in either 
;;; case, the default .config file is used instead.

;;; The user-specified name needn't end in ".config".  If some other
;;; file type is present, it will be used; otherwise ".config" will
;;; be added via merge-pathnames.  With this mechanism, it is not
;;; possible to refer to a file with no type in its name.

(defparameter *world-config-file*   "world-default.config")

(defvar *config-records* nil)

(defun is-configure ()
  (let* ((std-dir (concat-string (get-parameter :world-dir) "/lib/"))
	 (default *world-config-file*))
    ;; Process the config file.
    (setq *config-records* nil)
    (process-config-file
      (or (and (get-parameter :config)
	       (find-config-file (get-parameter :config) std-dir))
	  (concat-string std-dir default)))))

(defun find-config-file (filename std-dir)
  (let ((config-file
	 (merge-pathnames filename
			  (make-pathname :type "config"))))
    ;; Try filename in the current directory first, then in the standard
    ;; directory.
    (or (probe-file config-file)
	(probe-file (concat-string std-dir (namestring config-file)))
	(error "Can't find config file ~S." filename))))

(defun process-config-file (filename)
  (with-open-file (str filename :direction :input)
    (multiple-value-bind (records error-count)
	(read-config-records str)
      (if (> error-count 0)
	  (error "~D errors in config file ~S." error-count filename)
	(setq *config-records*
	      (append *config-records* records))))))

(defun read-config-records (instream)
  (labels
      ((<config-file> ()
	 (one-or-more #'<config-entry> :until :eof))
       (<config-entry> ()
	 (must-be :config)
	 (cons (<name> "a component name")
	       (one-or-more #'<config-line> :until :end)))
       (<config-line> ()
	 (token-case
	   ((:window-args) (<window-args-line>))
	   ((:window)      (<window-line>))
	   ((:socket)      (<socket-line>))
	   (t (syntax-error "Invalid config entry starting with ~S."
			    (token)))))
       (<window-args-line> ()
         (list :window-args
	       (must-satisfy #'keywordp "a window tag")
	       (must-satisfy #'listp    "a list of window arguments")))
       (<window-line> ()
	 (list :window
	       (must-satisfy #'keywordp "a window tag")
	       (must-satisfy #'stringp  "a window title")
	       (must-satisfy #'numberp  "an x position")
	       (must-satisfy #'numberp  "a y position")
	       (must-satisfy #'numberp  "a width")
	       (must-satisfy #'numberp  "a height")
	       (must-satisfy #'stringp  "a string of xterm args")))
       (<socket-line> ()
	 (list :socket (<socket-role>) (<name> "a socket-name")))
       (<socket-role> ()
	 (must-be-member '(:server :client) "a socket role" "socket roles"))
       (<name> (description)
	 (must-satisfy #'keywordp description)))
    (let ((*end-token* :eof))
      (test-compile
        #'<config-file>
	#'(lambda ()
	    (read instream nil *end-token*))))))


;;;; Init routines

(defun is-init (who)
  "This is the main init function for setting up the supporting structure.
   ie, the communication channels, and the IO windows.
   Arguments:
     who - Process to set up. This is a string which is used to search the
           config file to find the set up info.
   Returns:
     t or nil, to indicate success or failure.
   Side Effects:
     Underlying system would have been set up."

  (let ((entry (assoc who *config-records*)))
    (when entry
      (dolist (line (cdr entry))
	(ecase (car line)
	  (:window-args nil)		;handled by the process
	  (:window (is-setup-window (cdr line)))
	  (:socket (is-setup-socket (cdr line)))))
      t)))

(defun is-get-config-entry (who)
  "Return the config entry for a proccess, or nil if there is none."
  (cdr (assoc who *config-records*)))

(defun is-get-window-args (window-tag)
  "Get the list of arguments from a :window-args line."
  (let ((window-line (find-config-line :window-args window-tag)))
    (if window-line
	(caddr window-line)
      nil)))

(defun is-get-window-width (window-tag)
  "Get the width of a :WINDOW window."
  (let ((window-line (find-config-line :window window-tag)))
    (if window-line
	(nth 5 window-line)
      (error "Can't find window ~S." window-tag))))

(defun find-config-line (type tag)
  ;; N.B. Assumes unique tag for a given type.  Two processes can't
  ;; have windows with the same tag, for instance.
  (some #'(lambda (config-record)
	    (find-if #'(lambda (line)
			 (and (eq (car line) type)
			      (eq (cadr line) tag)))
		     (cdr config-record)))
	*config-records*))

(defun is-setup-window (win)
  "Sets up a window for IO, taking the required info from the config file.
   The structure of the config file entry is :-
     :WINDOW <tag> <title> <xpos> <ypos> <width> <height> <string of extras>
   Arguments:
     win - A list describing the window and containing the same sequence
           of items as the entry in the file.
   Results: t or nil.
   Side Effects: A window should now be open with a stream set up."
  (multiple-value-bind (tag title xpos ypos width height extras)
                       (values-list win)
    ;; /\/ This allows the geometry entries to be nil, but the parser
    ;; in read-config-records (above) doesn't.
    (let ((string
	   (if (and xpos ypos width height)
	       (format nil " -title ~S -geometry ~Dx~D+~D+~D ~A"
		              title width height xpos ypos extras)
	       (format nil " -title ~S ~A" title extras))))
      (x-open-and-register-io-win tag string))))

(defun is-setup-socket (soc)
  "Sets up a socket for IPC, taking the required info from the config file.
   The structure of the config file entry is :-
     :SOCKET {:server | :client} <file name>
   Arguments:
     soc - A list describing the socket and containing the same sequence
           of items as the entry in the file.
   Results:
     t or nil.
   Side Effects:
     A socket should be either created (if this is the server end),
     or connected to (if this is the client end)."
  (let* ((mode (first soc))
	 (name (second soc)))
    (ecase mode
      (:server
       ;; This process is a server, so set up the socket.
       (ipc-set-up-and-register-server name))
      (:client
       ;; This process is a client of a socket, so register it.
       (ipc-set-up-and-register-client name)))))


;;; Define is-patch-loader to load .o files if they exist and are
;;; newer than the source.

(defun is-patch-loader ()
  "Checks if any patches to load in."
  (unless (get-parameter :no-patches)
    (let ((files
	   (find-all-files
	     (concat-string (get-parameter :world-dir) "/patch/")
	     "lsp")))
      (dolist (file files)
	(load-most-recent file)))))

;;; ---------------------------- Change History ----------------------------
;;; (Initials) (Date of change)           (Comment)
