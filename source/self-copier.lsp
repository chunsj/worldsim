;;; WorldSim "self-copier" or, rather, parts of it.

(in-package :world)

(import '(simple-defsystem::file-source-name
	  simple-defsystem::system-files
	  simple-defsystem::find-all-required-systems
	  simple-defsystem::find-system))

(defun write-copy-script (&optional (filename "copy-source"))
  ;; /\/: Explicit call to namestring avoids a KCL bug:
  ;; the ~A output begins with "#" when the directory part is
  ;; empty.
  (with-open-file (out filename :direction :output)
    (dolist (f (all-world-files))
      (format out "~&cp -p ~A $1/~A~%"
        (namestring (file-source-name f))
	(namestring (file-source-name f))))))

(defun write-manifest (filename)
  (with-open-file (out filename :direction :output)
    (dolist (f (all-world-files))
      (princ (file-source-name f) out))
    (terpri out)))

(defun all-world-files ()
  (mapcan #'(lambda (sys)
	      (copy-list (system-files sys)))
	  (find-all-required-systems (find-system 'world))))
