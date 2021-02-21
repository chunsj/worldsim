;;;; File: save-image.lsp
;;; SCCS Version: %W%
;;; Contains: save-image procedure for various Lisps
;;; Author: Jeff Dalton <J.Dalton@ed.ac.uk>
;;; Created: November 1994
;;; Updated: Sun Nov  6 17:28:06 1994 by Jeff Dalton
;;; Release Version: %Y%
;;; Copyright: (c) 1994, AIAI, University of Edinburgh
;;; This material may be reproduced by or for the U.S. Government pursuant
;;; to the copyright license under the clause at DFARS 252.227-7032
;;; (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

(in-package :util)

(export 'save-image)


;;; AKCL version

#+akcl
(progn
  
  (defun save-image (filename top-level-fn)
    (setf (symbol-function 'si:top-level)
	  top-level-fn)
    (si:save-system filename))

  )

;;; Lucid version

#+lucid
(progn

  (defun save-image (filename top-level-fn)
    (lcl:disksave filename
      :restart-function top-level-fn
      :full-gc t
      :verbose t))

  )

;;; Allegro version

#+:allegro
(progn

  (defun save-image (filename top-level-fn)
    (excl:dumplisp
      :name filename
      :checkpoint nil
      :read-init-file t
      :ignore-command-line-arguments t
      :restart-function top-level-fn))
  
  )


;;; ---------------------------- Change History ----------------------------
;;; (Who)   (When) 		     (What)
;;;
