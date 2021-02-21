;;; Some things that don't fit naturally anywhere else

(in-package :world)

(defun choose-random-value (value-list)
  (let* ((count (length value-list))
         (pos (random count)))
    (nth pos value-list)))

;;; End
