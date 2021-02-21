;;; System definition for the Pacifica world model

(in-package :world)

(defsystem (pacifica-world
	    :required-systems (pacifica-parameters
			       pacifica-base
			       pacifica-defs))
  )

(defsystem (pacifica-parameters)

  ("DEF-NEO"
    :source-name "DEF-NEO.lisp")

  )

(defsystem (pacifica-defs :required-systems (pacifica-parameters))
    
  ("EEM-NEO"
    :source-name "EEM-NEO.lisp"
    :defines (:functions))
  ("MSG-NEO"
    :source-name "MSG-NEO.lisp"
    :defines (:messages :functions))
  ("OBJ-NEO"
    :source-name "OBJ-NEO.lisp"
    :defines (:classes :methods :functions :objects))

  )

(defsystem (pacifica-base :required-systems (pacifica-parameters))

  (pacifica-utils
    :defines (:functions))
    
  (eem
    :defines (:functions))
  (eem-script-reader
    :defines (:functions))		;some internal vars

  (probability
    ;; calls make-random-state when loaded
    :defines (:functions))

  )

;;; End
