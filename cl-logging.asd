;;;; cl-logging.asd
;;;;
;;;; This file is part of the common lisp library - check-links, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Katrevich Sergey <linkfly1@newmail.ru>

(defsystem :cl-logging
  :version "0.0.1"
  :depends-on (:lift :cl-fad :local-time :iterate :anaphora :alexandria)
  :components ((:module "src"			
			:components ((:file "logger")
				     (:file "save-init-hooks-port" :depends-on ("logger"))))))
				     
