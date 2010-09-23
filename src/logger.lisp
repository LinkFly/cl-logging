(defpackage :logging 
  (:use :cl :lift :cl-fad :local-time :iterate :anaphora :alexandria)
  (:shadowing-import-from :cl-fad #:copy-stream #:copy-file)
  (:export #:define-logging
	   #:define-default-logs
	   #:log-type-message
	   #:open-log-types-streams
	   #:close-log-types-streams
	   #:switch-log-type	   #:enable-log-type
	   #:for-test-created-logs
	   #:for-test-generated-functions))

(in-package :logging)
(deftestsuite logging-tests () 
  ()
  (:function 
   (get-test-data-path ()
		       (cl-fad:pathname-as-directory
			(merge-pathnames "test-logger" *default-pathname-defaults*))))
  (:function 
   (delete-test-directory ()
		       (cl-fad:delete-directory-and-files (get-test-data-path))))
  (:function
   
;(defun equal-with-ignore-unpackage-symbols (tree1 tree2)
   (equal-with-ignore-unpackage-symbols
    (tree1 tree2)
    (labels ((both-syms-unpackaged (sym1 sym2)
	       (and (null (symbol-package sym1))
		    (null (symbol-package sym2))))
	     (walk-trees (tree1 tree2 predicat)
	       (cond 
		 ((and (atom tree1) 
		       (atom tree2))
		  (funcall predicat tree1 tree2))
		 ((and (consp tree1) 
		       (consp tree2))
		  (and (walk-trees (first tree1) (first tree2) predicat)
		       (walk-trees (rest tree1) (rest tree2) predicat)))))
	     (equal-without-unpkg-syms (x y)
	       (cond
		 ((and (symbolp x)
		       (symbolp y))
		  (if (both-syms-unpackaged x y)
		      t
		      (equal x y)))
		 ((and (not (symbolp x))
		       (not (symbolp y)))
		  (equal x y)))))
      (walk-trees tree1 tree2 #'equal-without-unpkg-syms)))

#|(equal-with-ignore-unpackage-symbols 
 '(DEFINE-LOGGING
   (LET ((#:LOGS-DIR5241 "logs"))
     (IF (ABSOLUTE-PATHNAME-P #:LOGS-DIR5241)
	 #:LOGS-DIR5241
	 (PATHNAME-AS-DIRECTORY
	  (MERGE-PATHNAMES #:LOGS-DIR5241)))))
 '(DEFINE-LOGGING
   (LET ((#:LOGS-DIR23455241 "logs"))
     (IF (ABSOLUTE-PATHNAME-P #:LOGS-DIR5241323)
	 #:LOGS-DIR524134
	 (PATHNAME-AS-DIRECTORY
	  (MERGE-PATHNAMES #:LOGS-DIR524134))))))
|#
))

(defun log-type-message (type log-types-streams log-types-switches fmt-message &rest args)
  (when (enable-log-type-p type log-types-switches)
    (awhen (getf log-types-streams type)
      (format it
	      "~&(:~A (~A) :time \"~A\")~%"
	      (package-name (load-time-value *package*))
	      (apply #'format nil fmt-message args)
	      (local-time:now))
      (finish-output it)
      )))
(addtest log-type-message-test
  (ensure
   (list
    (subseq (with-output-to-string (s)
	      (log-type-message :my-type `(:my-type ,s) '(:my-type t) "log-message. arg: ~a" 'is-arg))
	    0 44)
    "(:LOGGING (log-message. arg: IS-ARG) :time \"")))

(defun open-log-types-streams (&key log-types-streams prefix-logs place (types '(:info :warn :error)))  
;  (break "place: ~S" place)
  (unless place (setf place *standard-output*))
  (when log-types-streams (close-log-types-streams log-types-streams :types types))
  (setq place
	(cl-fad:pathname-as-directory
	 (cond 
	   ((typep place 'stream) 
	    (return-from open-log-types-streams
	      (if (not log-types-streams)
		  (mapcan #'(lambda (type) (list type place)) types)		  
		  (dolist (type types log-types-streams)
		    (setf (getf log-types-streams type) place)))))
	   ((or (typep place 'string)
		(typep place 'pathname))
	    (pathname place)))))
  ;(break "place: ~S" place)	 
  (ensure-directories-exist place)
  (loop 
     with log-types-streams = (or log-types-streams)
     for type in types 
;     unless (aif (getf log-types-streams type)
;		 (open-stream-p it))
     do (setf (getf log-types-streams type)
	      (open (merge-pathnames (concatenate 'string
						  prefix-logs
						  (when prefix-logs "-")
						  (string-downcase (symbol-name type)))
				     place)
		    :direction :output
		    :if-does-not-exist :create
		    :if-exists :append))
     finally (return log-types-streams)))
(addtest open-log-types-streams-test
  (ensure
   (let* ((prefix "test-prefix")
	  (types '(:info :warn :my-type))
	  (types-streams 
	   (open-log-types-streams :place (get-test-data-path)
				   :types types
				   :prefix-logs prefix)))
     (prog1 
	 (for-test-created-logs (get-test-data-path)
				:log-types types
				:prefix-logs prefix)
       (close-log-types-streams types-streams)
       (delete-test-directory)))))

(defun close-log-types-streams (log-types-streams &key (types '(:info :warn :error)))
	 (loop for type in types 
	    do (aif (getf log-types-streams type)
		    (when (open-stream-p it)
		      (close it)))
	    finally (return t)))
(addtest close-log-types-streams-test
  (ensure
   (flet ((open-test-file (file)
	    (open (make-pathname :defaults (get-test-data-path) 
				 :name (string file))
		  :direction :output :if-does-not-exist :create :if-exists :append)))
     (ensure-directories-exist (get-test-data-path))
     (let ((types-streams
	    (mapcan #'(lambda (type)
			(list type (open-test-file type)))
		    '(:info :warn :error))))       
       (close-log-types-streams types-streams)
       (prog1 
	   (notany #'identity (mapcar #'open-stream-p 
				      (remove-if #'keywordp types-streams)))
	 (mapc (compose #'delete-file #'pathname) 
	       (remove-if #'keywordp types-streams)))))))

(defun switch-log-type (type value-p log-types-switches)
  (setf (getf log-types-switches type) value-p)
  log-types-switches)

(defun enable-log-type-p (type log-types-switches)
  (getf log-types-switches type))

(defun for-test-created-logs (path &key prefix-logs (log-types '(:info :warn :error)))
  (loop for file in (mapcar #'(lambda (type)
				 (make-pathname :name
						(concatenate 'string 
							     prefix-logs
							     (when prefix-logs "-")
							     (string-downcase (as-string type)))
						:defaults path))
			    log-types)
      always (probe-file file)))

(defun for-test-generated-functions (types &optional prefix-functions)
  (loop for function in (mapcar #'(lambda (type) 
				    	  (symcat 
					   prefix-functions
					   (when prefix-functions "-")
					   "LOG-"
					   type))
				types)
     always (fboundp function)))

(defmacro define-logging (place &key 
			  prefix-functions
			  prefix-logs
			  (log-types '(:info :warn :error))
			  disable-log-types)
  (labels ((get-prefix ()
	     (concatenate 'string  prefix-functions (when prefix-functions "-")))
	   (fn-gen-fn-name (&optional (prefix-before-log ""))
	     #'(lambda (log-type)
		 (symcat (get-prefix) 
			 prefix-before-log
			 (unless (zerop (length prefix-before-log)) "-")
			 "LOG-"
			 log-type)))
	   (gen-fn-name-enable-p (log-type)
	     (symcat (get-prefix) "LOG-" log-type "-ENABLE-P"))
	   (gen-true-symbol (symbol) 
	     (add-pkg-prefix symbol *package*))
	   (*log-types-streams* () (gen-true-symbol '*log-types-streams*))
	   (*log-types-switches* () (gen-true-symbol '*log-types-switches*))
	   (open-log-streams () (gen-true-symbol 'open-log-streams))
	   (close-log-streams () (gen-true-symbol 'close-log-streams))
	   (log-message () (gen-true-symbol 'log-message)))
    
;    (break "log-tp: ~s" log-types)
;    (setq log-types (replace-many :std-log-types '(:info :warn :error) log-types))					
    `(progn 
       (defparameter ,(*log-types-streams*) nil)
       (defparameter ,(*log-types-switches*)
	 ',(mapcan #'(lambda (type) (list type t))
		   log-types))
       (dolist (type ,disable-log-types)
	 (setf (getf ,(*log-types-switches*) type) nil))
       
       (defun ,(open-log-streams) (&key (place ,place) (types ',log-types))
	 (setf ,(*log-types-streams*) 
	       (open-log-types-streams 
		:place place
		:types types
		:prefix-logs ,prefix-logs)))

       (defun ,(close-log-streams) (&key (types ',log-types))
	 (close-log-types-streams ,(*log-types-streams*) :types types))
	    
       (defun ,(log-message) (type fmt-message &rest args)
	 (apply #'log-type-message type ,(*log-types-streams*) ,(*log-types-switches*) fmt-message args))

       ,@(loop for type in log-types 
	    collect `(defun ,(funcall (fn-gen-fn-name) type) (fmt-message &rest args)
		       (apply #',(log-message) ,type fmt-message args)))
#|
       ,@(loop for (fn-type value-p) in '(("ENABLE" t) ("DISABLE" nil))
	      (loop for type in log-types
		 collect `(defun ,(funcall (fn-gen-fn-name "ENABLE") type) ()
			 (switch-log-type ,type t ,(*log-types-switches*)))))
|#

       ,@(loop for type in log-types
	    collect `(defun ,(funcall (fn-gen-fn-name "ENABLE") type) ()
			 (switch-log-type ,type t ,(*log-types-switches*))))

       ,@(loop for type in log-types
	    collect `(defun ,(funcall (fn-gen-fn-name "DISABLE") type) ()
			 (switch-log-type ,type nil ,(*log-types-switches*))))
       
       ,@(loop for type in log-types
	    collect `(defun ,(gen-fn-name-enable-p type) ()
			 (enable-log-type-p ,type ,(*log-types-switches*))))
       (,(open-log-streams))

       (save-init-hooks :save (symbol-function ',(close-log-streams))
			:init (symbol-function ',(open-log-streams))))))
(addtest define-logging-test
  (ensure-same
   (macroexpand-1 '(define-logging
		    (merge-pathnames "test-logs-dir" *default-pathname-defaults*)
		    :log-types (:info :warn :error :bad-links :details)
		    :prefix-logs "check-links"))
   '(PROGN
    (DEFPARAMETER *LOG-TYPES-STREAMS* NIL)
    (DEFPARAMETER *LOG-TYPES-SWITCHES*
      '(:INFO T :WARN T :ERROR T :BAD-LINKS T :DETAILS T))
    (DOLIST (TYPE NIL) (SETF (GETF *LOG-TYPES-SWITCHES* TYPE) NIL))
    (DEFUN OPEN-LOG-STREAMS
           (&KEY
            (PLACE
             (MERGE-PATHNAMES "test-logs-dir" *DEFAULT-PATHNAME-DEFAULTS*))
            (TYPES (QUOTE (:INFO :WARN :ERROR :BAD-LINKS :DETAILS))))
      (SETF *LOG-TYPES-STREAMS*
              (OPEN-LOG-TYPES-STREAMS :PLACE PLACE :TYPES TYPES :PREFIX-LOGS
                                      "check-links")))
    (DEFUN CLOSE-LOG-STREAMS
           (&KEY (TYPES (QUOTE (:INFO :WARN :ERROR :BAD-LINKS :DETAILS))))
      (CLOSE-LOG-TYPES-STREAMS *LOG-TYPES-STREAMS* :TYPES TYPES))
    (DEFUN LOG-MESSAGE (TYPE FMT-MESSAGE &REST ARGS)
      (APPLY #'LOG-TYPE-MESSAGE TYPE *LOG-TYPES-STREAMS* *LOG-TYPES-SWITCHES*
             FMT-MESSAGE ARGS))
    (DEFUN LOG-INFO (FMT-MESSAGE &REST ARGS)
      (APPLY #'LOG-MESSAGE :INFO FMT-MESSAGE ARGS))
    (DEFUN LOG-WARN (FMT-MESSAGE &REST ARGS)
      (APPLY #'LOG-MESSAGE :WARN FMT-MESSAGE ARGS))
    (DEFUN LOG-ERROR (FMT-MESSAGE &REST ARGS)
      (APPLY #'LOG-MESSAGE :ERROR FMT-MESSAGE ARGS))
    (DEFUN LOG-BAD-LINKS (FMT-MESSAGE &REST ARGS)
      (APPLY #'LOG-MESSAGE :BAD-LINKS FMT-MESSAGE ARGS))
    (DEFUN LOG-DETAILS (FMT-MESSAGE &REST ARGS)
      (APPLY #'LOG-MESSAGE :DETAILS FMT-MESSAGE ARGS))
    (DEFUN ENABLE-LOG-INFO () (SWITCH-LOG-TYPE :INFO T *LOG-TYPES-SWITCHES*))
    (DEFUN ENABLE-LOG-WARN () (SWITCH-LOG-TYPE :WARN T *LOG-TYPES-SWITCHES*))
    (DEFUN ENABLE-LOG-ERROR () (SWITCH-LOG-TYPE :ERROR T *LOG-TYPES-SWITCHES*))
    (DEFUN ENABLE-LOG-BAD-LINKS ()
      (SWITCH-LOG-TYPE :BAD-LINKS T *LOG-TYPES-SWITCHES*))
    (DEFUN ENABLE-LOG-DETAILS ()
      (SWITCH-LOG-TYPE :DETAILS T *LOG-TYPES-SWITCHES*))
    (DEFUN DISABLE-LOG-INFO ()
      (SWITCH-LOG-TYPE :INFO NIL *LOG-TYPES-SWITCHES*))
    (DEFUN DISABLE-LOG-WARN ()
      (SWITCH-LOG-TYPE :WARN NIL *LOG-TYPES-SWITCHES*))
    (DEFUN DISABLE-LOG-ERROR ()
      (SWITCH-LOG-TYPE :ERROR NIL *LOG-TYPES-SWITCHES*))
    (DEFUN DISABLE-LOG-BAD-LINKS ()
      (SWITCH-LOG-TYPE :BAD-LINKS NIL *LOG-TYPES-SWITCHES*))
    (DEFUN DISABLE-LOG-DETAILS ()
      (SWITCH-LOG-TYPE :DETAILS NIL *LOG-TYPES-SWITCHES*))
    (DEFUN LOG-INFO-ENABLE-P () (ENABLE-LOG-TYPE-P :INFO *LOG-TYPES-SWITCHES*))
    (DEFUN LOG-WARN-ENABLE-P () (ENABLE-LOG-TYPE-P :WARN *LOG-TYPES-SWITCHES*))
    (DEFUN LOG-ERROR-ENABLE-P ()
      (ENABLE-LOG-TYPE-P :ERROR *LOG-TYPES-SWITCHES*))
    (DEFUN LOG-BAD-LINKS-ENABLE-P ()
      (ENABLE-LOG-TYPE-P :BAD-LINKS *LOG-TYPES-SWITCHES*))
    (DEFUN LOG-DETAILS-ENABLE-P ()
      (ENABLE-LOG-TYPE-P :DETAILS *LOG-TYPES-SWITCHES*))
    (OPEN-LOG-STREAMS)
    (SAVE-INIT-HOOKS :SAVE (SYMBOL-FUNCTION 'CLOSE-LOG-STREAMS) :INIT
                     (SYMBOL-FUNCTION 'OPEN-LOG-STREAMS)))))

(defmacro define-default-logs (&key 
			       (previous-dir-p nil)
			       (logs-pathname "logs")
			       extra-log-types)
  (labels ((get-dir-code (pathname)
	     `(make-pathname
	       :name nil :type nil
	       :defaults ,pathname))
	   (if-previous-dir-p (pathname)
	     (if previous-dir-p
		 (get-dir-code `(cl-fad:pathname-as-file ,pathname))
		 pathname)))		 
    (with-gensyms (logs-dir)
      `(define-logging
	   (let ((,logs-dir ,logs-pathname))
	     (if (absolute-pathname-p ,logs-dir) 
		 ,logs-dir
		 (cl-fad:pathname-as-directory 
		  (merge-pathnames ,logs-dir
				   ,(if-previous-dir-p 
				     (get-dir-code 
				      '(cl-fad:pathname-as-file
					(or *load-pathname* *default-pathname-defaults*))))))))
	   :log-types ,(append '(:info :warn :error) extra-log-types)
	   :prefix-logs (string-downcase (package-name *package*))))))
(addtest define-default-logs-test
  (ensure
   (equal-with-ignore-unpackage-symbols
    (macroexpand-1 '(define-default-logs :previous-dir-p t :extra-log-types (:bad-links :details)))
    '(DEFINE-LOGGING
    (LET ((#:LOGS-DIR5432 "logs"))
      (IF (ABSOLUTE-PATHNAME-P #:LOGS-DIR5432)
          #:LOGS-DIR5432
          (PATHNAME-AS-DIRECTORY
           (MERGE-PATHNAMES #:LOGS-DIR5432
                            (MAKE-PATHNAME :NAME NIL :TYPE NIL :DEFAULTS
                                           (PATHNAME-AS-FILE
                                            (MAKE-PATHNAME :NAME NIL :TYPE NIL
                                                           :DEFAULTS
                                                           (PATHNAME-AS-FILE
                                                            (OR *LOAD-PATHNAME*
                                                                *DEFAULT-PATHNAME-DEFAULTS*)))))))))
    :LOG-TYPES (:INFO :WARN :ERROR :BAD-LINKS :DETAILS) :PREFIX-LOGS
    (STRING-DOWNCASE (PACKAGE-NAME *PACKAGE*))))))

;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;
(defun as-string (obj)
  (typecase obj
    (string obj)
    (package (package-name obj))
    (symbol (symbol-name obj))))
(addtest as-string-test
  (ensure (every #'identity
		 (mapcar #'as-string
			 (list "sdf" *package* 'sym)))))

(defun replace-many (old-elem new-list list)
  (loop 
     for type in list
     if (eq type old-elem)
     append new-list
     else 
     collect type))
(addtest replace-many-test
  (ensure-same
   (replace-many :std-log-types '(:info :warn :error) '(a b :std-log-types c d))
   '(a b :info :warn :error c d)))

(defun symcat (&rest syms)     
;  (break "syms: ~S" syms)
  (read-from-string (string-upcase
		     (apply #'concatenate 'string
			    (mapcar #'string 
				    (remove nil syms))))))
;(apply #'symcat '(NIL NIL "LOG-" :INFO))
(addtest symcat-test
  (ensure-same 
   (symcat "PREFIX" "-log-" :mykey)
   'PREFIX-LOG-MYKEY))	  

(defun add-pkg-prefix (sym package)
  (read-from-string 
   (concatenate 'string (string-upcase (typecase package
					 (package (package-name package))
					 (string package))) "::" (symbol-name sym))))
(addtest add-pkg-prefix-test
  (ensure-same 
   (add-pkg-prefix 'defun "CL-USER")
   'cl-user::defun))

;;;!!! Not portability (not work on windows)
(defun absolute-pathname-p (pathname)
  (eq :absolute (first (pathname-directory pathname))))
(addtest absolute-pathname-p-test
  (ensure 
   (and (not (absolute-pathname-p "media/WORK_PARTITION/sdf"))
        (absolute-pathname-p "/media/WORK_PARTITION/sdf"))))

#|
(defmacro with-gensyms ((&rest syms) &body body)
  `(let ,(loop for sym in syms
	     collect `(,sym (gensym ,(concatenate 'string (symbol-name sym) "-"))))
    ,@body))
(addtest with-gensyms-test
  (ensure-same 
   (macroexpand-1 '(with-gensyms (arg1 arg2)
		    (operation1 arg1 arg2)
		    (operation2 arg1 arg2)))
   '(LET ((ARG1 (GENSYM "ARG1-")) (ARG2 (GENSYM "ARG2-")))
     (OPERATION1 ARG1 ARG2)
     (OPERATION2 ARG1 ARG2))))
|#
;;;;;;;;;;;;;;;;;;;;;
