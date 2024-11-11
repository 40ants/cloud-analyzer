(uiop:define-package #:app/server
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:woo)
  (:import-from #:bordeaux-threads)
  (:import-from #:lparallel)
  (:import-from #:app/logging)
  (:import-from #:app/app
                #:disk-analyzer)
  (:import-from #:reblocks/server)
  (:import-from #:app/slynk
                #:start-slynk-if-needed)
  (:export
   #:start))
(in-package #:app/server)


(defvar *gc-thread* nil)


(defun call-gc ()
  (loop do (sb-ext:gc :full t)
           (sleep 60)))


(defun start-gc-thread ()
  (when (or (null *gc-thread*)
            (not (bordeaux-threads:thread-alive-p *gc-thread*)))
    (setf *gc-thread*
          (bordeaux-threads:make-thread #'call-gc
                                        :name "Periodical GC"))))

(defun make-lparallel-kernel-if-needed ()
  (let ((num-workers (parse-integer
		      (or (uiop:getenv "NUM_LPARALLEL_WORKERS")
			  "10"))))
    (unless lparallel:*kernel*
      (setf lparallel:*kernel*
	    (lparallel:make-kernel num-workers)))))


(defun load-local-config ()
  (let ((filename (asdf/system:system-relative-pathname :app ".local-config.lisp")))
    (when (probe-file filename)
      (load filename))))


(defun start (&key (port 8080) (interface "localhost"))
  ;; Just to suppres debug logs to TTY from Reblocks.
  ;; I'll need to fix Reblocks to prohibit it from
  ;; configure logging if they are already configured.
  (load-local-config)
  (app/logging::setup)
  (start-slynk-if-needed)
  (make-lparallel-kernel-if-needed)
  (reblocks/server:start :port port
			 :interface interface
                         :apps 'disk-analyzer
                         ;; Woo fails for version
                         ;; woo-20240910140304 and SBCL "2.4.5"
                         ;; Ultralisp version 20240911125500
			 ;; :server-type :woo
                         )
  (start-gc-thread)
  (app/logging::setup)
  (log:info "Server started"))


(defun cl-user::start-server ()
  ;; Entry point for webapp, started in the Docker
  (start :port (parse-integer (or (uiop:getenv "APP_PORT")
				  "80"))
	 :interface (or (uiop:getenv "APP_INTERFACE")
			"0.0.0.0"))
  (loop do (sleep 5)))
