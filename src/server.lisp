(uiop:define-package #:app/server
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:woo)
  (:import-from #:app/app
                #:disk-analyzer)
  (:import-from #:app/slynk
                #:start-slynk-if-needed)
  (:export
   #:start))
(in-package #:app/server)


(defun make-lparallel-kernel-if-needed ()
  (let ((num-workers (parse-integer
		      (or (uiop:getenv "NUM_LPARALLEL_WORKERS")
			  "10"))))
    (unless lparallel:*kernel*
      (setf lparallel:*kernel*
	    (lparallel:make-kernel num-workers)))))


(defun start (&key (port 8080)
		(interface "localhost"))
  (start-slynk-if-needed)
  (make-lparallel-kernel-if-needed)
  (reblocks/server:start :port port
			 :interface interface
                         :apps 'disk-analyzer
			 :server-type :woo)
  (log:config :sane2 :warn))


(defun cl-user::start-server ()
  ;; Entry point for webapp, started in the Docker
  (start :port (parse-integer (or (uiop:getenv "APP_PORT")
				  "80"))
	 :interface (or (uiop:getenv "APP_INTERFACE")
			"0.0.0.0"))
  (loop do (sleep 5)))
