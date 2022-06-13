(uiop:define-package #:yandex-disk-cleaner/server
  (:use #:cl)
  (:import-from #:woo)
  (:import-from #:yandex-disk-cleaner/app
                #:disk-analyzer))
(in-package #:yandex-disk-cleaner/server)


(defun start (&key (port 8080)
		(interface "localhost"))
  (reblocks/server:start :port port
			 :interface interface
                         :apps 'disk-analyzer
			 :server-type :woo))


(defun cl-user::start-server ()
  ;; Entry point for webapp, started in the Docker
  (start :port (parse-integer (or (uiop:getenv "APP_PORT")
				  "80"))
	 :interface (or (uiop:getenv "APP_INTERFACE")
			"0.0.0.0"))
  (loop do (sleep 5)))
