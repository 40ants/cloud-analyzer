(uiop:define-package #:app/logging
  (:use #:cl)
  (:import-from #:log4cl-extras)
  (:import-from #:log4cl-extras/config))
(in-package #:app/logging)


(defun current-environment ()
  (or (uiop:getenv "ENVIRONMENT")
      "development"))


(defun setup ()
;  (unless (string-equal (current-environment)
;			"development")
;    (setf log4cl::*global-console*
;	  (make-synonym-stream '*standard-output*)))

  (let ((appenders
          (if (string-equal (current-environment)
                            "development")
           (list '(this-console
                   :layout :plain
                   :filter :warn))
           (list `(this-console
		   :stream ,*standard-output*
                   :layout :json
                   :filter :warn)) )))
    (log4cl-extras/config:setup
     (list :level :info
           :appenders appenders))))
