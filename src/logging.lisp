(uiop:define-package #:app/logging
  (:use #:cl)
  (:import-from #:log4cl-extras)
  (:import-from #:log4cl-extras/config))
(in-package #:app/logging)


(defun setup ()
  (let ((appenders
          (append (list '(this-console
                          :layout :plain
                          :filter :warn))
                  (when (probe-file "/app/logs")
                    (list '(daily
                            :layout :json
                            :name-format "/app/logs/app.log"
                            :backup-name-format "app%Y%m%d.log"))) )))
    (log4cl-extras/config:setup
     (list :level :info
           :appenders appenders))))
