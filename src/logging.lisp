(uiop:define-package #:app/logging
  (:use #:cl)
  (:import-from #:log4cl-extras)
  (:import-from #:log4cl-extras/config))
(in-package #:app/logging)


(defun setup ()
  (log4cl-extras/config:setup
   '(:level :info
     :appenders ((this-console :layout :plain
                               :filter :warn)
                 (daily :layout :json
                        :name-format "/app/logs/app.log"
                        :backup-name-format "app%Y%m%d.log")))))
