(uiop:define-package #:yandex-disk-cleaner/server
  (:use #:cl)
  (:import-from #:yandex-disk-cleaner/app
                #:disk-analyzer))
(in-package #:yandex-disk-cleaner/server)


(defun start (&key (port 8080))
  (reblocks/server:start :port port
                         :apps 'disk-analyzer))
