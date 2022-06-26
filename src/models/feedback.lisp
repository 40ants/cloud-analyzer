(uiop:define-package #:app/models/feedback
  (:use #:cl)
  (:import-from #:app/models/db
                #:query
                #:execute
                #:with-connection))
(in-package #:app/models/feedback)


;; create table feedback (created_at timestamp, username text, message text, seen boolean default false);

(defun save-feedback (username message)
  (with-connection
      (execute "INSERT INTO feedback (created_at, username, message) VALUES (now(), ?, ?)"
               username
               message)))
