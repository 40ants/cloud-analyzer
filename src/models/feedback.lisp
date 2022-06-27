(uiop:define-package #:app/models/feedback
  (:use #:cl)
  (:import-from #:app/models/db
                #:query
                #:execute
                #:with-connection))
(in-package #:app/models/feedback)


;; create table feedback (created_at timestamp with time zone, username text, message text, seen boolean default false);
;; alter table feedback add column email text;

(defun save-feedback (username message &key email)
  (with-connection
      (execute "INSERT INTO feedback (created_at, username, message, email) VALUES (now(), ?, ?, ?)"
               username
               message
               email)))
