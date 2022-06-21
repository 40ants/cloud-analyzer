(uiop:define-package #:app/widgets/login
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:app/api
                #:retrieve-token
                #:get-auth-url))
(in-package #:app/widgets/login)


(defwidget login-page ()
  ())


(defun make-login-page ()
  (make-instance 'login-page))


(defvar *token-response* nil)


(defun get-username ()
  (reblocks/session:get-value :username))


(defun get-token ()
  (reblocks/session:get-value :token))


(defmethod reblocks/widget:render ((widget login-page))
  (let ((code (reblocks/request:get-parameter "code"))
        (username (get-username)))
    (cond
      (username
       (reblocks/response:redirect "/analyzer")
       ;; (reblocks/html:with-html
       ;;   (:p "User logged as:"
       ;;       (:b username)))
       )
      (code
       (let ((app/api::*token* (retrieve-token code)))
         (setf (reblocks/session:get-value :token)
               app/api::*token*
               (reblocks/session:get-value :username)
               (app/api::get-login))
         (reblocks/response:redirect "/analyzer"))
       )
      (t
       (reblocks/response:redirect (get-auth-url))))))
