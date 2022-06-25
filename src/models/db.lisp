(uiop:define-package #:cloud-analyzer/models/db
  (:use #:cl))
(in-package #:cloud-analyzer/models/db)


(defvar *connection* nil)

(defun get-db-host ()
  "localhost")

(defun get-db-name ()
  "art")

(defun get-db-user ()
  "art")

(defun get-db-pass ()
  nil)


(defmacro with-connection (&body body)
  `(dbi:with-connection (*connection*
                         :postgres
                         :host (get-db-host)
                         :database-name (get-db-name)
                         :username (get-db-user)
                         :password (get-db-pass))
     ,@body))


(defun query (query &rest params)
  (unless *connection*
    (error "Please, use QUERY inside WITH-CONNECTION block."))
  
  (let* ((prepared (dbi:prepare *connection* query))
         (results (dbi:execute prepared params)))
    (dbi:fetch-all results)))


(defun execute (query &rest params)
  (unless *connection*
    (error "Please, use EXECUTE inside WITH-CONNECTION block."))
  
  (let* ((prepared (dbi:prepare *connection* query)))
    (dbi:execute prepared params)
    (values)))
