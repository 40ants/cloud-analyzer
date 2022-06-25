(uiop:define-package #:cloud-analyzer/models/db
  (:use #:cl)
  (:import-from #:function-cache
                #:defcached))
(in-package #:cloud-analyzer/models/db)


(defvar *connection* nil)

(defun whoami ()
  (uiop:run-program "whoami" :output '(:string :stripped t)))

(defcached get-db-host ()
  (or (uiop:getenv "DB_HOST")
      "localhost"))

(defcached get-db-name ()
  (or (uiop:getenv "DB_NAME")
      (whoami)))

(defcached get-db-user ()
  (or (uiop:getenv "DB_USER")
      (whoami)))

(defcached get-db-pass ()
  (uiop:getenv "DB_PASS"))


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
