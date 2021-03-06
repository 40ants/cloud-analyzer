(uiop:define-package #:app/models/db
  (:use #:cl)
  (:import-from #:dbi)
  (:import-from #:dbd-postgres)
  (:import-from #:function-cache
                #:defcached))
(in-package #:app/models/db)


(defvar *connection* nil)

(defun whoami ()
  (uiop:run-program "whoami" :output '(:string :stripped t)))

(defcached get-db-host ()
  (or (uiop:getenv "DB_HOST")
      "localhost"))

(defcached get-db-port ()
  (parse-integer
   (or (uiop:getenv "DB_PORT")
       "5432")))

(defcached get-db-name ()
  (or (uiop:getenv "DB_NAME")
      (whoami)))

(defcached get-db-user ()
  (or (uiop:getenv "DB_USER")
      (whoami)))

(defcached get-db-pass ()
  (uiop:getenv "DB_PASS"))


(defun run-with-connection (thunk)
  (dbi:with-connection (*connection*
                        :postgres
                        :host (get-db-host)
                        :port (get-db-port)
                        :database-name (get-db-name)
                        :username (get-db-user)
                        :password (get-db-pass))
    (funcall thunk)))


(defmacro with-connection (&body body)
  `(run-with-connection (lambda ()
                          ,@body)))


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
