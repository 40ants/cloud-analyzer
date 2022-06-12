(uiop:define-package #:yandex-disk-cleaner/api
  (:use #:cl)
  (:import-from #:dexador)
  (:import-from #:jonathan)
  (:import-from #:lparallel)
  (:import-from #:yandex-disk-cleaner/utils
                #:humanize-size))
(in-package #:yandex-disk-cleaner/api)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *client-id* (or (uiop:getenv "OAUTH_CLIENT_ID")
                                ;; This is an id of a test oauth app:
                                "aea7e40eb78b4c029521a4bdae19e71b"))
  
  (defparameter *client-secret* (or (uiop:getenv "OAUTH_CLIENT_SECRET")
                                    "4a025622819440429a9291ea2ee01ed3")))

(defvar *token*)


(defun get-auth-url ()
  (format nil "https://oauth.yandex.ru/authorize?response_type=code&client_id=~A"
          *client-id*))


(defun retrieve-token (code)
  (let* ((url "https://oauth.yandex.ru/token")
         (response (dex:post url
                             :content (list (cons "grant_type" "authorization_code")
                                            (cons "code" code)
                                            (cons "client_id" *client-id*)
                                            (cons "client_secret" *client-secret*))))
         (data (jonathan:parse response)))
    (values (getf data :|access_token|)
            (getf data :|refresh_token|))))


(defun refresh (refresh-token)
  (let* ((url "https://oauth.yandex.ru/token")
         (response (dex:post url
                             :content (list (cons "grant_type" "refresh_token")
                                            (cons "refresh_token" refresh-token) 
                                            (cons "client_id" *client-id*)
                                            (cons "client_secret" *client-secret*))))
         (data (jonathan:parse response)))
    (values (getf data :|access_token|)
            (getf data :|refresh_token|))))


(defun get-login ()
  (let* ((headers (list (cons "Authorization"
                              (format nil "OAuth ~A"
                                      *token*))))
         (response (dex:get "https://login.yandex.ru/info?format=json"
                            :headers headers))
         (data (jonathan:parse response)))
    (getf data :|login|)))


(defun make-request (path &key (num-retries 5)
                            (retry-interval 3))
  (unless *token*
    (error "Нужен токен"))
  
  (let* ((headers (list (cons "Authorization"
                              (format nil "OAuth ~A"
                                      *token*))))
         (retry-request (dex:retry-request num-retries :interval retry-interval)))
    (handler-bind ((dex:http-request-failed retry-request))
      (multiple-value-bind (response status-code response-headers)
          (dex:get (concatenate 'string
                                "https://cloud-api.yandex.net/v1"
                                path)
                   :headers headers)
        (unless (= status-code 200)
          (error "Bad status code: ~A"
                 status-code))
        (let ((data (jonathan:parse response)))
          (values data
                  response-headers))))))


(defun get-total-usage (&key unit)
  (let* ((data (make-request "/disk/"))
         (value (getf data
                      :|used_space|))
         (total (getf data
                      :|total_space|)))
    (case unit
      (:raw (values value
                    total))
      (t
       (values (humanize-size value :unit unit)
               (humanize-size total :unit unit))))))


(defun ls ()
  ;; fields name,type,size
  (make-request "/disk/resources/?path=/Books&offset=20&fields=_embedded.items.path,_embedded.items.name,_embedded.items.type,_embedded.items.size"))


(defun %list-dir (dir)
  ;; fields name,type,size
  (let ((limit 1000)
        (offset 0)
        (fields "_embedded.total,_embedded.offset,_embedded.limit,_embedded.items.path,_embedded.items.name,_embedded.items.type,_embedded.items.size,_embedded.items.media_type"))
    (flet ((get-chunk ()
             (let* ((uri (format nil "/disk/resources/?~A"
                                 (quri:url-encode-params (list (cons "path" dir)
                                                               (cons "limit" limit)
                                                               (cons "offset" offset)
                                                               (cons "fields" fields)))))
                    (data (make-request uri)))
               (incf offset limit)
               (getf (getf data :|_embedded|)
                     :|items|))))
      (loop for data = (get-chunk) then (get-chunk)
            while data
            append data))))


(defparameter *num-processed* nil)


;; depth 2 non-parallel: 375s
;; depth 2 10 threads: 131s
;; depth 2 20 threads: 141
;; depth 1000 10 threads: 384s (size 410.54G, total: 408.49G) saved to *folders*


;; (defun du (&optional (path "/") (name path) (max-depth 2))
;;   (unless (null *num-processed*)
;;     (incf *num-processed*)
;;     (when (zerop (mod *num-processed* 10))
;;       (format t ".")
;;       (finish-output)))
  
;;   (loop with size = 0
;;         with subfolders = nil
;;         for item in (%list-dir path)
;;         for type = (getf item :|type|)
;;         for item-path = (getf item :|path|)
;;         for item-name = (getf item :|name|)
;;         do (cond
;;              ((string= type "dir")
;;               (unless (zerop max-depth)
;;                 (let ((subfolder (du item-path
;;                                      item-name
;;                                      (1- max-depth))))
;;                   (push subfolder subfolders)
;;                   (incf size (getf subfolder :size)))))
;;              ((string= type "file")
;;               (incf size (getf item :|size|))))
;;         finally (return (list :name name
;;                               :size size
;;                               :subfolders (sort subfolders
;;                                                 #'>
;;                                                 :key (lambda (item)
;;                                                        (getf item :size)))))))

(defun %most-popular-key (hash)
  (loop with max-key = nil
        with max-value = 0
        for key being the hash-key of hash
          using (hash-value value)
        when (> value max-value)
          do (setf max-value value
                   max-key key)
        finally (return max-key)))


(defun %du (&optional (path "/") (name path) (max-depth 2) (increment-progress nil))
  (unless (null *num-processed*)
    (incf *num-processed*)
    (when (zerop (mod *num-processed* 10))
      (format t ".")
      (finish-output)))

  (let ((size 0)
        (subfolders nil)
        (media-types (make-hash-table :test 'equal))
        (lock (bt:make-lock))
        ;; To pass token inside lparallel threads:
        (token *token*))
  
    (lparallel:pmapc
     (lambda (item)
       (let ((*token* token)
             (lparallel:*debug-tasks-p* nil)
             (type (getf item :|type|))
             (item-path (getf item :|path|))
             (item-name (getf item :|name|)))
         (cond
           ((string= type "dir")
            (unless (zerop max-depth)
              (when increment-progress
                (funcall increment-progress
                         (format nil "~A/" item-name)
                         ;; we don't count subfolders size
                         ;; because their size alread counted inside
                         ;; nested %du call
                         0))
              
              (let ((subfolder (%du item-path
                                    item-name
                                    (1- max-depth)
                                    increment-progress)))
                (bt:with-lock-held (lock)
                  (incf (gethash (getf subfolder :media-type)
                                 media-types
                                 0))
                  (push subfolder subfolders)
                  (incf size (getf subfolder :size))))))
           ((string= type "file")
            (let ((file-size (getf item :|size|)))
              (bt:with-lock-held (lock)
                (incf (gethash (getf item :|media_type|)
                               media-types
                               0))
                (incf size file-size)
                (when increment-progress
                  (funcall increment-progress
                           item-name
                           file-size))))))))
     (%list-dir path))
    
    (list :name name
          :size size
          :subfolders (sort subfolders
                            #'>
                            :key (lambda (item)
                                   (getf item :size)))
          :media-type (%most-popular-key media-types))))


(defun du (&key
             (path "/")
             (name path)
             (max-depth 100000)
             (increment-progress nil))
  (let ((lparallel:*kernel* (lparallel:make-kernel 10)))
    (%du path name max-depth increment-progress)))


(defvar *data* nil)
