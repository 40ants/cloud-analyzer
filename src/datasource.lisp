(uiop:define-package #:app/datasource
  (:use #:cl)
  (:import-from #:app/widgets/tree
                #:retrieve-nodes)
  (:import-from #:app/utils
                #:path-to-string))
(in-package #:app/datasource)


(defclass disk-tree ()
  ((data :initarg :data
         :reader data)))


(defun make-disk-tree (data)
  (make-instance 'disk-tree
                 :data data))


(defun dir-files (path)
  (loop for item in (app/api::%list-dir (path-to-string path))
        when (string= (getf item :|type|)
                      "file")
          collect (list :name (getf item :|name|)
                        :type :file
                        :size (getf item :|size|)
                        :media-type (getf item :|media_type|))))


(defmethod retrieve-nodes ((tree disk-tree) full-path)
  (labels ((process (folder path)
             (when path
               (when (string= (getf folder :name)
                              (car path))
                 (let ((subfolders (getf folder :subfolders)))
                   (cond
                     ((null (cdr path))
                      (return-from retrieve-nodes
                        (append
                         (loop for sub in subfolders
                               collect (list :name (getf sub :name)
                                             :size (getf sub :size)
                                             :type :dir))
                         (sort (dir-files full-path)
                               #'>
                               :key (lambda (file)
                                      (getf file :size))))))
                     (t
                      (loop for sub in subfolders
                            do (process sub (cdr path))))))))))
    (process (data tree)
             full-path)
    ;; If path was found we'll make a nonlocal exit from PROCESS
    ;; and return the value.
    (values)))
