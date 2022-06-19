(defun change-pathnames-to-local ()
  (setf (logical-pathname-translations "app")
	(list (list "app:**;*.*"
		    #P"/Users/art/projects/lisp/cloud-analyzer/**/*.*"))))
  

(let* ((current-file (or *compile-file-truename*
			 *load-truename*))
       (current-path (uiop:pathname-directory-pathname current-file))
       (target-pattern-path (progn
			      (format *error-output* "Current path:~%")
			      (describe current-path *error-output*)
			      (merge-pathnames #P"**/*.*" current-path)))
       (source-pattern-path
	 "app:**;*.*.*"))
  (format *error-output* "Source pattern:~%")
  (describe source-pattern-path *error-output*)
  
  (format *error-output* "Target pattern:~%")
  (describe target-pattern-path *error-output*)

  
  (setf (logical-pathname-translations "app")
	(list (list source-pattern-path
		    target-pattern-path))))
  

(defsystem "app"
  :depends-on ("yandex-disk-cleaner"))
