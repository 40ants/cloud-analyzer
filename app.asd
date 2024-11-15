(defun cl-user::set-local-pathname-translations ()
  (setf (logical-pathname-translations "app-local")
	(list (list "app-local:**;*.*.*"
		    #P"/Users/art/projects/lisp/cloud-analyzer/**/*.*")))
  (values))



(defun cl-user::change-pathnames-to-remote ()
  (setf (logical-pathname-translations "app")
	(list (list "app:**;*.*.*"
		    #P"/app/**/*.*")))
  (values))
  

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


(cl-user::set-local-pathname-translations)


(pushnew "~/projects/lisp/cffi/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/cl-plus-ssl/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/reblocks/" asdf:*central-registry*
         :test #'equal)


(defsystem "app"
  :class :package-inferred-system
  :pathname #P"app:src;"
  :depends-on ("app/widgets/analyzer"
	       "app/server"
               ;; We need to ensure that reblocks
               ;; if fully loaded because otherwise some important
               ;; parts of it such as jquery backend might be missing.
               "reblocks"))


(asdf:register-system-packages "colored" '(#:org.shirakumo.alloy.colored))

