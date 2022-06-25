(uiop:define-package #:cloud-analyzer/widgets/adminka
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/widgets/login
                #:get-username)
  (:import-from #:cloud-analyzer/models/run-stats
                #:get-stats))
(in-package #:cloud-analyzer/widgets/adminka)


(defcached is-admin (username)
  (string= username
           (or (uiop:getenv "ADMIN_USERNAME")
               "sasha@svetlyak.ru")))


(defwidget adminka ()
  ())


(defun make-adminka ()
  (make-instance 'adminka))


(defmethod render ((widget adminka))
  (cond
    ((is-admin (get-username))
     (with-html
       (multiple-value-bind (num-runs num-users most-active)
           (get-stats)
         (:h2 "За последние сутки")
         (:p (format nil "Запусков анализа: ~A" num-runs))
         (:p (format nil "Уникальных пользователей: ~A" num-users))
         (:p (format nil "Самый активный: ~A (~A запусков)"
                     (getf most-active
                           :|username|)
                     (getf most-active
                           :|count|))))))
    (t
     (reblocks/response:redirect "/"))))
