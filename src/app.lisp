(uiop:define-package #:app/app
  (:use #:cl)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:app/widgets/login
                #:get-token
                #:make-login-page)
  (:import-from #:app/widgets/landing
                #:make-landing-page)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:app/widgets/analyzer
                #:make-analyzer)
  (:import-from #:app/api
                #:*token*)
  (:import-from #:cloud-analyzer/widgets/adminka
                #:make-adminka)
  (:import-from #:cloud-analyzer/widgets/header
                #:make-page-with-header))
(in-package #:app/app)


(defapp disk-analyzer
  :prefix "/")


(defroutes routes
    ("/login" (make-login-page))
  ("/analyzer"  (make-page-with-header
                 (make-analyzer)))
  ("/adminka" (make-page-with-header
               (make-adminka)))
  ("/" (make-page-with-header
        (make-landing-page))))


(defmethod reblocks/session:init ((app disk-analyzer))  
  (make-routes))



(reblocks/hooks:on-application-hook-handle-http-request set-token (env)
  (let ((*token* (get-token)))
    (reblocks/hooks:call-next-hook)))
