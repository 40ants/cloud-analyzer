(uiop:define-package #:yandex-disk-cleaner/app
  (:use #:cl)
  (:import-from #:reblocks/app
                #:defapp)
  (:import-from #:yandex-disk-cleaner/widgets/login
                #:get-token
                #:make-login-page)
  (:import-from #:yandex-disk-cleaner/widgets/landing
                #:make-landing-page)
  (:import-from #:reblocks-navigation-widget
                #:defroutes)
  (:import-from #:yandex-disk-cleaner/widgets/analyzer
                #:make-analyzer)
  (:import-from #:yandex-disk-cleaner/api
                #:*token*))
(in-package #:yandex-disk-cleaner/app)


(defapp disk-analyzer
  :prefix "/")


(defroutes routes
    ("/login" (make-login-page))
  ("/analyzer" (let ((*token* (get-token)))
                 (make-analyzer)))
  ("/" (make-landing-page)))


(defmethod reblocks/session:init ((app disk-analyzer))  
  (make-routes))



(reblocks/hooks:on-application-hook-handle-http-request set-token (env)
  (let ((*token* (get-token)))
    (reblocks/hooks:call-next-hook)))
