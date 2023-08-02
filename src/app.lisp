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
  (:import-from #:app/widgets/adminka
                #:make-adminka)
  (:import-from #:app/widgets/header
                #:make-page-with-header)
  (:import-from #:app/widgets/feedback
                #:make-feedback)
  (:import-from #:reblocks/page)
  (:import-from #:app/widgets/policy
                #:make-policy-page))
(in-package #:app/app)


(defapp disk-analyzer
  :prefix "/")


(defroutes routes
    ("/login" (make-login-page))
  ("/policy"  (make-page-with-header
               (make-policy-page)))
  ("/analyzer"  (make-page-with-header
                 (make-analyzer)))
  ("/feedback" (make-page-with-header
                (make-feedback)))
  ("/adminka" (make-page-with-header
               (make-adminka)))
  ("/" (make-page-with-header
        (make-landing-page))))


(defmethod reblocks/page:init-page ((app disk-analyzer) (url-path string) expire-at)
  (make-routes))


(reblocks/hooks:on-application-hook-handle-http-request set-token (env)
  (let ((*token* (get-token)))
    (reblocks/hooks:call-next-hook)))



(defmethod reblocks/page:render :before ((app disk-analyzer) inner-html &rest rest)
  (declare (ignore rest))
  
  (setf (reblocks/page:get-title)
        "Cloud Analyzer - Экономит место и деньги!")
  (setf (reblocks/page:get-description)
        "Помогает сэкономить на хранении данных в \"облаках\".")
  (setf (reblocks/page:get-keywords)
        (list "облака"
              "хранилище"
              "анализатор"
              "место"
              "освободить")))
