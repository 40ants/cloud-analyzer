(uiop:define-package #:app/widgets/header
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:app/widgets/login
                #:get-username)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:reblocks/dependencies
                #:get-dependencies))
(in-package #:app/widgets/header)


(defwidget page-with-header ()
  ((content :initarg :content
            :reader content)))


(defun make-page-with-header (content)
  (make-instance 'page-with-header :content content))


(defmethod render ((widget page-with-header))
  (flet ((logout (&rest rest)
           (declare (ignore rest))
           (reblocks/session:reset)
           (reblocks/response:redirect "/"))
         (login (&rest rest)
           (declare (ignore rest))
           (reblocks/response:redirect "/login")))
    (reblocks/html:with-html
      (:header
       (:h1 :class "header"
            (:a :href "/"
                "Cloud Analyzer"))
       (cond
         ((get-username)
          (render-form-and-button "Выйти"
                                  #'logout
                                  :method :post
                                  :button-class "button secondary logout"))
         (t
          (render-form-and-button "Войти"
                                  #'login
                                  :method :post
                                  :button-class "button secondary login"))))

      (render (content widget)))))



(defmethod get-dependencies ((widget page-with-header))
  (list
   (reblocks-lass:make-dependency
     '(.page-with-header
       :display flex
       :flex-direction column
       :align-items center
       (header
        :width 100%
        :display flex
        :flex-direction row
        :align-items center
        :justify-content space-between
        (h1 :margin-left 1em
            :width 100%
            :text-align center)
        
        (form
         :justify-self end
         :margin-right 1em)

        ((:or
          (:and .button .logout)
          (:and .button .logout))
         :margin 0))))))
