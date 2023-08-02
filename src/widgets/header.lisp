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
      (:script :src "https://cdn.tailwindcss.com")

      (:div :class "w-full bg-white flex-col justify-start items-center gap-12 inline-flex"
            (:div :class "Header self-stretch
                          pl-4 pr-5
                          lg:pl-28 lg:pr-24
                          lg:py-6
                          bg-lime-200 justify-between items-center inline-flex"
                  (:div :class "CloudAnalyzer grow shrink basis-0 text-black
                                text-[24px] lg:text-[64px]
                                font-bold leading-[64.01px]"
                        (:a :href "/"
                            "Cloud Analyzer"))
                  (:div :class "text-black
                                text-[16px] lg:text-[32px]
                                font-bold leading-loose" 
                        (cond
                          ((get-username)
                           (render-form-and-button "Выйти"
                                                   #'logout
                                                   :method :post
                                                   :button-class "hover:text-red-500 px-[16px] rounded-lg rounded-lg drop-shadow-lg border border-orange-800"))
                          (t
                           (render-form-and-button "Войти"
                                                   #'login
                                                   :method :post
                                                   :button-class "hover:text-red-500 px-[16px] rounded-lg rounded-lg drop-shadow-lg border border-orange-800")))))

            (render (content widget))
            
            (:div :class "Footer self-stretch py-6 bg-lime-200 justify-center items-start gap-32 inline-flex"
                  (:div :class "Frame25 flex-col justify-start items-start gap-4 inline-flex"
                        ;; (:div :class " text-black text-sl font-normal" "Тарифы")
                        (:div :class " text-black text-sl font-normal"
                              (:a :href "/policy"
                                  "Политика"
                                  (:br)
                                  "конфиденциальности")))
                  (:div :class "Frame24 flex-col justify-start items-start gap-4 inline-flex"
                        (:div :class " text-black text-sm font-normal"
                              (:a :href "https://github.com/40ants/cloud-analyzer"
                                  "Исходный код"))
                        ;; (:div :class " text-black text-sm font-normal" "Контакты")
                        (when (get-username)
                          (:div :class " text-black text-sm font-normal"
                                (:a :href "/feedback"
                                    "Обратная связь")))))))))



(defmethod get-dependencies ((widget page-with-header))
  (list
   ;; (reblocks-lass:make-dependency
   ;;   '(.page-with-header
   ;;     :display flex
   ;;     :flex-direction column
   ;;     :align-items center
   ;;     (header
   ;;      :width 100%
   ;;      :display flex
   ;;      :flex-direction row
   ;;      :align-items center
   ;;      :justify-content space-between
   ;;      (h1 :margin-left 1em
   ;;          :width 100%
   ;;          :text-align center)
        
   ;;      (form
   ;;       :justify-self end
   ;;       :margin-right 1em)

   ;;      ((:or
   ;;        (:and .button .logout)
   ;;        (:and .button .logout))
   ;;       :margin 0))))
   ))
