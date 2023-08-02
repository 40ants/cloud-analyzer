(uiop:define-package #:app/widgets/feedback
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/widget
                #:render
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:app/widgets/login
                #:get-username)
  (:import-from #:reblocks/response
                #:redirect)
  (:import-from #:reblocks-ui/form
                #:with-html-form)
  (:import-from #:reblocks/dependencies
                #:get-dependencies)
  (:import-from #:app/models/feedback
                #:save-feedback))
(in-package #:app/widgets/feedback)


(defwidget feedback ()
  ((sent :initform nil
         :accessor sent)))


(defun make-feedback ()
  (make-instance 'feedback))


(defmethod render ((widget feedback))
  (flet ((process-submit (&key message email &allow-other-keys)
           (let ((username (get-username)))
             (log:error "Form submitted" username message)
             (save-feedback username message
                            :email email)
             (setf (sent widget) t)
             (reblocks/widget:update widget))))
    (cond
      ((sent widget)
       (with-html
         (:p "Спасибо!")
         (:p ("Перейти [на главную страницу](/) или [к анализу](/analyzer)."))))
      
      ((get-username)
       (with-html
         (:p "Я буду рад узнать о ваших идеях о том, как можно сделать этот сервис лучше.")
         (with-html-form (:post #'process-submit
                          :class "flex flex-col gap-4")
           (:div
            (:label :for "email"
                    "Если хотите чтобы я ответил, оставьте свой email:")
            (:input :name "email"
                    :type "email"
                    :placeholder "somebody@yandex.ru"))
           (:div
            (:label :for "message"
                    "Идея, багрепорт или пожелание счастья:")
            (:textarea :name "message"))
           (:div :class "flex justify-end"
            (:input :type "submit"
                    :class "try-button-2 group px-[38px] py-[15px] bg-red-500 hover:bg-red-700 rounded-lg drop-shadow-xl hover:drop-shadow-md border border-orange-800 justify-center items-center gap-2.5 inline-flex text-slate-100 text-[29.11px] font-bold hover:text-slate-200 cursor-pointer"
                    :name "submit-button"
                    :value "Отправить")))))
      (t
       (redirect "/")))))



(defmethod get-dependencies ((widget feedback))
  (list
   (reblocks-lass:make-dependency
     '(.feedback
       :width 50%
       (textarea :height 10em)

       ))))
