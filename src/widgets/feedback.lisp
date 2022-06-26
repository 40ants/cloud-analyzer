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
  (flet ((process-submit (&key message &allow-other-keys)
           (let ((username (get-username)))
             (log:error "Form submitted" username message)
             (save-feedback username message)
             (setf (sent widget) t)
             (reblocks/widget:update widget))))
    (cond
      ((sent widget)
       (with-html
         (:p "Спасибо!")
         (:p ("Перейти [на главную страницу](/) или [к анализу](/analyzer)."))))
      
      ((get-username)
       (with-html
         (:p "Я буду рад узнать о ваших идеях о том, как можно сделать этот сервис лучше:")
         (with-html-form (:post #'process-submit)
           (:textarea :name "message")
           (:input :type "submit"
                   :class "button"
                   :name "submit-button"
                   :value "Отправить"))))
      (t
       (redirect "/")))))



(defmethod get-dependencies ((widget feedback))
  (list
   (reblocks-lass:make-dependency
     '(.feedback
       :width 50%
       (textarea :height 10em)

       ))))
