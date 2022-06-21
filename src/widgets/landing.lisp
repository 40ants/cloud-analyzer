(uiop:define-package #:app/widgets/landing
  (:use #:cl)
  (:import-from #:reblocks-lass)
  (:import-from #:reblocks/dependencies)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:app/widgets/yandex-metrika
                #:render-counter))
(in-package #:app/widgets/landing)


(defwidget landing-page ()
  ())


(defun make-landing-page ()
  (make-instance 'landing-page))


(defmethod reblocks/widget:render ((widget landing-page))
  (reblocks/html:with-html
    (render-counter)
    
    (:p "Тут будет красивый landing с рассказом про сервис. А пока можно только залогиниться.")
    (:p (:a :href "/login"
            (:img :src "https://yastatic.net/s3/doc-binary/freeze/ru/id/816840a644c625b1bf570b6351bdf6e4221538c5.svg")))))


(defmethod reblocks/dependencies:get-dependencies ((widget landing-page))
  (list
   (reblocks-lass:make-dependency
     '(.landing-page
       :margin-top 4em
       :display flex
       :flex-direction column
       :align-items center))))
