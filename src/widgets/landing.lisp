(uiop:define-package #:app/widgets/landing
  (:use #:cl)
  (:import-from #:spinneret/cl-markdown)
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
    (:h1 "Cloud Analyzer")

    (:section :class "proposition"
              (:p "Однажды мне нужно было закачать на Яндекс Диск видео со школьного концерта дочери. Но места оказалось недостаточно. Что делать? Покупать дополнительный террабайт за 400р в месяц, или поискать что нибудь ненужное, что можно удалить и освободить место? Но если поискать, то как? Ведь на облачном диске можно долго перебирать файлы и потратить кучу времени и сил.")
              (:p "Тогда я решил сделать этот небольшой сервис, по примеру многих анализаторов места на диске, что существуют для обычных компьютеров. Только Cloud Analyzer умеет оптимизировать хранение файлов на облачном диске и помогает экономить время и деньги.")
              (:p "Вот что он может для вас сделать:")
              (:ul (:li "Проанализировать использование места на Яндекс Диске.")
                   (:li "Показать какие папки занимают наибольший объем.")
                   (:li "Найти файлы-дубликаты (пока в разработке)."))
              (:p "Сначала наведите порядок на Яндекс Диске, и только если это не поможет — платите за более дорогой тариф.")
              (:p (:a :class "login-button"
                      :href "/login"
                      (:img :src
                            "https://yastatic.net/s3/doc-binary/freeze/ru/id/8c9292ce69ef01f902b0ef13c29167a2110139fe.svg"))))
    (:section :class "example"
              (:p "Вот как выглядят результаты анализа:")
              (:p (:img :src "https://storage.yandexcloud.net/cloud-analyzer-static/landing-demo.png"
                        :width "1024"
                        :height "607")))
    (:section :class "eula"
              (:p "Cloud Analyzer не сохраняет информацию с вашего диска в свою базу данных.")
              (:p ("Вы можете лично убедиться в этом, проанализировав [исходные коды приложения](https://github.com/40ants/cloud-analyzer).")))))


(defmethod reblocks/dependencies:get-dependencies ((widget landing-page))
  (list
   (reblocks-lass:make-dependency
     '(.landing-page
       :display flex
       :flex-direction column
       :align-items center
       (.login-button :width 280px
                      :height 76px
                      :background white
                      :display flex
                      :flex-direction column
                      :align-items center
                      :padding-top 10px
                      :border-radius 10px
        (img :width 260px
             :height 56px))
       (section :width 100%
                :font-size 1.2em
                :padding-top 2em
                :padding-bottom 1em
                :display flex
                :flex-direction column
                :align-items center)
       ((:or
         .proposition
         .eula) :background "#43225a"
                :color "#e3e3e3"
                :padding-left 100px
                :padding-right 100px)))))
