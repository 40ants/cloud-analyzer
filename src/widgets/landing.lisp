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
    
    (:div :class "Content px-4 lg:px-24 flex-col justify-start items-center gap-12 flex"
          (:div :class "Callout self-stretch lg:justify-between lg:items-start items-center gap-[70px] inline-flex lg:flex-row flex-col-reverse"
                (:div :class "self-stretch px-4 flex-col justify-start items-start gap-3.5 inline-flex"
                      (:div :class "self-stretch text-gray-700 text-[32px] lg:text-[64px] font-bold lg:leading-[64.01px]"
                            "Освободи место в облаке!")
                      (:div :class "MailRuDropboxGoogleDriveCloudAnalyzer self-stretch text-gray-600 text-2xl font-medium"
                            "Заканчивается место на Яндекс Диске, Mail.ru, Dropbox или Google Drive?"
                            (:br) (:br)
                            "Cloud Analyzer поможет сэкономить на покупке более дорогого тарифа!")
                      (:div :class "Actions self-stretch justify-start items-center gap-[42px] inline-flex
                                    flex-col
                                    lg:flex-row"
                            (:div :class "Spacer grow shrink basis-0 h-[37px]")
                            (:div :class "try-button-1 group px-[38px] py-[15px] bg-red-500 hover:bg-red-700 rounded-lg drop-shadow-xl hover:drop-shadow-md border border-orange-800 justify-center items-center gap-2.5 inline-flex cursor-pointer"
                                  (:a :class "text-slate-100 text-[29.11px] font-bold group-hover:text-slate-200"
                                      :href "/login" "Попробовать"))))
                (:img :class "CloudAnalyzerDiagram1
                              w-[330px] h-[330px]
                              lg:w-[440px] lg:h-[440px]
                              drop-shadow-xl"
                      :src "https://storage.yandexcloud.net/cloud-analyzer-static/landing-diagram2.png"))
          
          (:div :class "Howthisworks self-stretch px-4 flex-col justify-start items-center gap-8 flex"
                (:div :class " text-center text-black text-[32px] font-bold" "Как это работает?")
                (:div :class "HowItems self-stretch justify-center
                              lg:items-start
                              items-center
                              gap-[46px] inline-flex
                              flex-col
                              lg:flex-row"
                      (:div :class "Card1 w-5/6 md:w-2/3 lg:w-[350px] p-2 rounded-lg border border-zinc-300 flex-col justify-start items-start gap-4 inline-flex"
                            (:div :class " self-stretch text-black text-2xl font-bold" "Анализ")
                            (:div :class "Cloudanalyzer self-stretch text-black text-base font-medium" "CloudAnalyzer обходит все папки в вашем облачном диске, подсчитывая их размер."))
                      (:div :class "Card2 w-5/6 md:w-2/3 lg:w-[350px] p-2 rounded-lg border border-zinc-300 flex-col justify-start items-start gap-4 inline-flex"
                            (:div :class " self-stretch text-black text-2xl font-bold" "Представление")
                            (:div :class "Cloudanalyzer self-stretch text-black text-base font-medium" "Собранные данные представляются в виде наглядной диаграммы из которой понятно, какие папки занимают больше всего места."))
                      (:div :class "Card3 w-5/6 md:w-2/3 lg:w-[350px] p-2 rounded-lg border border-zinc-300 flex-col justify-start items-start gap-4 inline-flex"
                            (:div :class " self-stretch text-black text-2xl font-bold" "Очистка")
                            (:div :class "Cloudanalyzer self-stretch text-black text-base font-medium" "Вы принимаете решение, как осободить место на диске."))))

          (:div :class "Faq self-stretch flex-col justify-center items-center gap-[46px] flex"
                (:div :class " self-stretch text-center text-black text-[32px] font-bold" "Часто задаваемые вопросы")
                (:div :class "FaqItem self-stretch flex-col justify-start items-start gap-6 flex"
                      (:div :class "FaqItemHeader self-stretch pl-4 py-2 bg-zinc-300 bg-opacity-0 border-b-2 border-red-500 justify-center items-center gap-2.5 inline-flex" (:div :class " grow shrink basis-0 text-black text-2xl font-bold" "Почему мне нужно давать доступ к облачному Диску?"))
                      (:div :class "CloudanalyzerApiApi self-stretch text-black text-base font-medium" "CloudAnalyzer использует API облачной платформы, чтобы проанализировать папки и файлы и показать их объём. Так же он может использовать это API для поиска дубликатов ко- файлов которые присутствуют на диске в одном или нескольких экземплярах.")))

          (:div :class "LastAction self-stretch h-[85px] p-2.5 flex-col justify-start items-center gap-2.5 flex"
                (:div :class "try-button-2 group px-[38px] py-[15px] bg-red-500 hover:bg-red-700 rounded-lg drop-shadow-xl hover:drop-shadow-md border border-orange-800 justify-center items-center gap-2.5 inline-flex cursor-pointer"
                      (:a :class "text-slate-100 text-[29.11px] font-bold group-hover:text-slate-200"
                          :href "/login" "Попробовать"))))
    
    ;;           (:p "Cloud Analyzer не сохраняет информацию с вашего диска в свою базу данных.")
    ;;           (:p ("Вы можете лично убедиться в этом, проанализировав [исходные коды приложения](https://github.com/40ants/cloud-analyzer).")))
    ))


(defmethod reblocks/dependencies:get-dependencies ((widget landing-page))
  (list
   ;; (reblocks-lass:make-dependency
   ;;   '(.landing-page
   ;;     :display flex
   ;;     :flex-direction column
   ;;     :align-items center
   ;;     (.login-button :width 280px
   ;;                    :height 76px
   ;;                    :background white
   ;;                    :display flex
   ;;                    :flex-direction column
   ;;                    :align-items center
   ;;                    :padding-top 10px
   ;;                    :border-radius 10px
   ;;      (img :width 260px
   ;;           :height 56px))
   ;;     (section :width 100%
   ;;              :font-size 1.2em
   ;;              :padding-top 2em
   ;;              :padding-bottom 1em
   ;;              :display flex
   ;;              :flex-direction column
   ;;              :align-items center)
   ;;     ((:or
   ;;       .proposition
   ;;       .eula) :background "#43225a"
   ;;              :color "#e3e3e3"
   ;;              :padding-left 100px
   ;;              :padding-right 100px)))
   ))
