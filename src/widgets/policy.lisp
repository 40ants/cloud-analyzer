(uiop:define-package #:app/widgets/policy
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html))
(in-package #:app/widgets/policy)


(defwidget policy-page ()
  ())


(defun make-policy-page ()
  (make-instance 'policy-page))


(defmethod reblocks/widget:render ((widget policy-page))
  (with-html
    (:ol :class "w-2/3 list-decimal"
     (:li
      (:p "Мы не скачиваем и не сохраняем непосредственно ваши данные. Сервис использует API облачных сервисов, чтобы получить список файлов, их размеры и хэшсуммы. Это необходимо для рассчёта занятого места и представлении этих данных в виде диаграммы. Хранение метаданных носит сессионный характер и все данные удаляются из памяти сервера после вашего выхода из системы."))

     (:li
      (:p "Решение об удалении файлов принимаете вы сами."))

     (:li
      (:p "Создатели сервиса Cloud Analyzer не несут ответственности за ошибочно удалённые файлы."))
     
     (:li
      (:p
       ("При желании, вы может провести аудит [исходного кода сервиса](https://github.com/40ants/cloud-analyzer), чтобы убедиться в его надёжности."))))))


(defmethod reblocks/widget:get-css-classes ((widget policy-page))
  (list* "flex"
         "justify-around"
         (call-next-method)))
