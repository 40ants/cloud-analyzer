(uiop:define-package #:yandex-disk-cleaner/widgets/analyzer
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:yandex-disk-cleaner/widgets/diagram
                #:make-disk-size
                #:highlight-block)
  (:import-from #:yandex-disk-cleaner/widgets/tree
                #:expand-path
                #:make-tree-widget)
  (:import-from #:yandex-disk-cleaner/datasource
                #:make-disk-tree)
  (:import-from #:yandex-disk-cleaner/widgets/progress-bar
                #:on-next-update
                #:increment-progress
                #:make-progress-bar)
  (:import-from #:yandex-disk-cleaner/api
                #:du
                #:get-total-usage)
  (:import-from #:yandex-disk-cleaner/widgets/login
                #:get-token
                #:get-username)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button))
(in-package #:yandex-disk-cleaner/widgets/analyzer)


(defwidget analyzer ()
  ((progress :initarg :progress
             :initform nil
             :accessor progress)
   (usage-progress :initarg :usage-progress
                   :initform nil
                   :accessor usage-progress)
   (files :initarg :files
          :initform nil
          :accessor files)
   (disk-space :initarg :disk-space
               :initform nil
               :accessor disk-space)
   (processing-error :initform nil
                     :documentation "Here we'll save error object if data processing will break."
                     :accessor processing-error)))


(defun on-block-selection (analyzer path)
  (log:info "Changing path to" path)
  (expand-path (files analyzer)
               path))

(defun on-file-selection (analyzer path)
  (log:info "Changing path to" path)
  (expand-path (files analyzer)
               path)
  (highlight-block (disk-space analyzer)
                   path))


(defun start-processing (widget)
  (let ((progress-bar (progress widget))
        (username (get-username))
        (token (get-token)))
    (labels ((retrieve-data ()
               (handler-case
                   (with-log-unhandled ()
                     (let ((data (du :increment-progress #'increment-progress-wrapper)))
                       (set-data widget data)

                       (setf (on-next-update progress-bar)
                             (lambda ()
                               (reblocks/widget:update widget)))))
                 (error (condition)
                   (setf (on-next-update progress-bar)
                         (lambda ()
                           (setf (processing-error widget)
                                 condition)
                           (reblocks/widget:update widget))))))
             (increment-progress-wrapper (filename size)
               (increment-progress progress-bar filename size)))
      (bt:make-thread #'retrieve-data
                      :name (format nil "data-retrieve-for ~A"
                                    username)
                      :initial-bindings (list*
                                         (cons 'yandex-disk-cleaner/api::*token*
                                               token)
                                         bordeaux-threads:*default-special-bindings*))
      (values))))


(defun make-analyzer ()
  (let* ((total-size (get-total-usage :unit :raw))
         (progress-bar (make-progress-bar total-size))
         (usage-progress (multiple-value-bind (usage total)
                             (yandex-disk-cleaner/api::get-total-usage)
                           (multiple-value-bind (raw-usage raw-total)
                               (yandex-disk-cleaner/api::get-total-usage :unit :raw)
                             (let ((title (format nil "Занято ~A из ~A."
                                                  usage total)))
                               (make-progress-bar raw-total
                                                  :progress raw-usage
                                                  :title title)))))
         (analyzer (make-instance 'analyzer
                                  :progress progress-bar
                                  :usage-progress usage-progress)))
    (start-processing analyzer)
    (values analyzer)))


(defun set-data (analyzer data)
  (let* ((datasource (make-disk-tree data))
         (tree (make-tree-widget datasource (list "/")))
         (diagram (make-disk-size data)))
    (flet ((on-block-selection-wrapper (path)
             (on-block-selection analyzer path))
           (on-file-selection-wrapper (path)
             (on-file-selection analyzer path)))
      (setf (yandex-disk-cleaner/widgets/diagram::on-block-selection diagram)
            #'on-block-selection-wrapper)
      (setf (yandex-disk-cleaner/widgets/tree::on-click tree)
            #'on-file-selection-wrapper)
      (setf (files analyzer)
            tree)
      (setf (disk-space analyzer)
            diagram)
      analyzer)))


(defmethod reblocks/widget:render ((widget analyzer))
  (flet ((try-again (&rest rest)
           (declare (ignore rest))
           (setf (disk-space widget) nil
                 (files widget) nil
                 (processing-error widget) nil
                 (progress widget) (make-progress-bar (get-total-usage :unit :raw)))
           (start-processing widget)
           (reblocks/widget:update widget)))
    (reblocks/html:with-html
      (:h1 :class "header"
           (:a :href "/"
               "Cloud Analyzer"))
      (cond
        ((processing-error widget)
         (:div :class "callout alert"
               (:p "При обработке данных произошла ошибка:")
               (:p (format nil "\"~A\"" (processing-error widget))))
         (:p (render-form-and-button
              "Попробовать снова"
              #'try-again
              :method :post)))
        ((files widget)
         (:table
          (:tr :style "vertical-align: top;"
               (:td :style "height: 600px; overflow: scroll; display: block;"
                    (reblocks/widget:render (files widget)))
               (:td (reblocks/widget:render (disk-space widget))))
          (when (usage-progress widget)
            (:tr (:td)
                 (:td "Заполненность диска:"
                      (reblocks/widget:render (usage-progress widget)))))
          (:tr (:td)
               (:td (render-form-and-button
                     "Обновить"
                     #'try-again
                     :method :post)))))
        (t
         (reblocks/widget:render (progress widget)))))))


(defmethod reblocks/dependencies:get-dependencies ((widget analyzer))
  (list
   (reblocks-lass:make-dependency
     '(.analyzer
       :display flex
       :flex-direction column
       :align-items center))))
