(uiop:define-package #:app/widgets/analyzer
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:app/widgets/diagram
                #:make-disk-size
                #:highlight-block)
  (:import-from #:app/widgets/tree
                #:expand-path
                #:make-tree-widget)
  (:import-from #:app/datasource
                #:make-disk-tree)
  (:import-from #:app/widgets/progress-bar
                #:on-next-update
                #:increment-progress
                #:make-progress-bar)
  (:import-from #:app/api
                #:du
                #:get-total-usage)
  (:import-from #:app/api
                #:*token*)
  (:import-from #:app/widgets/login
                #:get-token
                #:get-username)
  (:import-from #:log4cl-extras/error
                #:with-log-unhandled)
  (:import-from #:reblocks-ui/form
                #:render-form-and-button)
  (:import-from #:app/widgets/yandex-metrika
                #:reach-goal
                #:render-counter)
  (:import-from #:app/models/run-stats
                #:save-run-stats))
(in-package #:app/widgets/analyzer)


(defwidget analyzer ()
  ((username :initarg :username
             :reader username)
   (progress :initarg :progress
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
                     (multiple-value-bind (data num-dirs num-files)
                         (du :increment-progress #'increment-progress-wrapper)
                       (set-data widget data num-dirs num-files)

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
                                         (cons 'app/api::*token*
                                               token)
                                         bordeaux-threads:*default-special-bindings*))
      (values))))


(defun make-analyzer ()
  (let* ((*token* (get-token))
         (total-size (get-total-usage :unit :raw))
         (progress-bar (make-progress-bar total-size))
         (usage-progress (multiple-value-bind (usage total)
                             (app/api::get-total-usage)
                           (multiple-value-bind (raw-usage raw-total)
                               (app/api::get-total-usage :unit :raw)
                             (let ((title (format nil "Занято ~A из ~A."
                                                  usage total)))
                               (make-progress-bar raw-total
                                                  :refresh nil
                                                  :progress raw-usage
                                                  :title title)))))
         (analyzer (make-instance 'analyzer
                                  :username (reblocks/session:get-value :username)
                                  :progress progress-bar
                                  :usage-progress usage-progress)))
    (start-processing analyzer)
    (values analyzer)))


(defun set-data (analyzer data num-dirs num-files)
  (let* ((datasource (make-disk-tree data))
         (tree (make-tree-widget datasource (list "/")))
         (diagram (make-disk-size data)))
    (flet ((on-block-selection-wrapper (path)
             (on-block-selection analyzer path))
           (on-file-selection-wrapper (path)
             (on-file-selection analyzer path)))
      (setf (app/widgets/diagram::on-block-selection diagram)
            #'on-block-selection-wrapper)
      (setf (app/widgets/tree::on-click tree)
            #'on-file-selection-wrapper)
      (setf (files analyzer)
            tree)
      (setf (disk-space analyzer)
            diagram)
      (multiple-value-bind (used total)
          (get-total-usage :unit :raw)
        (save-run-stats (username analyzer)
                        total
                        used
                        num-dirs
                        num-files))
      analyzer)))


(defmethod reblocks/widget:render ((widget analyzer))
  (flet ((try-again (&rest rest)
           (declare (ignore rest))
           (reach-goal "disk-size-recalculation")
           (setf (disk-space widget) nil
                 (files widget) nil
                 (processing-error widget) nil
                 (progress widget) (make-progress-bar (get-total-usage :unit :raw)))
           (start-processing widget)
           (reblocks/widget:update widget)))
    (render-counter)
    
    (reblocks/html:with-html
      (cond
        ((processing-error widget)
         (reach-goal "error-shown")
         (:div :class "callout alert"
               (:p "При обработке данных произошла ошибка:")
               (:p (format nil "\"~A\"" (processing-error widget))))
         (:p (render-form-and-button
              "Попробовать снова"
              #'try-again
              :method :post)))
        ((files widget)
         (reach-goal "diagram-shown")
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


;; (defmethod reblocks/dependencies:get-dependencies ((widget analyzer))
;;   nil
;;   ;; (list
;;   ;;  ;; (reblocks-lass:make-dependency
;;   ;;  ;;   '(.analyzer
;;   ;;  ;;     :display flex
;;   ;;  ;;     :flex-direction column
;;   ;;  ;;     :align-items center
;;   ;;  ;;     (header
;;   ;;  ;;      :width 100%
;;   ;;  ;;      :display flex
;;   ;;  ;;      :flex-direction row
;;   ;;  ;;      :align-items center
;;   ;;  ;;      :justify-content space-between
;;   ;;  ;;      (h1 :margin-left 1em
;;   ;;  ;;          :width 100%
;;   ;;  ;;          :text-align center)

;;   ;;  ;;      (form
;;   ;;  ;;       :justify-self end
;;   ;;  ;;       :margin-right 1em)

;;   ;;  ;;      ((:and .button .logout)
;;   ;;  ;;       :margin 0))))
;;   ;;  )
;;   )
