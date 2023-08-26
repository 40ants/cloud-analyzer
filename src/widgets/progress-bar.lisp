(uiop:define-package #:app/widgets/progress-bar
  (:use #:cl)
  (:import-from #:bordeaux-threads)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:app/widgets/yandex-metrika
                #:reach-goal))
(in-package #:app/widgets/progress-bar)


(defwidget progress-bar ()
  ((total-size :initarg :total-size
               :reader total-size)
   (progress :initform 0
             :initarg :progress
             :accessor progress)
   (last-filename :initform nil
                  :initarg :last-filename
                  :accessor last-filename)
   (lock :initform (bt:make-lock)
         :reader lock)
   (on-next-update :initform nil
                   :accessor on-next-update)
   (refresh-code :initform nil
                 :accessor refresh-code)
   (title :initform nil
          :initarg :title
          :reader title)

   (intermediate-goals :initform
                       (list (cons 1  "processed-01")
                             (cons 25 "processed-25")
                             (cons 50 "processed-50")
                             (cons 75 "processed-75")
                             (cons 90 "processed-90"))
                       :accessor intermediate-goals)))


(defun make-progress-bar (total-size &key
                                       (progress 0)
                                       (refresh t)
                                       title)
  (let* ((progress (make-instance 'progress-bar
                                  :total-size total-size
                                  :progress progress
                                  :title title)))
    (when refresh
      (let* ((code (reblocks/actions:make-action
                    (lambda (&rest rest)
                      (declare (ignore rest))
                      (if (on-next-update progress)
                          (funcall (on-next-update progress))
                          (reblocks/widget:update progress)))))
             (refresh-code
               (ps:ps* `(ps:chain window
                                  (set-timeout
                                   (lambda ()
                                     (initiate-action ,code))
                                   1000)))))
        (setf (refresh-code progress)
              refresh-code)))
    (values progress)))


(defun increment-progress (progress-bar filename size)
  (bt:with-lock-held ((lock progress-bar))
    (incf (progress progress-bar) size)
    (setf (last-filename progress-bar) filename)))


(defmethod reblocks/widget:render ((widget progress-bar))
  (let* ((value (* (/ (progress widget)
                      (total-size widget))
                   100))
         (percent
           (format nil "~A%"
                   ;; We need to remove dot if current
                   ;; progress has no fractional part.
                   ;; Otherwise, there will be a bug
                   ;; displaying empty progress on the frontend:
                   (string-right-trim
                    '(#\.)
                    (if (< value 10)
                        (format nil "~,2F"
                                value)
                        (format nil "~,1F"
                                value)))))
         (next-goal (first
                     (intermediate-goals widget))))

    ;; Зафиксируем прогресс в метрике
    (when (and next-goal
               (> value (car next-goal)))
      (reach-goal (cdr next-goal))
      (setf (intermediate-goals widget)
            (cdr (intermediate-goals widget))))
    
    (reblocks/html:with-html
      (:div :class "progress-container"
            :title (title widget)
            (:div :class "filler"
                  :style (format nil "width: ~A"
                                 percent)
                  "")
            (:div :class "percent"
                  percent))
      (when (last-filename widget)
        (:div :class "filename"
              (last-filename widget)))
      (when (refresh-code widget)
        (:script (:raw (refresh-code widget)))))))


(defmethod reblocks/widget:get-css-classes ((widget progress-bar))
  (call-next-method))


(defmethod reblocks/dependencies:get-dependencies ((widget progress-bar))
  (list
   (reblocks-lass:make-dependency
     '(.progress-bar
       :width 400px
       :display flex
       :flex-direction column
       (.progress-container
        :display flex
        :flex-direction row
        :border 1px solid gray)
       (.filler :background "#69ff68")
       (.percent :padding-left 0.2em)
       (.filename :overflow hidden
                  :white-space nowrap)))))
