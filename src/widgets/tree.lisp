(uiop:define-package #:yandex-disk-cleaner/widgets/tree
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:yandex-disk-cleaner/utils
                #:humanize-size)
  (:import-from #:reblocks/dependencies))
(in-package #:yandex-disk-cleaner/widgets/tree)


(defwidget node ()
  ((label :initarg :label
          :accessor label)
   (parent :initarg :parent
           :reader parent)
   (selected :initform nil
             :accessor selected)
   (size :initform 0
         :initarg :size
         :reader size)))


(defwidget expandable-node (node)
  ((expanded :initform nil
             :accessor expanded)
   (content :initarg :content
            :initform nil
            :accessor content)))


(defwidget leaf-node (node)
  ())


(defwidget tree-widget ()
  ((data-source :initform nil
                :initarg :data-source
                :reader data-source)
   (path :initform nil
         :initarg :path
         :reader path)
   (nodes :initform nil
          :initarg :nodes
          :accessor nodes)
   (on-click :initform nil
             :initarg :on-click
             :documentation "A function to be called when user selects one of nodes."
             :accessor on-click)))


(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S" (label obj))))


(defmethod print-object ((obj tree-widget) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S" (path obj))))


(defun make-tree-widget (data-source path &key on-click)
  (check-type path list)
  (let* ((tree (make-instance 'tree-widget
                              :data-source data-source
                              :path path
                              :on-click on-click))
         (nodes (loop for item in (retrieve-nodes data-source path)
                      for name = (getf item :name)
                      for type = (getf item :type)
                      for size = (getf item :size)
                      if (eql type :dir)
                        collect (make-instance 'expandable-node
                                               :label name
                                               :parent tree
                                               :size size)
                      else
                        collect (make-instance 'leaf-node
                                               :label name
                                               :parent tree
                                               :size size))))
    (setf (nodes tree)
          nodes)
    (values tree)))


(defgeneric retrieve-nodes (datasource path))


(defmethod reblocks/widget:render ((widget tree-widget))
  (reblocks/html:with-html
    (:ul :class "tree"
         (loop for node in (nodes widget)
               do (reblocks/widget:render node)))))


(defmethod reblocks/widget:render ((widget expandable-node))
  (let ((toggle (reblocks/actions:make-js-action
                 (lambda (&rest args)
                   (declare (ignore args))
                   (cond
                     ((expanded widget)
                      (collapse widget))
                     (t
                      (expand widget)))
                   (reblocks/widget:update widget))))
        (on-click
          (reblocks/actions:make-js-action
           (lambda (&rest rest)
             (declare (ignore rest))
             (let ((full-path (append
                               (path (parent widget))
                               (list (label widget)))))
               (when (on-click (parent widget))
                 (funcall (on-click (parent widget))
                          full-path)))))))
    (reblocks/html:with-html
      (:span :class "switcher"
             :onclick toggle)
      (:span :class "filename"
             :onclick on-click
             :data-path (let ((parent-path (path (parent widget))))
                          (format nil
                                  "~A~{~A~^/~}/~A"
                                  (first parent-path)
                                  (rest parent-path)
                                  (label widget)))
             (label widget))
      (when (size widget)
        (:span :class "size"
               (humanize-size (size widget))))
      
      (when (expanded widget)
        (if (content widget)
            (reblocks/widget:render (content widget))
            (:p "No content"))))))


(defmethod reblocks/widget:render ((widget leaf-node))
  (let ((on-click
          (reblocks/actions:make-js-action
           (lambda (&rest rest)
             (declare (ignore rest))
             (let ((full-path (append
                               (path (parent widget))
                               (list (label widget)))))
               (when (on-click (parent widget))
                 (funcall (on-click (parent widget))
                          full-path)))))))
    (reblocks/html:with-html
      (:span :class "filename"
             :onclick on-click
             :data-path (let ((parent-path (path (parent widget))))
                          (format nil
                                  "~A~{~A~^/~}/~A"
                                  (first parent-path)
                                  (rest parent-path)
                                  (label widget)))
             (label widget))
      
      (when (size widget)
       (:span :class "size"
              (humanize-size (size widget)))))))


(defmethod reblocks/widget:get-html-tag ((widget expandable-node))
  :li)

(defmethod reblocks/widget:get-html-tag ((widget leaf-node))
  :li)


(defmethod reblocks/widget:get-css-classes ((widget expandable-node))
  (append (unless (expanded widget)
            (list "closed"))
          (when (selected widget)
            (list "selected"))
          (call-next-method)))


(defmethod reblocks/dependencies:get-dependencies ((widget tree-widget))
  ;; based on 
  (list
   (reblocks-lass:make-dependency
     '(body
       (.tree :list-style none
              :cursor pointer
              :padding-left 24px
              :overflow hidden
              :transition "height 150ms ease-out, opacity 150ms ease-out"
        ((.selected > .filename) :background "#c6e3f9")
        (.size :margin-left 1em
               :color "#4949ff")
        (.switcher :display inline-block
                   :vertical-align middle
                   :width 20px
                   :height 20px
                   :cursor pointer
                   :position relative
                   :transition "transform 150ms ease-out")
        ((:and .switcher :before) :position absolute
                                  :top 8px
                                  :left 6px
                                  :display block
                                  :content " "
                                  :border 4px solid transparent
                                  :border-top 4px solid "#00000077"
                                  :transition border-color 150ms)
        ((:and .switcher :hover :before) :border-top 4px solid "#000000DD")
        (.closed
         (.switcher :transform "rotate(-90deg)")))))))


(defun expand (widget)
  (check-type widget expandable-node)
  (unless (content widget)
    (let* ((parent (parent widget))
           (current-path (append (path parent)
                                 (list (label widget))))
           (data-source (data-source parent)))
      (setf (content widget)
            (make-tree-widget data-source current-path
                              :on-click (on-click parent)))))
  (setf (expanded widget) t))


(defun collapse (widget)
  (check-type widget expandable-node)
  (setf (expanded widget) nil))


(defun deselect-all-nodes (tree-widget)
  (labels ((recursive-deselect (tree)
             (loop for node in (nodes tree)
                   do (setf (selected node)
                            nil)
                   when (and (typep node 'expandable-node)
                             (content node))
                     do (recursive-deselect (content node)))))
    (recursive-deselect tree-widget)
    (values)))


(defun select (node)
  (setf (selected node)
        t))


(defun expand-path (tree-widget path)
  (unless (string= (car path)
                   "/")
    (error "First path element should be equal to \"/\""))

  (deselect-all-nodes tree-widget)
  
  (let ((leaf-node nil))
    (labels ((recursive-expand (tree path)
               (when path
                 (loop with label-to-expand = (car path)
                       for node in (nodes tree)
                       when (and (equal (label node)
                                        label-to-expand)
                                 (typep node 'expandable-node))
                         do (expand node)
                            (setf leaf-node node)
                            (recursive-expand (content node)
                                              (cdr path))
                            (return)))))
      (recursive-expand tree-widget (cdr path))
      
      (when leaf-node
        (select leaf-node)
      
        (reblocks/widget:update tree-widget)
        
        (let ((js-code
                (ps:ps*
                 `(ps:chain document
                            (get-element-by-id ,(reblocks/widgets/dom:dom-id leaf-node))
                            (scroll-into-view (ps:create :behavior "smooth"
                                                         :block "center"))))))
          (reblocks/response:send-script js-code)))
      (values))))
