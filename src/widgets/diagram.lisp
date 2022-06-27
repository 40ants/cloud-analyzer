(uiop:define-package #:app/widgets/diagram
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:org.shirakumo.alloy.colored)
  (:import-from #:reblocks/actions)
  (:import-from #:reblocks/dependencies)
  (:import-from #:reblocks-lass)
  (:import-from #:app/api)
  (:import-from #:reblocks/widget
                #:defwidget))
(in-package #:app/widgets/diagram)


(defwidget disk-size ()
  ((highlight :documentation "Path to currently selected block, starting from \"/\"."
              :initform nil
              :accessor highlight)
   (data :documentation "Folders structure."
         :initarg :data
         :reader data)
   (on-block-selection :initform nil
                       :initarg :on-block-selection
                       :accessor on-block-selection)))


(defun make-disk-size (data &key on-block-selection)
  (make-instance 'disk-size
                 :data data
                 :on-block-selection on-block-selection))


(defparameter *colors* (make-hash-table :test 'equal)
  "Media type to color.")


(defun %random-hue ()
   ;; Here we are skipping Hue from 0 to 50 and from 280 to 360 because it is RED
   ;; and we are using this color for selection border:
  (flet ((min-distance (value)
           (cond
             ((zerop (hash-table-count *colors*))
              360.0)
             (t
              (loop for color being the hash-value of *colors*
                    for distance = (abs (- (org.shirakumo.alloy.colored:h color)
                                           value))
                    minimize distance))))
         (random-h ()
           (random 360.0)
           ;; (+ 50.0 (random  (- 280.0 50.0)))
           ))
    (loop with max-distance = 0
          with better-h = nil
          repeat 100
          for value = (random-h)
          for distance = (min-distance value)
          when (> distance max-distance)
            do (setf max-distance distance
                     better-h value)
          finally (return better-h))))


(defun random-color ()
  (org.shirakumo.alloy.colored:hsl
   (%random-hue)
   (+ 0.5 (random 0.5))
   (+ 0.5 (random 0.5))))

(defparameter *known-media-types* (make-hash-table :test 'equal))


(defun get-color (media-type)
  (let ((media-type (or media-type "unknown")))
    (or (gethash media-type *colors*)
        (setf (gethash media-type *colors*)
              (random-color)))))


(defun lighter (hsl-color &optional (value 0.1))
  (org.shirakumo.alloy.colored:hsl
   (org.shirakumo.alloy.colored:h hsl-color)
   (org.shirakumo.alloy.colored:s hsl-color)
   (* (org.shirakumo.alloy.colored:l hsl-color)
      (+ 1.0 value))))


(defun darker (hsl-color &optional (value 0.1))
  (org.shirakumo.alloy.colored:hsl
   (org.shirakumo.alloy.colored:h hsl-color)
   (org.shirakumo.alloy.colored:s hsl-color)
   (* (org.shirakumo.alloy.colored:l hsl-color)
      (- 1.0 value))))


(defun render-folder (widget folder used-media-types)
  (let* ((highlight
           ;; To simplify comparison with paths inside data
           (reverse (highlight widget)))
         (total (getf folder :size))
         (min-block-size (/ total
                            5000)))
    (labels ((too-small (folder)
               (< (getf folder :size)
                  min-block-size))
             (remove-small-folders (folders)
               (remove-if #'too-small folders))
             (full (path)
               (with-output-to-string (out)
                 (loop for part in (reverse path)
                       do (write-string part out)
                       unless (str:ends-with-p "/" part)
                         do (write-string "/" out))))
             (render (subfolders path &key (vertical t))
               (let* ((largest (first subfolders))
                      (largest-size (if largest
                                        (getf largest :size)
                                        0))
                      (lesser (rest subfolders))
                      (lesser-size (loop for subfolder in lesser
                                         sum (getf subfolder :size)))
                      (total (+ largest-size lesser-size))
                      (nested-subfolders (remove-small-folders (getf largest :subfolders)))
                      (largest-path
                        (cons (getf largest :name)
                              path))
                      (nested-blocks-orientation
                        ;; This is orientation for nested
                        ;; blocks:
                        (if vertical
                            "row"
                            "column")))

                 (when (> total 0)
                   (reblocks/html:with-html
                     (setf (gethash (getf largest :media-type)
                                    used-media-types)
                           t)
                     (:div :class (str:join " "
                                            (list* (format nil "b-~A" (getf largest :media-type))
                                                   (when (equal largest-path highlight)
                                                     (list "selected"))))
                           :style (format nil "flex-grow: ~4F; flex-direction: ~A;"
                                          (/ largest-size
                                             total)
                                          nested-blocks-orientation)
                           :title (full largest-path)
                           (if nested-subfolders
                               (render nested-subfolders
                                       largest-path
                                       :vertical (not vertical))
                               " "))
                     (when lesser
                       (:div :style (format nil "flex-grow: ~4F; flex-direction: ~A;"
                                            (/ lesser-size
                                               total)
                                            nested-blocks-orientation)
                             (render lesser
                                     path
                                     :vertical (not vertical)))))))))
      (render (remove-small-folders (getf folder :subfolders))
              (list (getf folder :name))))))


(defun hsl-to-css (hsl-color)
  (let ((rgb (org.shirakumo.alloy.colored:convert hsl-color 'org.shirakumo.alloy.colored:rgb)))
    (format nil "rgba(~A, ~A, ~A, ~A)"
            (floor (* (org.shirakumo.alloy.colored:red rgb)
                      255))
            (floor (* (org.shirakumo.alloy.colored:green rgb)
                      255))
            (floor (* (org.shirakumo.alloy.colored:blue rgb)
                      255))
            (floor (* (org.shirakumo.alloy.colored:alpha rgb)
                      255)))))


(defun complement-color (hsl-color)
  (let* ((h (org.shirakumo.alloy.colored:h hsl-color))
         (new-h (mod (+ h 180.0)
                     360.)))
    (org.shirakumo.alloy.colored:hsl new-h
                                     (org.shirakumo.alloy.colored:s hsl-color)
                                     (org.shirakumo.alloy.colored:l hsl-color))))


(defun make-color-gradients (used-media-types)
  (lass:compile-and-write
   (list* '.disk-size
          (loop for media-type being the hash-key of used-media-types
                for color = (get-color media-type)
                for light = (lighter color)
                for dark = (darker color 0.3)
                collect (list (format nil ".b-~A" media-type)
                              :background
                              (format nil
                                      "linear-gradient(110deg, ~A 0%, ~A 100%);"
                                      (hsl-to-css light)
                                      (hsl-to-css dark)))))))


(defmethod reblocks/widget:render ((widget disk-size))
  (let* ((on-click-action-code
           (reblocks/actions:make-action
            (lambda (&key path &allow-other-keys)
              (let ((new-path (list* "/"
                                     (str:split #\/ (string-trim '(#\/) path)))))
                (setf (highlight widget)
                      new-path)
                (when (on-block-selection widget)
                  (funcall (on-block-selection widget)
                           new-path)))
              (reblocks/widget:update widget))))
         (on-click (format nil "initiateAction(\"~A\", {\"args\": {\"path\": event.target.title}}); event.stopPropagation(); return false;"
                           on-click-action-code)))
    (let ((used-media-types (make-hash-table :test 'equal)))
      (reblocks/html:with-html
        (:div :class "container"
              :style "width: 600px; height: 600px; flex-direction: column;"
              :onclick on-click
              " "
              (render-folder widget
                             (data widget)
                             used-media-types))
        (:div :class "legend"
              (loop for media-type in (sort (alexandria:hash-table-keys *colors*)
                                            #'string<)
                    for color = (gethash media-type *colors*)
                    do (:div :class (format nil "legend-item b-~A"
                                            media-type)
                             media-type)))
        (:style (make-color-gradients used-media-types))))))


(defmethod reblocks/dependencies:get-dependencies ((widget disk-size))
  (list
   (reblocks-lass:make-dependency
     '(:keyframes "rotate"
       (100% :transform "rotate(1turn)")))
   (reblocks-lass:make-dependency
     '(.disk-size
       (.selected :position relative
                  :z-index 0
                  ;; :width 400px
                  ;; :height 300px
                  :border-radius 2px
                  :overflow hidden
                  :padding 3px
        )
       ((:and .selected :before)
        :content ""
	:position absolute
	:z-index 2
	:left -50%
	:top -50%
	:width 200%
	:height 200%
	:background-color "#399953"
	:background-repeat no-repeat
	:background-size "50% 50%, 50% 50%"
	:background-position "0 0, 100% 0, 100% 100%, 0 100%"
	:background-image "linear-gradient(#399953, #399953), linear-gradient(#fbb300, #fbb300), linear-gradient(#d53e33, #d53e33), linear-gradient(#377af5, #377af5)"
        :animation "rotate 10s linear infinite")
       ((:and .selected :after)
        :content ""
        :position absolute
        :z-index -1
        :left 3px
        :top 3px
        :width "calc(100% - 6px)"
        :height "calc(100% - 6px)"
        :background white
        :border-radius 5px)
        
       (.legend :margin-top 1em
                :display flex
                :width 600px
                :flex-wrap wrap
                :justify-content space-evenly
        
        (.legend-item :display inline-block
                      :padding-left 0.3em
                      :padding-right 0.3em
                      :margin-top 0.5em
                      :margin-right 1em
                      :margin-left 1em))
       (.container
        :cursor pointer
        :display flex
        :flex-wrap nowrap
        :box-sizing border-box
        
        (div :display flex
             :flex-wrap nowrap
             :box-sizing border-box))))))


(defun highlight-block (widget path)
  (setf (highlight widget) path)
  (reblocks/widget:update widget))
