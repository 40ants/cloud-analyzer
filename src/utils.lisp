(uiop:define-package #:app/utils
  (:use #:cl))
(in-package #:app/utils)


(defun path-to-string (path)
  "PATH is a list starting from \"/\""
  (check-type path list)
  
  (format nil
          "~A~{~A~^/~}"
          (first path)
          (rest path)))


;; This code was taken from https://github.com/muyinliu/cl-diskspace
;; and modified to accept optional UNIT keyword
(defun humanize-size (number &key unit)
  (check-type number integer)
  (let ((sizes '(80 70 60 50 40 30 20 10))
        (units '(:YB :ZB :EB :PB :TB :GB :MB :KB)))
    (flet ((return-result (size unit)
             (return-from humanize-size
               (format nil "~,2F ~A"
                       (float (/ number (ash 1 size)))
                       unit))))
      (cond
        (unit
         (unless (member unit units)
           (error "UNIT should be one of ~A" units))
        
         (let* ((position (position unit units))
                (size (elt sizes position)))
           (return-result size unit)))
        (t
         (loop for size in sizes
               and unit in units
               when (> (ash number (- size)) 0)
                 do (return-result size unit)))))))
