(uiop:define-package #:cloud-analyzer/models/run-stats
  (:use #:cl)
  (:import-from #:cloud-analyzer/models/db
                #:execute
                #:with-connection))
(in-package #:cloud-analyzer/models/run-stats)

;; Schema
;; create table run_stats (username text, created_at timestamp, total bigint, used bigint, folders_count bigint, files_count bigint);
;; create index run_stats_idx on run_stats (username, created_at);

(defun save-run-stats (username total used folders-count files-count)
  (with-connection
      (execute "INSERT INTO run_stats (created_at, username, total, used, folders_count, files_count) VALUES (now(), ?, ?, ?, ?, ?)"
               username
               total
               used
               folders-count
               files-count)))



