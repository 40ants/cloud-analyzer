(uiop:define-package #:app/models/run-stats
  (:use #:cl)
  (:import-from #:app/models/db
                #:query
                #:execute
                #:with-connection))
(in-package #:app/models/run-stats)

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



(defun get-stats ()
  (with-connection
      (values
       (getf (first (query "SELECT COUNT(*) as value FROM run_stats WHERE created_at > NOW() - '24 hour'::interval"))
             :|value|)
       (getf (first (query "SELECT COUNT(DISTINCT username) as value FROM run_stats WHERE created_at > NOW() - '24 hour'::interval"))
             :|value|)
       (first (query "SELECT username,
                             COUNT(*) as count
                        FROM run_stats
                       WHERE created_at > NOW() - '24 hour'::interval
                    GROUP BY username
                    ORDER BY count DESC
                       LIMIT 1")))))
