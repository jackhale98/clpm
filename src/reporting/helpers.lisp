;;;; src/reporting/helpers.lisp
;;;; Helper functions for working with project reports

(in-package #:project-juggler)

;;; ============================================================================
;;; Report Generation Helpers
;;; ============================================================================

(defun generate-project-report (project report-id)
  "Generate a report by ID from the project's registered reports.

   Usage:
     (generate-project-report *current-project* 'summary)

   Returns the generated report as a string (HTML or CSV)."
  (let ((report (gethash report-id (project-reports project))))
    (unless report
      (error "No report with ID ~A found in project" report-id))
    (generate-report report project)))

(defun save-project-report (project report-id filepath)
  "Generate and save a report to a file.

   Usage:
     (save-project-report *current-project* 'summary \"report.html\")

   Returns the filepath."
  (let ((content (generate-project-report project report-id)))
    (with-open-file (out filepath
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (write-string content out))
    filepath))

(defun list-project-reports (project)
  "List all reports defined for a project.

   Returns a list of report IDs."
  (loop for id being the hash-keys of (project-reports project)
        collect id))

(defun get-project-report (project report-id)
  "Get a report instance by ID.

   Returns the report object or NIL if not found."
  (gethash report-id (project-reports project)))

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(defun generate-all-reports (project &optional output-directory)
  "Generate all registered reports for a project.

   If output-directory is provided, saves reports to files.
   Otherwise, returns a hash table mapping report-id to content.

   Usage:
     ;; Save all reports to files
     (generate-all-reports *current-project* \"reports/\")

     ;; Get all reports as strings
     (let ((reports (generate-all-reports *current-project*)))
       (maphash (lambda (id content) ...) reports))"
  (let ((results (make-hash-table :test 'eq)))
    (maphash (lambda (id report)
               (let ((content (generate-report report project)))
                 (if output-directory
                     ;; Save to file
                     (let* ((extension (ecase (report-format report)
                                        (:html ".html")
                                        (:csv ".csv")))
                            (filename (format nil "~A~A~A"
                                            output-directory
                                            (string-downcase (symbol-name id))
                                            extension)))
                       (ensure-directories-exist output-directory)
                       (with-open-file (out filename
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                         (write-string content out))
                       (setf (gethash id results) filename))
                     ;; Return content
                     (setf (gethash id results) content))))
             (project-reports project))
    results))

;;; ============================================================================
;;; Quick Report Creation (without DSL)
;;; ============================================================================

(defun quick-task-report (project
                          &key
                          (title "Task Report")
                          (format :html)
                          (columns '(:id :name :start :end :duration :priority))
                          filter
                          sort-by)
  "Quickly generate a task report without using defreport macro.

   Usage:
     (quick-task-report *current-project*
                        :format :csv
                        :columns '(:id :name :start :end)
                        :filter (lambda (task) (task-scheduled-p task))
                        :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

   Returns the generated report content as a string."
  (let ((report (make-instance 'task-report
                               :id (gensym "QUICK-REPORT-")
                               :title title
                               :format format
                               :columns columns
                               :filter filter
                               :sort-by sort-by)))
    (generate-report report project)))

(defun quick-resource-report (project
                               &key
                               (title "Resource Report")
                               (format :html)
                               (columns '(:id :name :efficiency :rate))
                               filter
                               sort-by)
  "Quickly generate a resource report without using defreport macro.

   Usage:
     (quick-resource-report *current-project*
                            :columns '(:id :name :criticalness))

   Returns the generated report content as a string."
  (let ((report (make-instance 'resource-report
                               :id (gensym "QUICK-REPORT-")
                               :title title
                               :format format
                               :columns columns
                               :filter filter
                               :sort-by sort-by)))
    (generate-report report project)))
