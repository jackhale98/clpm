;;;; src/reporting/reports.lisp
;;;; Base reporting functionality

(in-package #:project-juggler)

;;; ============================================================================
;;; Main Report Generation
;;; ============================================================================

(defgeneric generate-report (report project)
  (:documentation "Generate a report for the given project"))

(defmethod generate-report ((report task-report) project)
  "Generate a task report"
  (let ((tasks (collect-tasks-for-report report project)))
    (ecase (report-format report)
      (:html (generate-html-task-report report tasks))
      (:csv (generate-csv-task-report report tasks)))))

(defmethod generate-report ((report resource-report) project)
  "Generate a resource report"
  (let ((resources (collect-resources-for-report report project)))
    (ecase (report-format report)
      (:html (generate-html-resource-report report resources))
      (:csv (generate-csv-resource-report report resources)))))

;;; ============================================================================
;;; Task Collection and Filtering
;;; ============================================================================

(defun collect-tasks-for-report (report project)
  "Collect and filter tasks for a report"
  (let ((tasks (loop for task being the hash-values of (project-tasks project)
                    collect task)))
    ;; Apply filter if specified
    (when (report-filter report)
      (setf tasks (remove-if-not (report-filter report) tasks)))

    ;; Sort if specified
    (sort-tasks-for-report tasks report)))

(defun sort-tasks-for-report (tasks report)
  "Sort tasks according to report specification"
  (if (report-sort-by report)
      (sort (copy-list tasks) (report-sort-by report))
      tasks))

;;; ============================================================================
;;; Resource Collection and Filtering
;;; ============================================================================

(defun collect-resources-for-report (report project)
  "Collect and filter resources for a report"
  (let ((resources (loop for resource being the hash-values of (project-resources project)
                        collect resource)))
    ;; Apply filter if specified
    (when (report-filter report)
      (setf resources (remove-if-not (report-filter report) resources)))

    ;; Sort if specified
    (if (report-sort-by report)
        (sort (copy-list resources) (report-sort-by report))
        resources)))

;;; ============================================================================
;;; Cell Formatting
;;; ============================================================================

(defun format-cell (column-name value)
  "Format a cell value based on its column type"
  (cond
    ((null value) "")

    ;; Date columns
    ((member column-name '(:start :end :early-start :early-finish
                          :late-start :late-finish))
     (if (typep value 'pj-date)
         (format nil "~4,'0D-~2,'0D-~2,'0D"
                (date-year value)
                (date-month value)
                (date-day value))
         ""))

    ;; Duration columns
    ((member column-name '(:duration :effort :slack))
     (if (typep value 'duration)
         (format nil "~A ~(~A~)"
                (duration-value value)
                (duration-unit value))
         ""))

    ;; Numeric columns
    ((member column-name '(:priority :efficiency :rate :criticalness
                          :path-criticalness :complete))
     (if (numberp value)
         (if (integerp value)
             (format nil "~D" value)
             (format nil "~,2F" value))
         ""))

    ;; Boolean columns
    ((member column-name '(:milestone :scheduled))
     (if value "Yes" "No"))

    ;; Default: convert to string
    (t (format nil "~A" value))))
