;;;; src/dsl/defreport.lisp
;;;; defreport macro implementation with enhanced report types

(in-package #:project-juggler)

;;; ============================================================================
;;; Auto-Filter Definitions
;;; ============================================================================

(defparameter *auto-filters*
  (list
   ;; Task filters
   :critical (lambda (task)
               (and (task-slack task)
                    (zerop (task-slack task))))
   :scheduled (lambda (task)
                (task-scheduled-p task))
   :milestones (lambda (task)
                 (task-milestone-p task))
   :incomplete (lambda (task)
                 (< (or (task-complete task) 0) 100))
   :high-priority (lambda (task)
                    (> (task-priority task) 800))
   :overdue (lambda (task)
              (and (task-end task)
                   (< (or (task-complete task) 0) 100)
                   (date< (task-end task) (current-date)))))
  "Predefined auto-filter functions for common report patterns.")

(defun get-auto-filter (filter-name)
  "Get an auto-filter function by name."
  (getf *auto-filters* filter-name))

(defun current-date ()
  "Get current date as pj-date."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (date year month day)))

;;; ============================================================================
;;; Enhanced Report Classes
;;; ============================================================================

(defclass gantt-report (report)
  ((width :initarg :width :initform 1000 :accessor gantt-width)
   (height :initarg :height :initform nil :accessor gantt-height))
  (:documentation "Gantt chart report"))

(defclass critical-path-report (task-report)
  ()
  (:documentation "Report showing only critical path tasks"))

(defclass milestone-report (task-report)
  ()
  (:documentation "Report showing only milestones"))

(defclass evm-report (report)
  ((include-summary :initarg :include-summary :initform nil :accessor evm-include-summary)
   (status-date :initarg :status-date :initform nil :accessor evm-status-date))
  (:documentation "Earned Value Management report"))

(defclass simulation-report (report)
  ((trials :initarg :trials :initform 1000 :accessor simulation-trials-count)
   (percentiles :initarg :percentiles :initform '(10 50 75 90 95) :accessor simulation-percentiles-list)
   (include-histogram :initarg :include-histogram :initform nil :accessor simulation-include-histogram))
  (:documentation "Monte Carlo simulation report"))

(defclass risk-report (report)
  ((filter-status :initarg :filter-status :initform nil :accessor risk-filter-status)
   (sort-by-score :initarg :sort-by-score :initform nil :accessor risk-sort-by-score))
  (:documentation "Risk register report"))

;;; ============================================================================
;;; Report Generation Methods
;;; ============================================================================

(defmethod generate-report ((report gantt-report) project)
  "Generate a Gantt chart report."
  (ecase (report-format report)
    (:json (generate-gantt-json project))
    (:svg (generate-gantt-svg project
                              :width (gantt-width report)
                              :height (gantt-height report)))
    (:html (generate-gantt-html project
                                :width (gantt-width report)
                                :height (gantt-height report)
                                :title (report-title report)))))

(defmethod generate-report ((report critical-path-report) project)
  "Generate a critical path report (only tasks with zero slack)."
  (let ((tasks (collect-tasks-for-report report project)))
    ;; Filter to only critical path tasks
    (setf tasks (remove-if-not (lambda (task)
                                 (and (task-slack task)
                                      (zerop (task-slack task))))
                               tasks))
    ;; Sort by start date
    (setf tasks (sort tasks #'date< :key #'task-start))
    (ecase (report-format report)
      (:html (generate-html-task-report report tasks))
      (:csv (generate-csv-task-report report tasks)))))

(defmethod generate-report ((report milestone-report) project)
  "Generate a milestone report (only milestone tasks)."
  (let ((tasks (collect-tasks-for-report report project)))
    ;; Filter to only milestones
    (setf tasks (remove-if-not #'task-milestone-p tasks))
    ;; Sort by start date
    (setf tasks (sort tasks #'date< :key #'task-start))
    (ecase (report-format report)
      (:html (generate-html-task-report report tasks))
      (:csv (generate-csv-task-report report tasks)))))

(defmethod generate-report ((report evm-report) project)
  "Generate an EVM report with project metrics."
  (let ((status-date (or (evm-status-date report) (current-date)))
        (tasks (loop for task being the hash-values of (project-tasks project)
                    collect task)))
    ;; Sort by start date
    (setf tasks (sort tasks #'date< :key #'task-start))
    (ecase (report-format report)
      (:html (generate-html-evm-report report project tasks status-date))
      (:csv (generate-csv-evm-report report project tasks status-date)))))

(defmethod generate-report ((report simulation-report) project)
  "Generate a Monte Carlo simulation report."
  (let* ((trials (simulation-trials-count report))
         (results (run-monte-carlo-simulation project :trials trials))
         (summary (simulation-summary results)))
    (ecase (report-format report)
      (:html (generate-html-simulation-report report results summary))
      (:csv (generate-csv-simulation-report report results summary)))))

(defmethod generate-report ((report risk-report) project)
  "Generate a risk register report."
  (let ((risks (copy-list (project-risk-register project))))
    ;; Filter by status if specified
    (when (risk-filter-status report)
      (setf risks (remove-if-not
                   (lambda (risk)
                     (eq (risk-status risk) (risk-filter-status report)))
                   risks)))
    ;; Sort by score if requested
    (when (risk-sort-by-score report)
      (setf risks (sort risks #'>
                        :key (lambda (r) (risk-score r)))))
    (ecase (report-format report)
      (:html (generate-html-risk-report report risks))
      (:csv (generate-csv-risk-report report risks)))))

;;; ============================================================================
;;; EVM Report Generation
;;; ============================================================================

(defun generate-html-evm-report (report project tasks status-date)
  "Generate HTML EVM report."
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%<html>~%<head>~%")
    (format s "  <title>~A</title>~%" (report-title report))
    (format s "  <style>~%")
    (format s "    table { border-collapse: collapse; width: 100%%; }~%")
    (format s "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~%")
    (format s "    th { background-color: #4CAF50; color: white; }~%")
    (format s "    tr:nth-child(even) { background-color: #f2f2f2; }~%")
    (format s "    .summary { background-color: #e3f2fd; padding: 15px; margin-bottom: 20px; }~%")
    (format s "    .good { color: green; } .bad { color: red; }~%")
    (format s "  </style>~%")
    (format s "</head>~%<body>~%")
    (format s "<h1>~A</h1>~%" (report-title report))

    ;; Project summary
    (when (evm-include-summary report)
      (let ((pv (calculate-planned-value project status-date))
            (ev (calculate-earned-value project))
            (spi (calculate-spi project status-date)))
        (format s "<div class=\"summary\">~%")
        (format s "<h2>Project Summary</h2>~%")
        (format s "<p>Status Date: ~A</p>~%" (format-date-iso status-date))
        (format s "<p>Planned Value (PV): ~,1F%</p>~%" pv)
        (format s "<p>Earned Value (EV): ~,1F%</p>~%" ev)
        (format s "<p>Schedule Variance (SV): ~,1F%</p>~%" (- ev pv))
        (format s "<p class=\"~A\">Schedule Performance Index (SPI): ~,2F</p>~%"
                (if (>= spi 1.0) "good" "bad") spi)
        (format s "</div>~%")))

    ;; Task table
    (format s "<table>~%<tr>~%")
    (dolist (col (report-columns report))
      (format s "  <th>~A</th>~%" (string-capitalize (symbol-name col))))
    (format s "</tr>~%")

    (dolist (task tasks)
      (format s "<tr>~%")
      (dolist (col (report-columns report))
        (let ((value (get-evm-column-value task col project status-date)))
          (format s "  <td>~A</td>~%" (html-escape (format-evm-cell col value)))))
      (format s "</tr>~%"))

    (format s "</table>~%</body>~%</html>")))

(defun generate-csv-evm-report (report project tasks status-date)
  "Generate CSV EVM report."
  (with-output-to-string (s)
    (format s "~{~A~^,~}~%"
            (mapcar (lambda (col) (string-capitalize (symbol-name col)))
                   (report-columns report)))
    (dolist (task tasks)
      (format s "~{~A~^,~}~%"
              (mapcar (lambda (col)
                       (csv-escape (format-evm-cell col
                                    (get-evm-column-value task col project status-date))))
                     (report-columns report))))))

(defun get-evm-column-value (task column project status-date)
  "Get EVM-related column value for a task."
  (case column
    (:name (task-name task))
    (:id (task-id task))
    (:start (task-start task))
    (:end (task-end task))
    (:complete (task-complete task))
    (:pv (task-planned-value task status-date))
    (:ev (or (task-complete task) 0))
    (:sv (- (or (task-complete task) 0)
            (task-planned-value task status-date)))
    (:planned-value (task-planned-value task status-date))
    (:earned-value (or (task-complete task) 0))
    (:schedule-variance (- (or (task-complete task) 0)
                           (task-planned-value task status-date)))
    (t (get-task-column-value task column))))

(defun task-planned-value (task status-date)
  "Calculate what percentage of task should be complete by status date."
  (let ((start (task-start task))
        (end (task-end task)))
    (cond
      ((or (null start) (null end)) 0)
      ((date< status-date start) 0)
      ((date>= status-date end) 100)
      (t (let ((total-days (max 1 (days-between start end)))
               (elapsed-days (days-between start status-date)))
           (min 100 (round (* 100 (/ elapsed-days total-days)))))))))

(defun format-evm-cell (column value)
  "Format an EVM column value."
  (case column
    ((:pv :ev :sv :planned-value :earned-value :schedule-variance)
     (if (numberp value) (format nil "~,1F%" value) ""))
    ((:spi) (if (numberp value) (format nil "~,2F" value) ""))
    (t (format-cell column value))))

;;; ============================================================================
;;; Simulation Report Generation
;;; ============================================================================

(defun generate-html-simulation-report (report results summary)
  "Generate HTML simulation report."
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%<html>~%<head>~%")
    (format s "  <title>~A</title>~%" (report-title report))
    (format s "  <style>~%")
    (format s "    body { font-family: Arial, sans-serif; margin: 20px; }~%")
    (format s "    table { border-collapse: collapse; margin: 20px 0; }~%")
    (format s "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~%")
    (format s "    th { background-color: #4CAF50; color: white; }~%")
    (format s "    .stats { background-color: #f5f5f5; padding: 15px; margin: 10px 0; }~%")
    (format s "    .histogram { font-family: monospace; }~%")
    (format s "  </style>~%")
    (format s "</head>~%<body>~%")
    (format s "<h1>~A</h1>~%" (report-title report))

    ;; Statistics summary
    (format s "<div class=\"stats\">~%")
    (format s "<h2>Simulation Statistics</h2>~%")
    (format s "<p>Trials: ~A</p>~%" (getf summary :trial-count))
    (format s "<p>Mean Duration: ~,1F days</p>~%" (getf summary :mean))
    (format s "<p>Std Deviation: ~,2F days</p>~%" (getf summary :std-dev))
    (format s "<p>Minimum: ~,1F days</p>~%" (getf summary :min))
    (format s "<p>Maximum: ~,1F days</p>~%" (getf summary :max))
    (format s "</div>~%")

    ;; Percentiles table
    (format s "<h2>Percentile Analysis</h2>~%")
    (format s "<table>~%<tr><th>Percentile</th><th>Duration (days)</th></tr>~%")
    (dolist (p (simulation-percentiles-list report))
      (format s "<tr><td>P~A</td><td>~,1F</td></tr>~%"
              p (simulation-percentile results p)))
    (format s "</table>~%")

    ;; Histogram if requested
    (when (simulation-include-histogram report)
      (format s "<h2>Distribution</h2>~%")
      (format s "<pre class=\"histogram\">~%")
      (let ((histogram (simulation-histogram results :bins 10)))
        (dolist (bin histogram)
          (let* ((bin-min (first bin))
                 (bin-max (second bin))
                 (count (third bin))
                 (pct (/ count (/ (getf summary :trial-count) 100.0)))
                 (bar (make-string (min 50 (round pct)) :initial-element #\#)))
            (format s "~6,1F - ~6,1F: ~A (~,1F%)~%"
                    bin-min bin-max bar pct))))
      (format s "</pre>~%"))

    (format s "</body>~%</html>")))

(defun generate-csv-simulation-report (report results summary)
  "Generate CSV simulation report."
  (with-output-to-string (s)
    (format s "Percentile,Duration~%")
    (dolist (p (simulation-percentiles-list report))
      (format s "P~A,~,1F~%" p (simulation-percentile results p)))))

;;; ============================================================================
;;; Risk Report Generation
;;; ============================================================================

(defun generate-html-risk-report (report risks)
  "Generate HTML risk report."
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%<html>~%<head>~%")
    (format s "  <title>~A</title>~%" (report-title report))
    (format s "  <style>~%")
    (format s "    table { border-collapse: collapse; width: 100%%; }~%")
    (format s "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~%")
    (format s "    th { background-color: #4CAF50; color: white; }~%")
    (format s "    tr:nth-child(even) { background-color: #f2f2f2; }~%")
    (format s "    .critical { background-color: #ffebee; }~%")
    (format s "    .high { background-color: #fff3e0; }~%")
    (format s "  </style>~%")
    (format s "</head>~%<body>~%")
    (format s "<h1>~A</h1>~%" (report-title report))

    (format s "<table>~%<tr>~%")
    (dolist (col (report-columns report))
      (format s "  <th>~A</th>~%" (string-capitalize (symbol-name col))))
    (format s "</tr>~%")

    (dolist (risk risks)
      (let ((severity (risk-severity risk)))
        (format s "<tr~A>~%"
                (case severity
                  (:critical " class=\"critical\"")
                  (:high " class=\"high\"")
                  (t ""))))
      (dolist (col (report-columns report))
        (let ((value (get-risk-column-value risk col)))
          (format s "  <td>~A</td>~%" (html-escape (format-risk-cell col value)))))
      (format s "</tr>~%"))

    (format s "</table>~%</body>~%</html>")))

(defun generate-csv-risk-report (report risks)
  "Generate CSV risk report."
  (with-output-to-string (s)
    (format s "~{~A~^,~}~%"
            (mapcar (lambda (col) (string-capitalize (symbol-name col)))
                   (report-columns report)))
    (dolist (risk risks)
      (format s "~{~A~^,~}~%"
              (mapcar (lambda (col)
                       (csv-escape (format-risk-cell col
                                    (get-risk-column-value risk col))))
                     (report-columns report))))))

(defun get-risk-column-value (risk column)
  "Get column value from a risk."
  (case column
    (:id (risk-id risk))
    (:name (risk-name risk))
    (:description (risk-description risk))
    (:probability (risk-probability risk))
    (:impact (risk-impact risk))
    (:score (risk-score risk))
    (:severity (risk-severity risk))
    (:category (risk-category risk))
    (:status (risk-status risk))
    (:owner (risk-owner risk))
    (:mitigation (risk-mitigation risk))
    (:contingency (risk-contingency risk))
    (:tasks (risk-tasks risk))
    (:schedule-impact (risk-schedule-impact risk))
    (t nil)))

(defun format-risk-cell (column value)
  "Format a risk column value."
  (case column
    ((:probability :impact :score :schedule-impact)
     (if (numberp value) (format nil "~,0F%" (* 100 value)) ""))
    ((:severity :status :category)
     (if value (string-capitalize (symbol-name value)) ""))
    ((:tasks)
     (if value (format nil "~{~A~^, ~}" value) ""))
    (t (if value (format nil "~A" value) ""))))

;;; ============================================================================
;;; Enhanced defreport Macro
;;; ============================================================================

(defmacro defreport (id title &body body)
  "Define a report within a project.

   Usage:
     ;; Standard task report
     (defreport summary \"Project Summary\"
       :type :task
       :format :html
       :columns (:id :name :start :end :duration)
       :filter (lambda (task) (> (task-priority task) 500))
       :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

     ;; With auto-filter
     (defreport critical \"Critical Path\"
       :type :task
       :format :html
       :columns (:name :start :end :slack)
       :auto-filter :critical)

     ;; Critical path report (automatic filtering)
     (defreport cp \"Critical Path\"
       :type :critical-path
       :format :html
       :columns (:name :start :end :slack))

     ;; Milestone report
     (defreport milestones \"Milestones\"
       :type :milestone
       :format :html
       :columns (:name :start))

     ;; EVM report
     (defreport evm \"EVM Metrics\"
       :type :evm
       :format :html
       :columns (:name :pv :ev :sv)
       :include-summary t)

     ;; Simulation report
     (defreport sim \"Monte Carlo\"
       :type :simulation
       :format :html
       :trials 5000
       :percentiles (10 50 75 90 95)
       :include-histogram t)

     ;; Risk report
     (defreport risks \"Risk Register\"
       :type :risk
       :format :html
       :columns (:name :probability :impact :score :severity)
       :filter-status :open
       :sort-by :score-desc)

     ;; Gantt chart
     (defreport gantt \"Project Timeline\"
       :type :gantt
       :format :svg
       :width 1200
       :height 600)

   Report Types:
     :task          - Standard task report
     :resource      - Resource report
     :critical-path - Tasks on critical path only
     :milestone     - Milestone tasks only
     :evm           - Earned Value Management report
     :simulation    - Monte Carlo simulation report
     :risk          - Risk register report
     :gantt         - Gantt chart

   Formats:
     :html  - HTML table (default)
     :csv   - CSV export
     :json  - JSON (for :gantt)
     :svg   - SVG (for :gantt)

   Auto-Filters (for :type :task):
     :critical      - Only critical path tasks
     :scheduled     - Only scheduled tasks
     :milestones    - Only milestones
     :incomplete    - Tasks < 100% complete
     :high-priority - Priority > 800
     :overdue       - Past end date and incomplete"

  ;; Parse keyword arguments
  (let ((type-expr :task)
        (format-expr :html)
        (columns-expr nil)
        (filter-expr nil)
        (sort-by-expr nil)
        (auto-filter-expr nil)
        (width-expr 1000)
        (height-expr nil)
        (trials-expr 1000)
        (percentiles-expr '(10 50 75 90 95))
        (include-histogram-expr nil)
        (include-summary-expr nil)
        (filter-status-expr nil)
        (remaining body))

    (loop while (and remaining (keywordp (first remaining)))
          do (let ((keyword (first remaining))
                   (value (second remaining)))
               (case keyword
                 (:type (setf type-expr value))
                 (:format (setf format-expr value))
                 (:columns (setf columns-expr value))
                 (:filter (setf filter-expr value))
                 (:sort-by (setf sort-by-expr value))
                 (:auto-filter (setf auto-filter-expr value))
                 (:width (setf width-expr value))
                 (:height (setf height-expr value))
                 (:trials (setf trials-expr value))
                 (:percentiles (setf percentiles-expr value))
                 (:include-histogram (setf include-histogram-expr value))
                 (:include-summary (setf include-summary-expr value))
                 (:filter-status (setf filter-status-expr value))
                 (t (warn "Unknown keyword in defreport: ~A" keyword)))
               (setf remaining (cddr remaining))))

    ;; Determine report class
    (let ((report-class (case type-expr
                         (:task 'task-report)
                         (:resource 'resource-report)
                         (:critical-path 'critical-path-report)
                         (:milestone 'milestone-report)
                         (:evm 'evm-report)
                         (:simulation 'simulation-report)
                         (:risk 'risk-report)
                         (:gantt 'gantt-report)
                         (t (error "Unknown report type: ~A" type-expr)))))

      ;; Build filter expression with auto-filter support
      (let ((final-filter
              (cond
                (filter-expr filter-expr)
                (auto-filter-expr `(get-auto-filter ,auto-filter-expr))
                (t nil))))

        ;; Build report instance creation based on type
        `(let ((report (make-instance ',report-class
                                      :id ',id
                                      :title ,title
                                      :format ,format-expr
                                      ,@(when columns-expr `(:columns ',columns-expr))
                                      ,@(when final-filter `(:filter ,final-filter))
                                      ,@(when sort-by-expr
                                          (if (eq sort-by-expr :score-desc)
                                              `(:sort-by-score t)
                                              `(:sort-by ,sort-by-expr)))
                                      ,@(when (eq type-expr :gantt)
                                          `(:width ,width-expr :height ,height-expr))
                                      ,@(when (eq type-expr :simulation)
                                          `(:trials ,trials-expr
                                            :percentiles ',percentiles-expr
                                            :include-histogram ,include-histogram-expr))
                                      ,@(when (eq type-expr :evm)
                                          `(:include-summary ,include-summary-expr))
                                      ,@(when filter-status-expr
                                          `(:filter-status ,filter-status-expr)))))

           ;; Register report with project
           (setf (gethash ',id (project-reports *current-project*)) report)

           ;; Return the report
           report)))))
