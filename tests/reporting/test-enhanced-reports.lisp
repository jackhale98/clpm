;;;; tests/reporting/test-enhanced-reports.lisp
;;;; Tests for Enhanced Reporting Features

(in-package #:project-juggler-tests)

(def-suite enhanced-reporting-suite
  :in project-juggler-suite
  :description "Tests for enhanced reporting features")

(in-suite enhanced-reporting-suite)

;;; ============================================================================
;;; Gantt JSON Export Tests
;;; ============================================================================

(test gantt-json-basic
  "Generate Gantt chart as JSON"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((json (generate-gantt-json *current-project*)))
      (is (stringp json))
      ;; Should contain JSON structure markers
      (is (search "[" json))
      (is (search "]" json))
      (is (search "{" json))
      ;; Should contain task data
      (is (search "Task 1" json))
      (is (search "Task 2" json))
      ;; Should contain date info
      (is (search "2024" json)))))

(test gantt-json-includes-dependencies
  "Gantt JSON includes dependency information"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((json (generate-gantt-json *current-project*)))
      ;; Should have dependencies array
      (is (search "dependencies" json))
      ;; t2's dependency on t1 (symbol prints as uppercase T1)
      (is (search "T1" json)))))

(test gantt-json-includes-progress
  "Gantt JSON includes task completion/progress"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Set task to 50% complete
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 50)

    (let ((json (generate-gantt-json *current-project*)))
      (is (search "progress" json))
      (is (search "50" json)))))

(test gantt-json-includes-resources
  "Gantt JSON includes allocated resources"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((json (generate-gantt-json *current-project*)))
      (is (search "resources" json))
      (is (search "Developer" json)))))

(test gantt-json-includes-milestones
  "Gantt JSON marks milestones correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask m1 "Milestone 1"
      :depends-on (t1)
      :milestone t)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((json (generate-gantt-json *current-project*)))
      (is (search "milestone" json))
      (is (search "true" json)))))

(test gantt-json-includes-critical-path
  "Gantt JSON marks critical path tasks"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((json (generate-gantt-json *current-project*)))
      (is (search "critical" json)))))

;;; ============================================================================
;;; Gantt SVG Export Tests
;;; ============================================================================

(test gantt-svg-basic
  "Generate Gantt chart as SVG"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((svg (generate-gantt-svg *current-project*)))
      (is (stringp svg))
      ;; Should be valid SVG
      (is (search "<svg" svg))
      (is (search "</svg>" svg))
      ;; Should contain task labels
      (is (search "Task 1" svg))
      (is (search "Task 2" svg)))))

(test gantt-svg-has-bars
  "Gantt SVG contains task bars"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((svg (generate-gantt-svg *current-project*)))
      ;; Should have rect elements for bars
      (is (search "<rect" svg)))))

(test gantt-svg-has-dependency-arrows
  "Gantt SVG shows dependency arrows"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((svg (generate-gantt-svg *current-project*)))
      ;; Should have line or path elements for arrows
      (is (or (search "<line" svg)
              (search "<path" svg))))))

(test gantt-svg-critical-path-highlighted
  "Gantt SVG highlights critical path"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((svg (generate-gantt-svg *current-project*)))
      ;; Critical path should have different color (red or similar)
      (is (or (search "#ff" svg)
              (search "critical" svg)
              (search "red" svg))))))

(test gantt-svg-with-options
  "Gantt SVG respects width/height options"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((svg (generate-gantt-svg *current-project* :width 1200 :height 600)))
      (is (search "1200" svg))
      (is (search "600" svg)))))

;;; ============================================================================
;;; Critical Path Report Type Tests
;;; ============================================================================

(test defreport-critical-path-type
  "defreport supports :type :critical-path"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))
    (deftask t3 "Non-Critical Task"
      :start (date 2024 3 1)
      :duration (duration 2 :days))

    (defreport critical "Critical Path"
      :type :critical-path
      :format :html
      :columns (:name :start :end :slack))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'critical)))
      (is (stringp output))
      ;; Should include tasks on critical path (slack = 0)
      (is (search "Task 1" output))
      (is (search "Task 2" output))
      ;; Should NOT include non-critical task
      (is (not (search "Non-Critical Task" output))))))

(test critical-path-report-auto-sorted
  "Critical path report is sorted by start date"
  (with-test-project
    (deftask t1 "First Task"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Second Task"
      :depends-on (t1)
      :duration (duration 3 :days))

    (defreport critical "Critical Path"
      :type :critical-path
      :format :html
      :columns (:name :start :end))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'critical)))
      ;; First Task should appear before Second Task
      (is (< (search "First Task" output)
             (search "Second Task" output))))))

;;; ============================================================================
;;; Milestone Report Type Tests
;;; ============================================================================

(test defreport-milestone-type
  "defreport supports :type :milestone"
  (with-test-project
    (deftask t1 "Regular Task"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask m1 "Milestone 1"
      :depends-on (t1)
      :milestone t)
    (deftask m2 "Milestone 2"
      :start (date 2024 4 1)
      :milestone t)

    (defreport milestones "Project Milestones"
      :type :milestone
      :format :html
      :columns (:name :start))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'milestones)))
      (is (stringp output))
      ;; Should include milestones
      (is (search "Milestone 1" output))
      (is (search "Milestone 2" output))
      ;; Should NOT include regular tasks
      (is (not (search "Regular Task" output))))))

(test milestone-report-csv-format
  "Milestone report works with CSV format"
  (with-test-project
    (deftask m1 "Phase 1 Complete"
      :start (date 2024 3 15)
      :milestone t)

    (defreport ms-csv "Milestones CSV"
      :type :milestone
      :format :csv
      :columns (:name :start))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'ms-csv)))
      (is (search "Phase 1 Complete" output))
      (is (search "," output)))))

;;; ============================================================================
;;; EVM Report Type Tests
;;; ============================================================================

(test defreport-evm-type
  "defreport supports :type :evm"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 5 :days))

    (defreport evm-report "EVM Metrics"
      :type :evm
      :format :html
      :columns (:name :planned-value :earned-value :schedule-variance))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Set up baseline
    (let ((baseline (create-baseline *current-project* :name "Original")))
      (set-project-baseline *current-project* baseline))

    ;; Mark task 50% complete
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 50)

    (let ((output (generate-project-report *current-project* 'evm-report)))
      (is (stringp output))
      ;; Should include EVM metrics in output
      (is (search "EVM Metrics" output)))))

(test evm-report-project-summary
  "EVM report includes project-level summary"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))

    (defreport evm "EVM Summary"
      :type :evm
      :format :html
      :columns (:name :pv :ev :sv :spi)
      :include-summary t)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((baseline (create-baseline *current-project* :name "Plan")))
      (set-project-baseline *current-project* baseline))

    (let ((output (generate-project-report *current-project* 'evm)))
      ;; Should have summary section
      (is (or (search "Summary" output)
              (search "Total" output)
              (search "Project" output))))))

;;; ============================================================================
;;; Simulation Report Type Tests
;;; ============================================================================

(test defreport-simulation-type
  "defreport supports :type :simulation"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask t1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 7 :days)
                 :pessimistic (duration 14 :days))
      :allocate (dev1))

    (defreport sim-report "Monte Carlo Results"
      :type :simulation
      :format :html
      :trials 100
      :columns (:percentile :duration))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'sim-report)))
      (is (stringp output))
      ;; Should include percentile information
      (is (or (search "P50" output)
              (search "50" output)
              (search "percentile" output))))))

(test simulation-report-percentiles
  "Simulation report shows key percentiles"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask t1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 20 :days))
      :allocate (dev1))

    (defreport sim "Simulation"
      :type :simulation
      :format :html
      :trials 500
      :percentiles (10 50 75 90 95))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'sim)))
      ;; Should show specified percentiles
      (is (search "10" output))
      (is (search "50" output))
      (is (search "90" output)))))

(test simulation-report-includes-histogram
  "Simulation report can include histogram data"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask t1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 20 :days))
      :allocate (dev1))

    (defreport sim "Simulation with Histogram"
      :type :simulation
      :format :html
      :trials 200
      :include-histogram t)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'sim)))
      ;; Should have histogram-related content
      (is (or (search "histogram" output)
              (search "distribution" output)
              (search "bin" output)
              (search "count" output))))))

;;; ============================================================================
;;; Risk Report Type Tests
;;; ============================================================================

(test defreport-risk-type
  "defreport supports :type :risk"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (create-risk *current-project* 'risk1 "Technical Risk"
                 :probability 0.3
                 :impact 0.8
                 :category :technical)

    (create-risk *current-project* 'risk2 "Resource Risk"
                 :probability 0.5
                 :impact 0.4
                 :category :resource)

    (defreport risks "Risk Register"
      :type :risk
      :format :html
      :columns (:name :probability :impact :score :severity))

    (finalize-project *current-project*)

    (let ((output (generate-project-report *current-project* 'risks)))
      (is (stringp output))
      (is (search "Technical Risk" output))
      (is (search "Resource Risk" output)))))

(test risk-report-sorted-by-score
  "Risk report can be sorted by risk score"
  (with-test-project
    (create-risk *current-project* 'risk1 "Low Risk"
                 :probability 0.1
                 :impact 0.2)
    (create-risk *current-project* 'risk2 "High Risk"
                 :probability 0.8
                 :impact 0.9)

    (defreport risks "Risks by Score"
      :type :risk
      :format :html
      :columns (:name :score)
      :sort-by :score-desc)

    (let ((output (generate-project-report *current-project* 'risks)))
      ;; High Risk should appear before Low Risk
      (is (< (search "High Risk" output)
             (search "Low Risk" output))))))

(test risk-report-filter-by-status
  "Risk report can filter by status"
  (with-test-project
    (create-risk *current-project* 'risk1 "Open Risk"
                 :probability 0.5
                 :impact 0.5)
    (create-risk *current-project* 'risk2 "Closed Risk"
                 :probability 0.3
                 :impact 0.3)

    ;; Close the second risk
    (let ((risk (get-risk *current-project* 'risk2)))
      (setf (risk-status risk) :closed))

    (defreport open-risks "Open Risks Only"
      :type :risk
      :format :html
      :columns (:name :status)
      :filter-status :open)

    (let ((output (generate-project-report *current-project* 'open-risks)))
      (is (search "Open Risk" output))
      (is (not (search "Closed Risk" output))))))

;;; ============================================================================
;;; Auto-Filter Tests
;;; ============================================================================

(test auto-filter-critical
  "Auto-filter :critical shows only critical path tasks"
  (with-test-project
    (deftask t1 "Critical Task"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Another Critical"
      :depends-on (t1)
      :duration (duration 3 :days))
    (deftask t3 "Non-Critical"
      :start (date 2024 3 1)
      :duration (duration 2 :days))

    (defreport critical "Critical Tasks"
      :type :task
      :format :html
      :columns (:name :slack)
      :auto-filter :critical)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'critical)))
      (is (search "Critical Task" output))
      (is (search "Another Critical" output))
      (is (not (search "Non-Critical" output))))))

(test auto-filter-scheduled
  "Auto-filter :scheduled shows only scheduled tasks"
  (with-test-project
    (deftask t1 "Scheduled Task"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    ;; A milestone without explicit start acts as unscheduled until deps resolve
    ;; But for testing, we'll check the task after scheduling
    (deftask t2 "Another Scheduled"
      :depends-on (t1)
      :duration (duration 3 :days))

    (defreport scheduled "Scheduled Tasks"
      :type :task
      :format :html
      :columns (:name :start :end)
      :auto-filter :scheduled)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'scheduled)))
      ;; Both tasks should be scheduled after scheduling
      (is (search "Scheduled Task" output))
      (is (search "Another Scheduled" output)))))

(test auto-filter-milestones
  "Auto-filter :milestones shows only milestones"
  (with-test-project
    (deftask t1 "Regular Task"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask m1 "Milestone"
      :depends-on (t1)
      :milestone t)

    (defreport ms "Milestones"
      :type :task
      :format :html
      :columns (:name :start)
      :auto-filter :milestones)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'ms)))
      (is (search "Milestone" output))
      (is (not (search "Regular Task" output))))))

(test auto-filter-overdue
  "Auto-filter :overdue shows tasks past their end date"
  ;; Need custom project with past dates for this test
  (let* ((*current-project* (make-instance 'project
                                           :id 'overdue-test
                                           :name "Overdue Test Project"
                                           :start (date 2020 1 1)
                                           :end (date 2030 12 31)))
         (*current-namespace* (ensure-namespace 'default)))
    ;; Create a task that should be complete (in the past)
    (deftask t1 "Overdue Task"
      :start (date 2020 1 1)
      :duration (duration 5 :days))
    (deftask t2 "Future Task"
      :start (date 2030 1 1)
      :duration (duration 5 :days))

    (defreport overdue "Overdue Tasks"
      :type :task
      :format :html
      :columns (:name :end :complete)
      :auto-filter :overdue)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'overdue)))
      (is (search "Overdue Task" output))
      (is (not (search "Future Task" output))))))

(test auto-filter-incomplete
  "Auto-filter :incomplete shows tasks not at 100%"
  (with-test-project
    (deftask t1 "Incomplete Task"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Complete Task"
      :start (date 2024 3 1)
      :duration (duration 3 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Mark t2 as complete
    (setf (task-complete (gethash 't2 (project-tasks *current-project*))) 100)

    (defreport incomplete "Incomplete Tasks"
      :type :task
      :format :html
      :columns (:name :complete)
      :auto-filter :incomplete)

    (let ((output (generate-project-report *current-project* 'incomplete)))
      (is (search "Incomplete Task" output))
      (is (not (search "Complete Task" output))))))

(test auto-filter-high-priority
  "Auto-filter :high-priority shows priority > 800"
  (with-test-project
    (deftask t1 "High Priority"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :priority 900)
    (deftask t2 "Normal Priority"
      :start (date 2024 3 1)
      :duration (duration 3 :days)
      :priority 500)

    (defreport high-pri "High Priority Tasks"
      :type :task
      :format :html
      :columns (:name :priority)
      :auto-filter :high-priority)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'high-pri)))
      (is (search "High Priority" output))
      (is (not (search "Normal Priority" output))))))

;;; ============================================================================
;;; Gantt Report Type in defreport Tests
;;; ============================================================================

(test defreport-gantt-json
  "defreport with :type :gantt :format :json"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 3 :days))

    (defreport gantt-json "Gantt JSON Export"
      :type :gantt
      :format :json)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'gantt-json)))
      (is (stringp output))
      (is (search "{" output))
      (is (search "Task 1" output)))))

(test defreport-gantt-svg
  "defreport with :type :gantt :format :svg"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (defreport gantt-svg "Gantt SVG Export"
      :type :gantt
      :format :svg
      :width 1000
      :height 400)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'gantt-svg)))
      (is (stringp output))
      (is (search "<svg" output))
      (is (search "Task 1" output)))))

(test defreport-gantt-html
  "defreport with :type :gantt :format :html produces embedded SVG"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (defreport gantt-html "Gantt HTML"
      :type :gantt
      :format :html)

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((output (generate-project-report *current-project* 'gantt-html)))
      (is (stringp output))
      ;; HTML with embedded SVG
      (is (search "<html" output))
      (is (search "<svg" output)))))

;;; ============================================================================
;;; Format Cell Extensions Tests
;;; ============================================================================

(test format-cell-evm-columns
  "Format cell handles EVM-specific columns"
  (is (stringp (format-cell :pv 50.0)))
  (is (stringp (format-cell :ev 45.0)))
  (is (stringp (format-cell :sv -5.0)))
  (is (stringp (format-cell :spi 0.9))))

(test format-cell-risk-columns
  "Format cell handles risk-specific columns"
  (is (stringp (format-cell :probability 0.5)))
  (is (stringp (format-cell :impact 0.8)))
  (is (stringp (format-cell :score 0.4)))
  (is (stringp (format-cell :severity :high))))

;;; ============================================================================
;;; Resource Utilization Columns Tests
;;; ============================================================================

(test resource-report-utilization-columns
  "Resource reports support utilization columns"
  (with-test-project
    (defresource dev1 "Developer" :efficiency 1.0 :rate 100.0)
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :allocate (dev1))

    (defreport resources "Resource Utilization"
      :type :resource
      :format :html
      :columns (:name :efficiency :booked-hours :utilization))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Add a booking
    (let ((resource (gethash 'dev1 (project-resources *current-project*)))
          (task (gethash 't1 (project-tasks *current-project*))))
      (add-booking task resource
                   (date 2024 3 1 9 0 0)
                   (duration 8 :hours)))

    (let ((output (generate-project-report *current-project* 'resources)))
      (is (stringp output))
      (is (search "Developer" output)))))
