;;;; examples/simple-project.lisp
;;;; Example: Simple Website Project
;;;;
;;;; This example demonstrates:
;;;; - Basic task definition
;;;; - Resource allocation
;;;; - Task dependencies
;;;; - Critical path analysis
;;;; - TaskJuggler-style scenarios for what-if analysis
;;;; - EVM tracking
;;;; - Report generation

;;; Load the project-juggler system
(require :asdf)
(push (truename "../") asdf:*central-registry*)

;; Load quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load dependencies and project-juggler
(ql:quickload :project-juggler :silent t)

(in-package :project-juggler)

;;; =============================================================================
;;; PROJECT DEFINITION
;;; =============================================================================

(defproject simple-website "Company Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 5 31)
  :scenarios (plan delayed)  ; Two scenarios: plan (baseline) and delayed

  ;;; Resources
  (defresource developer "Web Developer"
    :efficiency 1.0
    :rate 100.0)

  (defresource designer "UI/UX Designer"
    :efficiency 1.0
    :rate 90.0)

  (defresource qa "QA Tester"
    :efficiency 0.9
    :rate 70.0)

  ;;; Tasks with scenario-specific values
  (deftask sp-requirements "Requirements Gathering"
    :duration (duration 1 :weeks)
    :delayed/duration (duration 2 :weeks)  ; Takes longer in delayed scenario
    :allocate (developer designer)
    :priority 1000)

  (deftask sp-design "UI/UX Design"
    :duration (duration 2 :weeks)
    :delayed/duration (duration 3 :weeks)
    :depends-on (sp-requirements)
    :allocate (designer)
    :priority 900)

  (deftask sp-frontend "Frontend Development"
    :duration (duration 3 :weeks)
    :delayed/duration (duration 4 :weeks)
    :depends-on (sp-design)
    :allocate (developer)
    :priority 850)

  (deftask sp-backend "Backend Development"
    :duration (duration 3 :weeks)
    :delayed/duration (duration 4 :weeks)
    :depends-on (sp-requirements)
    :allocate (developer)
    :priority 850)

  (deftask sp-integration "Integration & Testing"
    :duration (duration 1 :weeks)
    :delayed/duration (duration 2 :weeks)
    :depends-on (sp-frontend sp-backend)
    :allocate (developer qa)
    :priority 950)

  (deftask sp-deployment "Deployment & Launch"
    :duration (duration 3 :days)
    :depends-on (sp-integration)
    :allocate (developer)
    :priority 1000)

  ;;; Reports

  ;; Standard task summary report
  (defreport summary "Project Task Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration :priority))

  ;; CSV export for spreadsheet analysis
  (defreport csv-export "Task Export"
    :type :task
    :format :csv
    :columns (:id :name :start :end :duration))

  ;; Critical path report
  (defreport critical-tasks "Critical Path Tasks"
    :type :critical-path
    :format :html
    :columns (:name :start :end :slack :priority))

  ;; Gantt chart
  (defreport gantt-chart "Project Timeline"
    :type :gantt
    :format :html
    :width 1000
    :height 300))

;;; =============================================================================
;;; ANALYSIS AND REPORTING
;;; =============================================================================

(format t "~%==============================================================~%")
(format t "         Company Website Redesign - Project                     ~%")
(format t "==============================================================~%~%")

;; Finalize and schedule
(format t "> Finalizing project...~%")
(finalize-project *current-project*)

(format t "> Scheduling tasks...~%")
(schedule *current-project*)

(format t "Done!~%~%")

;; Display timeline
(format t "--------------------------------------------------------------~%")
(format t "PROJECT TIMELINE~%")
(format t "--------------------------------------------------------------~%")
(format t "Project: ~A~%" (project-name *current-project*))
(format t "Start:   ~A~%" (project-start *current-project*))
(format t "End:     ~A~%" (project-end *current-project*))
(format t "Tasks:   ~A~%~%" (hash-table-count (project-tasks *current-project*)))

;; Display task schedule
(format t "--------------------------------------------------------------~%")
(format t "TASK SCHEDULE~%")
(format t "--------------------------------------------------------------~%")
(maphash (lambda (id task)
           (declare (ignore id))
           (format t "- ~A~%" (task-name task))
           (format t "  ~A -> ~A (~A days)~%"
                   (task-start task)
                   (task-end task)
                   (if (task-duration task)
                       (duration-in-days (task-duration task))
                       0)))
         (project-tasks *current-project*))
(format t "~%")

;; Critical path analysis
(format t "--------------------------------------------------------------~%")
(format t "CRITICAL PATH ANALYSIS~%")
(format t "--------------------------------------------------------------~%")

(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical path (~A tasks):~%~%" (length critical-tasks))
  (dolist (task critical-tasks)
    (format t "- ~A (Slack: ~A days)~%"
            (task-name task)
            (task-slack task))))
(format t "~%")

;; Scenario comparison
(format t "--------------------------------------------------------------~%")
(format t "SCENARIO COMPARISON (TaskJuggler-style)~%")
(format t "--------------------------------------------------------------~%")

(format t "Scenarios: ~A~%" (list-scenarios *current-project*))
(format t "Baseline:  ~A (first scenario)~%~%" (baseline-scenario-id *current-project*))

(let ((comparison (compare-scenarios *current-project* 'plan 'delayed)))
  (format t "Plan scenario:~%")
  (format t "  Total duration: ~A days~%" (getf comparison :duration-1))
  (format t "~%")
  (format t "Delayed scenario:~%")
  (format t "  Total duration: ~A days~%" (getf comparison :duration-2))
  (format t "~%")
  (format t "Difference: +~A days in delayed scenario~%"
          (- (getf comparison :duration-2) (getf comparison :duration-1))))
(format t "~%")

;; Resource analysis
(format t "--------------------------------------------------------------~%")
(format t "RESOURCE ALLOCATION~%")
(format t "--------------------------------------------------------------~%")

(let ((overallocations (detect-resource-overallocations *current-project*)))
  (if (null overallocations)
      (format t "No resource over-allocations detected!~%~%")
      (progn
        (format t "WARNING: ~A over-allocations detected~%~%" (length overallocations))
        (dolist (oa overallocations)
          (format t "- ~A overallocated on ~A~%"
                  (overallocation-resource-id oa)
                  (overallocation-date oa))))))

;; Generate reports
(format t "--------------------------------------------------------------~%")
(format t "GENERATING REPORTS~%")
(format t "--------------------------------------------------------------~%")

(save-project-report *current-project* 'summary "simple-project-report.html")
(format t "  HTML report: simple-project-report.html~%")

(save-project-report *current-project* 'csv-export "simple-project-tasks.csv")
(format t "  CSV export: simple-project-tasks.csv~%")

(save-project-report *current-project* 'critical-tasks "simple-project-critical.html")
(format t "  Critical path: simple-project-critical.html~%")

(save-project-report *current-project* 'gantt-chart "simple-project-gantt.html")
(format t "  Gantt chart: simple-project-gantt.html~%~%")

(format t "--------------------------------------------------------------~%")
(format t "EXAMPLE COMPLETE!~%")
(format t "--------------------------------------------------------------~%")
(format t "~%Try these commands in the REPL:~%~%")

(format t ";; Compare task values between scenarios~%")
(format t "(let ((task (gethash 'sp-frontend (project-tasks *current-project*))))~%")
(format t "  (format t \"Plan duration: ~~A days~~%%\"~%")
(format t "          (duration-in-days (task-duration-for-scenario task 'plan)))~%")
(format t "  (format t \"Delayed duration: ~~A days~~%%\"~%")
(format t "          (duration-in-days (task-duration-for-scenario task 'delayed))))~%~%")

(format t ";; Get scenario summary~%")
(format t "(scenario-summary *current-project* 'plan)~%")
(format t "(scenario-summary *current-project* 'delayed)~%~%")

(format t ";; Mark a task as 50%% complete and check EVM~%")
(format t "(setf (task-complete (gethash 'sp-requirements~%")
(format t "                      (project-tasks *current-project*))) 50)~%")
(format t "(calculate-earned-value *current-project*)~%~%")

(format t "==============================================================~%~%")
