;;;; examples/simple-project.lisp
;;;; Example: Simple Website Project
;;;;
;;;; This is a straightforward example demonstrating:
;;;; - Basic task definition
;;;; - Resource allocation
;;;; - Task dependencies
;;;; - Critical path analysis
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

  ;;; Tasks
  (deftask sp-requirements "Requirements Gathering"
    :duration (duration 1 :weeks)
    :allocate (developer designer)
    :priority 1000)

  (deftask sp-design "UI/UX Design"
    :duration (duration 2 :weeks)
    :depends-on (sp-requirements)
    :allocate (designer)
    :priority 900)

  (deftask sp-frontend "Frontend Development"
    :duration (duration 3 :weeks)
    :depends-on (sp-design)
    :allocate (developer)
    :priority 850)

  (deftask sp-backend "Backend Development"
    :duration (duration 3 :weeks)
    :depends-on (sp-requirements)
    :allocate (developer)
    :priority 850)

  (deftask sp-integration "Integration & Testing"
    :duration (duration 1 :weeks)
    :depends-on (sp-frontend sp-backend)
    :allocate (developer qa)
    :priority 950)

  (deftask sp-deployment "Deployment & Launch"
    :duration (duration 3 :days)
    :depends-on (sp-integration)
    :allocate (developer)
    :priority 1000)

  ;;; Reports - using defreport DSL
  (defreport summary "Project Task Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration :priority))

  (defreport csv-export "Task Export"
    :type :task
    :format :csv
    :columns (:id :name :start :end :duration))

  (defreport critical-tasks "Critical Path Tasks"
    :type :task
    :format :html
    :columns (:id :name :start :end :slack :priority)
    :filter (lambda (task) (and (task-slack task) (zerop (task-slack task))))
    :sort-by (lambda (a b) (date< (task-start a) (task-start b)))))

;;; =============================================================================
;;; ANALYSIS AND REPORTING
;;; =============================================================================

(format t "~%╔══════════════════════════════════════════════════════════╗~%")
(format t "║         Company Website Redesign - Project              ║~%")
(format t "╚══════════════════════════════════════════════════════════╝~%~%")

;; Finalize and schedule
(format t "▶ Finalizing project...~%")
(finalize-project *current-project*)

(format t "▶ Scheduling tasks...~%")
(schedule *current-project*)

(format t "✓ Project scheduled successfully!~%~%")

;; Display timeline
(format t "──────────────────────────────────────────────────────────~%")
(format t "PROJECT TIMELINE~%")
(format t "──────────────────────────────────────────────────────────~%")
(format t "Project: ~A~%" (project-name *current-project*))
(format t "Start:   ~A~%" (project-start *current-project*))
(format t "End:     ~A~%" (project-end *current-project*))
(format t "Tasks:   ~A~%~%" (hash-table-count (project-tasks *current-project*)))

;; Display task schedule
(format t "──────────────────────────────────────────────────────────~%")
(format t "TASK SCHEDULE~%")
(format t "──────────────────────────────────────────────────────────~%")
(maphash (lambda (id task)
           (declare (ignore id))
           (format t "• ~A~%" (task-name task))
           (format t "  ~A → ~A (~A days)~%"
                   (task-start task)
                   (task-end task)
                   (if (task-duration task)
                       (duration-in-days (task-duration task))
                       0)))
         (project-tasks *current-project*))
(format t "~%")

;; Critical path analysis
(format t "──────────────────────────────────────────────────────────~%")
(format t "CRITICAL PATH ANALYSIS~%")
(format t "──────────────────────────────────────────────────────────~%")

;; Note: schedule automatically calculates critical path using CPM
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical path (~A tasks):~%~%" (length critical-tasks))
  (dolist (task critical-tasks)
    (format t "• ~A (Slack: ~A days)~%"
            (task-name task)
            (task-slack task))))
(format t "~%")

;; Resource analysis
(format t "──────────────────────────────────────────────────────────~%")
(format t "RESOURCE ALLOCATION~%")
(format t "──────────────────────────────────────────────────────────~%")

(let ((overallocations (detect-resource-overallocations *current-project*)))
  (if (null overallocations)
      (format t "✓ No resource over-allocations detected!~%~%")
      (progn
        (format t "⚠ WARNING: ~A over-allocations detected~%~%" (length overallocations))
        (dolist (oa overallocations)
          (format t "• ~A overallocated on ~A~%"
                  (overallocation-resource-id oa)
                  (overallocation-date oa))))))

;; Create baseline
(format t "──────────────────────────────────────────────────────────~%")
(format t "BASELINE~%")
(format t "──────────────────────────────────────────────────────────~%")

(let ((baseline (create-baseline *current-project* :name "Original Plan")))
  (set-project-baseline *current-project* baseline)
  (format t "✓ Baseline created: ~A~%" (baseline-name baseline))
  (format t "  Tasks: ~A~%~%" (hash-table-count (baseline-tasks baseline))))

;; Generate reports using the new DSL
(format t "──────────────────────────────────────────────────────────~%")
(format t "GENERATING REPORTS~%")
(format t "──────────────────────────────────────────────────────────~%")

;; Use the save-project-report helper function
(save-project-report *current-project* 'summary "simple-project-report.html")
(format t "✓ HTML report: simple-project-report.html~%")

(save-project-report *current-project* 'csv-export "simple-project-tasks.csv")
(format t "✓ CSV export: simple-project-tasks.csv~%")

(save-project-report *current-project* 'critical-tasks "simple-project-critical.html")
(format t "✓ Critical path report: simple-project-critical.html~%~%")

(format t "──────────────────────────────────────────────────────────~%")
(format t "✓ EXAMPLE COMPLETE!~%")
(format t "──────────────────────────────────────────────────────────~%")
(format t "~%Generated files:~%")
(format t "• simple-project-report.html (open in browser)~%")
(format t "• simple-project-tasks.csv (open in spreadsheet)~%")
(format t "• simple-project-critical.html (critical path tasks only)~%~%")
(format t "Now try these commands in the REPL:~%~%")

(format t ";; View a specific task~%")
(format t "(let ((task (gethash 'sp-frontend (project-tasks *current-project*))))~%")
(format t "  (format t \"~~A: ~~A to ~~A~~%%\"~%")
(format t "          (task-name task) (task-start task) (task-end task)))~%~%")

(format t ";; Mark a task as 50%% complete~%")
(format t "(setf (task-complete (gethash 'sp-requirements~%")
(format t "                      (project-tasks *current-project*))) 50)~%~%")

(format t ";; Calculate EVM metrics~%")
(format t "(let ((pv (calculate-planned-value *current-project* (local-time:now)))~%")
(format t "      (ev (calculate-earned-value *current-project*))~%")
(format t "      (spi (calculate-spi *current-project* (local-time:now))))~%")
(format t "  (format t \"Schedule Performance: PV=~~A%%, EV=~~A%%, SPI=~~,2F~~%%\"~%")
(format t "          pv ev spi))~%~%")

(format t "══════════════════════════════════════════════════════════~%~%")
