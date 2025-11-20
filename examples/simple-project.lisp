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
  (deftask requirements "Requirements Gathering"
    :duration (duration 1 :weeks)
    :allocate (developer designer)
    :priority 1000)

  (deftask design "UI/UX Design"
    :duration (duration 2 :weeks)
    :depends-on (requirements)
    :allocate (designer)
    :priority 900)

  (deftask frontend "Frontend Development"
    :duration (duration 3 :weeks)
    :depends-on (design)
    :allocate (developer)
    :priority 850)

  (deftask backend "Backend Development"
    :duration (duration 3 :weeks)
    :depends-on (requirements)
    :allocate (developer)
    :priority 850)

  (deftask integration "Integration & Testing"
    :duration (duration 1 :weeks)
    :depends-on (frontend backend)
    :allocate (developer qa)
    :priority 950)

  (deftask deployment "Deployment & Launch"
    :duration (duration 3 :days)
    :depends-on (integration)
    :allocate (developer)
    :priority 1000))

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

(forward-pass *current-project*)
(backward-pass *current-project*)
(calculate-slack *current-project*)

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

;; Generate reports
(format t "──────────────────────────────────────────────────────────~%")
(format t "GENERATING REPORTS~%")
(format t "──────────────────────────────────────────────────────────~%")

(let ((report (make-instance 'task-report
                :id 'summary
                :title "Project Task Summary"
                :format :html
                :columns '(:id :name :start :end :duration :priority))))
  (with-open-file (out "examples/simple-project-report.html"
                       :direction :output
                       :if-exists :supersede)
    (write-string (generate-report report *current-project*) out))
  (format t "✓ HTML report: examples/simple-project-report.html~%"))

(let ((report (make-instance 'task-report
                :id 'csv
                :title "Task Export"
                :format :csv
                :columns '(:id :name :start :end :duration))))
  (with-open-file (out "examples/simple-project-tasks.csv"
                       :direction :output
                       :if-exists :supersede)
    (write-string (generate-report report *current-project*) out))
  (format t "✓ CSV export: examples/simple-project-tasks.csv~%~%"))

(format t "──────────────────────────────────────────────────────────~%")
(format t "✓ EXAMPLE COMPLETE!~%")
(format t "──────────────────────────────────────────────────────────~%")
(format t "~%Now try these commands:~%~%")

(format t ";; View a specific task~%")
(format t "(let ((task (gethash 'frontend (project-tasks *current-project*))))~%")
(format t "  (format t \"~A: ~A to ~A~~%%\"~%")
(format t "          (task-name task) (task-start task) (task-end task)))~%~%")

(format t ";; Mark a task as 50%% complete~%")
(format t "(setf (task-complete (gethash 'requirements~%")
(format t "                      (project-tasks *current-project*))) 50)~%~%")

(format t ";; Calculate EVM metrics~%")
(format t "(let ((pv (calculate-planned-value *current-project* (local-time:now)))~%")
(format t "      (ev (calculate-earned-value *current-project*))~%")
(format t "      (spi (calculate-spi *current-project* (local-time:now))))~%")
(format t "  (format t \"Schedule Performance: PV=~A%%, EV=~A%%, SPI=~,2F~~%%\"~%")
(format t "          pv ev spi))~%~%")

(format t "══════════════════════════════════════════════════════════~%~%")
