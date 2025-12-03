;;;; examples/web-application.lisp
;;;; Example: Web Application Development Project
;;;;
;;;; This example demonstrates a complete web application project with:
;;;; - Multiple teams (dev, design, QA)
;;;; - Task dependencies
;;;; - Resource allocation
;;;; - Milestones
;;;; - Critical path analysis
;;;; - TaskJuggler-style scenarios for what-if analysis
;;;; - EVM tracking (uses first scenario as baseline)

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

(defproject web-app "SaaS Platform Development"
  :start (date 2024 3 1)
  :end (date 2024 9 30)
  :scenarios (plan delayed)  ; First scenario is baseline for EVM

  ;;; ---------------------------------------------------------------------------
  ;;; RESOURCES
  ;;; ---------------------------------------------------------------------------

  ;; Development Team
  (defresource senior-dev "Senior Developer"
    :efficiency 1.2
    :rate 120.0)

  (defresource mid-dev "Mid-level Developer"
    :efficiency 1.0
    :rate 90.0)

  (defresource junior-dev "Junior Developer"
    :efficiency 0.8
    :rate 60.0)

  ;; Design Team
  (defresource ux-designer "UX Designer"
    :efficiency 1.0
    :rate 95.0)

  (defresource ui-designer "UI Designer"
    :efficiency 1.0
    :rate 85.0)

  ;; QA Team
  (defresource qa-lead "QA Lead"
    :efficiency 1.1
    :rate 80.0)

  (defresource qa-tester "QA Tester"
    :efficiency 0.9
    :rate 55.0)

  ;; DevOps
  (defresource devops "DevOps Engineer"
    :efficiency 1.0
    :rate 110.0)

  ;;; ---------------------------------------------------------------------------
  ;;; PHASE 1: PLANNING & DESIGN
  ;;; ---------------------------------------------------------------------------

  (deftask requirements "Requirements Gathering"
    :duration (duration 2 :weeks)
    :allocate (senior-dev ux-designer)
    :priority 1000)

  (deftask architecture "System Architecture Design"
    :duration (duration 2 :weeks)
    :depends-on (requirements)
    :allocate (senior-dev)
    :priority 950)

  (deftask ux-design "UX Design & Wireframes"
    :duration (duration 3 :weeks)
    :depends-on (requirements)
    :allocate (ux-designer ui-designer)
    :priority 900)

  (deftask ui-design "UI Design & Mockups"
    :duration (duration 2 :weeks)
    :depends-on (ux-design)
    :allocate (ui-designer)
    :priority 900)

  (deftask design-approval "Design Approval"
    :milestone t
    :depends-on (architecture ui-design))

  ;;; ---------------------------------------------------------------------------
  ;;; PHASE 2: BACKEND DEVELOPMENT
  ;;; ---------------------------------------------------------------------------

  (deftask database-design "Database Schema Design"
    :duration (duration 1 :weeks)
    :depends-on (design-approval)
    :allocate (senior-dev)
    :priority 950)

  (deftask api-framework "API Framework Setup"
    :duration (duration 1 :weeks)
    :depends-on (database-design)
    :allocate (senior-dev mid-dev)
    :priority 900)

  (deftask user-auth "User Authentication & Authorization"
    :duration (duration 2 :weeks)
    :depends-on (api-framework)
    :allocate (senior-dev)
    :priority 950)

  (deftask user-api "User Management API"
    :duration (duration 1 :weeks)
    :depends-on (api-framework)
    :allocate (mid-dev)
    :priority 850)

  (deftask data-api "Data Processing API"
    :duration (duration 2 :weeks)
    :depends-on (api-framework)
    :allocate (senior-dev mid-dev)
    :priority 850)

  (deftask reporting-api "Reporting API"
    :duration (duration 2 :weeks)
    :depends-on (api-framework)
    :allocate (mid-dev junior-dev)
    :priority 800)

  (deftask backend-tests "Backend Unit Tests"
    :duration (duration 2 :weeks)
    :depends-on (user-auth user-api data-api reporting-api)
    :allocate (mid-dev junior-dev)
    :priority 850)

  ;;; ---------------------------------------------------------------------------
  ;;; PHASE 3: FRONTEND DEVELOPMENT
  ;;; ---------------------------------------------------------------------------

  (deftask frontend-setup "Frontend Framework Setup"
    :duration (duration 1 :weeks)
    :depends-on (design-approval)
    :allocate (senior-dev)
    :priority 900)

  (deftask components "Component Library Development"
    :duration (duration 2 :weeks)
    :depends-on (frontend-setup)
    :allocate (mid-dev junior-dev ui-designer)
    :priority 850)

  (deftask dashboard "Dashboard Implementation"
    :duration (duration 3 :weeks)
    :depends-on (components)
    :allocate (mid-dev junior-dev)
    :priority 900)

  (deftask login-ui "Login & Registration UI"
    :duration (duration 1 :weeks)
    :depends-on (components)
    :allocate (junior-dev ui-designer)
    :priority 850)

  (deftask profile-ui "User Profile UI"
    :duration (duration 1 :weeks)
    :depends-on (components)
    :allocate (junior-dev)
    :priority 800)

  (deftask data-views "Data Visualization Views"
    :duration (duration 2 :weeks)
    :depends-on (components)
    :allocate (mid-dev junior-dev ui-designer)
    :priority 850)

  (deftask frontend-tests "Frontend Unit Tests"
    :duration (duration 2 :weeks)
    :depends-on (dashboard login-ui profile-ui data-views)
    :allocate (mid-dev junior-dev)
    :priority 800)

  ;;; ---------------------------------------------------------------------------
  ;;; PHASE 4: INTEGRATION & TESTING
  ;;; ---------------------------------------------------------------------------

  (deftask integration "Backend-Frontend Integration"
    :duration (duration 2 :weeks)
    :depends-on (backend-tests frontend-tests user-auth)
    :allocate (senior-dev mid-dev)
    :priority 950)

  (deftask functional-tests "Functional Testing"
    :duration (duration 2 :weeks)
    :depends-on (integration)
    :allocate (qa-lead qa-tester)
    :priority 950)

  (deftask performance-tests "Performance Testing"
    :duration (duration 1 :weeks)
    :depends-on (integration)
    :allocate (qa-lead)
    :priority 900)

  (deftask security-tests "Security Testing"
    :duration (duration 1 :weeks)
    :depends-on (integration)
    :allocate (qa-lead senior-dev)
    :priority 950)

  (deftask bug-fixing "Bug Fixing & Refinement"
    :duration (duration 2 :weeks)
    :depends-on (functional-tests performance-tests security-tests)
    :allocate (senior-dev mid-dev junior-dev)
    :priority 900)

  (deftask regression-tests "Regression Testing"
    :duration (duration 1 :weeks)
    :depends-on (bug-fixing)
    :allocate (qa-lead qa-tester)
    :priority 900)

  ;;; ---------------------------------------------------------------------------
  ;;; PHASE 5: DEPLOYMENT
  ;;; ---------------------------------------------------------------------------

  (deftask infrastructure "Infrastructure Setup"
    :duration (duration 2 :weeks)
    :depends-on (regression-tests)
    :allocate (devops senior-dev)
    :priority 950)

  (deftask ci-cd "CI/CD Pipeline"
    :duration (duration 1 :weeks)
    :depends-on (infrastructure)
    :allocate (devops)
    :priority 900)

  (deftask staging-deploy "Staging Deployment"
    :duration (duration 1 :weeks)
    :depends-on (ci-cd)
    :allocate (devops senior-dev)
    :priority 950)

  (deftask staging-validation "Staging Validation"
    :duration (duration 1 :weeks)
    :depends-on (staging-deploy)
    :allocate (qa-lead senior-dev)
    :priority 950)

  (deftask prod-deploy "Production Deployment"
    :duration (duration 3 :days)
    :depends-on (staging-validation)
    :allocate (devops senior-dev)
    :priority 1000)

  (deftask launch "Product Launch"
    :milestone t
    :depends-on (prod-deploy))

  ;;; ---------------------------------------------------------------------------
  ;;; PHASE 6: POST-LAUNCH
  ;;; ---------------------------------------------------------------------------

  (deftask monitoring "Monitoring & Alerts Setup"
    :duration (duration 1 :weeks)
    :depends-on (launch)
    :allocate (devops)
    :priority 900)

  (deftask documentation "User Documentation"
    :duration (duration 2 :weeks)
    :depends-on (launch)
    :allocate (ux-designer mid-dev)
    :priority 700)

  (deftask support "Initial Support Period"
    :duration (duration 2 :weeks)
    :depends-on (launch)
    :allocate (senior-dev qa-lead)
    :priority 800)

  (deftask project-complete "Project Complete"
    :milestone t
    :depends-on (monitoring documentation support))

  ;;; ---------------------------------------------------------------------------
  ;;; REPORTS - Enhanced DSL with new report types
  ;;; ---------------------------------------------------------------------------

  ;; Standard task summary
  (defreport project-summary "SaaS Platform Development - Task Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration :priority)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

  ;; CSV export for spreadsheet analysis
  (defreport csv-export "Task Export"
    :type :task
    :format :csv
    :columns (:id :name :start :end :duration :priority))

  ;; Critical path report - NEW! Uses dedicated :critical-path type
  (defreport critical-path-report "Critical Path Tasks"
    :type :critical-path
    :format :html
    :columns (:name :start :end :slack :priority))

  ;; High priority tasks - NEW! Uses :auto-filter
  (defreport high-priority-tasks "High Priority Tasks"
    :type :task
    :format :html
    :columns (:name :start :end :priority :slack)
    :auto-filter :high-priority)

  ;; Incomplete tasks - NEW! Uses :auto-filter
  (defreport incomplete-tasks "Incomplete Tasks"
    :type :task
    :format :html
    :columns (:name :start :end :complete)
    :auto-filter :incomplete)

  ;; Resource utilization
  (defreport resource-utilization "Resource Utilization"
    :type :resource
    :format :html
    :columns (:id :name :efficiency :rate :criticalness))

  ;; Milestone report - NEW! Dedicated milestone type
  (defreport milestones "Project Milestones"
    :type :milestone
    :format :html
    :columns (:name :start))

  ;; Gantt chart - NEW! Visual SVG timeline
  (defreport gantt-svg "Project Timeline (SVG)"
    :type :gantt
    :format :svg
    :width 1200
    :height 800)

  ;; Gantt chart - NEW! HTML with embedded SVG
  (defreport gantt-html "Project Timeline"
    :type :gantt
    :format :html
    :width 1200)

  ;; EVM report - NEW! Earned Value Management tracking
  (defreport evm-status "EVM Status Report"
    :type :evm
    :format :html
    :columns (:name :pv :ev :sv)
    :include-summary t))

;;; =============================================================================
;;; ANALYSIS AND REPORTING
;;; =============================================================================

(format t "~%╔════════════════════════════════════════════════════════════════╗~%")
(format t "║  SaaS Platform Development - Project Schedule                 ║~%")
(format t "╚════════════════════════════════════════════════════════════════╝~%~%")

;; Finalize and schedule the project
(format t "▶ Finalizing project...~%")
(finalize-project *current-project*)

(format t "▶ Scheduling tasks...~%")
(schedule *current-project*)

(format t "✓ Project scheduled successfully!~%~%")

;; Display project timeline
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "PROJECT TIMELINE~%")
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "Project: ~A~%" (project-name *current-project*))
(format t "Start:   ~A~%" (project-start *current-project*))
(format t "End:     ~A~%" (project-end *current-project*))
(format t "Tasks:   ~A~%~%" (hash-table-count (project-tasks *current-project*)))

;; Display major milestones
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "MAJOR MILESTONES~%")
(format t "─────────────────────────────────────────────────────────────────~%")
(let ((milestones '(design-approval launch project-complete)))
  (dolist (milestone-id milestones)
    (let ((task (gethash milestone-id (project-tasks *current-project*))))
      (when task
        (format t "• ~A~%" (task-name task))
        (format t "  Date: ~A~%~%" (task-start task))))))

;; Calculate and display critical path
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "CRITICAL PATH ANALYSIS~%")
(format t "─────────────────────────────────────────────────────────────────~%")

;; Note: schedule automatically calculates critical path using CPM
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical path contains ~A tasks:~%~%" (length critical-tasks))
  (let ((display-count (min 10 (length critical-tasks))))
    (dotimes (i display-count)
      (let ((task (nth i critical-tasks)))
        (format t "~2D. ~A~%" (1+ i) (task-name task))
        (format t "    ~A → ~A (~A days)~%"
                (task-start task)
                (task-end task)
                (if (task-duration task)
                    (duration-in-days (task-duration task))
                    0))))
    (when (> (length critical-tasks) display-count)
      (format t "    ... and ~A more tasks~%" (- (length critical-tasks) display-count))))
  (format t "~%"))

;; Check for resource over-allocations
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "RESOURCE ALLOCATION ANALYSIS~%")
(format t "─────────────────────────────────────────────────────────────────~%")

(let ((overallocations (detect-resource-overallocations *current-project*)))
  (if (null overallocations)
      (format t "✓ No resource over-allocations detected!~%~%")
      (progn
        (format t "⚠ WARNING: ~A resource over-allocation(s) detected~%" (length overallocations))
        (let ((display-count (min 5 (length overallocations))))
          (dotimes (i display-count)
            (let ((oa (nth i overallocations)))
              (format t "• Resource ~A overallocated on ~A (load: ~,1F)~%"
                      (overallocation-resource-id oa)
                      (overallocation-date oa)
                      (overallocation-load oa))))
          (when (> (length overallocations) display-count)
            (format t "  ... and ~A more~%" (- (length overallocations) display-count))))
        (format t "~%"))))

;; Scenario comparison using TaskJuggler-style API
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "SCENARIO SYSTEM (TaskJuggler-style)~%")
(format t "─────────────────────────────────────────────────────────────────~%")

;; First scenario ('plan) is automatically the baseline for EVM
(format t "✓ Scenarios defined: ~A~%" (list-scenarios *current-project*))
(format t "  Baseline scenario (first): ~A~%" (baseline-scenario-id *current-project*))
(format t "  EVM calculations will use the baseline scenario~%~%")

;; Generate reports using enhanced DSL-defined reports
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "GENERATING REPORTS~%")
(format t "─────────────────────────────────────────────────────────────────~%")

;; Standard reports
(save-project-report *current-project* 'project-summary "web-application-report.html")
(format t "✓ HTML report: web-application-report.html~%")

(save-project-report *current-project* 'csv-export "web-application-tasks.csv")
(format t "✓ CSV export: web-application-tasks.csv~%")

(save-project-report *current-project* 'critical-path-report "web-application-critical.html")
(format t "✓ Critical path report: web-application-critical.html~%")

(save-project-report *current-project* 'high-priority-tasks "web-application-priority.html")
(format t "✓ High priority tasks: web-application-priority.html~%")

(save-project-report *current-project* 'resource-utilization "web-application-resources.html")
(format t "✓ Resource utilization: web-application-resources.html~%")

;; NEW enhanced reports
(save-project-report *current-project* 'milestones "web-application-milestones.html")
(format t "✓ Milestone report: web-application-milestones.html~%")

(save-project-report *current-project* 'gantt-html "web-application-gantt.html")
(format t "✓ Gantt chart (HTML): web-application-gantt.html~%")

(save-project-report *current-project* 'gantt-svg "web-application-gantt.svg")
(format t "✓ Gantt chart (SVG): web-application-gantt.svg~%")

(save-project-report *current-project* 'evm-status "web-application-evm.html")
(format t "✓ EVM status: web-application-evm.html~%")

(save-project-report *current-project* 'incomplete-tasks "web-application-incomplete.html")
(format t "✓ Incomplete tasks: web-application-incomplete.html~%")

(format t "~%─────────────────────────────────────────────────────────────────~%")
(format t "✓ EXAMPLE COMPLETE!~%")
(format t "─────────────────────────────────────────────────────────────────~%")
(format t "~%Generated files:~%")
(format t "• web-application-report.html (complete task summary)~%")
(format t "• web-application-tasks.csv (CSV export)~%")
(format t "• web-application-critical.html (critical path tasks only)~%")
(format t "• web-application-priority.html (high priority tasks)~%")
(format t "• web-application-resources.html (resource utilization)~%")
(format t "• web-application-milestones.html (project milestones)~%")
(format t "• web-application-gantt.html (visual Gantt chart)~%")
(format t "• web-application-gantt.svg (SVG Gantt for embedding)~%")
(format t "• web-application-evm.html (EVM status report)~%")
(format t "• web-application-incomplete.html (incomplete tasks)~%~%")
(format t "Next steps:~%")
(format t "• Try modifying task completion percentages~%")
(format t "• Calculate EVM metrics to track progress~%~%")

(format t "Example commands:~%~%")
(format t ";; Update task completion~%")
(format t "(setf (task-complete (gethash 'requirements~%")
(format t "                      (project-tasks *current-project*))) 100)~%~%")

(format t ";; Calculate EVM metrics (uses first scenario as baseline)~%")
(format t "(let ((pv (calculate-planned-value *current-project* (date 2024 4 1)))~%")
(format t "      (ev (calculate-earned-value *current-project*))~%")
(format t "      (spi (calculate-spi *current-project* (date 2024 4 1))))~%")
(format t "  (format t \"PV: ~~~~A%%, EV: ~~~~A%%, SPI: ~~~~,2F~~~~%%\" pv ev spi))~%~%")

(format t ";; Compare scenarios~%")
(format t "(compare-scenarios *current-project* 'plan 'delayed)~%~%")

(format t ";; Get scenario summary~%")
(format t "(scenario-summary *current-project* 'plan)~%~%")

(format t "═══════════════════════════════════════════════════════════════════~%~%")
