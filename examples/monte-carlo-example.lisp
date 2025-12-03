;;;; examples/monte-carlo-example.lisp
;;;; Example: Monte Carlo PERT Simulation for Schedule Risk Analysis
;;;;
;;;; This example demonstrates:
;;;; - PERT three-point estimation (optimistic, likely, pessimistic)
;;;; - Monte Carlo simulation for schedule uncertainty
;;;; - Risk register integration
;;;; - Probability distributions and confidence intervals
;;;; - Statistical analysis of project duration

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
;;; PROJECT DEFINITION WITH PERT ESTIMATES
;;; =============================================================================

(defproject risk-project "Software Release with Uncertainty"
  :start (date 2024 6 1)
  :end (date 2024 12 31)

  ;;; Resources
  (defresource dev-lead "Lead Developer"
    :efficiency 1.2
    :rate 150.0)

  (defresource developer "Developer"
    :efficiency 1.0
    :rate 100.0)

  (defresource qa "QA Engineer"
    :efficiency 0.9
    :rate 80.0)

  ;;; Tasks with PERT three-point estimates
  ;;; Each task has :optimistic, :likely, and :pessimistic duration estimates

  (deftask requirements "Requirements Analysis"
    :estimate (:optimistic (duration 5 :days)
               :likely (duration 7 :days)
               :pessimistic (duration 14 :days))
    :allocate (dev-lead)
    :priority 1000)

  (deftask architecture "System Architecture"
    :estimate (:optimistic (duration 3 :days)
               :likely (duration 5 :days)
               :pessimistic (duration 10 :days))
    :depends-on (requirements)
    :allocate (dev-lead)
    :priority 950)

  (deftask backend "Backend Development"
    :estimate (:optimistic (duration 10 :days)
               :likely (duration 15 :days)
               :pessimistic (duration 30 :days))
    :depends-on (architecture)
    :allocate (developer)
    :priority 900)

  (deftask frontend "Frontend Development"
    :estimate (:optimistic (duration 8 :days)
               :likely (duration 12 :days)
               :pessimistic (duration 20 :days))
    :depends-on (architecture)
    :allocate (developer)
    :priority 900)

  (deftask integration "System Integration"
    :estimate (:optimistic (duration 3 :days)
               :likely (duration 5 :days)
               :pessimistic (duration 12 :days))
    :depends-on (backend frontend)
    :allocate (developer dev-lead)
    :priority 950)

  (deftask testing "QA Testing"
    :estimate (:optimistic (duration 5 :days)
               :likely (duration 10 :days)
               :pessimistic (duration 20 :days))
    :depends-on (integration)
    :allocate (qa)
    :priority 900)

  (deftask deployment "Production Deployment"
    :estimate (:optimistic (duration 1 :days)
               :likely (duration 2 :days)
               :pessimistic (duration 5 :days))
    :depends-on (testing)
    :allocate (dev-lead developer)
    :priority 1000)

  ;;; Reports - NEW! Using enhanced defreport DSL for simulation and risk reports

  ;; Simulation report - generates Monte Carlo analysis
  (defreport simulation-analysis "Schedule Uncertainty Analysis"
    :type :simulation
    :format :html
    :trials 5000
    :percentiles (10 25 50 75 90 95)
    :include-histogram t)

  ;; Risk register report - shows all project risks
  (defreport risk-register "Risk Register"
    :type :risk
    :format :html
    :columns (:name :probability :impact :score :severity :category)
    :sort-by :score-desc)

  ;; Open risks only
  (defreport open-risks "Active Risks"
    :type :risk
    :format :html
    :columns (:name :probability :impact :score :mitigation)
    :filter-status :open
    :sort-by :score-desc)

  ;; Critical path with slack
  (defreport critical-path "Critical Path"
    :type :critical-path
    :format :html
    :columns (:name :start :end :slack))

  ;; Gantt chart with critical path highlighting
  (defreport gantt-timeline "Project Timeline"
    :type :gantt
    :format :html
    :width 1000))

;;; =============================================================================
;;; RISK REGISTER
;;; =============================================================================

;; Add risks that could impact the schedule
(create-risk *current-project*
             'risk-scope-creep "Scope Creep"
             :description "Requirements may expand during development"
             :probability 0.4
             :impact 0.8
             :category :scope
             :tasks '(requirements backend frontend)
             :schedule-impact 0.3)  ; 30% schedule impact on affected tasks

(create-risk *current-project*
             'risk-integration-issues "Integration Complexity"
             :description "Unforeseen integration challenges"
             :probability 0.3
             :impact 0.5
             :category :technical
             :tasks '(integration)
             :schedule-impact 0.5)  ; 50% schedule impact

(create-risk *current-project*
             'risk-testing-defects "Critical Defects Found"
             :description "Major bugs discovered during testing"
             :probability 0.25
             :impact 0.8
             :category :quality
             :tasks '(testing)
             :schedule-impact 0.4)  ; 40% schedule impact

;;; =============================================================================
;;; ANALYSIS
;;; =============================================================================

(format t "~%")
(format t "======================================================================~%")
(format t "   MONTE CARLO PERT SIMULATION - Schedule Risk Analysis~%")
(format t "======================================================================~%~%")

;; Finalize and schedule with expected (mean) PERT durations
(format t ">>> Finalizing project...~%")
(finalize-project *current-project*)

(format t ">>> Scheduling with expected PERT durations...~%")
(schedule *current-project*)
(format t "~%")

;;; =============================================================================
;;; INDIVIDUAL TASK PERT ANALYSIS
;;; =============================================================================

(format t "----------------------------------------------------------------------~%")
(format t "PERT ESTIMATES BY TASK~%")
(format t "----------------------------------------------------------------------~%~%")

(maphash (lambda (id task)
           (declare (ignore id))
           (let ((estimate (task-estimate task)))
             (when estimate
               (format t "~A~%" (task-name task))
               (format t "  Optimistic:  ~,1F days~%"
                       (duration-in-days (estimate-optimistic estimate)))
               (format t "  Most Likely: ~,1F days~%"
                       (duration-in-days (estimate-likely estimate)))
               (format t "  Pessimistic: ~,1F days~%"
                       (duration-in-days (estimate-pessimistic estimate)))
               (format t "  Expected:    ~,1F days (PERT weighted mean)~%"
                       (pert-expected-duration task))
               (format t "  Std Dev:     ~,2F days~%~%"
                       (pert-standard-deviation task)))))
         (project-tasks *current-project*))

;;; =============================================================================
;;; RISK REGISTER SUMMARY
;;; =============================================================================

(format t "----------------------------------------------------------------------~%")
(format t "RISK REGISTER~%")
(format t "----------------------------------------------------------------------~%~%")

(dolist (risk (project-risk-register *current-project*))
  (format t "~A~%" (risk-name risk))
  (format t "  Probability:     ~,0F%~%" (* 100 (risk-probability risk)))
  (format t "  Impact:          ~A~%" (risk-impact risk))
  (format t "  Schedule Impact: +~,0F% on affected tasks~%"
          (* 100 (risk-schedule-impact risk)))
  (format t "  Affected Tasks:  ~{~A~^, ~}~%~%"
          (risk-tasks risk)))

;;; =============================================================================
;;; MONTE CARLO SIMULATION - WITHOUT RISKS
;;; =============================================================================

(format t "----------------------------------------------------------------------~%")
(format t "MONTE CARLO SIMULATION (PERT uncertainty only)~%")
(format t "----------------------------------------------------------------------~%~%")

(format t ">>> Running 10,000 simulation trials...~%~%")
(let* ((results (run-monte-carlo-simulation *current-project* :trials 10000))
       (summary (simulation-summary results)))

  ;; Display statistics
  (format t "Duration Statistics (days):~%")
  (format t "  Minimum:     ~,1F~%" (getf summary :min))
  (format t "  Maximum:     ~,1F~%" (getf summary :max))
  (format t "  Mean:        ~,1F~%" (getf summary :mean))
  (format t "  Std Dev:     ~,2F~%~%" (getf summary :std-dev))

  (format t "Percentile Analysis:~%")
  (format t "  P10 (optimistic):  ~,1F days~%" (getf summary :p10))
  (format t "  P50 (median):      ~,1F days~%" (getf summary :p50))
  (format t "  P75:               ~,1F days~%" (getf summary :p75))
  (format t "  P90 (conservative):~,1F days~%" (getf summary :p90))
  (format t "  P95 (high conf):   ~,1F days~%~%" (getf summary :p95))

  ;; Probability analysis for specific targets
  (format t "Probability of Completion:~%")
  (let ((p50 (getf summary :p50)))
    (dolist (target-offset '(-10 -5 0 5 10 15 20))
      (let* ((target (+ p50 target-offset))
             (prob (simulation-probability-of-completion results target)))
        (format t "  Within ~,0F days: ~,1F%~%" target (* 100 prob)))))
  (format t "~%")

  ;; Histogram
  (format t "Duration Distribution (histogram):~%")
  (let ((histogram (simulation-histogram results :bins 10)))
    (dolist (bin histogram)
      (let* ((bin-min (first bin))
             (bin-max (second bin))
             (count (third bin))
             (pct (/ count 100.0))
             (bar (make-string (min 50 (round pct)) :initial-element #\#)))
        (format t "  ~5,0F-~5,0F: ~A (~,1F%)~%"
                bin-min bin-max bar pct)))))

;;; =============================================================================
;;; MONTE CARLO SIMULATION - WITH RISKS
;;; =============================================================================

(format t "~%----------------------------------------------------------------------~%")
(format t "MONTE CARLO SIMULATION (PERT + Risk Integration)~%")
(format t "----------------------------------------------------------------------~%~%")

(format t ">>> Running 10,000 simulation trials with risk events...~%~%")
(let* ((results (run-risk-simulation *current-project* :trials 10000))
       (summary (simulation-summary results)))

  ;; Display statistics
  (format t "Duration Statistics (with risk events):~%")
  (format t "  Minimum:     ~,1F days~%" (getf summary :min))
  (format t "  Maximum:     ~,1F days~%" (getf summary :max))
  (format t "  Mean:        ~,1F days~%" (getf summary :mean))
  (format t "  Std Dev:     ~,2F days~%~%" (getf summary :std-dev))

  (format t "Percentile Analysis (with risks):~%")
  (format t "  P10 (optimistic):  ~,1F days~%" (getf summary :p10))
  (format t "  P50 (median):      ~,1F days~%" (getf summary :p50))
  (format t "  P75:               ~,1F days~%" (getf summary :p75))
  (format t "  P90 (conservative):~,1F days~%" (getf summary :p90))
  (format t "  P95 (high conf):   ~,1F days~%~%" (getf summary :p95))

  ;; Risk occurrence analysis
  (format t "Risk Occurrence Frequency:~%")
  (dolist (risk (project-risk-register *current-project*))
    (let* ((occurrences (simulation-risk-occurrences results (risk-id risk)))
           (pct (* 100 (/ occurrences 10000.0))))
      (format t "  ~A: ~,1F% of trials (~A occurrences)~%"
              (risk-name risk) pct occurrences)))
  (format t "~%")

  ;; Comparison
  (format t "Impact of Risk Events:~%")
  (let ((base-results (run-monte-carlo-simulation *current-project* :trials 1000)))
    (format t "  Without risks - Mean: ~,1F days, P90: ~,1F days~%"
            (simulation-mean base-results)
            (simulation-percentile base-results 90))
    (format t "  With risks    - Mean: ~,1F days, P90: ~,1F days~%"
            (getf summary :mean)
            (getf summary :p90))
    (format t "  Additional buffer needed: ~,1F days (for P90 confidence)~%"
            (- (getf summary :p90) (simulation-percentile base-results 90)))))

;;; =============================================================================
;;; RECOMMENDATIONS
;;; =============================================================================

(format t "~%----------------------------------------------------------------------~%")
(format t "SCHEDULING RECOMMENDATIONS~%")
(format t "----------------------------------------------------------------------~%~%")

(let* ((risk-results (run-risk-simulation *current-project* :trials 5000))
       (p50 (simulation-percentile risk-results 50))
       (p75 (simulation-percentile risk-results 75))
       (p90 (simulation-percentile risk-results 90)))

  (format t "Based on Monte Carlo analysis with ~A trials:~%~%"
          (simulation-trial-count risk-results))

  (format t "  Aggressive Schedule (50%% confidence):   ~,0F days~%" p50)
  (format t "  Balanced Schedule (75%% confidence):     ~,0F days~%" p75)
  (format t "  Conservative Schedule (90%% confidence): ~,0F days~%~%" p90)

  (let ((start (project-start *current-project*)))
    (format t "Target dates from project start (~A-~2,'0D-~2,'0D):~%~%"
            (date-year start) (date-month start) (date-day start))
    (let ((d1 (date+ start (duration (ceiling p50) :days)))
          (d2 (date+ start (duration (ceiling p75) :days)))
          (d3 (date+ start (duration (ceiling p90) :days))))
      (format t "  Aggressive:    ~A-~2,'0D-~2,'0D~%"
              (date-year d1) (date-month d1) (date-day d1))
      (format t "  Balanced:      ~A-~2,'0D-~2,'0D~%"
              (date-year d2) (date-month d2) (date-day d2))
      (format t "  Conservative:  ~A-~2,'0D-~2,'0D~%"
              (date-year d3) (date-month d3) (date-day d3)))))

;;; =============================================================================
;;; GENERATE REPORTS USING ENHANCED DSL
;;; =============================================================================

(format t "~%----------------------------------------------------------------------~%")
(format t "GENERATING REPORTS (Enhanced DSL)~%")
(format t "----------------------------------------------------------------------~%~%")

;; Generate simulation report - NEW! Automatic Monte Carlo analysis
(format t ">>> Generating simulation report...~%")
(save-project-report *current-project* 'simulation-analysis "monte-carlo-simulation.html")
(format t "✓ Simulation report: monte-carlo-simulation.html~%")

;; Generate risk reports - NEW! Risk register with sorting and filtering
(save-project-report *current-project* 'risk-register "monte-carlo-risks.html")
(format t "✓ Risk register: monte-carlo-risks.html~%")

(save-project-report *current-project* 'open-risks "monte-carlo-open-risks.html")
(format t "✓ Open risks: monte-carlo-open-risks.html~%")

;; Critical path and Gantt
(save-project-report *current-project* 'critical-path "monte-carlo-critical.html")
(format t "✓ Critical path: monte-carlo-critical.html~%")

(save-project-report *current-project* 'gantt-timeline "monte-carlo-gantt.html")
(format t "✓ Gantt chart: monte-carlo-gantt.html~%~%")

;;; =============================================================================
;;; COMPLETION
;;; =============================================================================

(format t "======================================================================~%")
(format t "   SIMULATION COMPLETE~%")
(format t "======================================================================~%~%")

(format t "Generated files:~%")
(format t "• monte-carlo-simulation.html (Monte Carlo percentile analysis)~%")
(format t "• monte-carlo-risks.html (risk register sorted by score)~%")
(format t "• monte-carlo-open-risks.html (active risks only)~%")
(format t "• monte-carlo-critical.html (critical path tasks)~%")
(format t "• monte-carlo-gantt.html (visual Gantt chart)~%~%")

(format t "Key Insights:~%")
(format t "  1. PERT estimates provide realistic three-point duration ranges~%")
(format t "  2. Monte Carlo simulation quantifies schedule uncertainty~%")
(format t "  3. Risk integration models the impact of potential issues~%")
(format t "  4. P90 confidence level recommended for critical deadlines~%")
(format t "  5. NEW: Use :type :simulation in defreport for automatic reports~%")
(format t "  6. NEW: Use :type :risk for risk register reports~%~%")

(format t "Try these commands in the REPL:~%~%")

(format t ";; Run a quick simulation~%")
(format t "(run-monte-carlo-simulation *current-project* :trials 1000)~%~%")

(format t ";; Get specific percentile~%")
(format t "(let ((results (run-monte-carlo-simulation *current-project*)))~%")
(format t "  (simulation-percentile results 90))  ; P90 duration~%~%")

(format t ";; Check probability of meeting a deadline~%")
(format t "(let ((results (run-risk-simulation *current-project*)))~%")
(format t "  (simulation-probability-of-completion results 60))  ; Within 60 days~%~%")

(format t ";; Sample a single task's duration~%")
(format t "(sample-pert-duration~%")
(format t "  (gethash 'backend (project-tasks *current-project*)))~%~%")

(format t "======================================================================~%~%")
