;;;; tests/tracking/test-baseline-scenarios.lisp
;;;; Tests for scenario-based baseline system (TaskJuggler-style)
;;;;
;;;; In TaskJuggler-style, the first scenario is the baseline.
;;;; There's no separate "baseline" concept - just scenarios.

(in-package #:project-juggler-tests)

(def-suite baseline-scenarios-suite
  :in project-juggler-suite
  :description "Tests for scenario-based baseline system")

(in-suite baseline-scenarios-suite)

;;; ============================================================================
;;; Baseline as First Scenario Tests
;;; ============================================================================

(test first-scenario-is-baseline
  "First declared scenario is the baseline"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-proj "Test Project"
             :start (date 2024 3 1)
             :end (date 2024 6 30)
             :scenarios (plan delayed)))
    (is (eq 'plan (baseline-scenario-id *current-project*)))))

(test baseline-scenario-accessor
  "Can get baseline scenario object"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-proj "Test Project"
             :start (date 2024 3 1)
             :end (date 2024 6 30)
             :scenarios (original revised)))
    (let ((baseline (baseline-scenario *current-project*)))
      (is (not (null baseline)))
      (is (eq 'original (scenario-id baseline))))))

;;; ============================================================================
;;; Scenario Comparison as Baseline Comparison
;;; ============================================================================

(test compare-baseline-to-alternative
  "Compare baseline scenario to alternative scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 5 :days)
             :delayed/duration (duration 10 :days)))
    (eval '(deftask t2 "Task 2"
             :depends-on (t1)
             :duration (duration 5 :days)
             :delayed/duration (duration 8 :days)))
    (let ((comparison (compare-scenarios *current-project* 'plan 'delayed)))
      ;; Baseline (plan) has shorter duration
      (is (< (getf comparison :duration-1)
             (getf comparison :duration-2))))))

(test compare-multiple-scenarios
  "Compare multiple alternative scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :effort (duration 10 :days)
             :optimistic/effort (duration 5 :days)
             :pessimistic/effort (duration 20 :days)))
    (let ((opt-vs-plan (compare-scenarios *current-project* 'optimistic 'plan))
          (plan-vs-pess (compare-scenarios *current-project* 'plan 'pessimistic)))
      ;; Optimistic < Plan
      (is (< (getf opt-vs-plan :effort-1)
             (getf opt-vs-plan :effort-2)))
      ;; Plan < Pessimistic
      (is (< (getf plan-vs-pess :effort-1)
             (getf plan-vs-pess :effort-2))))))

;;; ============================================================================
;;; Task-Level Baseline Comparison
;;; ============================================================================

(test compare-task-to-baseline
  "Compare individual task across scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 5 :days)
             :effort (duration 5 :days)
             :delayed/duration (duration 10 :days)
             :delayed/effort (duration 8 :days)))
    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (comparison (compare-task-scenarios task 'plan 'delayed)))
      ;; Plan (baseline) values
      (is (= 5 (getf comparison :duration-1)))
      (is (= 5 (getf comparison :effort-1)))
      ;; Delayed values
      (is (= 10 (getf comparison :duration-2)))
      (is (= 8 (getf comparison :effort-2))))))

;;; ============================================================================
;;; Scenario Summary for Baseline Analysis
;;; ============================================================================

(test baseline-scenario-summary
  "Get summary for baseline scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)
             :effort (duration 8 :days)))
    (eval '(deftask t2 "Task 2"
             :duration (duration 5 :days)
             :effort (duration 4 :days)))
    (let* ((baseline-id (baseline-scenario-id *current-project*))
           (summary (scenario-summary *current-project* baseline-id)))
      (is (= 15 (getf summary :total-duration)))
      (is (= 12 (getf summary :total-effort)))
      (is (= 2 (getf summary :task-count))))))

(test alternative-scenario-summary
  "Get summary for alternative scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)
             :delayed/duration (duration 15 :days)))
    (let ((baseline-summary (scenario-summary *current-project* 'plan))
          (delayed-summary (scenario-summary *current-project* 'delayed)))
      (is (= 10 (getf baseline-summary :total-duration)))
      (is (= 15 (getf delayed-summary :total-duration))))))

;;; ============================================================================
;;; EVM with Scenarios
;;; ============================================================================

(test evm-uses-baseline-scenario
  "EVM calculations use baseline scenario data by default"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 10 :days)
             :complete 50))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; EVM should work with baseline scenario values
    (let ((ev (calculate-earned-value *current-project*)))
      (is (numberp ev))
      (is (= 50.0 ev)))))

(test scenario-variance-analysis
  "Analyze variance between baseline and alternative scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)
             :delayed/duration (duration 15 :days)))
    (eval '(deftask t2 "Task 2"
             :duration (duration 5 :days)
             :delayed/duration (duration 8 :days)))
    (let ((comparison (compare-scenarios *current-project* 'plan 'delayed)))
      ;; Total variance = (15+8) - (10+5) = 8 days
      (is (= 8 (- (getf comparison :duration-2)
                  (getf comparison :duration-1)))))))

;;; ============================================================================
;;; Scheduled Values with Scenarios
;;; ============================================================================

(test scheduled-values-per-scenario
  "Get all scheduled values for a task in different scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 5 :days)
             :effort (duration 40 :hours)
             :complete 50
             :delayed/duration (duration 10 :days)
             :delayed/effort (duration 60 :hours)
             :delayed/complete 25))
    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (plan-vals (task-scheduled-values-for-scenario task 'plan))
           (delayed-vals (task-scheduled-values-for-scenario task 'delayed)))
      ;; Plan (baseline) values
      (is (= 5 (duration-in-days (getf plan-vals :duration))))
      (is (= 50 (getf plan-vals :complete)))
      ;; Delayed values
      (is (= 10 (duration-in-days (getf delayed-vals :duration))))
      (is (= 25 (getf delayed-vals :complete))))))
