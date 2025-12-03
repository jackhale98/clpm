;;;; tests/scenarios/test-scenarios.lisp
;;;; Tests for TaskJuggler-style scenario system

(in-package #:project-juggler-tests)

(def-suite scenarios-suite
  :in project-juggler-suite
  :description "Tests for TaskJuggler-style scenario system")

(in-suite scenarios-suite)

;;; ============================================================================
;;; Scenario Declaration Tests
;;; ============================================================================

(test defproject-creates-scenarios
  "Test that defproject creates declared scenarios"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-proj "Test Project"
             :start (date 2024 3 1)
             :end (date 2024 6 30)
             :scenarios (plan delayed)))
    (let ((scenarios (list-scenarios *current-project*)))
      (is (= 2 (length scenarios)))
      (is (member 'plan scenarios))
      (is (member 'delayed scenarios)))))

(test defproject-default-scenario-is-plan
  "Test that defproject defaults to plan scenario"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-proj "Test Project"
             :start (date 2024 3 1)
             :end (date 2024 6 30)))
    (let ((scenarios (list-scenarios *current-project*)))
      (is (= 1 (length scenarios)))
      ;; Compare by symbol name to avoid package issues
      (is (string= "PLAN" (symbol-name (first scenarios)))))))

(test get-scenario-by-id
  "Test retrieving a scenario by ID"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-proj "Test Project"
             :start (date 2024 3 1)
             :end (date 2024 6 30)
             :scenarios (plan delayed)))
    (let ((scenario (get-scenario *current-project* 'delayed)))
      (is (not (null scenario)))
      (is (eq 'delayed (scenario-id scenario))))))

(test baseline-scenario-is-first
  "Test that baseline scenario is the first declared"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-proj "Test Project"
             :start (date 2024 3 1)
             :end (date 2024 6 30)
             :scenarios (plan delayed)))
    (is (eq 'plan (baseline-scenario-id *current-project*)))))

;;; ============================================================================
;;; Scenario-Specific Task Values Tests
;;; ============================================================================

(test deftask-scenario-specific-effort
  "Test that deftask can set scenario-specific effort"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :effort (duration 35 :days)
             :delayed/effort (duration 40 :days)))
    (let ((task (gethash 't1 (project-tasks *current-project*))))
      ;; Base effort
      (is (= 35 (duration-in-days (task-effort task))))
      ;; Plan scenario uses base value
      (is (= 35 (duration-in-days (task-effort-for-scenario task 'plan))))
      ;; Delayed scenario uses override
      (is (= 40 (duration-in-days (task-effort-for-scenario task 'delayed)))))))

(test deftask-scenario-specific-duration
  "Test that deftask can set scenario-specific duration"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 5 :days)
             :delayed/duration (duration 10 :days)))
    (let ((task (gethash 't1 (project-tasks *current-project*))))
      (is (= 5 (duration-in-days (task-duration-for-scenario task 'plan))))
      (is (= 10 (duration-in-days (task-duration-for-scenario task 'delayed)))))))

(test scenario-value-fallback
  "Test that scenario values fall back to base when not overridden"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :effort (duration 20 :days)
             :duration (duration 10 :days)
             :delayed/effort (duration 30 :days)))  ; Only effort overridden
    (let ((task (gethash 't1 (project-tasks *current-project*))))
      ;; Effort is overridden for delayed
      (is (= 30 (duration-in-days (task-effort-for-scenario task 'delayed))))
      ;; Duration falls back to base for delayed
      (is (= 10 (duration-in-days (task-duration-for-scenario task 'delayed)))))))

(test multiple-scenario-overrides
  "Test task with multiple scenario overrides"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :effort (duration 10 :days)
             :optimistic/effort (duration 5 :days)
             :pessimistic/effort (duration 20 :days)))
    (let ((task (gethash 't1 (project-tasks *current-project*))))
      (is (= 10 (duration-in-days (task-effort-for-scenario task 'plan))))
      (is (= 5 (duration-in-days (task-effort-for-scenario task 'optimistic))))
      (is (= 20 (duration-in-days (task-effort-for-scenario task 'pessimistic)))))))

;;; ============================================================================
;;; Scenario Comparison Tests
;;; ============================================================================

(test compare-scenarios-duration
  "Test comparing total duration between scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)
             :delayed/duration (duration 15 :days)))
    (eval '(deftask t2 "Task 2"
             :duration (duration 5 :days)
             :delayed/duration (duration 8 :days)))
    (let ((comparison (compare-scenarios *current-project* 'plan 'delayed)))
      (is (= 15 (getf comparison :duration-1)))  ; plan: 10 + 5
      (is (= 23 (getf comparison :duration-2)))))) ; delayed: 15 + 8

(test compare-scenarios-effort
  "Test comparing total effort between scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :effort (duration 10 :days)
             :delayed/effort (duration 20 :days)))
    (let ((comparison (compare-scenarios *current-project* 'plan 'delayed)))
      (is (= 10 (getf comparison :effort-1)))
      (is (= 20 (getf comparison :effort-2))))))

(test compare-task-scenarios
  "Test comparing individual task between scenarios"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 5 :days)
             :effort (duration 5 :days)
             :delayed/duration (duration 10 :days)
             :delayed/effort (duration 8 :days)))
    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (comparison (compare-task-scenarios task 'plan 'delayed)))
      (is (= 5 (getf comparison :duration-1)))
      (is (= 10 (getf comparison :duration-2)))
      (is (= 5 (getf comparison :effort-1)))
      (is (= 8 (getf comparison :effort-2))))))

;;; ============================================================================
;;; Scenario Summary Tests
;;; ============================================================================

(test scenario-summary-basic
  "Test getting summary statistics for a scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)
             :effort (duration 8 :days)))
    (eval '(deftask t2 "Task 2"
             :duration (duration 5 :days)
             :effort (duration 4 :days)))
    (let ((summary (scenario-summary *current-project* 'plan)))
      (is (= 15 (getf summary :total-duration)))
      (is (= 12 (getf summary :total-effort)))
      (is (= 2 (getf summary :task-count))))))

(test scenario-summary-with-override
  "Test scenario summary reflects overridden values"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)
             :delayed/duration (duration 20 :days)))
    (let ((plan-summary (scenario-summary *current-project* 'plan))
          (delayed-summary (scenario-summary *current-project* 'delayed)))
      (is (= 10 (getf plan-summary :total-duration)))
      (is (= 20 (getf delayed-summary :total-duration))))))

;;; ============================================================================
;;; Task Scheduled Values Tests
;;; ============================================================================

(test task-scheduled-values-for-scenario
  "Test getting all scheduled values for a task in a scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 5 :days)
             :effort (duration 40 :hours)
             :complete 50
             :delayed/duration (duration 10 :days)
             :delayed/complete 25))
    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (plan-vals (task-scheduled-values-for-scenario task 'plan))
           (delayed-vals (task-scheduled-values-for-scenario task 'delayed)))
      ;; Plan uses base values
      (is (= 5 (duration-in-days (getf plan-vals :duration))))
      (is (= 50 (getf plan-vals :complete)))
      ;; Delayed uses overrides
      (is (= 10 (duration-in-days (getf delayed-vals :duration))))
      (is (= 25 (getf delayed-vals :complete))))))
