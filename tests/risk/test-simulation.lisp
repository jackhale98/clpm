;;;; tests/risk/test-simulation.lisp
;;;; Tests for Monte Carlo PERT simulation

(in-package #:project-juggler-tests)

(def-suite simulation-suite
  :in project-juggler-suite
  :description "Tests for Monte Carlo PERT simulation")

(in-suite simulation-suite)

;;; ============================================================================
;;; PERT Sampling Tests
;;; ============================================================================

(test sample-pert-duration-bounds
  "Test that sampled PERT durations stay within bounds"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 20 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Sample 100 times and verify all within bounds
      (dotimes (i 100)
        (let ((sample (sample-pert-duration task)))
          (is (>= sample 5.0))
          (is (<= sample 20.0)))))))

(test sample-pert-duration-distribution
  "Test that sampled mean approximates PERT expected value"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 20 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (expected (pert-expected-duration task))
           (samples (loop repeat 1000 collect (sample-pert-duration task)))
           (sample-mean (/ (reduce #'+ samples) (length samples))))
      ;; Sample mean should be within 10% of expected
      (is (< (abs (- sample-mean expected)) (* expected 0.1))))))

(test sample-non-pert-task
  "Test sampling a task without PERT estimate returns fixed duration"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Should always return the fixed duration
      (is (= 10.0 (sample-pert-duration task)))
      (is (= 10.0 (sample-pert-duration task))))))

;;; ============================================================================
;;; Single Trial Tests
;;; ============================================================================

(test run-single-trial-basic
  "Test running a single simulation trial"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((trial (run-simulation-trial *current-project* 1)))
      (is (not (null trial)))
      (is (= 1 (trial-number trial)))
      (is (numberp (trial-project-duration trial)))
      (is (> (trial-project-duration trial) 0)))))

(test trial-respects-dependencies
  "Test that simulation trials respect task dependencies"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (deftask task2 "Task 2"
      :estimate (:optimistic (duration 3 :days)
                 :likely (duration 5 :days)
                 :pessimistic (duration 8 :days))
      :depends-on (task1)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((trial (run-simulation-trial *current-project* 1)))
      ;; Project duration should be at least task1 + task2 minimum
      (is (>= (trial-project-duration trial) 8.0))
      ;; Task end dates should be in order
      (let ((task-ends (trial-task-end-dates trial)))
        (is (date<= (gethash 'task1 task-ends)
                    (gethash 'task2 task-ends)))))))

(test trial-records-task-durations
  "Test that trial records sampled durations for each task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (deftask task2 "Task 2"
      :duration (duration 5 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((trial (run-simulation-trial *current-project* 1)))
      (let ((durations (trial-task-durations trial)))
        (is (not (null durations)))
        (is (numberp (gethash 'task1 durations)))
        (is (= 5.0 (gethash 'task2 durations)))))))

;;; ============================================================================
;;; Monte Carlo Simulation Tests
;;; ============================================================================

(test monte-carlo-basic
  "Test basic Monte Carlo simulation runs successfully"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 100)))
      (is (not (null results)))
      (is (= 100 (simulation-trial-count results)))
      (is (= 100 (length (simulation-durations results)))))))

(test monte-carlo-statistics
  "Test Monte Carlo statistics calculations"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 500)))
      ;; Mean should be close to PERT expected (10 days)
      (is (< (abs (- (simulation-mean results) 10.0)) 1.0))
      ;; Std dev should be positive
      (is (> (simulation-std-dev results) 0))
      ;; Min should be >= optimistic
      (is (>= (simulation-min results) 5.0))
      ;; Max should be <= pessimistic
      (is (<= (simulation-max results) 15.0)))))

(test monte-carlo-percentiles
  "Test Monte Carlo percentile calculations"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 20 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 500)))
      (let ((p10 (simulation-percentile results 10))
            (p50 (simulation-percentile results 50))
            (p90 (simulation-percentile results 90)))
        ;; Percentiles should be ordered
        (is (< p10 p50))
        (is (< p50 p90))
        ;; P50 should be close to mean
        (is (< (abs (- p50 (simulation-mean results))) 2.0))))))

(test monte-carlo-probability-of-completion
  "Test probability of completion by target date"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 1000)))
      ;; Probability at min should be ~0
      (is (< (simulation-probability-of-completion results 5) 0.1))
      ;; Probability at expected should be ~50%
      (is (< (abs (- (simulation-probability-of-completion results 10) 0.5)) 0.15))
      ;; Probability at max should be ~100%
      (is (> (simulation-probability-of-completion results 15) 0.9)))))

(test monte-carlo-histogram
  "Test Monte Carlo histogram generation"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 20 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 500)))
      (let ((histogram (simulation-histogram results :bins 10)))
        ;; Should have 10 bins
        (is (= 10 (length histogram)))
        ;; Each bin should have (min max count)
        (is (= 3 (length (first histogram))))
        ;; Total count should equal trials
        (is (= 500 (reduce #'+ histogram :key #'third)))))))

;;; ============================================================================
;;; Convergence Tests
;;; ============================================================================

(test monte-carlo-convergence
  "Test that more trials lead to more stable results"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; Run twice with same trial count - means should be similar
    (let* ((results1 (run-monte-carlo-simulation *current-project* :trials 500))
           (results2 (run-monte-carlo-simulation *current-project* :trials 500))
           (mean1 (simulation-mean results1))
           (mean2 (simulation-mean results2)))
      ;; Two runs should produce similar means (within 1 day)
      (is (< (abs (- mean1 mean2)) 1.0)))))

;;; ============================================================================
;;; Multi-Task Project Tests
;;; ============================================================================

(test monte-carlo-multi-task-sequential
  "Test simulation with multiple sequential tasks"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 3 :days)
                 :likely (duration 5 :days)
                 :pessimistic (duration 8 :days))
      :allocate (dev1))
    (deftask task2 "Task 2"
      :estimate (:optimistic (duration 4 :days)
                 :likely (duration 6 :days)
                 :pessimistic (duration 10 :days))
      :depends-on (task1)
      :allocate (dev1))
    (deftask task3 "Task 3"
      :estimate (:optimistic (duration 2 :days)
                 :likely (duration 4 :days)
                 :pessimistic (duration 6 :days))
      :depends-on (task2)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 500)))
      ;; Total expected = 5 + 6 + 4 = 15 days (with ceiling rounding, may be higher)
      (is (< (abs (- (simulation-mean results) 17.0)) 3.0))
      ;; Min should be >= 3 + 4 + 2 = 9 (with ceiling rounding)
      (is (>= (simulation-min results) 9.0))
      ;; Max should be <= 8 + 10 + 6 = 24 (plus some tolerance for rounding)
      (is (<= (simulation-max results) 27.0)))))

(test monte-carlo-multi-task-parallel
  "Test simulation with parallel tasks"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (defresource dev2 "Developer 2" :rate 100.0)
    ;; Two parallel tasks
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (deftask task2 "Task 2"
      :estimate (:optimistic (duration 8 :days)
                 :likely (duration 12 :days)
                 :pessimistic (duration 18 :days))
      :allocate (dev2))
    ;; Final task depends on both
    (deftask task3 "Task 3"
      :estimate (:optimistic (duration 2 :days)
                 :likely (duration 3 :days)
                 :pessimistic (duration 5 :days))
      :depends-on (task1 task2)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 500)))
      ;; Critical path is task2 -> task3 (12 + 3 = 15 expected)
      ;; Total should be around max(task1, task2) + task3
      (is (< (abs (- (simulation-mean results) 15.0)) 2.0)))))

;;; ============================================================================
;;; Risk Integration Tests
;;; ============================================================================

(test risk-simulation-basic
  "Test Monte Carlo simulation with risk integration"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; Add a risk that impacts schedule
    (create-risk *current-project* 'risk1 "Key developer sick"
                 :probability 0.3
                 :impact 0.8
                 :tasks '(task1)
                 :schedule-impact 0.5)  ; 50% longer if occurs
    (let ((results (run-risk-simulation *current-project* :trials 500)))
      (is (not (null results)))
      ;; Mean should be higher than base PERT due to risk
      ;; Base expected = 10, with 30% chance of 50% increase
      ;; Expected with risk ≈ 10 + 0.3 * 5 = 11.5
      (is (> (simulation-mean results) 10.0)))))

(test risk-occurrence-tracking
  "Test that risk simulation tracks risk occurrences"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (create-risk *current-project* 'risk1 "Risk with 50% probability"
                 :probability 0.5
                 :impact 0.8
                 :tasks '(task1)
                 :schedule-impact 0.2)
    (let ((results (run-risk-simulation *current-project* :trials 1000)))
      (let ((occurrences (simulation-risk-occurrences results 'risk1)))
        ;; Should occur roughly 50% of time (within 10%)
        (is (< (abs (- (/ occurrences 1000.0) 0.5)) 0.1))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test simulation-single-task
  "Test simulation with single task project"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Only Task"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 100)))
      (is (not (null results)))
      (is (= 100 (simulation-trial-count results))))))

(test simulation-no-pert-tasks
  "Test simulation with no PERT estimates (deterministic)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 5 :days) :allocate (dev1))
    (deftask task2 "Task 2" :duration (duration 3 :days) :depends-on (task1) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 100)))
      ;; All trials should have same duration (5 + 3 = 8)
      (is (= 8.0 (simulation-mean results)))
      (is (= 0.0 (simulation-std-dev results)))
      (is (= 8.0 (simulation-min results)))
      (is (= 8.0 (simulation-max results))))))

(test simulation-mixed-tasks
  "Test simulation with mix of PERT and fixed duration tasks"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Fixed Task"
      :duration (duration 5 :days)
      :allocate (dev1))
    (deftask task2 "PERT Task"
      :estimate (:optimistic (duration 3 :days)
                 :likely (duration 5 :days)
                 :pessimistic (duration 10 :days))
      :depends-on (task1)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 500)))
      ;; Expected = 5 (fixed) + ~5.3 (PERT expected) = ~10.3
      ;; With ceiling rounding this can be higher, allow ±2 tolerance
      (is (< (abs (- (simulation-mean results) 11.0)) 2.0))
      ;; Min = 5 + ceil(3) = 8 minimum
      (is (>= (simulation-min results) 8.0))
      ;; Max = 5 + ceil(10) = 15 maximum (could be 16 with ceiling)
      (is (<= (simulation-max results) 16.0)))))

;;; ============================================================================
;;; Results Access Tests
;;; ============================================================================

(test simulation-results-accessors
  "Test simulation results accessor functions"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 10 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((results (run-monte-carlo-simulation *current-project* :trials 100)))
      (is (eq *current-project* (simulation-project results)))
      (is (= 100 (simulation-trial-count results)))
      (is (listp (simulation-trials results)))
      (is (listp (simulation-durations results)))
      (is (numberp (simulation-mean results)))
      (is (numberp (simulation-std-dev results)))
      (is (numberp (simulation-min results)))
      (is (numberp (simulation-max results))))))

