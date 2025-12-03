;;;; tests/scheduling/test-pert.lisp
;;;; Tests for PERT three-point estimation

(in-package #:project-juggler-tests)

(def-suite pert-suite
  :in project-juggler-suite
  :description "Tests for PERT three-point estimation")

(in-suite pert-suite)

;;; ============================================================================
;;; Basic PERT Estimation Tests
;;; ============================================================================

(test pert-estimate-definition
  "Test defining a task with PERT three-point estimate"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (is (not (null (task-estimate task))))
      (let ((est (task-estimate task)))
        (is (= 5 (duration-in-days (estimate-optimistic est))))
        (is (= 8 (duration-in-days (estimate-likely est))))
        (is (= 15 (duration-in-days (estimate-pessimistic est))))))))

(test pert-expected-duration
  "Test PERT expected duration calculation: (O + 4M + P) / 6"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Expected = (5 + 4*8 + 15) / 6 = (5 + 32 + 15) / 6 = 52/6 ≈ 8.67
      (is (< (abs (- (pert-expected-duration task) 8.67)) 0.1)))))

(test pert-standard-deviation
  "Test PERT standard deviation calculation: (P - O) / 6"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; SD = (15 - 5) / 6 = 10 / 6 ≈ 1.67
      (is (< (abs (- (pert-standard-deviation task) 1.67)) 0.1)))))

(test pert-variance
  "Test PERT variance calculation: σ²"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Variance = SD² = 1.67² ≈ 2.78
      (is (< (abs (- (pert-variance task) 2.78)) 0.1)))))

;;; ============================================================================
;;; Confidence Interval Tests
;;; ============================================================================

(test pert-confidence-interval-68
  "Test 68% confidence interval (±1σ)"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (interval (pert-confidence-interval task 68)))
      ;; Expected ≈ 8.67, SD ≈ 1.67
      ;; 68% interval: 8.67 ± 1.67 = (7.0, 10.34)
      (is (< (abs (- (first interval) 7.0)) 0.2))
      (is (< (abs (- (second interval) 10.34)) 0.2)))))

(test pert-confidence-interval-95
  "Test 95% confidence interval (±2σ)"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (interval (pert-confidence-interval task 95)))
      ;; Expected ≈ 8.67, SD ≈ 1.67
      ;; 95% interval: 8.67 ± 3.33 = (5.33, 12.0)
      (is (< (abs (- (first interval) 5.33)) 0.2))
      (is (< (abs (- (second interval) 12.0)) 0.2)))))

(test pert-confidence-interval-99
  "Test 99% confidence interval (±3σ)"
  (with-test-project
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (defresource dev1 "Developer" :rate 100.0)
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (interval (pert-confidence-interval task 99)))
      ;; Expected ≈ 8.67, SD ≈ 1.67
      ;; 99% interval: 8.67 ± 5.0 = (3.67, 13.67)
      (is (< (abs (- (first interval) 3.67)) 0.2))
      (is (< (abs (- (second interval) 13.67)) 0.2)))))

;;; ============================================================================
;;; Project-Level PERT Tests
;;; ============================================================================

(test project-pert-total-expected
  "Test project total expected duration with PERT"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    ;; Two sequential tasks
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 3 :days)
                 :likely (duration 5 :days)
                 :pessimistic (duration 10 :days))
      :allocate (dev1))
    (deftask task2 "Task 2"
      :estimate (:optimistic (duration 4 :days)
                 :likely (duration 6 :days)
                 :pessimistic (duration 12 :days))
      :depends-on (task1)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)  ; Need to schedule to get critical path
    ;; Task1 expected: (3 + 20 + 10)/6 = 5.5
    ;; Task2 expected: (4 + 24 + 12)/6 = 6.67
    ;; Total: 12.17
    (let ((total (project-pert-expected-duration *current-project*)))
      (is (< (abs (- total 12.17)) 0.2)))))

(test project-pert-variance
  "Test project variance (sum of critical path variances)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (deftask task2 "Task 2"
      :estimate (:optimistic (duration 4 :days)
                 :likely (duration 6 :days)
                 :pessimistic (duration 10 :days))
      :depends-on (task1)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)  ; Need to schedule to get critical path
    ;; Task1 variance: ((15-5)/6)² = 2.78
    ;; Task2 variance: ((10-4)/6)² = 1.0
    ;; Total variance: 3.78
    (let ((total-var (project-pert-variance *current-project*)))
      (is (< (abs (- total-var 3.78)) 0.2)))))

;;; ============================================================================
;;; Probability Tests
;;; ============================================================================

(test probability-of-completion-by-date
  "Test calculating probability of completing by a target date"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    ;; Expected: 8.67, SD: 1.67
    ;; Probability of completing by day 10:
    ;; Z = (10 - 8.67) / 1.67 ≈ 0.8
    ;; P(Z < 0.8) ≈ 0.79 (79%)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (prob (probability-of-completion-by task 10)))
      ;; Probability should be around 0.79 (79%)
      (is (> prob 0.7))   ; At least 70%
      (is (< prob 0.9))))) ; At most 90%

;;; ============================================================================
;;; PERT Scheduling Integration Tests
;;; ============================================================================

(test pert-uses-expected-for-scheduling
  "Test that scheduling uses PERT expected duration when estimate provided"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :likely (duration 8 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (start (task-start task))
           (end (task-end task))
           (start-ts (local-time:timestamp-to-unix (date-timestamp start)))
           (end-ts (local-time:timestamp-to-unix (date-timestamp end)))
           (duration-days (truncate (/ (- end-ts start-ts) 86400))))
      ;; Expected duration ≈ 8.67, so scheduled duration should be 9 days (ceiling)
      (is (= 9 duration-days)))))

(test pert-estimate-without-likely
  "Test PERT estimate with only optimistic and pessimistic (triangular)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :estimate (:optimistic (duration 5 :days)
                 :pessimistic (duration 15 :days))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (est (task-estimate task)))
      ;; When likely is not provided, use (O + P) / 2
      (is (= 10 (duration-in-days (estimate-likely est)))))))
