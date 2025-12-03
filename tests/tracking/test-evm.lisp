;;;; tests/tracking/test-evm.lisp
;;;; Tests for Earned Value Management
;;;;
;;;; EVM uses TaskJuggler-style scenarios. The first scenario is the baseline.

(in-package #:project-juggler-tests)

(def-suite evm-suite
  :in project-juggler-suite
  :description "Earned Value Management tests")

(in-suite evm-suite)

;;; ============================================================================
;;; EVM Metric Tests
;;; ============================================================================

(test calculate-planned-value
  "Calculate Planned Value (PV) correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 0)

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; At day 5 of 10-day task, PV should be 50%
    (let ((pv (calculate-planned-value *current-project* (date 2024 3 6))))
      (is (numberp pv))
      (is (>= pv 40.0))  ; Around 50% with some tolerance
      (is (<= pv 60.0)))))

(test calculate-earned-value
  "Calculate Earned Value (EV) correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Set task to 30% complete
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 30)

    (let ((ev (calculate-earned-value *current-project*)))
      (is (numberp ev))
      (is (= 30.0 ev)))))  ; 30% of 100%

(test calculate-schedule-variance
  "Calculate Schedule Variance (SV) correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Task is 30% complete at day 5 (should be 50%)
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 30)

    (let ((sv (calculate-schedule-variance *current-project* (date 2024 3 6))))
      (is (numberp sv))
      (is (< sv 0)))))  ; Behind schedule, so negative

(test calculate-schedule-performance-index
  "Calculate Schedule Performance Index (SPI) correctly"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Task is 30% complete at day 5 (should be ~50%)
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 30)

    (let ((spi (calculate-spi *current-project* (date 2024 3 6))))
      (is (numberp spi))
      (is (< spi 1.0)))))  ; Behind schedule, so < 1.0

(test evm-on-schedule
  "EVM metrics when project is on schedule"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Task is 50% complete at day 5 - exactly on schedule
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 50)

    (let ((sv (calculate-schedule-variance *current-project* (date 2024 3 6)))
          (spi (calculate-spi *current-project* (date 2024 3 6))))
      (is (numberp sv))
      (is (< (abs sv) 10))  ; Close to 0
      (is (numberp spi))
      (is (>= spi 0.9))     ; Close to 1.0
      (is (<= spi 1.1)))))

(test evm-ahead-of-schedule
  "EVM metrics when project is ahead of schedule"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 10 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Task is 70% complete at day 5 (should be ~50%) - ahead!
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 70)

    (let ((sv (calculate-schedule-variance *current-project* (date 2024 3 6)))
          (spi (calculate-spi *current-project* (date 2024 3 6))))
      (is (numberp sv))
      (is (> sv 0))         ; Ahead of schedule, so positive
      (is (numberp spi))
      (is (> spi 1.0)))))   ; Ahead of schedule, so > 1.0

;;; ============================================================================
;;; Multiple Task EVM Tests
;;; ============================================================================

(test evm-multiple-tasks
  "EVM calculations work with multiple tasks"
  (with-test-project
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))
    (deftask t2 "Task 2"
      :depends-on (t1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; T1 is complete, T2 is 50% complete
    (setf (task-complete (gethash 't1 (project-tasks *current-project*))) 100)
    (setf (task-complete (gethash 't2 (project-tasks *current-project*))) 50)

    (let ((ev (calculate-earned-value *current-project*)))
      (is (numberp ev))
      ;; Total EV = 100% of T1 + 50% of T2 = 150% / 2 tasks = 75%
      (is (>= ev 70.0))
      (is (<= ev 80.0)))))

;;; ============================================================================
;;; Resource Over-Allocation Tests
;;; ============================================================================

(test detect-resource-overallocation-none
  "Detect no over-allocation when resources are properly allocated"
  (with-test-project
    (defresource dev1 "Developer 1")
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :allocate (dev1))
    (deftask t2 "Task 2"
      :start (date 2024 3 10)  ; After t1
      :duration (duration 5 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((overallocations (detect-resource-overallocations *current-project*)))
      (is (listp overallocations))
      (is (= 0 (length overallocations))))))

(test detect-resource-overallocation-concurrent
  "Detect over-allocation when resource has concurrent tasks"
  (with-test-project
    (defresource dev1 "Developer 1")
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :allocate (dev1))
    (deftask t2 "Task 2"
      :start (date 2024 3 3)  ; Overlaps with t1
      :duration (duration 5 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((overallocations (detect-resource-overallocations *current-project*)))
      (is (listp overallocations))
      (is (> (length overallocations) 0))
      ;; Should detect dev1 is overallocated
      (let ((dev1-overalloc (find-if (lambda (oa)
                                      (eq 'dev1 (overallocation-resource-id oa)))
                                    overallocations)))
        (is (not (null dev1-overalloc)))))))

(test resource-load-calculation
  "Calculate resource load over time"
  (with-test-project
    (defresource dev1 "Developer 1")
    (deftask t1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Calculate load for the full task duration
    (let ((load-data (calculate-resource-load
                      (gethash 'dev1 (project-resources *current-project*))
                      *current-project*
                      (date 2024 3 1)
                      (date 2024 3 5))))
      (is (listp load-data))
      (is (> (length load-data) 0))
      ;; Each day should show 1.0 load (100% allocated)
      (dolist (day-load load-data)
        (is (= 1.0 (cdr day-load)))))))
