;;;; tests/scheduling/test-resource-allocation.lisp
;;;; Tests for resource allocation and leveling

(in-package #:project-juggler-tests)

(def-suite resource-leveling-suite
  :in scheduling-suite
  :description "Resource leveling tests")

(in-suite resource-leveling-suite)

;;; ============================================================================
;;; Resource Load Calculation Tests
;;; ============================================================================

(test calculate-resource-load-simple
  "Calculate resource load for a single task"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 5 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((dev1 (gethash 'dev1 (project-resources *current-project*)))
           (load (calculate-resource-load dev1 *current-project*
                                         (date 2024 3 1) (date 2024 3 5))))
      ;; Resource should be 100% allocated for 5 days
      (is (= 5 (length load)))
      (dolist (day-load load)
        (is (= 1.0 (cdr day-load)))))))

(test calculate-resource-load-overallocated
  "Detect over-allocation when resource assigned to multiple overlapping tasks"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :start (date 2024 3 2)
             :duration (duration 3 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((dev1 (gethash 'dev1 (project-resources *current-project*)))
           (load (calculate-resource-load dev1 *current-project*
                                         (date 2024 3 1) (date 2024 3 5))))
      ;; March 2-3 should be overallocated (200%)
      (is (some (lambda (day-load) (> (cdr day-load) 1.0)) load)))))

;;; ============================================================================
;;; Over-allocation Detection Tests
;;; ============================================================================

(test detect-overallocation-none
  "No over-allocation when tasks don't overlap"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :start (date 2024 3 5)
             :duration (duration 3 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((overallocations (detect-resource-overallocations *current-project*)))
      (is (= 0 (length overallocations))))))

(test detect-overallocation-exists
  "Detect over-allocation when tasks overlap on same resource"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 5 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :start (date 2024 3 3)
             :duration (duration 5 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((overallocations (detect-resource-overallocations *current-project*)))
      (is (> (length overallocations) 0)))))

(test detect-overallocation-multiple-resources
  "Detect over-allocation per resource correctly"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(defresource dev2 "Developer 2"))
    ;; dev1 is overallocated
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :start (date 2024 3 2)
             :duration (duration 3 :days)
             :allocate (dev1)))
    ;; dev2 is not overallocated
    (eval '(deftask task3 "Task 3"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev2)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((overallocations (detect-resource-overallocations *current-project*)))
      ;; Only dev1 should be overallocated
      (is (= 1 (length overallocations)))
      (is (eq 'dev1 (overallocation-resource-id (first overallocations)))))))

;;; ============================================================================
;;; Resource Leveling Tests
;;; ============================================================================

(test level-resources-simple
  "Level resources by shifting non-critical task"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    ;; Two overlapping tasks - one should be shifted
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :start (date 2024 3 2)
             :duration (duration 3 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Before leveling - should have overallocation
    (is (> (length (detect-resource-overallocations *current-project*)) 0))

    (level-resources *current-project*)

    ;; After leveling - no overallocation
    (is (= 0 (length (detect-resource-overallocations *current-project*))))))

(test level-resources-preserve-dependencies
  "Leveling should respect task dependencies"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :duration (duration 3 :days)
             :depends-on (task1)
             :allocate (dev1)))
    ;; Overlap with task2's original position
    (eval '(deftask task3 "Task 3"
             :start (date 2024 3 4)
             :duration (duration 3 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)
    (level-resources *current-project*)

    ;; task2 must still start after task1 ends
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      (is (not (date< (task-start task2) (task-end task1)))))))

(test level-resources-use-slack
  "Leveling should prefer tasks with more slack"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    ;; Critical path task
    (eval '(deftask critical "Critical Task"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)
             :priority 1000))
    ;; Non-critical task with overlap
    (eval '(deftask noncritical "Non-Critical Task"
             :start (date 2024 3 2)
             :duration (duration 2 :days)
             :allocate (dev1)
             :priority 100))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((critical-start-before (task-start (gethash 'critical (project-tasks *current-project*)))))
      (level-resources *current-project*)

      ;; Critical task should NOT have moved
      (let ((critical-start-after (task-start (gethash 'critical (project-tasks *current-project*)))))
        (is (date= critical-start-before critical-start-after))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test level-resources-no-overallocation
  "Leveling does nothing when no over-allocation exists"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :duration (duration 3 :days)
             :allocate (dev1)))
    (eval '(deftask task2 "Task 2"
             :start (date 2024 3 10)
             :duration (duration 3 :days)
             :allocate (dev1)))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Store original dates
    (let ((t1-start (task-start (gethash 'task1 (project-tasks *current-project*))))
          (t2-start (task-start (gethash 'task2 (project-tasks *current-project*)))))

      (level-resources *current-project*)

      ;; Dates should be unchanged
      (is (date= t1-start (task-start (gethash 'task1 (project-tasks *current-project*)))))
      (is (date= t2-start (task-start (gethash 'task2 (project-tasks *current-project*))))))))
