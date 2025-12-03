;;;; tests/scheduling/test-dependency-types.lisp
;;;; Tests for extended dependency types (SS, FF, SF) and lag/lead times

(in-package #:project-juggler-tests)

(def-suite dependency-types-suite
  :in project-juggler-suite
  :description "Tests for extended dependency types")

(in-suite dependency-types-suite)

;;; ============================================================================
;;; Basic Dependency Type Tests
;;; ============================================================================

(test dependency-type-finish-to-start-default
  "Verify default dependency type is finish-to-start"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on (task1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start when Task1 finishes
      (is (date= (task-start task2) (task-end task1))))))

(test dependency-type-finish-to-start-explicit
  "Test explicit finish-to-start dependency"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :fs)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start when Task1 finishes
      (is (date= (task-start task2) (task-end task1))))))

(test dependency-type-start-to-start
  "Test start-to-start dependency - Task2 starts when Task1 starts"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ss)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start when Task1 starts
      (is (date= (task-start task2) (task-start task1))))))

(test dependency-type-finish-to-finish
  "Test finish-to-finish dependency - Task2 finishes when Task1 finishes"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ff)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should finish when Task1 finishes
      ;; So Task2 start = Task1 end - Task2 duration
      (is (date= (task-end task2) (task-end task1))))))

(test dependency-type-start-to-finish
  "Test start-to-finish dependency - Task2 finishes when Task1 starts"
  (with-test-project
    (deftask task1 "Task 1"
      :start (date 2024 1 10)
      :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :sf)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should finish when Task1 starts
      (is (date= (task-end task2) (task-start task1))))))

;;; ============================================================================
;;; Lag (Positive Delay) Tests
;;; ============================================================================

(test dependency-lag-finish-to-start
  "Test finish-to-start with positive lag (delay)"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :fs :lag (duration 2 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start 2 days after Task1 finishes
      (is (date= (task-start task2) (date+ (task-end task1) (duration 2 :days)))))))

(test dependency-lag-start-to-start
  "Test start-to-start with positive lag"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ss :lag (duration 1 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start 1 day after Task1 starts
      (is (date= (task-start task2) (date+ (task-start task1) (duration 1 :days)))))))

(test dependency-lag-finish-to-finish
  "Test finish-to-finish with positive lag"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ff :lag (duration 2 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should finish 2 days after Task1 finishes
      (is (date= (task-end task2) (date+ (task-end task1) (duration 2 :days)))))))

;;; ============================================================================
;;; Lead (Negative Lag) Tests
;;; ============================================================================

(test dependency-lead-finish-to-start
  "Test finish-to-start with negative lag (lead time - overlap)"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :fs :lag (duration -2 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start 2 days before Task1 finishes (overlap)
      (is (date= (task-start task2) (date+ (task-end task1) (duration -2 :days)))))))

(test dependency-lead-start-to-start
  "Test start-to-start with negative lag (lead time)"
  (with-test-project
    (deftask task1 "Task 1"
      :start (date 2024 1 5)
      :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ss :lag (duration -1 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start 1 day before Task1 starts
      (is (date= (task-start task2) (date+ (task-start task1) (duration -1 :days)))))))

;;; ============================================================================
;;; Multiple Dependencies Tests
;;; ============================================================================

(test multiple-dependencies-same-type
  "Test task with multiple FS dependencies"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 3 :days))
    (deftask task2 "Task 2" :duration (duration 5 :days))
    (deftask task3 "Task 3"
      :duration (duration 2 :days)
      :depends-on (task1 task2))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*)))
          (task3 (gethash 'task3 (project-tasks *current-project*))))
      ;; Task3 should start after both Task1 and Task2 finish (use latest)
      (is (date= (task-start task3) (task-end task2)))
      (is (date>= (task-start task3) (task-end task1))))))

(test multiple-dependencies-mixed-types
  "Test task with multiple dependencies of different types"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2" :duration (duration 3 :days))
    (deftask task3 "Task 3"
      :duration (duration 2 :days)
      :depends-on ((task1 :type :fs)
                   (task2 :type :ss)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*)))
          (task3 (gethash 'task3 (project-tasks *current-project*))))
      ;; Task3 must satisfy both constraints:
      ;; - Start after Task1 finishes (FS)
      ;; - Start when/after Task2 starts (SS)
      ;; The latest constraint wins
      (is (date>= (task-start task3) (task-end task1)))
      (is (date>= (task-start task3) (task-start task2))))))

(test multiple-dependencies-with-lags
  "Test task with multiple dependencies with different lag values"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 3 :days))
    (deftask task2 "Task 2" :duration (duration 5 :days))
    (deftask task3 "Task 3"
      :duration (duration 2 :days)
      :depends-on ((task1 :type :fs :lag (duration 1 :days))
                   (task2 :type :fs :lag (duration 2 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*)))
          (task3 (gethash 'task3 (project-tasks *current-project*))))
      ;; Task3 should satisfy both constraints (latest wins)
      (is (date>= (task-start task3) (date+ (task-end task1) (duration 1 :days))))
      (is (date>= (task-start task3) (date+ (task-end task2) (duration 2 :days)))))))

;;; ============================================================================
;;; Dependency Object Tests
;;; ============================================================================

(test dependency-object-stores-type
  "Verify dependency object stores correct type"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ss)))
    (finalize-project *current-project*)
    (let* ((task2 (gethash 'task2 (project-tasks *current-project*)))
           (deps (task-dependencies task2)))
      (is (= 1 (length deps)))
      (is (eq :ss (dependency-type (first deps)))))))

(test dependency-object-stores-lag
  "Verify dependency object stores correct lag"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :fs :lag (duration 3 :days))))
    (finalize-project *current-project*)
    (let* ((task2 (gethash 'task2 (project-tasks *current-project*)))
           (deps (task-dependencies task2))
           (dep-lag (dependency-gap (first deps))))
      (is (= 1 (length deps)))
      (is (duration-p dep-lag))
      (is (= 3 (duration-in-days dep-lag))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test dependency-zero-lag
  "Test dependency with zero lag (equivalent to no lag)"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :fs :lag (duration 0 :days))))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      (is (date= (task-start task2) (task-end task1))))))

(test dependency-backward-compatible-syntax
  "Test that simple symbol syntax still works (backward compatibility)"
  (with-test-project
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on (task1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      (is (date= (task-start task2) (task-end task1)))
      ;; Verify the dependency type defaults to FS
      (let ((dep (first (task-dependencies task2))))
        (is (eq :fs (dependency-type dep)))))))

(test finish-to-finish-calculates-start-correctly
  "Verify FF dependency correctly calculates start date from end constraint"
  (with-test-project
    ;; Task1: 5 days starting at project start (2024-01-01)
    ;; Task2: 3 days, must finish when Task1 finishes
    (deftask task1 "Task 1" :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on ((task1 :type :ff)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task1: Jan 1 - Jan 6
      ;; Task2 must end Jan 6, duration 3 days, so starts Jan 3
      (is (date= (task-end task2) (task-end task1)))
      ;; Verify start is calculated correctly (end - duration)
      (is (date= (task-start task2) (date+ (task-end task1) (duration -3 :days)))))))

(test start-to-finish-with-fixed-target-start
  "Test SF dependency when target has fixed start date"
  (with-test-project
    ;; Task1 starts on Jan 10
    ;; Task2 must finish when Task1 starts
    (deftask task1 "Task 1"
      :start (date 2024 1 10)
      :duration (duration 5 :days))
    (deftask task2 "Task 2"
      :duration (duration 4 :days)
      :depends-on ((task1 :type :sf)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
          (task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 must end when Task1 starts (Jan 10)
      ;; Task2 duration is 4 days, so it starts Jan 6
      (is (date= (task-end task2) (task-start task1)))
      (is (date= (task-start task2) (date+ (task-start task1) (duration -4 :days)))))))
