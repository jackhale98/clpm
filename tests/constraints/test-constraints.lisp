;;;; tests/constraints/test-constraints.lisp
;;;; Tests for task constraints and recurring tasks

(in-package #:project-juggler-tests)

(def-suite constraints-suite
  :in project-juggler-suite
  :description "Tests for task constraints and recurring tasks")

(in-suite constraints-suite)

;;; ============================================================================
;;; Start Constraint Tests (SNET - Start No Earlier Than)
;;; ============================================================================

(test constraint-start-no-earlier-than
  "Test SNET (Start No Earlier Than) constraint"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :start-constraint (:snet (date 2024 3 15))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task should start on or after March 15
      (is (date>= (task-start task) (date 2024 3 15))))))

(test constraint-snet-with-dependency
  "Test SNET constraint combined with dependency"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1))
    ;; Task2 depends on task1 but has SNET of March 20
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on (task1)
      :start-constraint (:snet (date 2024 3 20))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task2 (gethash 'task2 (project-tasks *current-project*))))
      ;; Task2 should start on March 20 (constraint) or later
      (is (date>= (task-start task2) (date 2024 3 20))))))

;;; ============================================================================
;;; Start Constraint Tests (SNLT - Start No Later Than)
;;; ============================================================================

(test constraint-start-no-later-than
  "Test SNLT (Start No Later Than) constraint"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :start-constraint (:snlt (date 2024 3 10))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task should start on or before March 10
      (is (date<= (task-start task) (date 2024 3 10))))))

;;; ============================================================================
;;; Finish Constraint Tests (FNET - Finish No Earlier Than)
;;; ============================================================================

(test constraint-finish-no-earlier-than
  "Test FNET (Finish No Earlier Than) constraint"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :finish-constraint (:fnet (date 2024 3 20))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task should finish on or after March 20
      (is (date>= (task-end task) (date 2024 3 20))))))

;;; ============================================================================
;;; Finish Constraint Tests (FNLT - Finish No Later Than)
;;; ============================================================================

(test constraint-finish-no-later-than
  "Test FNLT (Finish No Later Than) constraint"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :finish-constraint (:fnlt (date 2024 3 15))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task should finish on or before March 15
      (is (date<= (task-end task) (date 2024 3 15))))))

;;; ============================================================================
;;; Must Start/Finish On Constraints
;;; ============================================================================

(test constraint-must-start-on
  "Test MSO (Must Start On) constraint"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :start-constraint (:mso (date 2024 3 15))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task must start exactly on March 15
      (is (date= (task-start task) (date 2024 3 15))))))

(test constraint-must-finish-on
  "Test MFO (Must Finish On) constraint"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :finish-constraint (:mfo (date 2024 3 20))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task must finish exactly on March 20
      (is (date= (task-end task) (date 2024 3 20))))))

;;; ============================================================================
;;; Constraint Conflict Detection
;;; ============================================================================

(test constraint-conflict-detection
  "Test detection of constraint conflicts"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    ;; Task1 is 10 days long starting from SNET March 1
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :start-constraint (:snet (date 2024 3 1))
      :allocate (dev1))
    ;; Task2 depends on task1 but must start on March 5 - conflict!
    ;; task1 ends March 11, so task2 starting March 5 violates the dependency
    (deftask task2 "Task 2"
      :duration (duration 3 :days)
      :depends-on (task1)
      :start-constraint (:mso (date 2024 3 5))
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((conflicts (detect-constraint-conflicts *current-project*)))
      (is (not (null conflicts))))))

;;; ============================================================================
;;; Recurring Task Tests
;;; ============================================================================

(test recurring-task-daily
  "Test daily recurring task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask standup "Daily Standup"
      :duration (duration 15 :minutes)
      :recurring (:daily :start (date 2024 3 1) :end (date 2024 3 7))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((instances (expand-recurring-tasks *current-project*)))
      ;; Should have 7 instances (March 1-7)
      (is (= 7 (length (getf instances 'standup)))))))

(test recurring-task-weekly
  "Test weekly recurring task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask review "Weekly Review"
      :duration (duration 1 :hours)
      :recurring (:weekly :day :friday :start (date 2024 3 1) :end (date 2024 3 31))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((instances (expand-recurring-tasks *current-project*)))
      ;; Should have instances for each Friday in March 2024
      ;; March 1, 8, 15, 22, 29 = 5 Fridays
      (is (= 5 (length (getf instances 'review)))))))

(test recurring-task-workdays
  "Test workdays-only recurring task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask checkin "Daily Check-in"
      :duration (duration 30 :minutes)
      :recurring (:workdays :start (date 2024 3 4) :end (date 2024 3 10))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((instances (expand-recurring-tasks *current-project*)))
      ;; March 4-10: Mon-Fri (5 workdays), Sat-Sun skipped
      (is (= 5 (length (getf instances 'checkin)))))))

(test recurring-task-specific-days
  "Test recurring task on specific days of week"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask sync "Bi-weekly Sync"
      :duration (duration 1 :hours)
      :recurring (:days (:monday :wednesday) :start (date 2024 3 1) :end (date 2024 3 15))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((instances (expand-recurring-tasks *current-project*)))
      ;; March 1-15 has Mon: 4,11 and Wed: 6,13 = 4 instances
      (is (= 4 (length (getf instances 'sync)))))))

(test recurring-task-monthly
  "Test monthly recurring task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask report "Monthly Report"
      :duration (duration 4 :hours)
      :recurring (:monthly :day 1 :start (date 2024 1 1) :end (date 2024 6 30))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((instances (expand-recurring-tasks *current-project*)))
      ;; Should have 6 instances (Jan 1, Feb 1, Mar 1, Apr 1, May 1, Jun 1)
      (is (= 6 (length (getf instances 'report)))))))

;;; ============================================================================
;;; Recurring Task with Exceptions
;;; ============================================================================

(test recurring-task-with-exceptions
  "Test recurring task with exception dates"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask standup "Daily Standup"
      :duration (duration 15 :minutes)
      :recurring (:daily
                  :start (date 2024 3 1)
                  :end (date 2024 3 7)
                  :except ((date 2024 3 3) (date 2024 3 5)))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((instances (expand-recurring-tasks *current-project*)))
      ;; 7 days minus 2 exceptions = 5 instances
      (is (= 5 (length (getf instances 'standup)))))))

;;; ============================================================================
;;; Task Constraint Class Tests
;;; ============================================================================

(test task-constraint-accessors
  "Test task constraint accessors"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :start-constraint (:snet (date 2024 3 15))
      :finish-constraint (:fnlt (date 2024 3 25))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (is (not (null (task-start-constraint task))))
      (is (not (null (task-finish-constraint task))))
      (is (eq :snet (constraint-type (task-start-constraint task))))
      (is (eq :fnlt (constraint-type (task-finish-constraint task)))))))

;;; ============================================================================
;;; Recurring Task Definition Tests
;;; ============================================================================

(test recurring-task-definition
  "Test recurring task definition structure"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask standup "Daily Standup"
      :duration (duration 15 :minutes)
      :recurring (:daily :start (date 2024 3 1) :end (date 2024 3 7))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((task (gethash 'standup (project-tasks *current-project*))))
      (is (not (null (task-recurring task))))
      (is (eq :daily (recurring-frequency (task-recurring task)))))))

(test get-recurring-tasks
  "Test getting all recurring tasks from project"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Regular Task"
      :duration (duration 5 :days)
      :allocate (dev1))
    (deftask standup "Daily Standup"
      :duration (duration 15 :minutes)
      :recurring (:daily :start (date 2024 3 1) :end (date 2024 3 7))
      :allocate (dev1))
    (deftask review "Weekly Review"
      :duration (duration 1 :hours)
      :recurring (:weekly :day :friday :start (date 2024 3 1) :end (date 2024 3 31))
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((recurring (get-recurring-tasks *current-project*)))
      (is (= 2 (length recurring))))))

