;;;; tests/core/test-summary-tasks.lisp
;;;; Tests for summary task (phase) aggregation

(in-package #:claps-tests)

(def-suite summary-tasks-suite
  :in project-juggler-suite
  :description "Tests for summary task aggregation")

(in-suite summary-tasks-suite)

;;; ============================================================================
;;; Summary Task Detection Tests
;;; ============================================================================

(test summary-task-detection
  "Test identifying summary vs leaf tasks"
  (with-test-project
    ;; Create parent with subtasks
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1"
               :duration (duration 5 :days))
             (deftask t2 "Task 2"
               :duration (duration 5 :days))))

    (let ((phase (gethash 'phase1 (project-tasks *current-project*)))
          (t1 (gethash 't1 (project-tasks *current-project*)))
          (t2 (gethash 't2 (project-tasks *current-project*))))
      (is (summary-task-p phase))
      (is (leaf-task-p t1))
      (is (leaf-task-p t2))
      (is (not (summary-task-p t1))))))

;;; ============================================================================
;;; Summary Task Aggregation Tests
;;; ============================================================================

(test aggregate-summary-duration
  "Test that summary task duration is calculated from subtasks"
  (with-test-project
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1"
               :start (date 2024 3 1)
               :duration (duration 5 :days))
             (deftask t2 "Task 2"
               :start (date 2024 3 8)
               :duration (duration 3 :days))))

    (finalize-project *current-project*)
    (schedule *current-project*)
    (aggregate-all-summary-tasks *current-project*)

    (let ((phase (gethash 'phase1 (project-tasks *current-project*))))
      ;; Phase should span from Mar 1 to Mar 10 (8 days later)
      (is (not (null (task-start phase))))
      (is (not (null (task-end phase)))))))

(test aggregate-summary-effort
  "Test that summary task effort is sum of subtask efforts"
  (with-test-project
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1"
               :effort (duration 8 :days))
             (deftask t2 "Task 2"
               :effort (duration 12 :days))))

    (finalize-project *current-project*)
    (aggregate-all-summary-tasks *current-project*)

    (let ((phase (gethash 'phase1 (project-tasks *current-project*))))
      (is (not (null (task-effort phase))))
      (is (= 20 (duration-in-days (task-effort phase)))))))

(test aggregate-summary-completion
  "Test that summary task completion is weighted average"
  (with-test-project
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1"
               :effort (duration 10 :days)
               :complete 100)
             (deftask t2 "Task 2"
               :effort (duration 10 :days)
               :complete 0)))

    (finalize-project *current-project*)
    (aggregate-all-summary-tasks *current-project*)

    (let ((phase (gethash 'phase1 (project-tasks *current-project*))))
      ;; Weighted average: (10*100 + 10*0) / 20 = 50
      (is (= 50 (task-complete phase))))))

;;; ============================================================================
;;; Nested Summary Task Tests
;;; ============================================================================

(test nested-summary-tasks
  "Test aggregation of nested summary tasks"
  (with-test-project
    (eval '(deftask project "Project"
             (deftask phase1 "Phase 1"
               (deftask t1 "Task 1"
                 :effort (duration 5 :days))
               (deftask t2 "Task 2"
                 :effort (duration 5 :days)))
             (deftask phase2 "Phase 2"
               (deftask t3 "Task 3"
                 :effort (duration 10 :days)))))

    (finalize-project *current-project*)
    (aggregate-all-summary-tasks *current-project*)

    (let ((project-task (gethash 'project (project-tasks *current-project*)))
          (phase1 (gethash 'phase1 (project-tasks *current-project*))))
      ;; Phase 1 effort: 5 + 5 = 10
      (is (= 10 (duration-in-days (task-effort phase1))))
      ;; Project effort: 10 + 10 = 20
      (is (= 20 (duration-in-days (task-effort project-task)))))))

;;; ============================================================================
;;; Helper Function Tests
;;; ============================================================================

(test get-all-subtasks
  "Test getting all subtasks recursively"
  (with-test-project
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1")
             (deftask sub-phase "Sub Phase"
               (deftask t2 "Task 2")
               (deftask t3 "Task 3"))))

    (let* ((phase (gethash 'phase1 (project-tasks *current-project*)))
           (all-subtasks (get-all-subtasks phase)))
      ;; Should include t1, sub-phase, t2, t3 = 4 tasks
      (is (= 4 (length all-subtasks))))))

(test get-leaf-tasks
  "Test getting only leaf tasks"
  (with-test-project
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1")
             (deftask sub-phase "Sub Phase"
               (deftask t2 "Task 2")
               (deftask t3 "Task 3"))))

    (let* ((phase (gethash 'phase1 (project-tasks *current-project*)))
           (leaf-tasks (get-leaf-tasks phase)))
      ;; Only t1, t2, t3 are leaves = 3 tasks
      (is (= 3 (length leaf-tasks))))))

(test summary-task-progress
  "Test progress tracking for summary task"
  (with-test-project
    (eval '(deftask phase1 "Phase 1"
             (deftask t1 "Task 1"
               :effort (duration 5 :days)
               :complete 100)
             (deftask t2 "Task 2"
               :effort (duration 5 :days)
               :complete 50)
             (deftask t3 "Task 3"
               :effort (duration 5 :days)
               :complete 0)))

    (finalize-project *current-project*)
    (aggregate-all-summary-tasks *current-project*)

    (let* ((phase (gethash 'phase1 (project-tasks *current-project*)))
           (progress (summary-task-progress phase)))
      (is (= 3 (getf progress :leaf-tasks)))
      (is (= 1 (getf progress :completed)))
      (is (= 1 (getf progress :in-progress)))
      (is (= 1 (getf progress :not-started))))))
