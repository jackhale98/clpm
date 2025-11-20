;;;; tests/scheduling/test-critical-path.lisp
;;;; Tests for CPM Critical Path Method (slack-based)

(in-package #:project-juggler-tests)

(in-suite scheduling-suite)

;;; ============================================================================
;;; Forward Pass Tests (Early Start / Early Finish)
;;; ============================================================================

(test forward-pass-single-task
  "Forward pass calculates ES and EF for single task"
  (with-test-project
    (let ((task (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project *current-project*
                              :start (date 2024 3 1)
                              :duration (duration 5 :days))))
      (register-task task)
      (finalize-project *current-project*)
      (schedule *current-project*)
      
      ;; Run forward pass
      (forward-pass *current-project*)
      
      ;; ES should be start date (2024-03-01)
      (is (date= (date 2024 3 1) (task-early-start task)))
      ;; EF should be ES + duration (2024-03-06)
      (is (date= (date 2024 3 6) (task-early-finish task))))))

(test forward-pass-dependent-tasks
  "Forward pass calculates ES/EF for dependent tasks"
  (with-test-project
    ;; t2 depends on t1
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :duration (duration 3 :days))))
      (register-task t1)
      (register-task t2)
      
      (let ((dep (make-instance 'dependency :source t2 :target-ref 't1)))
        (setf (task-dependencies t2) (list dep)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      (forward-pass *current-project*)
      
      ;; t1: ES=2024-03-01, EF=2024-03-06
      (is (date= (date 2024 3 1) (task-early-start t1)))
      (is (date= (date 2024 3 6) (task-early-finish t1)))
      
      ;; t2: ES=2024-03-06 (after t1), EF=2024-03-09
      (is (date= (date 2024 3 6) (task-early-start t2)))
      (is (date= (date 2024 3 9) (task-early-finish t2))))))

(test forward-pass-multiple-predecessors
  "Forward pass handles task with multiple predecessors"
  (with-test-project
    ;; t3 depends on both t1 and t2
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 7 :days)))
          (t3 (make-instance 'task
                            :id 't3
                            :name "Task 3"
                            :project *current-project*
                            :duration (duration 2 :days))))
      (register-task t1)
      (register-task t2)
      (register-task t3)
      
      (let ((dep1 (make-instance 'dependency :source t3 :target-ref 't1))
            (dep2 (make-instance 'dependency :source t3 :target-ref 't2)))
        (setf (task-dependencies t3) (list dep1 dep2)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      (forward-pass *current-project*)
      
      ;; t3 starts after the LATEST predecessor finishes
      ;; t1 finishes 2024-03-06, t2 finishes 2024-03-08
      ;; So t3 ES = 2024-03-08 (max of predecessors)
      (is (date= (date 2024 3 8) (task-early-start t3)))
      (is (date= (date 2024 3 10) (task-early-finish t3))))))

;;; ============================================================================
;;; Backward Pass Tests (Late Start / Late Finish)
;;; ============================================================================

(test backward-pass-single-task
  "Backward pass calculates LS and LF for single task"
  (with-test-project
    (let ((task (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project *current-project*
                              :start (date 2024 3 1)
                              :duration (duration 5 :days))))
      (register-task task)
      (finalize-project *current-project*)
      (schedule *current-project*)
      (forward-pass *current-project*)
      (backward-pass *current-project*)
      
      ;; For single task with no successors, LS = ES, LF = EF (zero slack)
      (is (date= (task-early-start task) (task-late-start task)))
      (is (date= (task-early-finish task) (task-late-finish task))))))

(test backward-pass-dependent-tasks
  "Backward pass calculates LS/LF for dependent tasks"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :duration (duration 3 :days))))
      (register-task t1)
      (register-task t2)
      
      (let ((dep (make-instance 'dependency :source t2 :target-ref 't1)))
        (setf (task-dependencies t2) (list dep)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      (forward-pass *current-project*)
      (backward-pass *current-project*)
      
      ;; Both tasks on critical path, so LS = ES, LF = EF
      (is (date= (task-early-start t1) (task-late-start t1)))
      (is (date= (task-early-finish t1) (task-late-finish t1)))
      (is (date= (task-early-start t2) (task-late-start t2)))
      (is (date= (task-early-finish t2) (task-late-finish t2))))))

;;; ============================================================================
;;; Slack Calculation Tests
;;; ============================================================================

(test calculate-slack-zero-slack
  "Tasks on critical path have zero slack"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :duration (duration 3 :days))))
      (register-task t1)
      (register-task t2)
      
      (let ((dep (make-instance 'dependency :source t2 :target-ref 't1)))
        (setf (task-dependencies t2) (list dep)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      (forward-pass *current-project*)
      (backward-pass *current-project*)
      (calculate-slack *current-project*)
      
      ;; Both tasks on critical path = zero slack
      (is (= 0 (task-slack t1)))
      (is (= 0 (task-slack t2))))))

(test calculate-slack-with-float
  "Non-critical tasks have positive slack"
  (with-test-project
    ;; Create scenario where one path is shorter
    ;; t1 -> t3 (5+2=7 days)
    ;; t2 -> t3 (3+2=5 days, so t2 has slack)
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 3 :days)))
          (t3 (make-instance 'task
                            :id 't3
                            :name "Task 3"
                            :project *current-project*
                            :duration (duration 2 :days))))
      (register-task t1)
      (register-task t2)
      (register-task t3)
      
      (let ((dep1 (make-instance 'dependency :source t3 :target-ref 't1))
            (dep2 (make-instance 'dependency :source t3 :target-ref 't2)))
        (setf (task-dependencies t3) (list dep1 dep2)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      (forward-pass *current-project*)
      (backward-pass *current-project*)
      (calculate-slack *current-project*)
      
      ;; t1 and t3 on critical path (zero slack)
      (is (= 0 (task-slack t1)))
      (is (= 0 (task-slack t3)))
      
      ;; t2 has slack (can start 2 days later)
      (is (> (task-slack t2) 0)))))

;;; ============================================================================
;;; Critical Path Identification Tests
;;; ============================================================================

(test identify-critical-path-simple
  "Critical path identifies tasks with zero slack"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :duration (duration 3 :days))))
      (register-task t1)
      (register-task t2)
      
      (let ((dep (make-instance 'dependency :source t2 :target-ref 't1)))
        (setf (task-dependencies t2) (list dep)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      
      ;; Calculate critical path
      (let ((critical-tasks (critical-path *current-project*)))
        
        ;; Both tasks should be on critical path
        (is (= 2 (length critical-tasks)))
        (is (member t1 critical-tasks))
        (is (member t2 critical-tasks))))))

(test identify-critical-path-with-slack
  "Critical path excludes tasks with slack"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task
                            :id 't2
                            :name "Task 2"
                            :project *current-project*
                            :start (date 2024 3 1)
                            :duration (duration 3 :days)))
          (t3 (make-instance 'task
                            :id 't3
                            :name "Task 3"
                            :project *current-project*
                            :duration (duration 2 :days))))
      (register-task t1)
      (register-task t2)
      (register-task t3)
      
      (let ((dep1 (make-instance 'dependency :source t3 :target-ref 't1))
            (dep2 (make-instance 'dependency :source t3 :target-ref 't2)))
        (setf (task-dependencies t3) (list dep1 dep2)))
      
      (finalize-project *current-project*)
      (schedule *current-project*)
      
      (let ((critical-tasks (critical-path *current-project*)))
        
        ;; Only t1 and t3 on critical path (t2 has slack)
        (is (= 2 (length critical-tasks)))
        (is (member t1 critical-tasks))
        (is (member t3 critical-tasks))
        (is (not (member t2 critical-tasks)))))))
