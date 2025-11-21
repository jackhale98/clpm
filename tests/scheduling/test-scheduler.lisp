;;;; tests/scheduling/test-scheduler.lisp
;;;; Tests for TaskJuggler-style scheduling algorithm

(in-package #:project-juggler-tests)

(def-suite scheduling-suite
  :in project-juggler-suite
  :description "Scheduling algorithm tests")

(in-suite scheduling-suite)

;;; ============================================================================
;;; Resource Criticalness Tests
;;; ============================================================================

(test calculate-resource-criticalness-basic
  "Calculate basic resource criticalness"
  (with-test-project
    ;; Create resource with known available time
    (let ((dev (make-instance 'resource
                             :id 'dev1
                             :name "Developer 1"
                             :project *current-project*)))
      (register-resource dev)

      ;; Set allocated effort: 40 hours
      (setf (resource-allocated-effort dev) 40.0)
      ;; Set available effort: 80 hours
      (setf (resource-available-effort dev) 80.0)

      ;; Calculate criticalness
      (calculate-resource-criticalness *current-project*)

      ;; Criticalness should be 40/80 = 0.5
      (is (= 0.5 (resource-criticalness dev))))))

(test calculate-resource-criticalness-overallocated
  "Resource with more allocated than available has criticalness > 1.0"
  (with-test-project
    (let ((dev (make-instance 'resource
                             :id 'dev1
                             :name "Developer 1"
                             :project *current-project*)))
      (register-resource dev)

      ;; Allocated: 100 hours, Available: 80 hours
      (setf (resource-allocated-effort dev) 100.0)
      (setf (resource-available-effort dev) 80.0)

      (calculate-resource-criticalness *current-project*)

      ;; Criticalness = 100/80 = 1.25
      (is (= 1.25 (resource-criticalness dev))))))

(test calculate-resource-criticalness-zero-available
  "Resource with zero available effort has criticalness 0.0"
  (with-test-project
    (let ((dev (make-instance 'resource
                             :id 'dev1
                             :name "Developer 1"
                             :project *current-project*)))
      (register-resource dev)

      (setf (resource-allocated-effort dev) 50.0)
      (setf (resource-available-effort dev) 0.0)

      (calculate-resource-criticalness *current-project*)

      ;; Zero available = 0.0 criticalness (not infinity)
      (is (= 0.0 (resource-criticalness dev))))))

;;; ============================================================================
;;; Task Criticalness Tests
;;; ============================================================================

(test calculate-task-criticalness-milestone
  "Milestone criticalness based on priority"
  (with-test-project
    (let ((task (make-instance 'task
                              :id 't1
                              :name "Milestone 1"
                              :project *current-project*
                              :milestone t
                              :priority 750)))
      (register-task task)

      (calculate-task-criticalness *current-project*)

      ;; Milestone criticalness = priority / 500.0 = 750/500 = 1.5
      (is (= 1.5 (task-criticalness task))))))

(test calculate-task-criticalness-effort-task
  "Effort task criticalness based on effort × avg resource criticalness"
  (with-test-project
    ;; Create resource with criticalness
    (let ((dev (make-instance 'resource
                             :id 'dev1
                             :name "Developer 1"
                             :project *current-project*)))
      (register-resource dev)
      (setf (resource-criticalness dev) 0.8)

      ;; Create task with effort and allocation
      (let ((task (make-instance 'task
                                :id 't1
                                :name "Task 1"
                                :project *current-project*
                                :effort (duration 40 :hours))))
        (register-task task)

        ;; Create allocation
        (let ((alloc (make-instance 'allocation
                                   :task task
                                   :resources (list dev))))
          (setf (task-allocations task) (list alloc))

          (calculate-task-criticalness *current-project*)

          ;; Task criticalness = effort(hours) × avg(resource criticalness)
          ;; = 40 × 0.8 = 32.0
          (is (= 32.0 (task-criticalness task))))))))

;;; ============================================================================
;;; Path Criticalness Tests
;;; ============================================================================

(test calculate-path-criticalness-single-task
  "Path criticalness for single task equals task criticalness"
  (with-test-project
    (let ((task (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project *current-project*
                              :milestone t
                              :priority 500)))
      (register-task task)
      (setf (task-criticalness task) 1.0)

      (calculate-path-criticalness *current-project*)

      ;; Path criticalness = task criticalness = 1.0
      (is (= 1.0 (task-path-criticalness task))))))

(test calculate-path-criticalness-linear-chain
  "Path criticalness accumulates along dependency chain"
  (with-test-project
    ;; Create chain: t1 -> t2 -> t3
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1" :project *current-project*))
          (t2 (make-instance 'task :id 't2 :name "Task 2" :project *current-project*))
          (t3 (make-instance 'task :id 't3 :name "Task 3" :project *current-project*)))
      (register-task t1)
      (register-task t2)
      (register-task t3)

      ;; Set task criticalness
      (setf (task-criticalness t1) 10.0)
      (setf (task-criticalness t2) 20.0)
      (setf (task-criticalness t3) 30.0)

      ;; Create dependencies
      (let ((dep1 (make-instance 'dependency :source t2 :target-ref 't1 :target t1))
            (dep2 (make-instance 'dependency :source t3 :target-ref 't2 :target t2)))
        (setf (task-dependencies t2) (list dep1))
        (setf (task-dependencies t3) (list dep2)))

      (calculate-path-criticalness *current-project*)

      ;; t1 path = 10.0
      ;; t2 path = 20.0 + 10.0 = 30.0
      ;; t3 path = 30.0 + 20.0 + 10.0 = 60.0
      (is (= 10.0 (task-path-criticalness t1)))
      (is (= 30.0 (task-path-criticalness t2)))
      (is (= 60.0 (task-path-criticalness t3))))))

;;; ============================================================================
;;; Simple Scheduling Tests
;;; ============================================================================

(test schedule-single-task-with-duration
  "Schedule single task with fixed duration"
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

      ;; Task should be scheduled
      (is (task-scheduled-p task))
      ;; Start date should remain 2024-03-01
      (is (date= (date 2024 3 1) (task-start task)))
      ;; End date should be start + 5 days = 2024-03-06
      (is (date= (date 2024 3 6) (task-end task))))))

(test schedule-milestone
  "Schedule milestone task"
  (with-test-project
    (let ((task (make-instance 'task
                              :id 'm1
                              :name "Milestone 1"
                              :project *current-project*
                              :milestone t
                              :start (date 2024 3 1))))
      (register-task task)
      (finalize-project *current-project*)
      (schedule *current-project*)

      ;; Milestone should be scheduled
      (is (task-scheduled-p task))
      ;; Start and end should be the same
      (is (date= (date 2024 3 1) (task-start task)))
      (is (date= (date 2024 3 1) (task-end task))))))

(test schedule-dependent-tasks
  "Schedule tasks with dependencies"
  (with-test-project
    ;; Create two tasks: t2 depends on t1
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

      ;; Create dependency
      (let ((dep (make-instance 'dependency
                               :source t2
                               :target-ref 't1)))
        (setf (task-dependencies t2) (list dep)))

      (finalize-project *current-project*)
      (schedule *current-project*)

      ;; t1 should end on 2024-03-06
      (is (date= (date 2024 3 6) (task-end t1)))

      ;; t2 should start after t1 ends (2024-03-06) and last 3 days
      (is (date= (date 2024 3 6) (task-start t2)))
      (is (date= (date 2024 3 9) (task-end t2))))))

;;; ============================================================================
;;; Effort-Based Scheduling Tests
;;; ============================================================================

(test effort-with-single-resource
  "Schedule effort-based task with single resource"
  (with-test-project
    (defresource dev "Developer" :efficiency 1.0)

    (deftask task1 "Task 1"
      :start (date 2024 3 1)
      :effort (duration 10 :days)
      :allocate (dev))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Task should be scheduled
      (is (task-scheduled-p task))
      ;; Start date should be 2024-03-01
      (is (date= (date 2024 3 1) (task-start task)))
      ;; Duration = effort / efficiency = 10 / 1.0 = 10 days
      ;; End date should be 2024-03-11
      (is (date= (date 2024 3 11) (task-end task))))))

(test effort-with-high-efficiency-resource
  "Effort-based task with high efficiency resource completes faster"
  (with-test-project
    (defresource senior-dev "Senior Developer" :efficiency 2.0)

    (deftask task1 "Task 1"
      :start (date 2024 3 1)
      :effort (duration 10 :days)
      :allocate (senior-dev))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Duration = effort / efficiency = 10 / 2.0 = 5 days
      ;; End date should be 2024-03-06
      (is (date= (date 2024 3 6) (task-end task))))))

(test effort-with-low-efficiency-resource
  "Effort-based task with low efficiency resource takes longer"
  (with-test-project
    (defresource junior-dev "Junior Developer" :efficiency 0.5)

    (deftask task1 "Task 1"
      :start (date 2024 3 1)
      :effort (duration 10 :days)
      :allocate (junior-dev))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Duration = effort / efficiency = 10 / 0.5 = 20 days
      ;; End date should be 2024-03-21
      (is (date= (date 2024 3 21) (task-end task))))))

(test effort-with-multiple-resources
  "Effort-based task with multiple resources combines efficiency"
  (with-test-project
    (defresource dev1 "Developer 1" :efficiency 1.0)
    (defresource dev2 "Developer 2" :efficiency 1.5)

    (deftask task1 "Task 1"
      :start (date 2024 3 1)
      :effort (duration 20 :days)
      :allocate (dev1 dev2))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Total efficiency = 1.0 + 1.5 = 2.5
      ;; Duration = effort / total_efficiency = 20 / 2.5 = 8 days
      ;; End date should be 2024-03-09
      (is (date= (date 2024 3 9) (task-end task))))))

(test effort-without-resources-uses-effort-as-duration
  "Effort-based task without resources treats effort as duration"
  (with-test-project
    (deftask task1 "Task 1"
      :start (date 2024 3 1)
      :effort (duration 10 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Should treat effort as duration when no resources allocated
      ;; End date should be 2024-03-11
      (is (date= (date 2024 3 11) (task-end task))))))

(test calculate-duration-from-effort-function
  "Test the calculate-duration-from-effort helper function"
  (with-test-project
    (defresource dev "Developer" :efficiency 1.5)

    (deftask task1 "Task 1"
      :effort (duration 15 :days)
      :allocate (dev))

    (finalize-project *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (let ((calculated-duration (calculate-duration-from-effort task)))
        ;; Duration = 15 / 1.5 = 10 days
        (is (= 10 (duration-in-days calculated-duration)))))))
