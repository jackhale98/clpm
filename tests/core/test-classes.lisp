;;;; tests/core/test-classes.lisp
;;;; Tests for core CLOS classes

(in-package #:project-juggler-tests)

(in-suite classes-suite)

;;; Project tests

(test create-project
  "Can create a project"
  (let ((p (make-instance 'project
                          :id 'test
                          :name "Test Project"
                          :start (date 2024 1 1)
                          :end (date 2024 12 31))))
    (is (project-p p))
    (is (eq 'test (project-id p)))
    (is (string= "Test Project" (project-name p)))
    (is (date= (date 2024 1 1) (project-start p)))
    (is (date= (date 2024 12 31) (project-end p)))
    (is (hash-table-p (project-tasks p)))
    (is (hash-table-p (project-resources p)))))

(test project-default-scenario
  "Project has default scenario"
  (let ((p (make-instance 'project
                          :id 'test
                          :name "Test"
                          :start (date 2024 1 1)
                          :end (date 2024 12 31))))
    (is (eq :plan (project-current-scenario p)))))

;;; Task tests

(test create-task
  "Can create a task"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project)))
    (is (task-p task))
    (is (eq 't1 (task-id task)))
    (is (string= "Task 1" (task-name task)))
    (is (eq project (task-project task)))
    (is (null (task-parent task)))
    (is (null (task-subtasks task)))
    (is (null (task-dependencies task)))
    (is (null (task-allocations task)))))

(test task-default-values
  "Task has correct default values"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project)))
    (is (= 500 (task-priority task)))
    (is (= 0 (task-complete task)))
    (is (null (task-milestone-p task)))
    (is (null (task-scheduled-p task)))
    (is (= 0.0 (task-criticalness task)))
    (is (= 0.0 (task-path-criticalness task)))))

(test task-hierarchy
  "Tasks can have parent/child relationships"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (parent (make-instance 'task
                               :id 'parent
                               :name "Parent"
                               :project project))
         (child (make-instance 'task
                              :id 'child
                              :name "Child"
                              :project project
                              :parent parent)))
    (is (eq parent (task-parent child)))
    (push child (task-subtasks parent))
    (is (= 1 (length (task-subtasks parent))))
    (is (member child (task-subtasks parent)))))

(test task-with-effort
  "Task can have effort"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project
                             :effort (duration 40 :hours))))
    (is (duration-p (task-effort task)))
    (is (= 40 (duration-value (task-effort task))))
    (is (eq :hours (duration-unit (task-effort task))))))

(test task-with-duration
  "Task can have duration"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project
                             :duration (duration 5 :days))))
    (is (duration-p (task-duration task)))
    (is (= 5 (duration-value (task-duration task))))))

(test task-with-dates
  "Task can have start and end dates"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project
                             :start (date 2024 3 1)
                             :end (date 2024 3 5))))
    (is (date= (date 2024 3 1) (task-start task)))
    (is (date= (date 2024 3 5) (task-end task)))))

(test task-milestone
  "Task can be a milestone"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Milestone 1"
                             :project project
                             :milestone t)))
    (is (task-milestone-p task))))

;;; Resource tests

(test create-resource
  "Can create a resource"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (resource (make-instance 'resource
                                 :id 'dev1
                                 :name "Developer 1"
                                 :project project)))
    (is (resource-p resource))
    (is (eq 'dev1 (resource-id resource)))
    (is (string= "Developer 1" (resource-name resource)))
    (is (eq project (resource-project resource)))))

(test resource-default-values
  "Resource has correct default values"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (resource (make-instance 'resource
                                 :id 'dev1
                                 :name "Developer 1"
                                 :project project)))
    (is (= 1.0 (resource-efficiency resource)))
    (is (null (resource-rate resource)))
    (is (= 0.0 (resource-criticalness resource)))
    (is (= 0.0 (resource-allocated-effort resource)))
    (is (= 0.0 (resource-available-effort resource)))))

(test resource-with-efficiency
  "Resource can have custom efficiency"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (resource (make-instance 'resource
                                 :id 'dev1
                                 :name "Developer 1"
                                 :project project
                                 :efficiency 1.2)))
    (is (= 1.2 (resource-efficiency resource)))))

;;; Dependency tests

(test create-dependency
  "Can create a dependency"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task1 (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project project))
         (dep (make-instance 'dependency
                            :source task1
                            :target-ref 't2
                            :type :finish-start)))
    (is (dependency-p dep))
    (is (eq task1 (dependency-source dep)))
    (is (eq 't2 (dependency-target-ref dep)))
    (is (eq :finish-start (dependency-type dep)))
    (is (null (dependency-target dep)))))

(test dependency-default-type
  "Dependency has default finish-start type"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task1 (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project project))
         (dep (make-instance 'dependency
                            :source task1
                            :target-ref 't2)))
    (is (eq :finish-start (dependency-type dep)))))

(test dependency-with-gap
  "Dependency can have gap duration"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task1 (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project project))
         (dep (make-instance 'dependency
                            :source task1
                            :target-ref 't2
                            :gap (duration 2 :days))))
    (is (duration-p (dependency-gap dep)))
    (is (= 2 (duration-value (dependency-gap dep))))))

;;; Allocation tests

(test create-allocation
  "Can create an allocation"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project))
         (alloc (make-instance 'allocation
                              :task task
                              :resource-refs '(dev1 dev2))))
    (is (allocation-p alloc))
    (is (eq task (allocation-task alloc)))
    (is (equal '(dev1 dev2) (allocation-resource-refs alloc)))
    (is (null (allocation-resources alloc)))
    (is (null (allocation-mandatory-p alloc)))))

(test allocation-mandatory
  "Allocation can be mandatory"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project project))
         (alloc (make-instance 'allocation
                              :task task
                              :resource-refs '(dev1)
                              :mandatory t)))
    (is (allocation-mandatory-p alloc))))

;;; Integration tests

(test task-with-dependencies-and-allocations
  "Task can have both dependencies and allocations"
  (let* ((project (make-instance 'project
                                 :id 'test
                                 :name "Test"
                                 :start (date 2024 1 1)
                                 :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't2
                             :name "Task 2"
                             :project project))
         (dep (make-instance 'dependency
                            :source task
                            :target-ref 't1))
         (alloc (make-instance 'allocation
                              :task task
                              :resource-refs '(dev1))))
    (push dep (task-dependencies task))
    (push alloc (task-allocations task))
    (is (= 1 (length (task-dependencies task))))
    (is (= 1 (length (task-allocations task))))
    (is (eq dep (first (task-dependencies task))))
    (is (eq alloc (first (task-allocations task))))))
