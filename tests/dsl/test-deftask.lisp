;;;; tests/dsl/test-deftask.lisp
;;;; Tests for deftask macro

(in-package #:project-juggler-tests)

(in-suite dsl-suite)

;;; Basic deftask tests

(test deftask-basic
  "Can define a basic task"
  (with-test-project
    (eval '(deftask task1 "Task 1"))

    (let ((task (resolve-task-reference 'task1)))
      (is (not (null task)))
      (is (task-p task))
      (is (eq 'task1 (task-id task)))
      (is (string= "Task 1" (task-name task)))
      (is (eq *current-project* (task-project task))))))

(test deftask-with-effort
  "Can define task with effort"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :effort (duration 40 :hours)))

    (let ((task (resolve-task-reference 'task1)))
      (is (duration-p (task-effort task)))
      (is (= 40 (duration-value (task-effort task))))
      (is (eq :hours (duration-unit (task-effort task)))))))

(test deftask-with-duration
  "Can define task with duration"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :duration (duration 5 :days)))

    (let ((task (resolve-task-reference 'task1)))
      (is (duration-p (task-duration task)))
      (is (= 5 (duration-value (task-duration task)))))))

(test deftask-with-dates
  "Can define task with start and end dates"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :start (date 2024 3 1)
             :end (date 2024 3 5)))

    (let ((task (resolve-task-reference 'task1)))
      (is (date= (date 2024 3 1) (task-start task)))
      (is (date= (date 2024 3 5) (task-end task))))))

(test deftask-with-priority
  "Can define task with priority"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :priority 700))

    (let ((task (resolve-task-reference 'task1)))
      (is (= 700 (task-priority task))))))

(test deftask-milestone
  "Can define milestone task"
  (with-test-project
    (eval '(deftask milestone1 "Milestone 1"
             :milestone t))

    (let ((task (resolve-task-reference 'milestone1)))
      (is (task-milestone-p task)))))

(test deftask-registers-in-namespace
  "deftask registers task in current namespace"
  (with-test-project
    (eval '(deftask task1 "Task 1"))

    (is (not (null (gethash 'task1 (namespace-tasks *current-namespace*)))))))

(test deftask-registers-in-project
  "deftask registers task in current project"
  (with-test-project
    (eval '(deftask task1 "Task 1"))

    (is (not (null (gethash 'task1 (project-tasks *current-project*)))))))

;;; deftask with dependencies

(test deftask-with-simple-dependency
  "Can define task with simple dependency"
  (with-test-project
    (eval '(deftask task1 "Task 1"))
    (eval '(deftask task2 "Task 2"
             :depends-on (task1)))

    (let ((task2 (resolve-task-reference 'task2)))
      (is (= 1 (length (task-dependencies task2))))
      (let ((dep (first (task-dependencies task2))))
        (is (dependency-p dep))
        (is (eq task2 (dependency-source dep)))
        (is (eq 'task1 (dependency-target-ref dep)))))))

(test deftask-with-multiple-dependencies
  "Can define task with multiple dependencies"
  (with-test-project
    (eval '(deftask task1 "Task 1"))
    (eval '(deftask task2 "Task 2"))
    (eval '(deftask task3 "Task 3"
             :depends-on (task1 task2)))

    (let ((task3 (resolve-task-reference 'task3)))
      (is (= 2 (length (task-dependencies task3)))))))

(test deftask-with-qualified-dependency
  "Can define task with qualified dependency"
  (with-test-project
    (let ((ns-infra (make-instance 'namespace :name 'infra)))
      (setf (gethash 'infra *namespace-registry*) ns-infra)

      ;; Create task in infra namespace
      (let ((*current-namespace* ns-infra))
        (eval '(deftask servers "Setup Servers")))

      ;; Create task in default namespace that depends on infra task
      (eval '(deftask deploy "Deploy App"
               :depends-on ((intern "INFRA:SERVERS"))))

      (let ((task (resolve-task-reference 'deploy)))
        (is (= 1 (length (task-dependencies task))))))))

;;; deftask with allocations

(test deftask-with-simple-allocation
  "Can define task with resource allocation"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1"
             :allocate (dev1)))

    (let ((task (resolve-task-reference 'task1)))
      (is (= 1 (length (task-allocations task))))
      (let ((alloc (first (task-allocations task))))
        (is (allocation-p alloc))
        (is (eq task (allocation-task alloc)))
        (is (equal '(dev1) (allocation-resource-refs alloc)))))))

(test deftask-with-multiple-allocations
  "Can define task with multiple resource allocations"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(defresource dev2 "Developer 2"))
    (eval '(deftask task1 "Task 1"
             :allocate (dev1 dev2)))

    (let ((task (resolve-task-reference 'task1)))
      (is (= 1 (length (task-allocations task))))
      (let ((alloc (first (task-allocations task))))
        (is (= 2 (length (allocation-resource-refs alloc))))))))

;;; deftask with complete percentage

(test deftask-with-complete
  "Can define task with completion percentage"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :duration (duration 10 :days)
             :complete 75))

    (let ((task (resolve-task-reference 'task1)))
      (is (= 75 (task-complete task))))))

(test deftask-with-complete-zero
  "Can define task with zero completion"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :duration (duration 5 :days)
             :complete 0))

    (let ((task (resolve-task-reference 'task1)))
      (is (= 0 (task-complete task))))))

(test deftask-with-complete-100
  "Can define task with 100% completion"
  (with-test-project
    (eval '(deftask task1 "Task 1"
             :duration (duration 5 :days)
             :complete 100))

    (let ((task (resolve-task-reference 'task1)))
      (is (= 100 (task-complete task))))))

;;; deftask hierarchy

(test deftask-with-subtasks
  "Can define task with subtasks using body"
  (with-test-project
    (eval '(deftask parent "Parent Task"
             (deftask child1 "Child 1")
             (deftask child2 "Child 2")))

    (let ((parent (resolve-task-reference 'parent)))
      (is (= 2 (length (task-subtasks parent))))
      (let ((child1 (resolve-task-reference 'child1))
            (child2 (resolve-task-reference 'child2)))
        (is (eq parent (task-parent child1)))
        (is (eq parent (task-parent child2)))))))

;;; Parent task duration calculation

(test parent-task-duration-from-subtasks
  "Parent task should allow no duration when it has subtasks"
  (with-test-project
    ;; Parent task with no duration - should be allowed
    (eval '(deftask parent "Parent Task"
             :priority 1000
             (deftask child1 "Child 1"
               :duration (duration 5 :days))
             (deftask child2 "Child 2"
               :duration (duration 3 :days)
               :depends-on (child1))))

    (let ((parent (resolve-task-reference 'parent)))
      ;; Parent should exist and have subtasks
      (is (not (null parent)))
      (is (= 2 (length (task-subtasks parent))))
      ;; Parent should not have its own duration/effort
      (is (null (task-duration parent)))
      (is (null (task-effort parent))))))

(test parent-task-scheduling
  "Parent task dates should be calculated from subtask span after scheduling"
  (with-test-project
    (eval '(deftask parent "Parent Task"
             (deftask child1 "Child 1"
               :start (date 2024 3 1)
               :duration (duration 5 :days))
             (deftask child2 "Child 2"
               :duration (duration 3 :days)
               :depends-on (child1))))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((parent (resolve-task-reference 'parent))
          (child1 (resolve-task-reference 'child1))
          (child2 (resolve-task-reference 'child2)))
      ;; Parent start should be earliest child start
      (is (date= (task-start child1) (task-start parent)))
      ;; Parent end should be latest child end
      (is (date= (task-end child2) (task-end parent))))))
