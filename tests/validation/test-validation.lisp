;;;; tests/validation/test-validation.lisp
;;;; Tests for project validation and finalization

(in-package #:project-juggler-tests)

(def-suite validation-suite
  :in project-juggler-suite
  :description "Project validation and finalization tests")

(in-suite validation-suite)

;;; ============================================================================
;;; Reference Resolution Tests
;;; ============================================================================

(test resolve-simple-task-reference
  "Resolve task reference in same namespace"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*)))
      (register-task t1)

      (let ((t2 (make-instance 'task
                              :id 't2
                              :name "Task 2"
                              :project *current-project*)))
        (register-task t2)

        ;; Create dependency with unresolved reference
        (let ((dep (make-instance 'dependency
                                 :source t2
                                 :target-ref 't1)))
          (push dep (task-dependencies t2))

          ;; Finalize should resolve references
          (finalize-project *current-project*)

          ;; Check that dependency target was resolved
          (is (eq t1 (dependency-target dep))))))))

(test resolve-qualified-task-reference
  "Resolve namespace-qualified task reference"
  (with-test-project
    ;; Create two namespaces
    (let ((ns1 (make-instance 'namespace :name 'ns1))
          (ns2 (make-instance 'namespace :name 'ns2)))
      (setf (gethash 'ns1 *namespace-registry*) ns1)
      (setf (gethash 'ns2 *namespace-registry*) ns2)

      ;; Create task in ns1
      (let ((*current-namespace* ns1))
        (let ((t1 (make-instance 'task
                                :id 't1
                                :name "Task 1"
                                :project *current-project*)))
          (register-task t1)))

      ;; Create task in ns2 that depends on ns1:t1
      (let ((*current-namespace* ns2))
        (let ((t2 (make-instance 'task
                                :id 't2
                                :name "Task 2"
                                :project *current-project*)))
          (register-task t2)

          ;; Create dependency with qualified reference
          (let ((dep (make-instance 'dependency
                                   :source t2
                                   :target-ref (intern "NS1:T1"))))
            (push dep (task-dependencies t2))

            ;; Finalize should resolve qualified reference
            (finalize-project *current-project*)

            ;; Check that dependency target was resolved
            (is (not (null (dependency-target dep))))
            (is (eq 't1 (task-id (dependency-target dep))))))))))

(test resolve-resource-reference
  "Resolve resource allocation references"
  (with-test-project
    (let ((r1 (make-instance 'resource
                            :id 'dev1
                            :name "Developer 1"
                            :project *current-project*)))
      (register-resource r1)

      (let ((t1 (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project *current-project*)))
        (register-task t1)

        ;; Create allocation with unresolved reference
        (let ((alloc (make-instance 'allocation
                                   :task t1
                                   :resource-refs '(dev1))))
          (push alloc (task-allocations t1))

          ;; Finalize should resolve resource references
          (finalize-project *current-project*)

          ;; Check that allocation resources were resolved
          (is (not (null (allocation-resources alloc))))
          (is (= 1 (length (allocation-resources alloc))))
          (is (eq r1 (first (allocation-resources alloc)))))))))

(test unresolved-task-reference-error
  "Error on unresolved task reference"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*)))
      (register-task t1)

      ;; Create dependency to nonexistent task
      (let ((dep (make-instance 'dependency
                               :source t1
                               :target-ref 'nonexistent)))
        (push dep (task-dependencies t1))

        ;; Should signal reference-error
        (signals reference-error
          (finalize-project *current-project*))))))

(test unresolved-resource-reference-error
  "Error on unresolved resource reference"
  (with-test-project
    (let ((t1 (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project *current-project*)))
      (register-task t1)

      ;; Create allocation with nonexistent resource
      (let ((alloc (make-instance 'allocation
                                 :task t1
                                 :resource-refs '(nonexistent))))
        (push alloc (task-allocations t1))

        ;; Should signal reference-error
        (signals reference-error
          (finalize-project *current-project*))))))

;;; ============================================================================
;;; Circular Dependency Detection Tests
;;; ============================================================================

(test detect-simple-circular-dependency
  "Detect simple circular dependency (A → B → A)"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*))
          (t2 (make-instance 'task :id 't2 :name "Task 2"
                            :project *current-project*)))
      (register-task t1)
      (register-task t2)

      ;; t1 depends on t2, t2 depends on t1
      (let ((dep1 (make-instance 'dependency :source t1 :target-ref 't2))
            (dep2 (make-instance 'dependency :source t2 :target-ref 't1)))
        (push dep1 (task-dependencies t1))
        (push dep2 (task-dependencies t2))

        ;; Should signal circular-dependency-error
        (signals circular-dependency-error
          (finalize-project *current-project*))))))

(test detect-complex-circular-dependency
  "Detect complex circular dependency (A → B → C → A)"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*))
          (t2 (make-instance 'task :id 't2 :name "Task 2"
                            :project *current-project*))
          (t3 (make-instance 'task :id 't3 :name "Task 3"
                            :project *current-project*)))
      (register-task t1)
      (register-task t2)
      (register-task t3)

      ;; t1 → t2 → t3 → t1 (cycle)
      (let ((dep1 (make-instance 'dependency :source t1 :target-ref 't2))
            (dep2 (make-instance 'dependency :source t2 :target-ref 't3))
            (dep3 (make-instance 'dependency :source t3 :target-ref 't1)))
        (push dep1 (task-dependencies t1))
        (push dep2 (task-dependencies t2))
        (push dep3 (task-dependencies t3))

        ;; Should signal circular-dependency-error
        (signals circular-dependency-error
          (finalize-project *current-project*))))))

(test no-circular-dependency-in-valid-graph
  "No false positives for valid dependency graph"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*))
          (t2 (make-instance 'task :id 't2 :name "Task 2"
                            :project *current-project*))
          (t3 (make-instance 'task :id 't3 :name "Task 3"
                            :project *current-project*)))
      (register-task t1)
      (register-task t2)
      (register-task t3)

      ;; Valid DAG: t1 → t2, t1 → t3, t2 → t3
      (let ((dep1 (make-instance 'dependency :source t2 :target-ref 't1))
            (dep2 (make-instance 'dependency :source t3 :target-ref 't1))
            (dep3 (make-instance 'dependency :source t3 :target-ref 't2)))
        (push dep1 (task-dependencies t2))
        (push dep2 (task-dependencies t3))
        (push dep3 (task-dependencies t3))

        ;; Should NOT signal error
        (finishes
          (finalize-project *current-project*))))))

(test detect-self-dependency
  "Detect task depending on itself"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*)))
      (register-task t1)

      ;; t1 depends on t1 (self-loop)
      (let ((dep (make-instance 'dependency :source t1 :target-ref 't1)))
        (push dep (task-dependencies t1))

        ;; Should signal circular-dependency-error
        (signals circular-dependency-error
          (finalize-project *current-project*))))))

;;; ============================================================================
;;; Constraint Validation Tests
;;; ============================================================================

(test validate-task-has-effort-or-duration
  "Task must have either effort or duration (not both)"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*
                            :effort (duration 40 :hours)
                            :duration (duration 5 :days))))
      (register-task t1)

      ;; Task with both effort and duration should fail validation
      (signals validation-error
        (finalize-project *current-project*)))))

(test validate-milestone-has-no-effort
  "Milestone cannot have effort"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Milestone 1"
                            :project *current-project*
                            :milestone t
                            :effort (duration 10 :hours))))
      (register-task t1)

      ;; Milestone with effort should fail validation
      (signals validation-error
        (finalize-project *current-project*)))))

(test validate-effort-task-has-allocations
  "Task with effort should have resource allocations"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*
                            :effort (duration 40 :hours))))
      (register-task t1)

      ;; Task with effort but no allocations should pass with warning
      ;; (not an error, just potentially problematic)
      (finishes
        (finalize-project *current-project*)))))

(test validate-project-date-range
  "Task dates should be within project date range"
  (with-test-project
    (let ((t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*
                            :start (date 2023 1 1)))) ; Before project start
      (register-task t1)

      ;; Task outside project range should fail validation
      (signals validation-error
        (finalize-project *current-project*)))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test finalize-project-complete
  "Complete finalization of valid project"
  (with-test-project
    (let ((r1 (make-instance 'resource :id 'dev :name "Developer"
                            :project *current-project*))
          (t1 (make-instance 'task :id 't1 :name "Task 1"
                            :project *current-project*
                            :duration (duration 5 :days)))
          (t2 (make-instance 'task :id 't2 :name "Task 2"
                            :project *current-project*
                            :effort (duration 40 :hours))))
      (register-resource r1)
      (register-task t1)
      (register-task t2)

      ;; Add dependency and allocation
      (let ((dep (make-instance 'dependency :source t2 :target-ref 't1))
            (alloc (make-instance 'allocation :task t2 :resource-refs '(dev))))
        (push dep (task-dependencies t2))
        (push alloc (task-allocations t2))

        ;; Should finalize successfully
        (finishes
          (finalize-project *current-project*))

        ;; Verify references resolved
        (is (eq t1 (dependency-target dep)))
        (is (eq r1 (first (allocation-resources alloc))))))))

(test finalize-project-with-namespaces
  "Finalize project with multiple namespaces"
  (with-test-project
    ;; Create namespaces
    (let ((ns1 (make-instance 'namespace :name 'infra))
          (ns2 (make-instance 'namespace :name 'app)))
      (setf (gethash 'infra *namespace-registry*) ns1)
      (setf (gethash 'app *namespace-registry*) ns2)

      ;; Create tasks in different namespaces
      (let ((*current-namespace* ns1))
        (let ((t1 (make-instance 'task :id 'servers :name "Servers"
                                :project *current-project*
                                :duration (duration 3 :days))))
          (register-task t1)))

      (let ((*current-namespace* ns2))
        (let ((t2 (make-instance 'task :id 'deployment :name "Deployment"
                                :project *current-project*
                                :duration (duration 2 :days))))
          (register-task t2)

          ;; App deployment depends on infra:servers
          (let ((dep (make-instance 'dependency :source t2
                                   :target-ref (intern "INFRA:SERVERS"))))
            (push dep (task-dependencies t2))

            ;; Should finalize successfully
            (finishes
              (finalize-project *current-project*))

            ;; Verify cross-namespace reference resolved
            (is (not (null (dependency-target dep))))
            (is (eq 'servers (task-id (dependency-target dep))))))))))
