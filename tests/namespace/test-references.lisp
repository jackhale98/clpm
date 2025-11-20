;;;; tests/namespace/test-references.lisp
;;;; Tests for reference resolution

(in-package #:project-juggler-tests)

(in-suite namespace-suite)

;;; Task reference resolution

(test resolve-simple-task-reference
  "Can resolve simple (unqualified) task reference"
  (with-test-project
    (let* ((*current-namespace* (make-instance 'namespace :name nil))
           (task (make-instance 'task
                               :id 'task1
                               :name "Task 1"
                               :project *current-project*)))
      (setf (gethash nil *namespace-registry*) *current-namespace*)
      (register-task task)

      ;; Resolve simple reference
      (let ((resolved (resolve-task-reference 'task1)))
        (is (eq task resolved))))))

(test resolve-qualified-task-reference
  "Can resolve qualified task reference from different namespace"
  (with-test-project
    (let ((ns-main (make-instance 'namespace :name nil))
          (ns-infra (make-instance 'namespace :name 'infra)))
      (setf (gethash nil *namespace-registry*) ns-main)
      (setf (gethash 'infra *namespace-registry*) ns-infra)

      ;; Create task in infra namespace
      (let* ((*current-namespace* ns-infra)
             (task (make-instance 'task
                                 :id 'servers
                                 :name "Setup Servers"
                                 :project *current-project*)))
        (register-task task)

        ;; Switch to main namespace and resolve qualified reference
        (setf *current-namespace* ns-main)
        (let* ((qualified-ref (intern "INFRA:SERVERS"))
               (resolved (resolve-task-reference qualified-ref)))
          (is (eq task resolved)))))))

(test resolve-task-reference-current-namespace-priority
  "Task resolution prioritizes current namespace"
  (with-test-project
    (let ((ns-main (make-instance 'namespace :name nil))
          (ns-other (make-instance 'namespace :name 'other)))
      (setf (gethash nil *namespace-registry*) ns-main)
      (setf (gethash 'other *namespace-registry*) ns-other)

      ;; Create same ID in both namespaces
      (let* ((*current-namespace* ns-main)
             (task-main (make-instance 'task
                                      :id 'task1
                                      :name "Main Task 1"
                                      :project *current-project*)))
        (register-task task-main)

        (setf *current-namespace* ns-other)
        (let ((task-other (make-instance 'task
                                        :id 'task1
                                        :name "Other Task 1"
                                        :project *current-project*)))
          (register-task task-other)

          ;; Resolving 'task1 from other namespace should get other's task
          (let ((resolved (resolve-task-reference 'task1)))
            (is (eq task-other resolved))
            (is (not (eq task-main resolved)))))))))

(test resolve-task-reference-not-found
  "Task resolution signals error when not found"
  (with-test-project
    (let ((*current-namespace* (make-instance 'namespace :name nil)))
      (setf (gethash nil *namespace-registry*) *current-namespace*)

      ;; Should signal reference-error
      (signals reference-error
        (resolve-task-reference 'nonexistent)))))

;;; Resource reference resolution

(test resolve-simple-resource-reference
  "Can resolve simple (unqualified) resource reference"
  (with-test-project
    (let* ((*current-namespace* (make-instance 'namespace :name nil))
           (resource (make-instance 'resource
                                   :id 'dev1
                                   :name "Developer 1"
                                   :project *current-project*)))
      (setf (gethash nil *namespace-registry*) *current-namespace*)
      (register-resource resource)

      ;; Resolve simple reference
      (let ((resolved (resolve-resource-reference 'dev1)))
        (is (eq resource resolved))))))

(test resolve-qualified-resource-reference
  "Can resolve qualified resource reference from different namespace"
  (with-test-project
    (let ((ns-main (make-instance 'namespace :name nil))
          (ns-team (make-instance 'namespace :name 'team)))
      (setf (gethash nil *namespace-registry*) ns-main)
      (setf (gethash 'team *namespace-registry*) ns-team)

      ;; Create resource in team namespace
      (let* ((*current-namespace* ns-team)
             (resource (make-instance 'resource
                                     :id 'alice
                                     :name "Alice"
                                     :project *current-project*)))
        (register-resource resource)

        ;; Switch to main namespace and resolve qualified reference
        (setf *current-namespace* ns-main)
        (let* ((qualified-ref (intern "TEAM:ALICE"))
               (resolved (resolve-resource-reference qualified-ref)))
          (is (eq resource resolved)))))))

(test resolve-resource-reference-not-found
  "Resource resolution signals error when not found"
  (with-test-project
    (let ((*current-namespace* (make-instance 'namespace :name nil)))
      (setf (gethash nil *namespace-registry*) *current-namespace*)

      ;; Should signal reference-error
      (signals reference-error
        (resolve-resource-reference 'nonexistent)))))

;;; Reference resolution in dependencies

(test dependency-reference-resolution
  "Dependency target references can be resolved"
  (with-test-project
    (let* ((*current-namespace* (make-instance 'namespace :name nil))
           (task1 (make-instance 'task
                                :id 'task1
                                :name "Task 1"
                                :project *current-project*))
           (task2 (make-instance 'task
                                :id 'task2
                                :name "Task 2"
                                :project *current-project*))
           (dep (make-instance 'dependency
                              :source task2
                              :target-ref 'task1)))
      (setf (gethash nil *namespace-registry*) *current-namespace*)
      (register-task task1)
      (register-task task2)

      ;; Resolve dependency target
      (let ((resolved (resolve-task-reference (dependency-target-ref dep))))
        (is (eq task1 resolved))))))

(test dependency-qualified-reference-resolution
  "Dependency can reference tasks in other namespaces"
  (with-test-project
    (let ((ns-main (make-instance 'namespace :name nil))
          (ns-infra (make-instance 'namespace :name 'infra)))
      (setf (gethash nil *namespace-registry*) ns-main)
      (setf (gethash 'infra *namespace-registry*) ns-infra)

      ;; Create task in infra namespace
      (let ((*current-namespace* ns-infra))
        (register-task (make-instance 'task
                                     :id 'setup
                                     :name "Setup Infrastructure"
                                     :project *current-project*)))

      ;; Create task in main namespace that depends on infra task
      (setf *current-namespace* ns-main)
      (let* ((main-task (make-instance 'task
                                      :id 'deploy
                                      :name "Deploy Application"
                                      :project *current-project*))
             (dep (make-instance 'dependency
                                :source main-task
                                :target-ref (intern "INFRA:SETUP"))))
        (register-task main-task)

        ;; Resolve cross-namespace dependency
        (let ((resolved (resolve-task-reference (dependency-target-ref dep))))
          (is (not (null resolved)))
          (is (eq 'setup (task-id resolved))))))))

;;; Reference resolution in allocations

(test allocation-reference-resolution
  "Allocation resource references can be resolved"
  (with-test-project
    (let* ((*current-namespace* (make-instance 'namespace :name nil))
           (resource (make-instance 'resource
                                   :id 'dev1
                                   :name "Developer 1"
                                   :project *current-project*))
           (task (make-instance 'task
                               :id 'task1
                               :name "Task 1"
                               :project *current-project*))
           (alloc (make-instance 'allocation
                                :task task
                                :resource-refs '(dev1))))
      (setf (gethash nil *namespace-registry*) *current-namespace*)
      (register-resource resource)
      (register-task task)

      ;; Resolve allocation resource references
      (let ((resolved (resolve-resource-reference (first (allocation-resource-refs alloc)))))
        (is (eq resource resolved))))))

(test allocation-multiple-resource-resolution
  "Allocation can reference multiple resources"
  (with-test-project
    (let* ((*current-namespace* (make-instance 'namespace :name nil))
           (dev1 (make-instance 'resource
                               :id 'dev1
                               :name "Developer 1"
                               :project *current-project*))
           (dev2 (make-instance 'resource
                               :id 'dev2
                               :name "Developer 2"
                               :project *current-project*))
           (task (make-instance 'task
                               :id 'task1
                               :name "Task 1"
                               :project *current-project*))
           (alloc (make-instance 'allocation
                                :task task
                                :resource-refs '(dev1 dev2))))
      (setf (gethash nil *namespace-registry*) *current-namespace*)
      (register-resource dev1)
      (register-resource dev2)
      (register-task task)

      ;; Resolve all allocation resource references
      (let ((resolved-refs (mapcar #'resolve-resource-reference
                                   (allocation-resource-refs alloc))))
        (is (= 2 (length resolved-refs)))
        (is (member dev1 resolved-refs))
        (is (member dev2 resolved-refs))))))
