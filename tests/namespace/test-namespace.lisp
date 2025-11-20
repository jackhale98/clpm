;;;; tests/namespace/test-namespace.lisp
;;;; Tests for namespace system

(in-package #:project-juggler-tests)

(in-suite namespace-suite)

;;; Namespace creation and registration

(test namespace-creation
  "Can create and register namespaces"
  (let ((ns (make-instance 'namespace :name 'test-ns)))
    (is (eq 'test-ns (namespace-name ns)))
    (is (hash-table-p (namespace-tasks ns)))
    (is (hash-table-p (namespace-resources ns)))))

(test namespace-registration
  "Namespaces can be registered"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (ns (make-instance 'namespace :name 'test-ns)))
    (setf (gethash 'test-ns *namespace-registry*) ns)
    (is (eq ns (gethash 'test-ns *namespace-registry*)))))

(test default-namespace
  "Default namespace has nil name"
  (let ((ns (make-instance 'namespace :name nil)))
    (is (null (namespace-name ns)))))

;;; Task registration in namespaces

(test task-registration-in-namespace
  "Tasks can be registered in namespace"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (*current-namespace* (make-instance 'namespace :name 'test))
         (*current-project* (make-instance 'project
                                           :id 'test
                                           :name "Test"
                                           :start (date 2024 1 1)
                                           :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project *current-project*)))
    (register-task task)
    (is (eq task (gethash 't1 (namespace-tasks *current-namespace*))))))

(test task-registration-in-default-namespace
  "Tasks can be registered in default (nil) namespace"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (*current-namespace* (make-instance 'namespace :name nil))
         (*current-project* (make-instance 'project
                                           :id 'test
                                           :name "Test"
                                           :start (date 2024 1 1)
                                           :end (date 2024 12 31)))
         (task (make-instance 'task
                             :id 't1
                             :name "Task 1"
                             :project *current-project*)))
    (register-task task)
    (is (eq task (gethash 't1 (namespace-tasks *current-namespace*))))))

(test task-registration-prevents-duplicates
  "Cannot register duplicate task IDs in same namespace"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (*current-namespace* (make-instance 'namespace :name 'test))
         (*current-project* (make-instance 'project
                                           :id 'test
                                           :name "Test"
                                           :start (date 2024 1 1)
                                           :end (date 2024 12 31)))
         (task1 (make-instance 'task
                              :id 't1
                              :name "Task 1"
                              :project *current-project*))
         (task2 (make-instance 'task
                              :id 't1
                              :name "Task 1 Duplicate"
                              :project *current-project*)))
    (register-task task1)
    ;; Second registration should signal a correctable error
    (signals error (register-task task2))))

;;; Resource registration in namespaces

(test resource-registration-in-namespace
  "Resources can be registered in namespace"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (*current-namespace* (make-instance 'namespace :name 'test))
         (*current-project* (make-instance 'project
                                           :id 'test
                                           :name "Test"
                                           :start (date 2024 1 1)
                                           :end (date 2024 12 31)))
         (resource (make-instance 'resource
                                 :id 'dev1
                                 :name "Developer 1"
                                 :project *current-project*)))
    (register-resource resource)
    (is (eq resource (gethash 'dev1 (namespace-resources *current-namespace*))))))

;;; Namespace isolation

(test namespace-isolation
  "Tasks in different namespaces don't conflict"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (ns1 (make-instance 'namespace :name 'ns1))
         (ns2 (make-instance 'namespace :name 'ns2))
         (*current-project* (make-instance 'project
                                           :id 'test
                                           :name "Test"
                                           :start (date 2024 1 1)
                                           :end (date 2024 12 31)))
         (task1 (make-instance 'task
                              :id 't1
                              :name "Task 1 in NS1"
                              :project *current-project*))
         (task2 (make-instance 'task
                              :id 't1
                              :name "Task 1 in NS2"
                              :project *current-project*)))
    (setf (gethash 'ns1 *namespace-registry*) ns1)
    (setf (gethash 'ns2 *namespace-registry*) ns2)

    ;; Register same ID in different namespaces
    (let ((*current-namespace* ns1))
      (register-task task1))
    (let ((*current-namespace* ns2))
      (register-task task2))

    ;; Both should exist in their respective namespaces
    (is (eq task1 (gethash 't1 (namespace-tasks ns1))))
    (is (eq task2 (gethash 't1 (namespace-tasks ns2))))
    (is (not (eq task1 task2)))))

;;; Qualified symbol parsing

(test parse-simple-symbol
  "Simple symbols have no namespace prefix"
  (let ((parts (parse-qualified-symbol 'task1)))
    (is (null (first parts)))   ; No namespace
    (is (eq 'task1 (second parts)))))

(test parse-qualified-symbol-with-prefix
  "Qualified symbols parse correctly"
  ;; Create symbol with colon in name using intern
  (let* ((qualified-sym (intern "INFRA:SERVERS"))
         (parts (parse-qualified-symbol qualified-sym)))
    (is (equal "INFRA" (symbol-name (first parts))))
    (is (equal "SERVERS" (symbol-name (second parts))))))

(test parse-keyword-qualified-symbol
  "Keyword namespaces parse correctly"
  ;; Create keyword with colon in name
  (let* ((qualified-kw (intern "INFRA:SERVERS" :keyword))
         (parts (parse-qualified-symbol qualified-kw)))
    (is (equal "INFRA" (symbol-name (first parts))))
    (is (equal "SERVERS" (symbol-name (second parts))))))

;;; In-namespace macro

(test in-namespace-sets-current
  "in-namespace macro sets *current-namespace*"
  (let ((*namespace-registry* (make-hash-table :test 'eq)))
    (eval '(in-namespace test-ns))
    (is (boundp '*current-namespace*))
    (is (eq 'test-ns (namespace-name *current-namespace*)))
    (is (eq *current-namespace* (gethash 'test-ns *namespace-registry*)))))

(test in-namespace-creates-if-not-exists
  "in-namespace creates namespace if it doesn't exist"
  (let ((*namespace-registry* (make-hash-table :test 'eq)))
    (is (null (gethash 'new-ns *namespace-registry*)))
    (eval '(in-namespace new-ns))
    (is (not (null (gethash 'new-ns *namespace-registry*))))))

(test in-namespace-reuses-existing
  "in-namespace reuses existing namespace"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (ns (make-instance 'namespace :name 'existing-ns)))
    (setf (gethash 'existing-ns *namespace-registry*) ns)
    (eval '(in-namespace existing-ns))
    (is (eq ns *current-namespace*))))

;;; Integration tests

(test namespace-workflow
  "Complete namespace workflow"
  (with-test-project
    ;; Create two namespaces
    (let ((ns-main (make-instance 'namespace :name nil))
          (ns-infra (make-instance 'namespace :name 'infra)))
      (setf (gethash nil *namespace-registry*) ns-main)
      (setf (gethash 'infra *namespace-registry*) ns-infra)

      ;; Register tasks in main namespace
      (let ((*current-namespace* ns-main))
        (register-task (make-instance 'task
                                     :id 'main-task
                                     :name "Main Task"
                                     :project *current-project*)))

      ;; Register tasks in infra namespace
      (let ((*current-namespace* ns-infra))
        (register-task (make-instance 'task
                                     :id 'servers
                                     :name "Setup Servers"
                                     :project *current-project*))
        (register-task (make-instance 'task
                                     :id 'network
                                     :name "Setup Network"
                                     :project *current-project*)))

      ;; Verify isolation
      (is (= 1 (hash-table-count (namespace-tasks ns-main))))
      (is (= 2 (hash-table-count (namespace-tasks ns-infra))))
      (is (gethash 'main-task (namespace-tasks ns-main)))
      (is (gethash 'servers (namespace-tasks ns-infra)))
      (is (null (gethash 'servers (namespace-tasks ns-main))))
      (is (null (gethash 'main-task (namespace-tasks ns-infra)))))))
