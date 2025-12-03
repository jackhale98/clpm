;;;; tests/dsl/test-defproject.lisp
;;;; Tests for defproject macro

(in-package #:project-juggler-tests)

(in-suite dsl-suite)

;;; Basic defproject tests

(test defproject-basic
  "Can define a basic project"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-project "Test Project"
             :start (date 2024 1 1)
             :end (date 2024 12 31)))

    (let ((project (gethash 'test-project *project-registry*)))
      (is (not (null project)))
      (is (project-p project))
      (is (eq 'test-project (project-id project)))
      (is (string= "Test Project" (project-name project)))
      (is (date= (date 2024 1 1) (project-start project)))
      (is (date= (date 2024 12 31) (project-end project))))))

(test defproject-sets-current-project
  "defproject sets *current-project*"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-project "Test Project"
             :start (date 2024 1 1)
             :end (date 2024 12 31)))

    (is (not (null *current-project*)))
    (is (eq 'test-project (project-id *current-project*)))))

(test defproject-creates-default-namespace
  "defproject creates default namespace"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-project "Test Project"
             :start (date 2024 1 1)
             :end (date 2024 12 31)))

    (is (not (null *current-namespace*)))
    (is (null (namespace-name *current-namespace*)))))

(test defproject-with-body
  "defproject can have body forms"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))

    (eval '(defproject test-project "Test Project"
             :start (date 2024 1 1)
             :end (date 2024 12 31)
             (deftask task1 "Task 1")))

    ;; Check that the task was created by the body
    (let ((task (resolve-task-reference 'task1)))
      (is (not (null task)))
      (is (task-p task)))))

;;; defproject with scenarios

(test defproject-default-scenario
  "defproject has default 'plan' scenario"
  (let ((*namespace-registry* (make-hash-table :test 'eq))
        (*project-registry* (make-hash-table :test 'eq)))
    (eval '(defproject test-project "Test Project"
             :start (date 2024 1 1)
             :end (date 2024 12 31)))

    ;; Current scenario is a symbol, not a keyword (TaskJuggler-style)
    ;; Compare symbol names since packages may differ
    (is (string= "PLAN" (symbol-name (project-current-scenario *current-project*))))))
