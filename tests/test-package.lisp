;;;; tests/test-package.lisp
;;;; Test package definition

(defpackage #:project-juggler-tests
  (:use #:cl #:fiveam #:project-juggler)
  (:export #:run-tests
           #:project-juggler-suite))

(in-package #:project-juggler-tests)

(def-suite project-juggler-suite
  :description "All Project Juggler tests")

(def-suite types-suite
  :in project-juggler-suite
  :description "Temporal types tests")

(def-suite classes-suite
  :in project-juggler-suite
  :description "Core CLOS classes tests")

(def-suite namespace-suite
  :in project-juggler-suite
  :description "Namespace system tests")

(def-suite dsl-suite
  :in project-juggler-suite
  :description "DSL macros tests")

(def-suite validation-suite
  :in project-juggler-suite
  :description "Validation and finalization tests")

(def-suite scheduling-suite
  :in project-juggler-suite
  :description "Scheduling algorithm tests")

(def-suite session-suite
  :in project-juggler-suite
  :description "Session management tests")

(def-suite reporting-suite
  :in project-juggler-suite
  :description "Reporting engine tests")

(def-suite integration-suite
  :in project-juggler-suite
  :description "Integration tests")

(defun run-tests ()
  "Run all Project Juggler tests"
  (run! 'project-juggler-suite))
