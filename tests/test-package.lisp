;;;; tests/test-package.lisp
;;;; Test package definition

(defpackage #:claps-tests
  (:use #:cl #:fiveam #:claps)
  (:nicknames #:project-juggler-tests)  ; Backwards compatibility
  (:export #:run-tests
           #:claps-suite
           #:project-juggler-suite))  ; Backwards compatibility

(in-package #:claps-tests)

(def-suite claps-suite
  :description "All CLAPS tests")

;; Backwards compatibility alias
(def-suite project-juggler-suite
  :description "Alias for CLAPS suite")

(def-suite types-suite
  :in claps-suite
  :description "Temporal types tests")

(def-suite classes-suite
  :in claps-suite
  :description "Core CLOS classes tests")

(def-suite namespace-suite
  :in claps-suite
  :description "Namespace system tests")

(def-suite dsl-suite
  :in claps-suite
  :description "DSL macros tests")

(def-suite validation-suite
  :in claps-suite
  :description "Validation and finalization tests")

(def-suite scheduling-suite
  :in claps-suite
  :description "Scheduling algorithm tests")

(def-suite session-suite
  :in claps-suite
  :description "Session management tests")

(def-suite reporting-suite
  :in claps-suite
  :description "Reporting engine tests")

(def-suite integration-suite
  :in claps-suite
  :description "Integration tests")

(def-suite cli-suite
  :in claps-suite
  :description "CLI tests")

(defun run-tests ()
  "Run all CLAPS tests"
  (run! 'claps-suite))
