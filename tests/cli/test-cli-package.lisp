;;;; tests/cli/test-cli-package.lisp
;;;; Test package and suite definition for CLI tests

(in-package #:claps-tests)

(def-suite args-suite
  :in cli-suite
  :description "Argument parsing tests")

(def-suite output-suite
  :in cli-suite
  :description "Output formatting tests")

(def-suite commands-suite
  :in cli-suite
  :description "Command execution tests")

(def-suite cli-integration-suite
  :in cli-suite
  :description "CLI integration tests")
