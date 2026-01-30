;;;; tests/cli/test-args.lisp
;;;; Tests for CLI argument parsing

(in-package #:claps-tests)

(in-suite args-suite)

;;; ============================================================================
;;; Basic Parsing Tests
;;; ============================================================================

(test parse-file-only
  "Parsing a single file argument"
  (let ((opts (claps/cli:parse-arguments '("project.lisp"))))
    (is (equal "project.lisp" (claps/cli:option-file opts)))
    (is (null (claps/cli:option-help opts)))
    (is (null (claps/cli:option-version opts)))))

(test parse-help-flag
  "Parsing --help flag"
  (let ((opts (claps/cli:parse-arguments '("--help"))))
    (is-true (claps/cli:option-help opts)))
  (let ((opts (claps/cli:parse-arguments '("-h"))))
    (is-true (claps/cli:option-help opts))))

(test parse-version-flag
  "Parsing --version flag"
  (let ((opts (claps/cli:parse-arguments '("--version"))))
    (is-true (claps/cli:option-version opts)))
  (let ((opts (claps/cli:parse-arguments '("-v"))))
    (is-true (claps/cli:option-version opts))))

;;; ============================================================================
;;; Analysis Options Tests
;;; ============================================================================

(test parse-validate-flag
  "Parsing --validate flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--validate"))))
    (is (equal "project.lisp" (claps/cli:option-file opts)))
    (is-true (claps/cli:option-validate opts))))

(test parse-critical-path-flag
  "Parsing --critical-path flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--critical-path"))))
    (is-true (claps/cli:option-critical-path opts))))

(test parse-summary-flag
  "Parsing --summary flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--summary"))))
    (is-true (claps/cli:option-summary opts))))

(test parse-milestones-flag
  "Parsing --milestones flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--milestones"))))
    (is-true (claps/cli:option-milestones opts))))

(test parse-resources-flag
  "Parsing --resources flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--resources"))))
    (is-true (claps/cli:option-resources opts))))

(test parse-overallocations-flag
  "Parsing --overallocations flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--overallocations"))))
    (is-true (claps/cli:option-overallocations opts))))

(test parse-evm-flag
  "Parsing --evm flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--evm"))))
    (is-true (claps/cli:option-evm opts))))

(test parse-simulate-flag
  "Parsing --simulate flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--simulate"))))
    (is-true (claps/cli:option-simulate opts))))

(test parse-scenarios-flag
  "Parsing --scenarios flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--scenarios"))))
    (is-true (claps/cli:option-scenarios opts))))

(test parse-repl-flag
  "Parsing --repl flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--repl"))))
    (is-true (claps/cli:option-repl opts))))

;;; ============================================================================
;;; Options with Values Tests
;;; ============================================================================

(test parse-output-dir
  "Parsing --output-dir with value"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--output-dir" "./out"))))
    (is (equal "./out" (claps/cli:option-output-dir opts)))))

(test parse-report
  "Parsing --report with value"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--report" "summary"))))
    (is (eq 'claps::summary (claps/cli:option-report opts)))))

(test parse-status-date
  "Parsing --status-date with value"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--evm" "--status-date" "2024-04-01"))))
    (is-true (claps/cli:option-status-date opts))
    (let ((d (claps/cli:option-status-date opts)))
      (is (= 2024 (claps:date-year d)))
      (is (= 4 (claps:date-month d)))
      (is (= 1 (claps:date-day d))))))

(test parse-trials
  "Parsing --trials with value"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--simulate" "--trials" "5000"))))
    (is (= 5000 (claps/cli:option-trials opts)))))

(test parse-compare-two-args
  "Parsing --compare with two scenario names"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--compare" "plan" "delayed"))))
    (is (equal '(claps::plan claps::delayed) (claps/cli:option-compare opts)))))

;;; ============================================================================
;;; Output Options Tests
;;; ============================================================================

(test parse-quiet-flag
  "Parsing --quiet flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--quiet"))))
    (is-true (claps/cli:option-quiet opts))))

(test parse-json-flag
  "Parsing --json flag"
  (let ((opts (claps/cli:parse-arguments '("project.lisp" "--json"))))
    (is-true (claps/cli:option-json opts))))

;;; ============================================================================
;;; Combined Options Tests
;;; ============================================================================

(test parse-multiple-flags
  "Parsing multiple flags together"
  (let ((opts (claps/cli:parse-arguments
               '("project.lisp" "--summary" "--milestones" "--json" "--quiet"))))
    (is (equal "project.lisp" (claps/cli:option-file opts)))
    (is-true (claps/cli:option-summary opts))
    (is-true (claps/cli:option-milestones opts))
    (is-true (claps/cli:option-json opts))
    (is-true (claps/cli:option-quiet opts))))

(test parse-file-with-path
  "Parsing file with full path"
  (let ((opts (claps/cli:parse-arguments '("/path/to/project.lisp"))))
    (is (equal "/path/to/project.lisp" (claps/cli:option-file opts)))))

;;; ============================================================================
;;; Error Cases Tests
;;; ============================================================================

(test parse-unknown-option-signals-error
  "Unknown option signals cli-argument-error"
  (signals claps/cli:cli-argument-error
    (claps/cli:parse-arguments '("--unknown-flag"))))

(test parse-missing-output-dir-value
  "Missing value for --output-dir signals error"
  (signals claps/cli:cli-argument-error
    (claps/cli:parse-arguments '("project.lisp" "--output-dir"))))

(test parse-missing-report-value
  "Missing value for --report signals error"
  (signals claps/cli:cli-argument-error
    (claps/cli:parse-arguments '("project.lisp" "--report"))))

(test parse-missing-compare-values
  "Missing values for --compare signals error"
  (signals claps/cli:cli-argument-error
    (claps/cli:parse-arguments '("project.lisp" "--compare" "plan"))))

(test parse-invalid-trials-value
  "Invalid value for --trials signals error"
  (signals claps/cli:cli-argument-error
    (claps/cli:parse-arguments '("project.lisp" "--trials" "abc"))))

(test parse-invalid-date-value
  "Invalid date format signals error"
  (signals claps/cli:cli-argument-error
    (claps/cli:parse-arguments '("project.lisp" "--status-date" "not-a-date"))))

;;; ============================================================================
;;; Default Values Tests
;;; ============================================================================

(test default-trials-value
  "Default trials value is 1000"
  (let ((opts (claps/cli:parse-arguments '("project.lisp"))))
    (is (= 1000 (claps/cli:option-trials opts)))))

(test default-output-dir-is-nil
  "Default output-dir is nil"
  (let ((opts (claps/cli:parse-arguments '("project.lisp"))))
    (is (null (claps/cli:option-output-dir opts)))))
