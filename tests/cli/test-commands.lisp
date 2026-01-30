;;;; tests/cli/test-commands.lisp
;;;; Tests for CLI commands

(in-package #:claps-tests)

(in-suite commands-suite)

;;; ============================================================================
;;; Helper Macros
;;; ============================================================================

(defvar *test-project-dir* nil)

(defmacro with-temp-project-file ((path-var content) &body body)
  "Create a temporary project file for testing"
  `(let* ((temp-dir (uiop:temporary-directory))
          (,path-var (namestring (merge-pathnames "test-project.lisp" temp-dir))))
     (unwind-protect
          (progn
            (with-open-file (out ,path-var :direction :output :if-exists :supersede)
              (write-string ,content out))
            ,@body)
       (when (probe-file ,path-var)
         (delete-file ,path-var)))))

(defparameter *simple-project-content*
  "(in-package :claps)
(defproject test-project \"Test Project\"
  :start (date 2024 3 1)
  :end (date 2024 6 30)
  (defresource dev \"Developer\")
  (deftask task1 \"Task 1\"
    :duration (duration 5 :days)
    :allocate (dev))
  (deftask task2 \"Task 2\"
    :duration (duration 3 :days)
    :depends-on (task1)
    :allocate (dev)))")

(defparameter *project-with-scenarios*
  "(in-package :claps)
(defproject test-project \"Test Project\"
  :start (date 2024 3 1)
  :end (date 2024 6 30)
  :scenarios (plan delayed)
  (defresource dev \"Developer\")
  (deftask task1 \"Task 1\"
    :duration (duration 5 :days)
    :delayed/duration (duration 10 :days)
    :allocate (dev)))")

(defparameter *project-with-milestone*
  "(in-package :claps)
(defproject test-project \"Test Project\"
  :start (date 2024 3 1)
  :end (date 2024 6 30)
  (defresource dev \"Developer\")
  (deftask task1 \"Task 1\"
    :duration (duration 5 :days)
    :allocate (dev))
  (deftask milestone1 \"Milestone 1\"
    :milestone t
    :depends-on (task1)))")

;;; ============================================================================
;;; Load Project Tests
;;; ============================================================================

(test load-project-file-success
  "Loading a valid project file succeeds"
  (with-temp-project-file (path *simple-project-content*)
    (finishes (claps/cli:load-project-file path))
    (is-true claps:*current-project*)))

(test load-project-file-not-found
  "Loading non-existent file signals error"
  (signals file-error
    (claps/cli:load-project-file "/nonexistent/path.lisp")))

;;; ============================================================================
;;; Validate Command Tests
;;; ============================================================================

(test execute-validate-command-success
  "Validate command succeeds on valid project"
  (with-temp-project-file (path *simple-project-content*)
    (let ((opts (claps/cli:parse-arguments (list path "--validate" "--quiet"))))
      (finishes (claps/cli:execute-validate-command opts)))))

;;; ============================================================================
;;; Critical Path Command Tests
;;; ============================================================================

(test execute-critical-path-command-success
  "Critical path command succeeds"
  (with-temp-project-file (path *simple-project-content*)
    (let ((opts (claps/cli:parse-arguments (list path "--critical-path" "--quiet"))))
      (finishes (claps/cli:execute-critical-path-command opts)))))

(test critical-path-json-output
  "Critical path command produces JSON output"
  (with-temp-project-file (path *simple-project-content*)
    (let ((opts (claps/cli:parse-arguments (list path "--critical-path" "--json" "--quiet"))))
      (let ((output (with-output-to-string (*standard-output*)
                      (claps/cli:execute-critical-path-command opts))))
        (is (search "[" output))  ; JSON array
        (is (search "task" (string-downcase output)))))))

;;; ============================================================================
;;; Summary Command Tests
;;; ============================================================================

(test execute-summary-command-success
  "Summary command succeeds"
  (with-temp-project-file (path *simple-project-content*)
    (let ((opts (claps/cli:parse-arguments (list path "--summary" "--quiet"))))
      (finishes (claps/cli:execute-summary-command opts)))))

(test summary-includes-task-count
  "Summary includes task count"
  (with-temp-project-file (path *simple-project-content*)
    (let ((opts (claps/cli:parse-arguments (list path "--summary" "--quiet"))))
      (let ((output (with-output-to-string (*standard-output*)
                      (claps/cli:execute-summary-command opts))))
        (is (search "Tasks" output))))))

;;; ============================================================================
;;; Milestones Command Tests
;;; ============================================================================

(test execute-milestones-command-success
  "Milestones command succeeds"
  (with-temp-project-file (path *project-with-milestone*)
    (let ((opts (claps/cli:parse-arguments (list path "--milestones" "--quiet"))))
      (finishes (claps/cli:execute-milestones-command opts)))))

(test milestones-shows-milestone
  "Milestones output includes milestone name"
  (with-temp-project-file (path *project-with-milestone*)
    (let ((opts (claps/cli:parse-arguments (list path "--milestones" "--quiet"))))
      (let ((output (with-output-to-string (*standard-output*)
                      (claps/cli:execute-milestones-command opts))))
        (is (search "Milestone 1" output))))))

;;; ============================================================================
;;; Resources Command Tests
;;; ============================================================================

(test execute-resources-command-success
  "Resources command succeeds"
  (with-temp-project-file (path *simple-project-content*)
    (let ((opts (claps/cli:parse-arguments (list path "--resources" "--quiet"))))
      (finishes (claps/cli:execute-resources-command opts)))))

;;; ============================================================================
;;; Scenarios Command Tests
;;; ============================================================================

(test execute-scenarios-command-success
  "Scenarios command succeeds"
  (with-temp-project-file (path *project-with-scenarios*)
    (let ((opts (claps/cli:parse-arguments (list path "--scenarios" "--quiet"))))
      (finishes (claps/cli:execute-scenarios-command opts)))))

(test scenarios-lists-all
  "Scenarios output lists all scenarios"
  (with-temp-project-file (path *project-with-scenarios*)
    (let ((opts (claps/cli:parse-arguments (list path "--scenarios" "--quiet"))))
      (let ((output (with-output-to-string (*standard-output*)
                      (claps/cli:execute-scenarios-command opts))))
        (is (search "PLAN" output))
        (is (search "DELAYED" output))))))

;;; ============================================================================
;;; Compare Command Tests
;;; ============================================================================

(test execute-compare-command-success
  "Compare command succeeds"
  (with-temp-project-file (path *project-with-scenarios*)
    (let ((opts (claps/cli:parse-arguments (list path "--compare" "plan" "delayed" "--quiet"))))
      (finishes (claps/cli:execute-compare-command opts)))))

;;; ============================================================================
;;; Main Dispatch Tests
;;; ============================================================================

(test dispatch-help-returns-zero
  "Help command returns exit code 0"
  (let ((opts (claps/cli:parse-arguments '("--help"))))
    (let ((exit-code nil))
      (with-output-to-string (*standard-output*)
        (setf exit-code (claps/cli:dispatch-command opts)))
      (is (= 0 exit-code)))))

(test dispatch-version-returns-zero
  "Version command returns exit code 0"
  (let ((opts (claps/cli:parse-arguments '("--version"))))
    (let ((exit-code nil))
      (with-output-to-string (*standard-output*)
        (setf exit-code (claps/cli:dispatch-command opts)))
      (is (= 0 exit-code)))))

(test dispatch-no-file-returns-one
  "Missing file returns exit code 1"
  (let ((opts (claps/cli:parse-arguments '("--summary"))))
    (let ((exit-code nil))
      (with-output-to-string (*error-output*)
        (setf exit-code (claps/cli:dispatch-command opts)))
      (is (= 1 exit-code)))))
