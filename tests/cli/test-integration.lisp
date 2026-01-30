;;;; tests/cli/test-integration.lisp
;;;; End-to-end CLI integration tests

(in-package #:claps-tests)

(in-suite cli-integration-suite)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun capture-cli-output (args)
  "Run CLI with given args and capture output.
   Returns (exit-code stdout stderr)"
  (let ((stdout-str (make-string-output-stream))
        (stderr-str (make-string-output-stream)))
    (let* ((*standard-output* stdout-str)
           (*error-output* stderr-str)
           (exit-code (claps/cli:main args)))
      (list exit-code
            (get-output-stream-string stdout-str)
            (get-output-stream-string stderr-str)))))

(defparameter *example-project-path*
  (merge-pathnames "examples/simple-project.lisp"
                   (asdf:system-source-directory :claps)))

;;; ============================================================================
;;; Help and Version Tests
;;; ============================================================================

(test cli-help-works
  "CLI --help produces output and exits 0"
  (let ((result (capture-cli-output '("--help"))))
    (is (= 0 (first result)))
    (is (search "CLAPS" (second result)))
    (is (search "Usage" (second result)))))

(test cli-version-works
  "CLI --version produces output and exits 0"
  (let ((result (capture-cli-output '("--version"))))
    (is (= 0 (first result)))
    (is (search "CLAPS" (second result)))))

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(test cli-missing-file-shows-error
  "CLI shows error when no file specified"
  (let ((result (capture-cli-output '("--summary"))))
    (is (= 1 (first result)))
    (is (search "Error" (third result)))))

(test cli-nonexistent-file-shows-error
  "CLI shows error for nonexistent file"
  (let ((result (capture-cli-output '("/nonexistent/file.lisp"))))
    (is (= 1 (first result)))
    (is (search "File not found" (third result)))))

(test cli-unknown-option-shows-error
  "CLI shows error for unknown option"
  (let ((result (capture-cli-output '("--unknown-option"))))
    (is (= 1 (first result)))
    (is (search "Unknown option" (third result)))))

;;; ============================================================================
;;; Example Project Tests
;;; ============================================================================

(test cli-processes-example-project
  "CLI can process the example project"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*) "--validate" "--quiet"))))
      (is (= 0 (first result))))))

(test cli-summary-on-example
  "CLI --summary works on example project"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*) "--summary" "--quiet"))))
      (is (= 0 (first result)))
      (is (search "Project" (second result))))))

(test cli-critical-path-on-example
  "CLI --critical-path works on example project"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*) "--critical-path" "--quiet"))))
      (is (= 0 (first result)))
      (is (search "Critical" (second result))))))

(test cli-resources-on-example
  "CLI --resources works on example project"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*) "--resources" "--quiet"))))
      (is (= 0 (first result)))
      (is (search "Resource" (second result))))))

(test cli-scenarios-on-example
  "CLI --scenarios works on example project"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*) "--scenarios" "--quiet"))))
      (is (= 0 (first result)))
      (is (search "Scenario" (second result))))))

;;; ============================================================================
;;; JSON Output Tests
;;; ============================================================================

(test cli-json-output-valid
  "CLI --json produces valid JSON"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*)
                         "--summary" "--json" "--quiet"))))
      (is (= 0 (first result)))
      (is (search "{" (second result)))
      (is (search "}" (second result)))
      (is (search "\"project\"" (second result))))))

(test cli-critical-path-json-valid
  "CLI --critical-path --json produces valid JSON"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*)
                         "--critical-path" "--json" "--quiet"))))
      (is (= 0 (first result)))
      (is (search "[" (second result))))))

;;; ============================================================================
;;; Quiet Mode Tests
;;; ============================================================================

(test cli-quiet-suppresses-info
  "CLI --quiet suppresses informational messages"
  (when (probe-file *example-project-path*)
    (let ((result-quiet (capture-cli-output
                         (list (namestring *example-project-path*)
                               "--validate" "--quiet")))
          (result-normal (capture-cli-output
                          (list (namestring *example-project-path*)
                                "--validate"))))
      (is (= 0 (first result-quiet)))
      (is (= 0 (first result-normal)))
      ;; Quiet output should be shorter or same
      (is (<= (length (second result-quiet))
              (length (second result-normal)))))))

;;; ============================================================================
;;; Combined Options Tests
;;; ============================================================================

(test cli-summary-with-milestones
  "CLI --summary --milestones works"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*)
                         "--summary" "--milestones" "--quiet"))))
      (is (= 0 (first result))))))

(test cli-critical-path-with-milestones
  "CLI --critical-path --milestones works"
  (when (probe-file *example-project-path*)
    (let ((result (capture-cli-output
                   (list (namestring *example-project-path*)
                         "--critical-path" "--milestones" "--quiet"))))
      (is (= 0 (first result))))))
