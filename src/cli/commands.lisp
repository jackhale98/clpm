;;;; src/cli/commands.lisp
;;;; Command implementations for CLAPS CLI

(in-package #:claps/cli)

;;; ============================================================================
;;; REPL Support Variables
;;; ============================================================================

(defvar *current-project-file* nil
  "Path to the currently loaded project file (for reload support)")

;;; ============================================================================
;;; Project Loading
;;; ============================================================================

(defun load-project-file (filepath)
  "Load a project file safely. Returns the project or signals an error."
  (unless (probe-file filepath)
    (error 'file-error :pathname filepath))

  ;; Load the file in a controlled environment
  (let ((claps:*current-project* nil)
        (claps:*current-task* nil)
        (claps:*namespace-registry* (make-hash-table :test 'eq))
        (claps:*project-registry* (make-hash-table :test 'eq)))
    (load filepath :verbose nil :print nil)
    (unless claps:*current-project*
      (error 'claps:validation-error
             :message "No project defined in file"))
    claps:*current-project*))

;;; ============================================================================
;;; Core Commands
;;; ============================================================================

(defun execute-default-command (opts)
  "Default command: load, finalize, schedule, and generate all reports"
  (let ((file (option-file opts)))
    (print-info "Loading project: ~A" file)
    (let ((project (load-project-file file)))
      ;; Finalize
      (print-info "Finalizing project...")
      (claps:finalize-project project)

      ;; Schedule
      (print-info "Scheduling tasks...")
      (claps:schedule project)

      ;; Generate reports
      (let* ((output-dir (or (option-output-dir opts) "."))
             (reports (claps:list-project-reports project))
             (specific-report (option-report opts)))
        (if specific-report
            ;; Generate specific report
            (if (member specific-report reports)
                (let ((filename (format nil "~A/~A.html" output-dir specific-report)))
                  (claps:save-project-report project specific-report filename)
                  (print-info "Generated: ~A" filename))
                (print-error "Unknown report: ~A~%Available: ~{~A~^, ~}"
                             specific-report reports))
            ;; Generate all reports
            (if reports
                (progn
                  (print-info "Generating ~D reports..." (length reports))
                  (claps:generate-all-reports project output-dir)
                  (print-info "Reports saved to: ~A" output-dir))
                (print-info "No reports defined in project"))))

      (print-info "Done.")
      project)))

(defun execute-validate-command (opts)
  "Validate a project without scheduling"
  (let ((file (option-file opts)))
    (print-info "Loading project: ~A" file)
    (let ((project (load-project-file file)))
      (print-info "Validating...")
      (claps:finalize-project project)  ; finalize-project does all validation
      (print-info "Project is valid.")
      project)))

;;; ============================================================================
;;; Analysis Commands
;;; ============================================================================

(defun execute-critical-path-command (opts)
  "Show critical path"
  (let ((project (execute-validate-and-schedule opts)))
    (format-critical-path project
                          :milestones (option-milestones opts)
                          :json (option-json opts))))

(defun execute-summary-command (opts)
  "Show project summary"
  (let ((project (execute-validate-and-schedule opts)))
    (format-summary project
                    :milestones (option-milestones opts)
                    :json (option-json opts))))

(defun execute-milestones-command (opts)
  "Show milestone timeline"
  (let ((project (execute-validate-and-schedule opts)))
    (format-milestones project :json (option-json opts))))

(defun execute-resources-command (opts)
  "Show resource utilization"
  (let ((project (execute-validate-and-schedule opts)))
    (format-resources project :json (option-json opts))))

(defun execute-overallocations-command (opts)
  "Show resource overallocations"
  (let ((project (execute-validate-and-schedule opts)))
    (format-overallocations project :json (option-json opts))))

(defun execute-evm-command (opts)
  "Show earned value metrics"
  (let* ((project (execute-validate-and-schedule opts))
         (status-date (or (option-status-date opts)
                          (claps:date (nth-value 5 (decode-universal-time (get-universal-time)))
                                      (nth-value 4 (decode-universal-time (get-universal-time)))
                                      (nth-value 3 (decode-universal-time (get-universal-time)))))))
    (format-evm project status-date :json (option-json opts))))

(defun execute-simulate-command (opts)
  "Run Monte Carlo simulation"
  (let ((project (execute-validate-and-schedule opts))
        (trials (option-trials opts)))
    (print-info "Running ~:D simulation trials..." trials)
    (let ((results (claps:run-monte-carlo-simulation project :trials trials)))
      (format-simulation results :json (option-json opts)))))

;;; ============================================================================
;;; Scenario Commands
;;; ============================================================================

(defun execute-scenarios-command (opts)
  "List available scenarios"
  (let ((project (load-and-finalize opts)))
    (format-scenarios project :json (option-json opts))))

(defun execute-compare-command (opts)
  "Compare two scenarios"
  (let ((project (load-and-finalize opts))
        (scenarios (option-compare opts)))
    (let ((s1 (first scenarios))
          (s2 (second scenarios)))
      (unless (claps:get-scenario project s1)
        (print-error "Unknown scenario: ~A" s1)
        (return-from execute-compare-command))
      (unless (claps:get-scenario project s2)
        (print-error "Unknown scenario: ~A" s2)
        (return-from execute-compare-command))
      (format-comparison project s1 s2 :json (option-json opts)))))

;;; ============================================================================
;;; Interactive REPL Command
;;; ============================================================================

(defun execute-repl-command (opts)
  "Load project and start interactive REPL"
  (let ((file (option-file opts)))
    (print-info "Loading project: ~A" file)
    (let ((project (load-project-file file)))
      ;; Finalize and schedule
      (print-info "Finalizing project...")
      (claps:finalize-project project)

      (print-info "Scheduling tasks...")
      (claps:schedule project)

      ;; Print helpful information
      (format t "~%")
      (format t "================================================================================~%")
      (format t "CLAPS Interactive REPL~%")
      (format t "================================================================================~%")
      (format t "~%")
      (format t "Project loaded: ~A~%" (claps:project-name project))
      (format t "Tasks: ~D  Resources: ~D  Scenarios: ~A~%"
              (hash-table-count (claps:project-tasks project))
              (hash-table-count (claps:project-resources project))
              (claps:list-scenarios project))
      (format t "~%")
      (format t "Useful variables:~%")
      (format t "  claps:*current-project*  - The loaded project~%")
      (format t "~%")
      (format t "Quick commands:~%")
      (format t "  (claps:critical-path claps:*current-project*)~%")
      (format t "  (claps:list-scenarios claps:*current-project*)~%")
      (format t "  (claps:detect-resource-overallocations claps:*current-project*)~%")
      (format t "  (claps:run-monte-carlo-simulation claps:*current-project* :trials 1000)~%")
      (format t "~%")
      (format t "To reload and reschedule after changes:~%")
      (format t "  (claps/cli:reload-project)~%")
      (format t "~%")
      (format t "Type (quit) or Ctrl-D to exit.~%")
      (format t "================================================================================~%")
      (format t "~%")

      ;; Store file path for reload
      (setf *current-project-file* file)

      ;; Enter SBCL's REPL
      (enter-repl))))

(defun reload-project ()
  "Reload and reschedule the current project from its file.
   Use this after making changes to the project file."
  (unless *current-project-file*
    (format t "No project file loaded. Use (claps/cli:load-project-file \"path\") first.~%")
    (return-from reload-project nil))

  (format t "Reloading: ~A~%" *current-project-file*)

  ;; Clear current state
  (setf claps:*current-project* nil)
  (setf claps:*current-task* nil)

  ;; Reload
  (let ((project (load-project-file *current-project-file*)))
    (format t "Finalizing...~%")
    (claps:finalize-project project)

    (format t "Scheduling...~%")
    (claps:schedule project)

    (format t "Done! Project reloaded.~%")
    (format t "Tasks: ~D  Critical path: ~D tasks~%"
            (hash-table-count (claps:project-tasks project))
            (length (claps:critical-path project)))
    project))

(defun enter-repl ()
  "Enter the SBCL interactive REPL.
   This gives full access to the debugger and inspector."
  #+sbcl
  (progn
    ;; Switch to CLAPS package for convenience
    (in-package :claps)
    ;; Enter SBCL's top-level REPL
    (sb-impl::toplevel-repl nil))
  #-sbcl
  (progn
    (format t "Interactive REPL is only supported on SBCL.~%")
    (format t "Please use your Lisp implementation's REPL directly.~%")))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun load-and-finalize (opts)
  "Load and finalize a project (for commands that don't need scheduling)"
  (let ((file (option-file opts)))
    (let ((project (load-project-file file)))
      (claps:finalize-project project)
      project)))

(defun execute-validate-and-schedule (opts)
  "Load, finalize, and schedule a project"
  (let ((file (option-file opts)))
    (let ((project (load-project-file file)))
      (claps:finalize-project project)
      (claps:schedule project)
      project)))

;;; ============================================================================
;;; Command Dispatch
;;; ============================================================================

(defun dispatch-command (opts)
  "Dispatch to the appropriate command based on options"
  (let ((*quiet* (option-quiet opts)))
    (cond
      ;; Help and version
      ((option-help opts)
       (print-usage)
       0)
      ((option-version opts)
       (print-version)
       0)

      ;; File required for remaining commands
      ((null (option-file opts))
       (print-error "No project file specified. Run 'claps --help' for usage.")
       1)

      ;; Analysis commands
      ((option-critical-path opts)
       (execute-critical-path-command opts)
       0)
      ((option-summary opts)
       (execute-summary-command opts)
       0)
      ((option-milestones opts)
       (execute-milestones-command opts)
       0)
      ((option-resources opts)
       (execute-resources-command opts)
       0)
      ((option-overallocations opts)
       (execute-overallocations-command opts)
       0)
      ((option-evm opts)
       (execute-evm-command opts)
       0)
      ((option-simulate opts)
       (execute-simulate-command opts)
       0)

      ;; Scenario commands
      ((option-scenarios opts)
       (execute-scenarios-command opts)
       0)
      ((option-compare opts)
       (execute-compare-command opts)
       0)

      ;; Validate only
      ((option-validate opts)
       (execute-validate-command opts)
       0)

      ;; Interactive REPL
      ((option-repl opts)
       (execute-repl-command opts)
       0)  ; Won't return if REPL starts

      ;; Default: process project and generate reports
      (t
       (execute-default-command opts)
       0))))
