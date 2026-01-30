;;;; src/cli/main.lisp
;;;; Main entry point for CLAPS CLI

(in-package #:claps/cli)

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for the CLI.
   ARGS should be a list of command-line argument strings.
   Returns an exit code (0 for success, non-zero for error)."
  (handler-case
      (let ((opts (parse-arguments args)))
        (dispatch-command opts))
    ;; CLI argument errors
    (cli-argument-error (e)
      (print-error "~A" (cli-argument-error-message e))
      (format *error-output* "Run 'claps --help' for usage.~%")
      1)
    ;; Reference errors (unknown task/resource)
    (claps:reference-error (e)
      (print-error "~A" (format-lisp-error e))
      1)
    ;; Circular dependency
    (claps:circular-dependency-error (e)
      (print-error "~A" (format-lisp-error e))
      1)
    ;; Validation error
    (claps:validation-error (e)
      (print-error "~A" (format-lisp-error e))
      1)
    ;; Scheduling error
    (claps:scheduling-error (e)
      (print-error "~A" (format-lisp-error e))
      1)
    ;; File errors
    (file-error (e)
      (print-error "File not found: ~A" (file-error-pathname e))
      1)
    ;; Reader errors (syntax errors)
    (reader-error (e)
      (print-error "Syntax error in project file: ~A" e)
      1)
    ;; Catch-all for unexpected errors
    (error (e)
      (print-error "Unexpected error: ~A" e)
      2)))

(defun run-cli ()
  "Run the CLI and exit with appropriate status code"
  (let ((exit-code (main)))
    (uiop:quit exit-code)))

;;; ============================================================================
;;; Build Support
;;; ============================================================================

(defun build-executable (&optional (output-path "claps"))
  "Build a standalone executable"
  #+sbcl
  (sb-ext:save-lisp-and-die output-path
                            :toplevel #'run-cli
                            :executable t
                            :compression t)
  #-sbcl
  (error "Building executables is only supported on SBCL"))
