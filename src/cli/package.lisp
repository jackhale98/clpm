;;;; src/cli/package.lisp
;;;; Package definition for CLAPS CLI

(defpackage #:claps/cli
  (:use #:cl #:claps)
  (:export
   ;; Main entry points
   #:main
   #:run-cli

   ;; CLI options
   #:cli-options
   #:option-file
   #:option-output-dir
   #:option-report
   #:option-validate
   #:option-critical-path
   #:option-scenarios
   #:option-compare
   #:option-summary
   #:option-resources
   #:option-overallocations
   #:option-evm
   #:option-status-date
   #:option-simulate
   #:option-trials
   #:option-milestones
   #:option-quiet
   #:option-json
   #:option-help
   #:option-version
   #:option-repl

   ;; Argument parsing
   #:parse-arguments
   #:cli-argument-error

   ;; Output formatting
   #:print-error
   #:print-info
   #:print-warning
   #:format-lisp-error
   #:format-critical-path
   #:format-scenarios
   #:format-comparison
   #:format-summary
   #:format-resources
   #:format-overallocations
   #:format-evm
   #:format-simulation
   #:format-milestones
   #:to-json-string

   ;; Commands
   #:dispatch-command
   #:load-project-file
   #:reload-project
   #:*current-project-file*
   #:execute-default-command
   #:execute-validate-command
   #:execute-critical-path-command
   #:execute-summary-command
   #:execute-milestones-command
   #:execute-resources-command
   #:execute-overallocations-command
   #:execute-evm-command
   #:execute-simulate-command
   #:execute-scenarios-command
   #:execute-compare-command

   ;; Build
   #:build-executable

   ;; Version
   #:*claps-version*))
