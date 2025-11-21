;;;; src/package.lisp
;;;; Package definition for Project Juggler

(defpackage #:project-juggler
  (:use #:cl #:alexandria)
  (:nicknames #:pj)
  (:export
   ;; Core temporal types
   #:pj-date
   #:date
   #:date-year
   #:date-month
   #:date-day
   #:date-hour
   #:date-minute
   #:date-second
   #:date-timestamp
   #:date+
   #:date-
   #:date<
   #:date<=
   #:date>
   #:date>=
   #:date=

   #:duration
   #:duration-value
   #:duration-unit
   #:duration-in-seconds
   #:duration-in-minutes
   #:duration-in-hours
   #:duration-in-days
   #:duration-in-weeks
   #:duration-p

   #:interval
   #:interval-start
   #:interval-end
   #:interval-duration-days
   #:interval-duration-hours
   #:contains-date-p
   #:overlaps-p

   ;; Core domain classes
   #:project
   #:project-id
   #:project-name
   #:project-start
   #:project-end
   #:project-tasks
   #:project-resources
   #:project-scenarios
   #:project-current-scenario
   #:project-reports
   #:project-p

   #:task
   #:task-id
   #:task-name
   #:task-project
   #:task-parent
   #:task-subtasks
   #:task-dependencies
   #:task-allocations
   #:task-effort
   #:task-duration
   #:task-start
   #:task-end
   #:task-priority
   #:task-milestone-p
   #:task-complete
   #:task-scheduled-p
   #:task-criticalness
   #:task-path-criticalness
   #:task-early-start
   #:task-early-finish
   #:task-late-start
   #:task-late-finish
   #:task-slack
   #:task-index
   #:task-p

   #:resource
   #:resource-id
   #:resource-name
   #:resource-project
   #:resource-parent
   #:resource-efficiency
   #:resource-rate
   #:resource-limits
   #:resource-criticalness
   #:resource-allocated-effort
   #:resource-available-effort
   #:resource-p

   #:dependency
   #:dependency-source
   #:dependency-target-ref
   #:dependency-target
   #:dependency-type
   #:dependency-gap
   #:dependency-p

   #:allocation
   #:allocation-task
   #:allocation-resource-refs
   #:allocation-resources
   #:allocation-mandatory-p
   #:allocation-p

   ;; Namespace system
   #:namespace
   #:namespace-name
   #:namespace-tasks
   #:namespace-resources
   #:namespace-source-file
   #:in-namespace
   #:include
   #:resolve-task-reference
   #:resolve-resource-reference
   #:register-task
   #:register-resource
   #:parse-qualified-symbol
   #:find-namespace
   #:ensure-namespace

   ;; DSL macros
   #:defproject
   #:deftask
   #:defresource
   #:defaccount
   #:depends-on
   #:allocate

   ;; Validation
   #:finalize-project
   #:validate-project
   #:circular-dependency-error
   #:reference-error

   ;; Scheduling (TaskJuggler heuristic)
   #:schedule
   #:schedule-task
   #:calculate-duration-from-effort
   #:calculate-resource-criticalness
   #:calculate-task-criticalness
   #:calculate-path-criticalness
   #:task-ready-p

   ;; Critical path (CPM slack-based - Phase 6)
   #:critical-path
   #:calculate-critical-path
   #:calculate-slack
   #:forward-pass
   #:backward-pass

   ;; Session management
   #:session
   #:session-project
   #:session-current-task
   #:session-changes
   #:load-project-session
   #:save-session
   #:undo
   #:redo
   #:add-task-to-session
   #:modify-task-in-session
   #:delete-task-from-session
   #:reset-session-changes

   ;; Reporting
   #:report
   #:report-id
   #:report-title
   #:report-format
   #:report-columns
   #:report-filter
   #:report-sort-by
   #:task-report
   #:resource-report
   #:generate-report
   #:collect-tasks-for-report
   #:collect-resources-for-report
   #:sort-tasks-for-report
   #:generate-gantt-data
   #:defreport
   #:generate-project-report
   #:save-project-report
   #:list-project-reports
   #:get-project-report
   #:generate-all-reports
   #:quick-task-report
   #:quick-resource-report

   ;; Tracking
   #:scenario
   #:booking
   #:add-booking

   ;; Baseline and EVM
   #:baseline
   #:baseline-name
   #:baseline-date
   #:baseline-tasks
   #:baseline-task
   #:baseline-task-id
   #:baseline-task-name
   #:baseline-task-start
   #:baseline-task-end
   #:baseline-task-duration
   #:baseline-task-priority
   #:create-baseline
   #:set-project-baseline
   #:project-baseline
   #:calculate-planned-value
   #:calculate-earned-value
   #:calculate-schedule-variance
   #:calculate-spi

   ;; Resource over-allocation
   #:overallocation
   #:overallocation-resource-id
   #:overallocation-date
   #:overallocation-load
   #:detect-resource-overallocations
   #:calculate-resource-load

   ;; Working time
   #:calendar
   #:working-hours
   #:working-day-p
   #:working-hours-between

   ;; Error conditions
   #:project-juggler-error
   #:reference-error
   #:circular-dependency-error
   #:scheduling-error
   #:validation-error

   ;; Special variables
   #:*current-project*
   #:*current-task*
   #:*current-namespace*
   #:*current-session*
   #:*namespace-registry*
   #:*project-registry*))
