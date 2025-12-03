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

   ;; Resource leveling
   #:calculate-resource-load
   #:detect-resource-overallocations
   #:level-resources
   #:overallocation
   #:overallocation-resource-id
   #:overallocation-resource
   #:overallocation-date
   #:overallocation-load
   #:overallocation-tasks

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
   #:generate-gantt-json
   #:generate-gantt-svg
   #:generate-gantt-html
   #:format-cell
   #:get-auto-filter
   #:gantt-report
   #:critical-path-report
   #:milestone-report
   #:evm-report
   #:simulation-report
   #:risk-report
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
   #:booking-p
   #:booking-resource
   #:booking-task
   #:booking-start
   #:booking-end
   #:booking-amount
   #:booking-duration-hours
   #:booking-duration-days
   #:add-booking
   #:total-booked-hours
   #:bookings-in-range
   #:update-task-completion-from-bookings
   #:task-bookings
   #:resource-bookings

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

   ;; Cost tracking
   #:project-budget
   #:task-fixed-cost
   #:calculate-task-planned-cost
   #:calculate-task-actual-cost
   #:calculate-project-planned-cost
   #:calculate-project-actual-cost
   #:project-budget-remaining
   #:calculate-actual-cost
   #:calculate-earned-value-cost
   #:calculate-cost-variance
   #:calculate-cpi-cost
   #:calculate-bac
   #:calculate-eac
   #:calculate-etc
   #:calculate-vac
   #:generate-cost-report

   ;; Working time calendars
   #:calendar
   #:calendar-p
   #:calendar-id
   #:calendar-name
   #:calendar-working-hours
   #:calendar-holidays
   #:calendar-timezone
   #:working-hours
   #:working-hours-p
   #:working-hours-days
   #:working-hours-start-time
   #:working-hours-end-time
   #:working-hours-per-day
   #:working-day-p
   #:working-hours-between
   #:working-hours-on-date
   #:add-holiday
   #:holiday-p
   #:date-day-of-week

   ;; PERT three-point estimation
   #:pert-estimate
   #:pert-estimate-p
   #:estimate-optimistic
   #:estimate-likely
   #:estimate-pessimistic
   #:task-estimate
   #:pert-expected-duration
   #:pert-standard-deviation
   #:pert-variance
   #:pert-confidence-interval
   #:project-pert-expected-duration
   #:project-pert-variance
   #:project-pert-standard-deviation
   #:project-pert-confidence-interval
   #:probability-of-completion-by
   #:project-probability-of-completion-by
   #:pert-duration-for-scheduling

   ;; What-if scenarios
   #:what-if-scenario
   #:create-scenario
   #:scenario-name
   #:scenario-project
   #:scenario-description
   #:scenario-created-at
   #:scenario-task-modifications
   #:scenario-task-snapshots
   #:scenario-added-dependencies
   #:scenario-removed-dependencies
   #:scenario-scheduled-data
   #:snapshot-task
   #:scenario-modify-task
   #:scenario-add-dependency
   #:scenario-remove-dependency
   #:schedule-scenario
   #:scenario-topological-sort
   #:schedule-scenario-task
   #:scenario-earliest-start
   #:get-scenario-task-data
   #:scenario-end-date
   #:scenario-total-duration
   #:scenario-total-cost
   #:list-scenarios
   #:get-scenario
   #:delete-scenario
   #:compare-scenarios
   #:scenario-summary
   #:scenario-critical-path

   ;; Resource availability
   #:leave
   #:leave-p
   #:leave-type
   #:leave-start
   #:leave-end
   #:leave-description
   #:resource-leaves
   #:resource-daily-limit
   #:resource-weekly-limit
   #:resource-calendar
   #:resource-available-p
   #:resource-on-leave-p
   #:resource-available-hours
   #:effective-available-hours
   #:detect-leave-conflicts
   #:leave-conflict
   #:conflict-resource
   #:conflict-leave
   #:conflict-task
   #:availability-summary
   #:allocation-percent
   #:allocation-resource-percents
   #:get-allocation-percent-for-resource
   #:calculate-effective-efficiency

   ;; Task constraints
   #:task-constraint
   #:task-constraint-p
   #:constraint-type
   #:constraint-date
   #:make-task-constraint
   #:task-start-constraint
   #:task-finish-constraint
   #:apply-start-constraint
   #:apply-finish-constraint
   #:constraint-conflict
   #:conflict-task-id
   #:conflict-description
   #:conflict-constraint-type
   #:conflict-expected-date
   #:conflict-actual-date
   #:detect-constraint-conflicts

   ;; Recurring tasks
   #:recurring-definition
   #:recurring-definition-p
   #:recurring-frequency
   #:recurring-start-date
   #:recurring-end-date
   #:recurring-day
   #:recurring-days
   #:recurring-exceptions
   #:task-recurring
   #:expand-recurring-tasks
   #:expand-recurring-definition
   #:get-recurring-tasks

   ;; Risk register
   #:risk
   #:risk-p
   #:risk-id
   #:risk-name
   #:risk-description
   #:risk-probability
   #:risk-impact
   #:risk-category
   #:risk-status
   #:risk-owner
   #:risk-mitigation
   #:risk-contingency
   #:risk-tasks
   #:risk-schedule-impact
   #:risk-cost-impact-amount
   #:risk-created-at
   #:risk-updated-at
   #:risk-response-history
   #:risk-score
   #:risk-severity
   #:create-risk
   #:get-risk
   #:delete-risk
   #:update-risk
   #:get-task-risks
   #:project-risks
   #:project-risk-register
   #:filter-risks-by-status
   #:filter-risks-by-category
   #:filter-risks-by-min-severity
   #:sort-risks-by-score
   #:risk-schedule-impact-days
   #:risk-register-summary
   #:project-expected-monetary-value
   #:add-risk-response

   ;; Monte Carlo simulation
   #:simulation-trial
   #:trial-number
   #:trial-project-duration
   #:trial-task-durations
   #:trial-task-end-dates
   #:trial-critical-path
   #:trial-end-date
   #:trial-risk-occurrences
   #:simulation-results
   #:simulation-project
   #:simulation-trials
   #:simulation-trial-count
   #:simulation-durations
   #:simulation-mean
   #:simulation-std-dev
   #:simulation-min
   #:simulation-max
   #:simulation-risk-occurrence-counts
   #:sample-pert-duration
   #:run-simulation-trial
   #:run-monte-carlo-simulation
   #:run-risk-simulation
   #:simulation-percentile
   #:simulation-probability-of-completion
   #:simulation-histogram
   #:simulation-risk-occurrences
   #:simulation-summary

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
