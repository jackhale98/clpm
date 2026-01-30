# CLAPS - Usage Guide

This guide shows how to use CLAPS for project management.

## Installation

### Option 1: Via Quicklisp (Recommended)

```bash
# Clone to your local Quicklisp directory
cd ~/quicklisp/local-projects/
git clone <repository-url> claps

# Or symlink it
ln -s /path/to/claps ~/quicklisp/local-projects/
```

Then from any directory:

```lisp
(ql:quickload :claps)
(in-package :claps)
```

### Option 2: Manual ASDF Configuration

Add to your `~/.sbclrc`:

```lisp
(push #P"/path/to/claps/" asdf:*central-registry*)
```

## Command-Line Interface

CLAPS includes a powerful CLI for quick project analysis without writing code.

### Building the CLI

```bash
./scripts/build-claps.sh
```

### CLI Usage

```bash
# Process project and generate all reports
claps project.lisp

# Quick project overview
claps project.lisp --summary

# Show critical path
claps project.lisp --critical-path

# Show resource utilization
claps project.lisp --resources

# Show resource conflicts
claps project.lisp --overallocations

# Show earned value metrics
claps project.lisp --evm
claps project.lisp --evm --status-date 2024-04-01

# Run Monte Carlo simulation
claps project.lisp --simulate
claps project.lisp --simulate --trials 5000

# List and compare scenarios
claps project.lisp --scenarios
claps project.lisp --compare plan delayed

# JSON output (for scripting/integration)
claps project.lisp --summary --json
claps project.lisp --critical-path --json

# Validate only (no scheduling)
claps project.lisp --validate

# Help
claps --help
```

## Quick Start (REPL)

```lisp
(ql:quickload :claps)
(in-package :claps)

(defproject website "Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 6 30)

  (defresource dev "Developer")
  (defresource designer "Designer")

  (deftask design "Design Phase"
    :duration (duration 2 :weeks)
    :allocate (designer))

  (deftask implement "Implementation"
    :duration (duration 4 :weeks)
    :depends-on (design)
    :allocate (dev))

  (deftask launch "Launch"
    :milestone t
    :depends-on (implement)))

(finalize-project *current-project*)
(schedule *current-project*)

;; View results
(format t "Critical Path: ~A tasks~%" (length (critical-path *current-project*)))
```

## Modular Project Organization

CLAPS supports TaskJuggler-style modular project organization. Split large projects across multiple files for better maintainability.

### Include Functionality

Include other files into your project using the `include` macro:

```lisp
(defproject saas-platform "SaaS Platform"
  :start (date 2024 11 1)
  :end (date 2025 3 31)

  ;; Include resources from separate file
  (include "resources/team.lisp")

  ;; Include tasks with namespace prefix
  ;; All IDs will be prefixed: api -> backend/api
  (include "phases/backend.lisp" :namespace backend)
  (include "phases/frontend.lisp" :namespace frontend)

  ;; Main project integration task
  (deftask integration "System Integration"
    :depends-on (backend/api frontend/ui)
    :allocate (fullstack-dev)))
```

### Namespace Prefixing

When including with `:namespace`, all task and resource IDs are automatically prefixed:

```lisp
;; phases/backend.lisp
(in-package :claps)

(deftask api "REST API"
  :effort (duration 60 :hours))

(deftask auth "Authentication"
  :depends-on (backend/api)  ; Use full prefixed name for deps
  :effort (duration 30 :hours))
```

**Note:** Dependencies within a namespaced file must use the full prefixed name (e.g., `backend/api` not just `api`).

### Convenience Macros

```lisp
;; Semantic aliases for include
(include-resources "team.lisp")           ; Resources file
(include-tasks "tasks.lisp" :namespace x) ; Tasks with namespace
(include-timesheets "nov.lisp")           ; Time tracking
(include-subproject "sub.lisp" :namespace sub) ; Full subproject
```

### Separate Time Tracking

Keep project schedules clean by putting time entries in separate files:

```lisp
;; main.lisp - clean schedule
(defproject my-project "My Project"
  (include "schedule/tasks.lisp")
  (include "timesheets/november.lisp"))  ; Load time entries separately
```

```lisp
;; timesheets/november.lisp
(in-package :claps)

(defbookings
  (backend/api alice (date 2024 11 4) 8)
  (backend/api alice (date 2024 11 5) 8))

(deftimesheet bob
  (frontend/ui (date 2024 11 4) 8)
  (frontend/ui (date 2024 11 5) 6))
```

See `examples/modular-project/` for a complete working example.

## Scenarios (TaskJuggler-style)

CLAPS uses TaskJuggler-style scenarios for what-if analysis and baseline tracking.

### Declaring Scenarios

Scenarios are declared at the project level:

```lisp
(defproject website "Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 6 30)
  :scenarios (plan delayed optimistic)  ; Declare multiple scenarios

  ;; The first scenario (plan) is the baseline

  (deftask backend "Backend Development"
    :effort (duration 20 :days)              ; Default (plan) value
    :delayed/effort (duration 30 :days)      ; Delayed scenario value
    :optimistic/effort (duration 15 :days))) ; Optimistic scenario value
```

### Scenario-Specific Task Values

Use the `/` separator to set values for specific scenarios:

```lisp
(deftask task1 "Task 1"
  :duration (duration 5 :days)           ; Base value (used by plan)
  :delayed/duration (duration 10 :days)  ; Override for delayed scenario
  :effort (duration 40 :hours)
  :delayed/effort (duration 60 :hours))
```

Supported scenario-specific properties:
- `:scenario/effort`
- `:scenario/duration`
- `:scenario/start`
- `:scenario/end`
- `:scenario/complete`

### Accessing Scenario Values

```lisp
;; Get task value for a specific scenario
(task-effort-for-scenario task 'plan)
(task-effort-for-scenario task 'delayed)
(task-duration-for-scenario task 'optimistic)

;; Get all scheduled values for a scenario
(task-scheduled-values-for-scenario task 'plan)
;; => (:start ... :end ... :duration ... :effort ... :complete ...)
```

### Comparing Scenarios

```lisp
;; Compare two scenarios across the whole project
(compare-scenarios *current-project* 'plan 'delayed)
;; => (:duration-1 45 :duration-2 60 :effort-1 ... :effort-2 ... :end-1 ... :end-2 ...)

;; Compare a single task between scenarios
(compare-task-scenarios task 'plan 'delayed)
;; => (:duration-1 5 :duration-2 10 :effort-1 ... :effort-2 ...)

;; Get scenario summary statistics
(scenario-summary *current-project* 'plan)
;; => (:total-duration 45 :total-effort 120 :end-date ... :task-count 10)
```

Or via CLI:
```bash
claps project.lisp --compare plan delayed
claps project.lisp --compare plan delayed --json
```

### Baseline (First Scenario)

The first declared scenario is automatically the baseline:

```lisp
(baseline-scenario-id *current-project*)
;; => PLAN

(baseline-scenario *current-project*)
;; => #<SCENARIO PLAN>
```

### Listing Scenarios

```lisp
(list-scenarios *current-project*)
;; => (PLAN DELAYED OPTIMISTIC)

(get-scenario *current-project* 'delayed)
;; => #<SCENARIO DELAYED>
```

Or via CLI:
```bash
claps project.lisp --scenarios
```

### Adding Scenarios Dynamically

You can create new scenarios (baselines) at any point:

```lisp
;; Create a snapshot of current project state as a new scenario
(add-scenario *current-project* 'revised "Revised Plan")

;; Modify values in the new scenario
(set-scenario-value *current-project* 'revised 'backend :duration (duration 25 :days))

;; Copy an existing scenario
(copy-scenario *current-project* 'plan 'plan-v2 "Plan Version 2")

;; Remove a scenario (cannot remove the baseline/first scenario)
(remove-scenario *current-project* 'revised)
```

## Summary Tasks (Phases)

Summary tasks are parent tasks that contain subtasks. Their values are automatically aggregated from children:

```lisp
(deftask phase1 "Phase 1: Design"
  (deftask ui-design "UI Design"
    :duration (duration 2 :weeks)
    :complete 100)
  (deftask ux-research "UX Research"
    :duration (duration 1 :weeks)
    :complete 50))

;; After scheduling, aggregate summary task values
(aggregate-all-summary-tasks *current-project*)

;; Summary task now has:
;; - Duration spanning from first subtask start to last subtask end
;; - Effort = sum of subtask efforts
;; - Complete = weighted average of subtask completion

;; Get progress info for a summary task
(summary-task-progress (gethash 'phase1 (project-tasks *current-project*)))
;; => (:total-subtasks 2 :leaf-tasks 2 :completed 1 :in-progress 1 :not-started 0 ...)
```

## Reporting

### Report Types

| Type | Description | Formats |
|------|-------------|---------|
| `:task` | Standard task report | :html, :csv |
| `:resource` | Resource report | :html, :csv |
| `:gantt` | Visual Gantt chart | :html, :svg, :json |
| `:critical-path` | Critical path tasks | :html, :csv |
| `:milestone` | Milestones only | :html, :csv |
| `:evm` | Earned Value Management | :html |
| `:simulation` | Monte Carlo results | :html |
| `:risk` | Risk register | :html, :csv |
| `:comparison` | Scenario comparison | :html, :csv |

### Defining Reports

```lisp
(defreport summary "Summary"
  :type :task
  :format :html
  :columns (:id :name :start :end :duration))

(defreport timeline "Project Timeline"
  :type :gantt
  :format :html
  :width 1000)

(defreport critical "Critical Path"
  :type :critical-path
  :format :html
  :columns (:name :start :end :slack))
```

### Generating Reports

```lisp
;; Save a single report
(save-project-report *current-project* 'summary "report.html")

;; Generate all reports
(generate-all-reports *current-project* "output/")
```

Or via CLI:
```bash
claps project.lisp --output-dir ./reports
claps project.lisp --report summary
```

## Earned Value Management (EVM)

EVM calculations use the baseline scenario (first scenario) by default:

```lisp
;; Calculate EVM metrics
(calculate-earned-value *current-project*)
(calculate-planned-value *current-project* (date 2024 4 1))
(calculate-schedule-variance *current-project* (date 2024 4 1))
(calculate-spi *current-project* (date 2024 4 1))

;; Use a specific scenario
(calculate-earned-value *current-project* :scenario 'delayed)
```

Or via CLI:
```bash
claps project.lisp --evm
claps project.lisp --evm --status-date 2024-04-01
claps project.lisp --evm --json
```

## Monte Carlo Simulation

For PERT-based schedule risk analysis:

```lisp
(deftask risky-task "Risky Task"
  :estimate (:optimistic (duration 5 :days)
             :likely (duration 10 :days)
             :pessimistic (duration 20 :days)))

;; Run simulation
(let ((results (run-monte-carlo-simulation *current-project* :trials 10000)))
  (format t "P50 completion: ~A~%" (simulation-percentile results 50))
  (format t "P90 completion: ~A~%" (simulation-percentile results 90)))
```

Or via CLI:
```bash
claps project.lisp --simulate
claps project.lisp --simulate --trials 10000
claps project.lisp --simulate --json
```

## Resource Management

### Defining Resources

```lisp
(defresource alice "Alice"
  :efficiency 1.5
  :rate 150.0)

(defresource bob "Bob"
  :efficiency 1.0
  :rate 100.0
  :leaves ((date 2024 3 15) (date 2024 3 22)))  ; Vacation
```

### Allocation

```lisp
(deftask task1 "Task 1"
  :effort (duration 40 :hours)
  :allocate (alice bob))                    ; Both assigned

(deftask task2 "Task 2"
  :effort (duration 20 :hours)
  :allocate ((alice :percent 50)))          ; Alice at 50%
```

### Detecting Overallocation

```lisp
(detect-resource-overallocations *current-project*)
```

Or via CLI:
```bash
claps project.lisp --resources
claps project.lisp --overallocations
```

## Dependencies

### Dependency Types

```lisp
(deftask t2 "Task 2"
  :depends-on (t1))                              ; Finish-to-Start (default)

(deftask t2 "Task 2"
  :depends-on ((t1 :type :ss)))                  ; Start-to-Start

(deftask t2 "Task 2"
  :depends-on ((t1 :type :ff)))                  ; Finish-to-Finish

(deftask t2 "Task 2"
  :depends-on ((t1 :lag (duration 2 :days))))    ; With 2-day lag
```

## Constraints

```lisp
(deftask milestone "Milestone"
  :start-constraint (:snet (date 2024 6 1)))     ; Start No Earlier Than

(deftask deadline "Deadline Task"
  :finish-constraint (:fnlt (date 2024 12 31)))  ; Finish No Later Than
```

Constraint types:
- `:snet` - Start No Earlier Than
- `:snlt` - Start No Later Than
- `:mso` - Must Start On
- `:fnet` - Finish No Earlier Than
- `:fnlt` - Finish No Later Than
- `:mfo` - Must Finish On

## Time Tracking

```lisp
;; Record work
(add-booking task resource
             (date 2024 3 15 9 0 0)
             (duration 8 :hours))

;; Update completion from bookings
(update-task-completion-from-bookings task)
```

## Best Practices

1. **Use scenarios for what-if analysis** - Instead of duplicating projects, use scenarios to model different outcomes
2. **First scenario is the baseline** - Use `plan` or `baseline` as your first scenario
3. **Version control your project files** - Project files are just `.lisp` files
4. **Use effort for work, duration for calendar time** - They schedule differently
5. **Use the CLI for quick analysis** - `claps project.lisp --summary` for instant feedback

## Example Project

```lisp
(ql:quickload :claps)
(in-package :claps)

(defproject mobile-app "Mobile App Development"
  :start (date 2024 3 1)
  :end (date 2024 9 30)
  :scenarios (plan delayed)

  ;; Resources
  (defresource dev1 "Senior Developer" :rate 150.0)
  (defresource dev2 "Junior Developer" :rate 80.0)
  (defresource designer "UI Designer" :rate 120.0)

  ;; Phase 1: Design
  (deftask design "UI/UX Design"
    :duration (duration 3 :weeks)
    :delayed/duration (duration 4 :weeks)
    :allocate (designer))

  ;; Phase 2: Development
  (deftask backend "Backend Development"
    :effort (duration 200 :hours)
    :delayed/effort (duration 280 :hours)
    :depends-on (design)
    :allocate (dev1))

  (deftask frontend "Frontend Development"
    :effort (duration 160 :hours)
    :delayed/effort (duration 200 :hours)
    :depends-on (design)
    :allocate (dev1 dev2))

  ;; Phase 3: Testing & Launch
  (deftask testing "QA Testing"
    :duration (duration 2 :weeks)
    :depends-on (backend frontend)
    :allocate (dev2))

  (deftask launch "App Store Launch"
    :milestone t
    :depends-on (testing)))

(finalize-project *current-project*)
(schedule *current-project*)

;; Compare scenarios
(let ((comparison (compare-scenarios *current-project* 'plan 'delayed)))
  (format t "Plan duration: ~A days~%" (getf comparison :duration-1))
  (format t "Delayed duration: ~A days~%" (getf comparison :duration-2)))
```

Or analyze via CLI:
```bash
claps mobile-app.lisp --summary
claps mobile-app.lisp --critical-path
claps mobile-app.lisp --compare plan delayed
claps mobile-app.lisp --simulate --trials 5000
```
