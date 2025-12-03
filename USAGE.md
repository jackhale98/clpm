# Project Juggler - Usage Guide

This guide shows how to use Project Juggler for project management.

## Installation

### Option 1: Via Quicklisp (Recommended)

```bash
# Clone to your local Quicklisp directory
cd ~/quicklisp/local-projects/
git clone <repository-url> project-juggler

# Or symlink it
ln -s /path/to/project-juggler ~/quicklisp/local-projects/
```

Then from any directory:

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)
```

### Option 2: Manual ASDF Configuration

Add to your `~/.sbclrc`:

```lisp
(push #P"/path/to/project-juggler/" asdf:*central-registry*)
```

## Quick Start

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

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

## Scenarios (TaskJuggler-style)

Project Juggler uses TaskJuggler-style scenarios for what-if analysis and baseline tracking.

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

## Monte Carlo Simulation

For PERT-based schedule risk analysis:

```lisp
(deftask risky-task "Risky Task"
  :estimate (:optimistic (duration 5 :days)
             :likely (duration 10 :days)
             :pessimistic (duration 20 :days)))

;; Run simulation
(let ((results (run-monte-carlo *current-project* :trials 10000)))
  (format t "P50 completion: ~A~%" (percentile results 50))
  (format t "P90 completion: ~A~%" (percentile results 90)))
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

## Example Project

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

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
