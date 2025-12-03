# Project Juggler Examples

This directory contains example projects demonstrating various features of Project Juggler.

## Enhanced Reporting DSL

All examples now use the **enhanced defreport DSL** with powerful new report types:

### New Report Types

| Type | Description |
|------|-------------|
| `:gantt` | Visual Gantt chart (HTML, SVG, or JSON) |
| `:critical-path` | Auto-filtered critical path tasks |
| `:milestone` | Auto-filtered milestones only |
| `:evm` | Earned Value Management metrics |
| `:simulation` | Monte Carlo simulation results |
| `:risk` | Risk register with filtering/sorting |

### Auto-Filters (for `:type :task`)

| Filter | Description |
|--------|-------------|
| `:critical` | Tasks with zero slack |
| `:scheduled` | Only scheduled tasks |
| `:milestones` | Only milestone tasks |
| `:incomplete` | Tasks < 100% complete |
| `:high-priority` | Priority > 800 |
| `:overdue` | Past end date and incomplete |

### Quick Examples

```lisp
;; Gantt chart as HTML with embedded SVG
(defreport gantt "Project Timeline"
  :type :gantt
  :format :html
  :width 1200)

;; Critical path report (automatic filtering)
(defreport critical "Critical Path"
  :type :critical-path
  :format :html
  :columns (:name :start :end :slack))

;; Monte Carlo simulation report
(defreport simulation "Schedule Risk Analysis"
  :type :simulation
  :format :html
  :trials 5000
  :percentiles (10 50 75 90 95)
  :include-histogram t)

;; Risk register sorted by score
(defreport risks "Risk Register"
  :type :risk
  :format :html
  :columns (:name :probability :impact :score)
  :sort-by :score-desc)
```

---

## Available Examples

### 1. time-tracking-project.lisp - Mobile App with Calendar & Bookings
**Complexity:** Intermediate
**Demonstrates:**
- **Working time calendars** with holidays and weekends
- **Actual time tracking** with bookings
- Auto-calculated task completion from bookings
- Calendar-aware working hours calculations
- EVM integration with real booking data
- Resource utilization tracking
- Holiday impact on project schedules

**Run it:**
```bash
cd /path/to/project-juggler
sbcl --script examples/time-tracking-project.lisp
```

**What you'll see:**
- Complete workflow from calendar setup to time tracking
- Week-by-week booking simulation
- Automatic task completion calculation (76 hours booked / 80 planned = 95%)
- EVM metrics based on actual work
- Calendar-aware analysis (skipping weekends and Thanksgiving)
- Resource utilization report
- Generated HTML report

**Key Concepts:**
- `working-hours`: Define Mon-Fri, 9-5 working schedule
- `calendar`: Track holidays and calculate realistic durations
- `add-booking`: Record actual work by resource and task
- `update-task-completion-from-bookings`: Auto-calculate % complete
- Integration with EVM for accurate progress tracking

### 2. simple-project.lisp - Website Redesign Project
**Complexity:** Basic
**Demonstrates:**
- Basic task definition with durations
- Resource allocation
- Task dependencies
- Critical path analysis (automatically calculated during scheduling)
- Resource over-allocation detection
- EVM baseline creation
- **Enhanced defreport DSL** with new report types:
  - `:type :critical-path` - automatic critical path filtering
  - `:type :milestone` - milestone reports
  - `:type :gantt` - visual Gantt charts (HTML, SVG, JSON)
  - `:auto-filter :high-priority` - filter by priority

**Run it:**
```bash
cd /path/to/project-juggler
sbcl --script examples/simple-project.lisp
```

**What you'll see:**
- Project timeline
- Task schedule
- Critical path (5 tasks on the critical path)
- Resource allocation analysis
- **7 generated reports** including Gantt charts

### 3. web-application.lisp - SaaS Platform Development
**Complexity:** Advanced
**Demonstrates:**
- Large multi-phase project (40+ tasks)
- Multiple resource teams
- Complex dependency chains
- Parallel task execution
- Milestones with multiple dependencies
- Resource over-allocation in realistic scenarios
- Complete project lifecycle (planning → deployment → support)
- **Enhanced defreport DSL** with 10 different report types:
  - `:type :critical-path` - critical path tasks
  - `:type :milestone` - project milestones
  - `:type :gantt` - SVG and HTML Gantt charts
  - `:type :evm` - Earned Value Management
  - `:auto-filter :high-priority` - priority filtering
  - `:auto-filter :incomplete` - incomplete tasks

**Run it:**
```bash
cd /path/to/project-juggler
sbcl --script examples/web-application.lisp
```

**What you'll see:**
- Complex project with 36 tasks spanning 6.5 months
- Multiple phases: requirements, design, development, testing, deployment, support
- **10 generated reports** including Gantt charts, EVM, and milestones
- Critical path analysis across the entire project

### 4. effort-scheduling.lisp - Effort-Based Scheduling Demo
**Complexity:** Intermediate
**Demonstrates:**
- **Effort-based scheduling** with resource efficiency
- How resource efficiency affects task duration
- Single resource efficiency examples (senior, mid-level, junior developers)
- Multiple resources combining efficiencies (pair programming)
- Comparing identical effort with different resource skill levels
- The formula: `Actual Duration = Effort / Total Resource Efficiency`

**Key Concepts:**
- Senior developer (1.5 efficiency) completes 10-day effort in 7 days
- Junior developer (0.6 efficiency) completes 10-day effort in 17 days
- Two resources working together combine their efficiencies

**Run it:**
```bash
cd /path/to/project-juggler
sbcl
```

```lisp
(push (truename ".") asdf:*central-registry*)
(ql:quickload :project-juggler)
(load "examples/effort-scheduling.lisp")
```

**What you'll see:**
- Comparison of how different resource efficiencies affect actual task duration
- Examples of pair programming where multiple resources combine efficiencies
- Real-world scenarios showing why senior developers complete work faster

### 5. monte-carlo-example.lisp - Schedule Risk Analysis
**Complexity:** Advanced
**Demonstrates:**
- PERT three-point estimation (optimistic, likely, pessimistic)
- Monte Carlo simulation for schedule uncertainty
- Risk register with probability and impact
- Statistical analysis (percentiles, histograms, confidence intervals)
- **Enhanced defreport DSL** with new simulation/risk types:
  - `:type :simulation` - Monte Carlo analysis with percentiles
  - `:type :risk` - Risk register reports with sorting/filtering
  - `:type :critical-path` - Critical path analysis
  - `:type :gantt` - Visual Gantt charts

**Run it:**
```bash
cd /path/to/project-juggler
sbcl --script examples/monte-carlo-example.lisp
```

**What you'll see:**
- Individual task PERT analysis
- 10,000-trial Monte Carlo simulation
- Percentile analysis (P10, P50, P75, P90, P95)
- Risk occurrence frequency analysis
- Duration distribution histograms
- Scheduling recommendations at different confidence levels
- **5 generated reports** including simulation and risk registers

**Key Concepts:**
- `(deftask ... :estimate (:optimistic X :likely Y :pessimistic Z))` - PERT estimates
- `(create-risk project id name :probability P :impact I ...)` - Risk registration
- `(defreport ... :type :simulation :trials 5000 ...)` - Auto simulation reports
- `(defreport ... :type :risk :sort-by :score-desc ...)` - Risk register reports

## How to Run Examples

### Method 1: Interactive REPL

```bash
sbcl
```

```lisp
;; Load the project
(push (truename "/path/to/project-juggler") asdf:*central-registry*)
(ql:quickload :project-juggler)

;; Run an example
(load "examples/simple-project.lisp")

;; Now the project is in *current-project* and you can interact with it
(hash-table-count (project-tasks *current-project*))  ; => 6

;; Update task completion
(setf (task-complete (gethash 'requirements
                              (project-tasks *current-project*))) 100)

;; Calculate EVM metrics
(let ((pv (calculate-planned-value *current-project* (local-time:now)))
      (ev (calculate-earned-value *current-project*)))
  (format t "Planned: ~A%, Actual: ~A%~%" pv ev))
```

### Method 2: Direct Load

```bash
sbcl --load examples/simple-project.lisp
```

### Method 3: From Command Line

```bash
sbcl --non-interactive \
  --eval "(require :asdf)" \
  --eval "(push (truename \".\") asdf:*central-registry*)" \
  --eval "(ql:quickload :project-juggler :silent t)" \
  --eval "(load \"examples/simple-project.lisp\")"
```

## Understanding the Output

### Project Timeline
Shows the overall project start/end dates and task count.

### Task Schedule
Lists each task with its start and end dates.

### Critical Path Analysis
Identifies tasks with zero slack - delays in these tasks will delay the entire project.

Example output:
```
Critical path (5 tasks):

• Deployment & Launch (Slack: 0 days)
• Integration & Testing (Slack: 0 days)
• Frontend Development (Slack: 0 days)
• UI/UX Design (Slack: 0 days)
• Requirements Gathering (Slack: 0 days)
```

### Resource Allocation
Shows if any resources are over-allocated (assigned to multiple tasks at the same time).

### Baseline
A snapshot of the initial plan for tracking progress.

### Generated Files

After running an example, you'll find:

**simple-project.lisp generates:**
- **`simple-project-report.html`** - Task summary HTML report
- **`simple-project-tasks.csv`** - CSV export for Excel/Google Sheets
- **`simple-project-critical.html`** - Critical path tasks only
- **`simple-project-milestones.html`** - Project milestones
- **`simple-project-gantt.html`** - Visual Gantt chart
- **`simple-project-gantt.json`** - Gantt data for external tools
- **`simple-project-priority.html`** - High priority tasks

**web-application.lisp generates:**
- **`web-application-report.html`** - Complete task summary
- **`web-application-tasks.csv`** - CSV export with all tasks
- **`web-application-critical.html`** - Critical path analysis
- **`web-application-priority.html`** - High-priority tasks
- **`web-application-resources.html`** - Resource utilization report
- **`web-application-milestones.html`** - Project milestones
- **`web-application-gantt.html`** - Visual Gantt chart (HTML)
- **`web-application-gantt.svg`** - Gantt chart (SVG)
- **`web-application-evm.html`** - EVM status report
- **`web-application-incomplete.html`** - Incomplete tasks

**monte-carlo-example.lisp generates:**
- **`monte-carlo-simulation.html`** - Monte Carlo percentile analysis
- **`monte-carlo-risks.html`** - Risk register sorted by score
- **`monte-carlo-open-risks.html`** - Active risks only
- **`monte-carlo-critical.html`** - Critical path tasks
- **`monte-carlo-gantt.html`** - Visual Gantt chart

**effort-scheduling.lisp generates:**
- **`effort-scheduling-report.html`** - Shows actual vs estimated durations for effort-based tasks

## Experimenting with Examples

After loading an example, try these commands:

### View a Specific Task
```lisp
(let ((task (gethash 'frontend (project-tasks *current-project*))))
  (format t "Task: ~A~%" (task-name task))
  (format t "Start: ~A~%" (task-start task))
  (format t "End: ~A~%" (task-end task))
  (format t "Duration: ~A days~%"
          (duration-in-days (task-duration task))))
```

### Update Task Completion
```lisp
;; Mark requirements as 50% complete
(setf (task-complete (gethash 'requirements
                              (project-tasks *current-project*))) 50)
```

### Calculate EVM Metrics
```lisp
(let* ((status-date (local-time:now))
       (pv (calculate-planned-value *current-project* status-date))
       (ev (calculate-earned-value *current-project*))
       (sv (calculate-schedule-variance *current-project* status-date))
       (spi (calculate-spi *current-project* status-date)))
  (format t "Planned Value: ~,1F%~%" pv)
  (format t "Earned Value: ~,1F%~%" ev)
  (format t "Schedule Variance: ~,1F%~%" sv)
  (format t "Schedule Performance Index: ~,2F~%"  spi)
  (cond
    ((> spi 1.0) (format t "Status: AHEAD of schedule~%"))
    ((< spi 1.0) (format t "Status: BEHIND schedule~%"))
    (t (format t "Status: ON schedule~%"))))
```

### List All Tasks
```lisp
(maphash (lambda (id task)
           (format t "~A: ~A~%" id (task-name task)))
         (project-tasks *current-project*))
```

### Save the Project
```lisp
(let ((session (make-instance 'session :project *current-project*)))
  (save-session session "my-modified-project.lisp"))
```

## Creating Your Own Examples

1. Copy `simple-project.lisp` as a template
2. Modify the project definition
3. Add your own tasks and resources
4. Run it to see the results

### Example Template

```lisp
(in-package :project-juggler)

(defproject my-project "My Project"
  :start (date 2024 3 1)
  :end (date 2024 12 31)

  ;; Define resources
  (defresource person1 "Person 1"
    :efficiency 1.0
    :rate 100.0)

  ;; Define tasks
  (deftask task1 "First Task"
    :duration (duration 1 :weeks)
    :allocate (person1)
    :priority 900)

  (deftask task2 "Second Task"
    :duration (duration 2 :weeks)
    :depends-on (task1)
    :allocate (person1))

  (deftask milestone1 "Phase 1 Complete"
    :milestone t
    :depends-on (task2))

  ;; Define reports using enhanced defreport DSL
  (defreport summary "Task Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration))

  ;; NEW: Critical path report (auto-filters to slack=0)
  (defreport critical "Critical Path"
    :type :critical-path
    :format :html
    :columns (:name :start :end :slack))

  ;; NEW: Gantt chart
  (defreport gantt "Project Timeline"
    :type :gantt
    :format :html
    :width 1000)

  ;; NEW: Milestones only
  (defreport milestones "Milestones"
    :type :milestone
    :format :html
    :columns (:name :start)))

;; Schedule and analyze
(finalize-project *current-project*)
(schedule *current-project*)

;; Display results
(format t "Project: ~A~%" (project-name *current-project*))
(format t "Tasks: ~A~%" (hash-table-count (project-tasks *current-project*)))

;; Generate reports
(save-project-report *current-project* 'summary "my-project-summary.html")
(save-project-report *current-project* 'critical "my-project-critical.html")
(save-project-report *current-project* 'gantt "my-project-gantt.html")
(save-project-report *current-project* 'milestones "my-project-milestones.html")
(format t "Reports saved!~%")
```

## Troubleshooting

### "Project has no baseline" Error
Create a baseline before calculating EVM metrics:
```lisp
(let ((baseline (create-baseline *current-project*)))
  (set-project-baseline *current-project* baseline))
```

### "Task has neither duration nor effort" Error
All tasks must have either `:duration` or `:effort`:
```lisp
(deftask mytask "My Task"
  :duration (duration 5 :days)  ; Add this
  :allocate (resource1))
```

### "Unresolved reference" Error
Make sure resources are defined before tasks that use them:
```lisp
;; Good - resource defined first
(defresource dev "Developer")
(deftask task1 "Task" :allocate (dev))

;; Bad - task references undefined resource
(deftask task1 "Task" :allocate (dev))
(defresource dev "Developer")
```

## Next Steps

- Read the full [TUTORIAL.md](../TUTORIAL.md) for comprehensive documentation
- Check the [README.md](../README.md) for API reference
- Review [PROGRESS.md](../PROGRESS.md) for implementation details

---

Happy project management! 🎉
