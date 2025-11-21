# Project Juggler Tutorial

Welcome to Project Juggler! This tutorial will guide you through creating and managing projects step-by-step.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Your First Project](#your-first-project)
3. [Working with Tasks](#working-with-tasks)
4. [Resource Management](#resource-management)
5. [Scheduling and Dependencies](#scheduling-and-dependencies)
6. [Effort-Based Scheduling](#effort-based-scheduling)
7. [Critical Path Analysis](#critical-path-analysis)
8. [Tracking Progress with EVM](#tracking-progress-with-evm)
9. [Generating Reports](#generating-reports)
10. [Session Management](#session-management)
11. [Advanced Features](#advanced-features)

## Getting Started

### Installation

First, ensure you have SBCL and Quicklisp installed:

```bash
# On Ubuntu/Debian
sudo apt-get install sbcl

# Install Quicklisp (if not already installed)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

Then load Project Juggler:

```bash
cd /path/to/project-juggler
sbcl
```

```lisp
;; In SBCL REPL
(push (truename ".") asdf:*central-registry*)
(ql:quickload :project-juggler)
(in-package :project-juggler)
```

## Your First Project

Let's create a simple website project with a few tasks.

### Step 1: Define the Project

```lisp
(defproject website "Company Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 5 31))
```

This creates a project that runs from March 1 to May 31, 2024. The project is automatically stored in `*current-project*`.

### Step 2: Add Resources

```lisp
;; Go back and redefine the project with resources
(defproject website "Company Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 5 31)

  ;; Add a web developer
  (defresource dev "Web Developer"
    :efficiency 1.0
    :rate 100.0)

  ;; Add a designer
  (defresource designer "UI/UX Designer"
    :efficiency 0.9
    :rate 90.0))
```

Resources have:
- **efficiency**: Productivity factor (1.0 = 100% productive)
- **rate**: Cost per hour (optional)

### Step 3: Add Tasks

```lisp
(defproject website "Company Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 5 31)

  (defresource dev "Web Developer"
    :efficiency 1.0
    :rate 100.0)

  (defresource designer "UI/UX Designer"
    :efficiency 0.9
    :rate 90.0)

  ;; Design phase
  (deftask design "Design Mockups"
    :duration (duration 2 :weeks)
    :allocate (designer)
    :priority 900)

  ;; Implementation phase
  (deftask implement "Implement Website"
    :duration (duration 4 :weeks)
    :depends-on (design)
    :allocate (dev)
    :priority 800)

  ;; Testing phase
  (deftask testing "Testing & QA"
    :duration (duration 1 :weeks)
    :depends-on (implement)
    :allocate (dev designer)
    :priority 900)

  ;; Launch milestone
  (deftask launch "Launch Website"
    :milestone t
    :depends-on (testing)))
```

### Step 4: Finalize and Schedule

```lisp
;; Validate the project and resolve all references
(finalize-project *current-project*)

;; Schedule the tasks
(schedule *current-project*)

;; Check the schedule
(let ((design-task (gethash 'design (project-tasks *current-project*)))
      (implement-task (gethash 'implement (project-tasks *current-project*))))
  (format t "Design starts: ~A~%" (task-start design-task))
  (format t "Design ends: ~A~%" (task-end design-task))
  (format t "Implementation starts: ~A~%" (task-start implement-task))
  (format t "Implementation ends: ~A~%" (task-end implement-task)))
```

Output:
```
Design starts: 2024-03-01
Design ends: 2024-03-15
Implementation starts: 2024-03-15
Implementation ends: 2024-04-12
```

## Working with Tasks

### Task Hierarchies

Tasks can have subtasks for better organization:

```lisp
(deftask development "Development Phase"
  :duration (duration 6 :weeks)
  :allocate (dev)

  (deftask frontend "Frontend Development"
    :duration (duration 3 :weeks))

  (deftask backend "Backend Development"
    :duration (duration 3 :weeks))

  (deftask database "Database Setup"
    :duration (duration 2 :weeks)))
```

### Task Properties

```lisp
(deftask complex-task "Complex Task"
  :duration (duration 5 :days)       ; Calendar duration
  :effort (duration 40 :hours)       ; Actual work effort
  :priority 800                      ; 0-1000 (higher = more important)
  :milestone nil                     ; Is this a milestone?
  :start (date 2024 3 15)            ; Fixed start date (optional)
  :depends-on (other-task)           ; Dependencies
  :allocate (dev designer))          ; Allocated resources
```

**Key differences:**
- **duration**: How long the task takes on the calendar
- **effort**: How much work is required (for effort-based scheduling)
- **milestone**: Zero-duration marker (e.g., "Product Launch")

### Milestones

Milestones are zero-duration markers:

```lisp
(deftask requirements-complete "Requirements Signed Off"
  :milestone t
  :depends-on (requirements-gathering))

(deftask next-phase "Next Phase"
  :depends-on (requirements-complete)
  :duration (duration 2 :weeks))
```

## Resource Management

### Defining Resources

```lisp
;; Simple resource
(defresource dev1 "Senior Developer"
  :efficiency 1.2     ; 120% productive
  :rate 120.0)        ; $120/hour

;; Resource with lower efficiency
(defresource junior "Junior Developer"
  :efficiency 0.7     ; 70% productive
  :rate 60.0)

;; Resource hierarchy
(defresource dev-team "Development Team"
  (defresource dev1 "Senior Dev")
  (defresource dev2 "Mid-level Dev")
  (defresource dev3 "Junior Dev"))
```

### Allocating Resources

```lisp
;; Single resource
(deftask task1 "Task 1"
  :duration (duration 5 :days)
  :allocate (dev1))

;; Multiple resources
(deftask task2 "Task 2"
  :duration (duration 3 :days)
  :allocate (dev1 dev2 designer))

;; Optional: Mandatory allocation
(deftask task3 "Task 3"
  :duration (duration 2 :days)
  :allocate (dev1 :mandatory t))
```

### Detecting Over-Allocation

```lisp
;; After scheduling, check for resource conflicts
(finalize-project *current-project*)
(schedule *current-project*)

(let ((overallocations (detect-resource-overallocations *current-project*)))
  (if (null overallocations)
      (format t "No resource conflicts!~%")
      (dolist (oa overallocations)
        (format t "WARNING: ~A is overallocated on ~A~%"
                (overallocation-resource-id oa)
                (overallocation-date oa)))))
```

## Scheduling and Dependencies

### Dependency Types

Currently, Project Juggler supports finish-start dependencies (task B starts after task A finishes):

```lisp
(deftask task-a "Task A"
  :duration (duration 5 :days))

(deftask task-b "Task B"
  :duration (duration 3 :days)
  :depends-on (task-a))

(deftask task-c "Task C"
  :duration (duration 2 :days)
  :depends-on (task-a task-b))  ; Multiple dependencies
```

### Scheduling Process

```lisp
;; 1. Define your project
(defproject my-project "My Project" ...)

;; 2. Finalize (validates and resolves references)
(finalize-project *current-project*)

;; 3. Schedule (assigns dates to tasks)
(schedule *current-project*)

;; 4. Analyze results
(let ((tasks (project-tasks *current-project*)))
  (maphash (lambda (id task)
             (format t "~A: ~A to ~A~%"
                     (task-name task)
                     (task-start task)
                     (task-end task)))
           tasks))
```

### Priority-Based Scheduling

Tasks with higher priority are scheduled first when resources are constrained:

```lisp
(deftask high-priority "Critical Feature"
  :priority 900     ; High priority
  :duration (duration 5 :days)
  :allocate (dev))

(deftask low-priority "Nice-to-Have Feature"
  :priority 300     ; Low priority
  :duration (duration 3 :days)
  :allocate (dev))
```

Priority range: 0-1000 (default: 500)

## Effort-Based Scheduling

Project Juggler supports both duration-based and effort-based scheduling. Effort-based tasks account for resource efficiency to calculate actual duration.

### Duration vs Effort

**Duration-based tasks** have fixed calendar time:
```lisp
;; Meeting takes exactly 2 hours regardless of who attends
(deftask kickoff-meeting "Project Kickoff"
  :duration (duration 2 :hours)
  :allocate (dev designer))
```

**Effort-based tasks** scale with resource efficiency:
```lisp
;; Task requires 10 days of work effort
;; Actual duration depends on resource efficiency
(deftask feature-development "Feature Development"
  :effort (duration 10 :days)
  :allocate (senior-dev))
```

### Resource Efficiency

Resources have an efficiency factor that affects effort-based tasks:

```lisp
(defresource senior-dev "Senior Developer"
  :efficiency 1.5)  ; 50% more productive than baseline

(defresource mid-dev "Mid-level Developer"
  :efficiency 1.0)  ; Baseline productivity

(defresource junior-dev "Junior Developer"
  :efficiency 0.6)  ; 60% as productive as baseline
```

### How Effort Scheduling Works

The formula: **Actual Duration = Effort / Total Resource Efficiency**

#### Example 1: Single Resource

```lisp
(defresource senior-dev "Senior Dev" :efficiency 1.5)

(deftask feature-a "Feature A"
  :effort (duration 10 :days)
  :allocate (senior-dev))

;; Actual duration = 10 / 1.5 = 7 days (rounded up)
```

#### Example 2: Multiple Resources

Resources working together combine their efficiencies:

```lisp
(defresource dev1 "Developer 1" :efficiency 1.0)
(defresource dev2 "Developer 2" :efficiency 1.5)

(deftask pair-programming "Pair Programming Task"
  :effort (duration 20 :days)
  :allocate (dev1 dev2))

;; Total efficiency = 1.0 + 1.5 = 2.5
;; Actual duration = 20 / 2.5 = 8 days
```

#### Example 3: Comparing Efficiency Levels

```lisp
(defproject effort-demo "Effort Scheduling Demo"
  :start (date 2024 6 1)
  :end (date 2024 9 30)

  (defresource senior-dev "Senior Developer" :efficiency 1.5)
  (defresource mid-dev "Mid-level Developer" :efficiency 1.0)
  (defresource junior-dev "Junior Developer" :efficiency 0.6)

  ;; Same 10-day effort, different actual durations
  (deftask task-senior "Task with Senior"
    :effort (duration 10 :days)
    :allocate (senior-dev))
    ;; Actual duration: 7 days

  (deftask task-mid "Task with Mid-level"
    :effort (duration 10 :days)
    :allocate (mid-dev))
    ;; Actual duration: 10 days

  (deftask task-junior "Task with Junior"
    :effort (duration 10 :days)
    :allocate (junior-dev))
    ;; Actual duration: 17 days
)

(finalize-project *current-project*)
(schedule *current-project*)
```

### When to Use Effort vs Duration

Use **duration** for:
- Meetings and reviews
- Waiting periods (approvals, deployments)
- Fixed-time activities

Use **effort** for:
- Development work
- Design tasks
- Any work that scales with team size or skill level

### Best Practices

1. **Set realistic efficiency values** based on team performance data
2. **Account for collaboration overhead** when allocating multiple resources
3. **Use effort for parallelizable work** where adding resources reduces time
4. **Validate estimates** by comparing scheduled vs actual completion times

For a complete example, see `examples/effort-scheduling.lisp`.

## Critical Path Analysis

The critical path is the sequence of tasks that determines the minimum project duration.

### Finding the Critical Path

```lisp
;; After scheduling, calculate the critical path
(finalize-project *current-project*)
(schedule *current-project*)

;; Calculate Early Start/Finish
(forward-pass *current-project*)

;; Calculate Late Start/Finish
(backward-pass *current-project*)

;; Calculate slack for all tasks
(calculate-slack *current-project*)

;; Get critical tasks (slack = 0)
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical Path (~A tasks):~%" (length critical-tasks))
  (dolist (task critical-tasks)
    (format t "  - ~A~%" (task-name task))))
```

### Understanding Slack/Float

Slack (or float) is how much a task can be delayed without affecting the project:

```lisp
(let ((task (gethash 'my-task (project-tasks *current-project*))))
  (format t "Task: ~A~%" (task-name task))
  (format t "Early Start: ~A~%" (task-early-start task))
  (format t "Late Start: ~A~%" (task-late-start task))
  (format t "Slack: ~A days~%" (task-slack task))
  (when (zerop (task-slack task))
    (format t "This task is on the CRITICAL PATH!~%")))
```

- **Slack = 0**: Task is on critical path; any delay delays the project
- **Slack > 0**: Task has some flexibility

## Tracking Progress with EVM

Earned Value Management (EVM) helps track project performance.

### Step 1: Create a Baseline

```lisp
;; After initial scheduling, create a baseline
(finalize-project *current-project*)
(schedule *current-project*)

(let ((baseline (create-baseline *current-project* :name "Original Plan")))
  (set-project-baseline *current-project* baseline)
  (format t "Baseline created with ~A tasks~%"
          (hash-table-count (baseline-tasks baseline))))
```

### Step 2: Update Task Completion

As work progresses, update the completion percentage:

```lisp
;; Task is 50% complete
(setf (task-complete (gethash 'design (project-tasks *current-project*))) 50)

;; Task is 100% complete
(setf (task-complete (gethash 'testing (project-tasks *current-project*))) 100)
```

### Step 3: Calculate EVM Metrics

```lisp
(let* ((status-date (date 2024 3 15))
       (pv (calculate-planned-value *current-project* status-date))
       (ev (calculate-earned-value *current-project*))
       (sv (calculate-schedule-variance *current-project* status-date))
       (spi (calculate-spi *current-project* status-date)))

  (format t "=== EVM Metrics as of ~A ===~%" status-date)
  (format t "Planned Value (PV):  ~,1F%~%" pv)
  (format t "Earned Value (EV):   ~,1F%~%" ev)
  (format t "Schedule Variance:   ~,1F%~%" sv)
  (format t "Schedule Performance Index: ~,2F~%~%" spi)

  (cond
    ((> spi 1.0)
     (format t "✓ Project is AHEAD of schedule!~%"))
    ((< spi 1.0)
     (format t "⚠ Project is BEHIND schedule!~%"))
    (t
     (format t "✓ Project is ON schedule!~%"))))
```

### Understanding EVM Metrics

- **PV (Planned Value)**: What % should be done by now (baseline)
- **EV (Earned Value)**: What % is actually done
- **SV (Schedule Variance)**: EV - PV
  - Positive = ahead of schedule
  - Negative = behind schedule
- **SPI (Schedule Performance Index)**: EV / PV
  - > 1.0 = ahead of schedule
  - < 1.0 = behind schedule
  - = 1.0 = exactly on schedule

## Generating Reports

Project Juggler provides two ways to create reports: the declarative **defreport DSL** (recommended) and manual report creation (for advanced use cases).

### Using the defreport DSL (Recommended)

The defreport macro lets you define reports alongside your tasks within the project definition:

```lisp
(defproject website "Company Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 5 31)

  ;; ... tasks and resources ...

  ;; Define reports as part of the project
  (defreport summary "Project Task Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration :priority)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

  (defreport csv-export "Task Export"
    :type :task
    :format :csv
    :columns (:id :name :start :end :duration))

  (defreport high-priority "High Priority Tasks"
    :type :task
    :format :html
    :columns (:id :name :priority :slack)
    :filter (lambda (task) (> (task-priority task) 800))
    :sort-by (lambda (a b) (> (task-priority a) (task-priority b)))))

;; After scheduling, generate reports by ID
(finalize-project *current-project*)
(schedule *current-project*)

(save-project-report *current-project* 'summary "summary.html")
(save-project-report *current-project* 'csv-export "tasks.csv")
(save-project-report *current-project* 'high-priority "high-priority.html")
```

### defreport Parameters

- **:type** - `:task` or `:resource` (default: `:task`)
- **:format** - `:html` or `:csv` (default: `:html`)
- **:columns** - List of columns to include (e.g., `(:id :name :start :end :duration :priority :slack)`)
- **:filter** - Optional lambda to filter items (e.g., `(lambda (task) (> (task-priority task) 700))`)
- **:sort-by** - Optional lambda to sort items (e.g., `(lambda (a b) (date< (task-start a) (task-start b)))`)

### Working with Project Reports

```lisp
;; List all defined reports
(list-project-reports *current-project*)
;; => (SUMMARY CSV-EXPORT HIGH-PRIORITY)

;; Generate a specific report
(let ((html (generate-project-report *current-project* 'summary)))
  (format t "~A~%" html))

;; Generate all reports at once
(generate-all-reports *current-project* "reports/")
;; Creates reports/summary.html, reports/csv-export.csv, etc.
```

### Filtering Examples

Show only critical path tasks:
```lisp
(defreport critical-only "Critical Path Tasks"
  :type :task
  :format :html
  :columns (:id :name :start :end :slack)
  :filter (lambda (task)
            (and (task-slack task)
                 (zerop (task-slack task))))
  :sort-by (lambda (a b) (date< (task-start a) (task-start b))))
```

Show incomplete tasks:
```lisp
(defreport incomplete "Incomplete Tasks"
  :type :task
  :format :html
  :columns (:id :name :start :end :complete)
  :filter (lambda (task) (< (task-complete task) 100)))
```

### Resource Reports

Generate resource utilization reports:
```lisp
(defreport resources "Resource Utilization"
  :type :resource
  :format :html
  :columns (:id :name :efficiency :rate :criticalness)
  :sort-by (lambda (a b) (> (resource-criticalness a)
                            (resource-criticalness b))))

(save-project-report *current-project* 'resources "resources.html")
```

### Manual Report Creation (Advanced)

For ad-hoc reports or advanced customization, you can create reports manually:

```lisp
;; Create a task report
(let ((report (make-instance 'task-report
                :id 'task-summary
                :title "Task Summary Report"
                :format :html
                :columns '(:id :name :start :end :duration :priority :complete))))

  ;; Generate and save
  (with-open-file (out "report.html"
                       :direction :output
                       :if-exists :supersede)
    (write-string (generate-report report *current-project*) out))

  (format t "Report saved to report.html~%"))
```

#### Manual Filtering and Sorting

```lisp
;; Show only high-priority tasks, sorted by start date
(let ((report (make-instance 'task-report
                :id 'high-priority
                :title "High Priority Tasks"
                :format :html
                :columns '(:id :name :start :end :priority)
                :filter (lambda (task)
                          (> (task-priority task) 700))
                :sort-by (lambda (a b)
                           (date< (task-start a) (task-start b))))))

  (with-open-file (out "high-priority.html"
                       :direction :output
                       :if-exists :supersede)
    (write-string (generate-report report *current-project*) out)))
```

#### Manual CSV Export

```lisp
;; Export to CSV for Excel/Google Sheets
(let ((report (make-instance 'task-report
                :id 'csv-export
                :title "Task Export"
                :format :csv
                :columns '(:id :name :start :end :duration :priority))))

  (with-open-file (out "tasks.csv"
                       :direction :output
                       :if-exists :supersede)
    (write-string (generate-report report *current-project*) out))

  (format t "CSV exported to tasks.csv~%"))
```

### Gantt Chart Data

```lisp
;; Generate structured data for Gantt visualization
(let ((gantt-data (generate-gantt-data *current-project*)))

  ;; Each entry is a plist with task info
  (dolist (entry gantt-data)
    (format t "Task: ~A~%" (getf entry :name))
    (format t "  Start: ~A~%" (getf entry :start))
    (format t "  End: ~A~%" (getf entry :end))
    (format t "  Dependencies: ~A~%~%" (getf entry :dependencies))))
```

## Session Management

### Saving Projects

```lisp
;; Save the current session to a file
(let ((session (make-instance 'session :project *current-project*)))
  (save-session session "my-project.lisp")
  (format t "Project saved!~%"))
```

### Loading Projects

```lisp
;; Load a project from file
(let ((session (load-project-session "my-project.lisp")))
  (setf *current-project* (session-project session))
  (format t "Project loaded: ~A~%" (project-name *current-project*)))
```

The saved file is in Lisp format and can be edited by hand!

### Undo/Redo

```lisp
;; Create a session
(setf *current-session* (make-instance 'session :project *current-project*))

;; Make changes with change tracking
(add-task-to-session 'new-task "New Feature"
                     :duration (duration 5 :days)
                     :allocate '(dev))

;; Oops, made a mistake - undo it
(undo)
(format t "Change undone!~%")

;; Actually, that was right - redo it
(redo)
(format t "Change redone!~%")
```

## Advanced Features

### Working with Dates

```lisp
;; Create dates
(date 2024 3 15)              ; March 15, 2024
(date 2024 3 15 14 30 0)      ; March 15, 2024, 2:30 PM

;; Date arithmetic
(let ((start (date 2024 3 1))
      (offset (duration 2 :weeks)))
  (date+ start offset))        ; => 2024-03-15

;; Date comparison
(date< (date 2024 3 1) (date 2024 3 15))  ; => T
(date= (date 2024 3 1) (date 2024 3 1))   ; => T
```

### Working with Durations

```lisp
;; Create durations
(duration 5 :days)
(duration 3 :weeks)
(duration 40 :hours)
(duration 2 :months)

;; Convert durations
(duration-in-hours (duration 2 :days))    ; => 48
(duration-in-days (duration 3 :weeks))    ; => 21
(duration-in-weeks (duration 14 :days))   ; => 2
```

### Namespaces for Large Projects

For large projects, use namespaces to organize tasks:

```lisp
(in-namespace frontend-ns

  (deftask ui "UI Components"
    :duration (duration 2 :weeks))

  (deftask styles "CSS Styling"
    :duration (duration 1 :weeks)))

(in-namespace backend-ns

  (deftask api "REST API"
    :duration (duration 3 :weeks))

  (deftask database "Database Schema"
    :duration (duration 1 :weeks)))

;; Cross-namespace dependencies
(deftask integration "Integration"
  :depends-on (frontend-ns:ui backend-ns:api))
```

### Validation Errors

Project Juggler validates your project:

```lisp
;; Circular dependencies are detected
(deftask task-a "Task A" :depends-on (task-b))
(deftask task-b "Task B" :depends-on (task-a))

(handler-case
    (finalize-project *current-project*)
  (circular-dependency-error (e)
    (format t "Error: Circular dependency detected!~%")))
```

## Best Practices

1. **Always finalize before scheduling**
   ```lisp
   (finalize-project *current-project*)
   (schedule *current-project*)
   ```

2. **Create baselines after initial scheduling**
   ```lisp
   (let ((baseline (create-baseline *current-project*)))
     (set-project-baseline *current-project* baseline))
   ```

3. **Use meaningful task IDs**
   ```lisp
   ;; Good
   (deftask frontend-implementation "Frontend Implementation" ...)

   ;; Less good
   (deftask t1 "Frontend Implementation" ...)
   ```

4. **Set priorities for resource-constrained projects**
   ```lisp
   (deftask critical-feature "Critical Feature"
     :priority 900 ...)
   ```

5. **Check for over-allocation**
   ```lisp
   (detect-resource-overallocations *current-project*)
   ```

6. **Save your work**
   ```lisp
   (save-session *current-session* "my-project.lisp")
   ```

## Next Steps

- Check out the [examples/](examples/) directory for complete project examples
- Read [PROGRESS.md](PROGRESS.md) for implementation details
- Explore the API reference in [README.md](README.md)
- Try building your own project!

## Troubleshooting

### "Project has no baseline" error

```lisp
;; Create a baseline first
(let ((baseline (create-baseline *current-project*)))
  (set-project-baseline *current-project* baseline))
```

### Tasks not scheduling

```lisp
;; Make sure to finalize first
(finalize-project *current-project*)
(schedule *current-project*)
```

### "Unresolved reference" error

```lisp
;; Make sure referenced tasks/resources are defined before use
(defresource dev "Developer")
(deftask task1 "Task" :allocate (dev))  ; Works

;; This will fail:
(deftask task1 "Task" :allocate (undefined-resource))
```

---

Happy project management! 🎉

For questions and issues, visit the [GitHub repository](https://github.com/yourusername/project-juggler).
