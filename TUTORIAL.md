# Project Juggler Tutorial

Welcome to Project Juggler! This tutorial will guide you through creating and managing projects step-by-step.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Your First Project](#your-first-project)
3. [Working with Tasks](#working-with-tasks)
4. [Resource Management](#resource-management)
5. [Scheduling and Dependencies](#scheduling-and-dependencies)
6. [Critical Path Analysis](#critical-path-analysis)
7. [Tracking Progress with EVM](#tracking-progress-with-evm)
8. [Generating Reports](#generating-reports)
9. [Session Management](#session-management)
10. [Advanced Features](#advanced-features)

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

### HTML Reports

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

### Filtering and Sorting

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

### CSV Export

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
