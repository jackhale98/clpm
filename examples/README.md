# Project Juggler Examples

This directory contains example projects demonstrating various features of Project Juggler.

## Available Examples

### 1. simple-project.lisp - Website Redesign Project
**Complexity:** Basic
**Demonstrates:**
- Basic task definition with durations
- Resource allocation
- Task dependencies
- Critical path analysis (automatically calculated during scheduling)
- Resource over-allocation detection
- EVM baseline creation
- **defreport DSL** for defining reports within the project
- HTML and CSV report generation

**Run it:**
```bash
cd /path/to/project-juggler
sbcl
```

```lisp
(push (truename ".") asdf:*central-registry*)
(ql:quickload :project-juggler)
(load "examples/simple-project.lisp")
```

**What you'll see:**
- Project timeline
- Task schedule
- Critical path (5 tasks on the critical path)
- Resource allocation analysis
- Generated reports in `examples/` directory

### 2. web-application.lisp - SaaS Platform Development
**Complexity:** Advanced
**Demonstrates:**
- Large multi-phase project (40+ tasks)
- Multiple resource teams
- Complex dependency chains
- Parallel task execution
- Milestones with multiple dependencies
- Resource over-allocation in realistic scenarios
- Complete project lifecycle (planning → deployment → support)
- **defreport DSL** with 5 different reports (task summary, CSV export, critical path, high-priority tasks, resource utilization)
- Filtering and sorting reports with lambda functions

**Run it:**
```bash
cd /path/to/project-juggler
sbcl
```

```lisp
(push (truename ".") asdf:*central-registry*)
(ql:quickload :project-juggler)
(load "examples/web-application.lisp")
```

**What you'll see:**
- Complex project with 36 tasks spanning 6.5 months
- Multiple phases: requirements, design, development, testing, deployment, support
- 5 generated reports demonstrating different defreport configurations
- Critical path analysis across the entire project

### 3. effort-scheduling.lisp - Effort-Based Scheduling Demo
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

**web-application.lisp generates:**
- **`web-application-report.html`** - Complete task summary
- **`web-application-tasks.csv`** - CSV export with all tasks
- **`web-application-critical.html`** - Critical path analysis
- **`web-application-high-priority.html`** - High-priority tasks (priority > 900)
- **`web-application-resources.html`** - Resource utilization report

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

  ;; Define reports using defreport DSL
  (defreport summary "Task Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b)))))

;; Schedule and analyze
(finalize-project *current-project*)
(schedule *current-project*)

;; Display results
(format t "Project: ~A~%" (project-name *current-project*))
(format t "Tasks: ~A~%" (hash-table-count (project-tasks *current-project*)))

;; Generate reports
(save-project-report *current-project* 'summary "my-project-summary.html")
(format t "Report saved to my-project-summary.html~%")
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
