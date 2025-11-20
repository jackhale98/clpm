# Project Juggler Examples

This directory contains example projects demonstrating various features of Project Juggler.

## Available Examples

### 1. simple-project.lisp - Website Redesign Project
**Complexity:** Basic
**Demonstrates:**
- Basic task definition with durations
- Resource allocation
- Task dependencies
- Critical path analysis
- Resource over-allocation detection
- EVM baseline creation
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
- Resource over-allocation in realistic scenarios
- Complete project lifecycle (planning → deployment → support)

**Note:** This example has a known limitation with milestone scheduling when dependencies are in parallel. See "Known Limitations" below.

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

- **`simple-project-report.html`** - HTML report you can open in a browser
- **`simple-project-tasks.csv`** - CSV export for Excel/Google Sheets

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
    :allocate (person1)))

;; Schedule and analyze
(finalize-project *current-project*)
(schedule *current-project*)

;; Display results
(format t "Project: ~A~%" (project-name *current-project*))
(format t "Tasks: ~A~%" (hash-table-count (project-tasks *current-project*)))
```

## Known Limitations

### Milestones with Parallel Dependencies
Currently, milestones that depend on multiple parallel tasks may not schedule correctly. This is a known limitation being addressed.

**Workaround:** Add an intermediate task that depends on the parallel tasks, then have the milestone depend on that task.

Instead of:
```lisp
(deftask milestone "Milestone"
  :milestone t
  :depends-on (task1 task2))  ; task1 and task2 run in parallel
```

Use:
```lisp
(deftask integration "Integration"
  :duration (duration 1 :days)
  :depends-on (task1 task2))

(deftask milestone "Milestone"
  :milestone t
  :depends-on (integration))
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
