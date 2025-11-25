# Project Juggler - Quick Start Guide

Get started with Project Juggler in 5 minutes!

## Step 1: Install (One Time)

```bash
# Install to Quicklisp local-projects
cd ~/quicklisp/local-projects/
git clone https://github.com/yourusername/project-juggler.git
```

**Or** if you're developing:

```bash
# Symlink your development directory
ln -s /path/to/your/project-juggler ~/quicklisp/local-projects/
```

## Step 2: Verify Installation

```bash
sbcl --eval "(ql:quickload :project-juggler)" --eval "(format t \"✓ Success!~%\")" --quit
```

You should see: `✓ Success!`

## Step 3: Create Your First Project

### Option A: Use the Template (Easiest)

```bash
# Copy the template to your directory
mkdir -p ~/my-projects
cp ~/quicklisp/local-projects/project-juggler/templates/project-template.lisp ~/my-projects/my-first-project.lisp

# Edit it (use your favorite editor)
nano ~/my-projects/my-first-project.lisp

# Run it!
cd ~/my-projects
sbcl --script my-first-project.lisp
```

The template includes everything you need with helpful comments!

### Option B: Start From Scratch

Create `~/my-projects/website.lisp`:

```lisp
;; Load Project Juggler
(ql:quickload :project-juggler :silent t)
(in-package :project-juggler)

;; Define your project
(defproject website "Website Redesign"
  :start (date 2024 3 1)
  :end (date 2024 6 30)

  ;; Add a resource (team member)
  (defresource dev "Developer")

  ;; Add tasks
  (deftask design "Design Phase"
    :duration (duration 2 :weeks)
    :allocate (dev))

  (deftask implement "Implementation"
    :duration (duration 4 :weeks)
    :depends-on (design)
    :allocate (dev))

  ;; Add a report
  (defreport summary "Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end)))

;; Schedule it
(finalize-project *current-project*)
(schedule *current-project*)

;; Show results
(format t "~%Project: ~A~%" (project-name *current-project*))
(format t "Critical Path: ~A tasks~%"
        (length (critical-path *current-project*)))

;; Save report
(save-project-report *current-project* 'summary "report.html")
(format t "✓ Report: report.html~%")
```

Run it:

```bash
cd ~/my-projects
sbcl --script website.lisp
open report.html  # or xdg-open on Linux
```

## Step 4: Interactive Mode (REPL)

```bash
sbcl
```

```lisp
;; Load the library
(ql:quickload :project-juggler)
(in-package :project-juggler)

;; Try it out!
(defproject demo "Demo"
  :start (date 2024 1 1)
  :end (date 2024 12 31)

  (defresource dev "Developer")

  (deftask task1 "First Task"
    :duration (duration 5 :days)
    :allocate (dev)))

(finalize-project *current-project*)
(schedule *current-project*)

;; Query the schedule
(let ((task (gethash 'task1 (project-tasks *current-project*))))
  (format t "~A: ~A to ~A~%"
          (task-name task)
          (task-start task)
          (task-end task)))
```

## What's Next?

### Learn More Features

1. **[TUTORIAL.md](TUTORIAL.md)** - Step-by-step learning guide
2. **[USAGE.md](USAGE.md)** - Real-world usage patterns
3. **[README.md](README.md)** - Complete API reference

### Try the Examples

```bash
cd ~/quicklisp/local-projects/project-juggler

# Simple project (6 tasks)
sbcl --script examples/simple-project.lisp

# Time tracking demo (calendars + bookings)
sbcl --script examples/time-tracking-project.lisp

# Complex project (40+ tasks)
sbcl --script examples/web-application.lisp

# Effort-based scheduling
sbcl --script examples/effort-scheduling.lisp
```

### Explore Features

#### Working Time Calendars

```lisp
;; Define working hours and holidays
(defvar *cal*
  (let ((wh (make-instance 'working-hours
                          :days '(:monday :tuesday :wednesday :thursday :friday)
                          :start-time "09:00"
                          :end-time "17:00"))
        (cal (make-instance 'calendar :id 'company :working-hours wh)))
    (add-holiday cal (date 2024 12 25) "Christmas")
    cal))

;; Check working days
(working-day-p (date 2024 11 18) *cal*)  ; => T (Monday)
(working-day-p (date 2024 11 16) *cal*)  ; => NIL (Saturday)

;; Calculate working hours (skips weekends/holidays)
(working-hours-between (date 2024 11 18) (date 2024 11 25) *cal*)
;; => 40 (5 days * 8 hours)
```

#### Time Tracking with Bookings

```lisp
;; Record actual work
(let ((task (gethash 'my-task (project-tasks *current-project*)))
      (dev (gethash 'dev (project-resources *current-project*))))

  ;; Record 8 hours of work
  (add-booking task dev
               (date 2024 11 18 9 0 0)
               (duration 8 :hours))

  ;; Auto-calculate completion
  (update-task-completion-from-bookings task)
  (format t "Task: ~A% complete~%" (task-complete task)))
```

#### Earned Value Management (EVM)

```lisp
;; Create baseline
(let ((baseline (create-baseline *current-project* :name "Plan")))
  (set-project-baseline *current-project* baseline))

;; Update completion
(setf (task-complete (gethash 'my-task (project-tasks *current-project*))) 50)

;; Calculate metrics
(let ((pv (calculate-planned-value *current-project* (local-time:now)))
      (ev (calculate-earned-value *current-project*))
      (spi (calculate-spi *current-project* (local-time:now))))
  (format t "Planned: ~A%, Earned: ~A%, SPI: ~,2F~%" pv ev spi))
```

## Common Tasks

### Load a Saved Project

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

(setf *current-session* (load-project-session "~/my-projects/my-project-saved.lisp"))
(setf *current-project* (session-project *current-session*))
```

### Update Task Completion

```lisp
;; Manual
(setf (task-complete (gethash 'task1 (project-tasks *current-project*))) 75)

;; Or from bookings
(update-task-completion-from-bookings
  (gethash 'task1 (project-tasks *current-project*)))
```

### Generate Reports

```lisp
;; Quick HTML report
(quick-task-report *current-project*
  :format :html
  :columns '(:id :name :start :end :complete))

;; Quick CSV export
(quick-task-report *current-project*
  :format :csv
  :columns '(:id :name :start :end :duration))
```

### Check Critical Path

```lisp
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical Path (~A tasks):~%" (length critical-tasks))
  (dolist (task critical-tasks)
    (format t "  • ~A (slack: ~A)~%"
            (task-name task)
            (task-slack task))))
```

### Detect Resource Conflicts

```lisp
(let ((overallocations (detect-resource-overallocations *current-project*)))
  (if (null overallocations)
      (format t "✓ No conflicts~%")
      (dolist (oa overallocations)
        (format t "⚠ ~A overallocated on ~A~%"
                (overallocation-resource-id oa)
                (overallocation-date oa)))))
```

## Troubleshooting

### "System not found" error

```bash
# Make sure project-juggler is in local-projects:
ls ~/quicklisp/local-projects/project-juggler

# If not there, install it:
cd ~/quicklisp/local-projects/
git clone https://github.com/yourusername/project-juggler.git
```

### "Package does not exist" error

```lisp
;; Always load before use:
(ql:quickload :project-juggler)
(in-package :project-juggler)
```

### Can't find my project file

```lisp
;; Use absolute paths:
(load-project-session (merge-pathnames "my-project.lisp" (user-homedir-pathname)))

;; Or check if file exists:
(probe-file "~/my-project.lisp")
```

## Get Help

- **Issues**: [GitHub Issues](https://github.com/yourusername/project-juggler/issues)
- **Tutorial**: [TUTORIAL.md](TUTORIAL.md) - Step-by-step guide
- **Usage Guide**: [USAGE.md](USAGE.md) - Real-world patterns
- **API Reference**: [README.md](README.md) - Complete documentation

## Summary

**Installation (once):**
```bash
cd ~/quicklisp/local-projects/ && git clone <repo>
```

**Create projects (anywhere):**
```bash
cp ~/quicklisp/local-projects/project-juggler/templates/project-template.lisp ~/my-project.lisp
# Edit my-project.lisp
sbcl --script ~/my-project.lisp
```

**That's it!** You're managing projects with Project Juggler! 🎉
