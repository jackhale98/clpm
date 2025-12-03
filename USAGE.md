# Project Juggler - Real-World Usage Guide

This guide shows how to use Project Juggler in your own projects, outside of the library directory.

## Installation

### Option 1: Via Quicklisp (Recommended - when published)

Once published to Quicklisp:

```bash
sbcl
```

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)
```

### Option 2: Local Development Installation

For development or before Quicklisp publication:

```bash
# Clone to your local Quicklisp directory
cd ~/quicklisp/local-projects/
git clone https://github.com/yourusername/project-juggler.git

# Or symlink it
ln -s /path/to/project-juggler ~/quicklisp/local-projects/
```

Now from **any directory**:

```bash
sbcl
```

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)
```

### Option 3: Manual ASDF Configuration

Add to your `~/.sbclrc` or `~/.config/common-lisp/source-registry.conf.d/projects.conf`:

```lisp
;; In ~/.sbclrc
(push #P"/path/to/project-juggler/" asdf:*central-registry*)
```

## Creating Your Own Projects

### Directory Structure

Your projects can live **anywhere**:

```
~/my-projects/
├── company-website.lisp
├── mobile-app.lisp
└── infrastructure-upgrade.lisp
```

### Example: Standalone Project File

Create `~/my-projects/website-redesign.lisp`:

```lisp
;;;; website-redesign.lisp
;;;; My company's website redesign project

;; Load the library (works from any directory!)
(ql:quickload :project-juggler :silent t)

(in-package :project-juggler)

;; Define your project
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
    :depends-on (implement))

  ;; Enhanced defreport DSL - define multiple report types
  (defreport summary "Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration))

  ;; Gantt chart report
  (defreport timeline "Project Timeline"
    :type :gantt
    :format :html
    :width 1000)

  ;; Critical path report (auto-filters to slack=0)
  (defreport critical "Critical Path"
    :type :critical-path
    :format :html
    :columns (:name :start :end :slack))

  ;; Milestone report
  (defreport milestones "Milestones"
    :type :milestone
    :format :html
    :columns (:name :start)))

;; Schedule and analyze
(finalize-project *current-project*)
(schedule *current-project*)

;; Display results
(format t "~%Project: ~A~%" (project-name *current-project*))
(format t "Critical Path: ~A tasks~%"
        (length (critical-path *current-project*)))

;; Save reports
(save-project-report *current-project* 'summary "website-report.html")
(save-project-report *current-project* 'timeline "website-gantt.html")
(save-project-report *current-project* 'critical "website-critical.html")
(save-project-report *current-project* 'milestones "website-milestones.html")
(format t "Reports saved!~%")
```

### Run From Anywhere

```bash
# From your project directory
cd ~/my-projects/
sbcl --script website-redesign.lisp

# Or from any other directory
sbcl --script ~/my-projects/website-redesign.lisp
```

## Enhanced Reporting DSL

The `defreport` macro supports multiple report types for common project management needs. No more writing custom loops or maphash functions for common reports!

### Report Types

| Type | Description | Formats |
|------|-------------|---------|
| `:task` | Standard task report | :html, :csv |
| `:resource` | Resource report | :html, :csv |
| `:gantt` | Visual Gantt chart | :html, :svg, :json |
| `:critical-path` | Auto-filtered to critical path tasks | :html, :csv |
| `:milestone` | Auto-filtered to milestones only | :html, :csv |
| `:evm` | Earned Value Management metrics | :html |
| `:simulation` | Monte Carlo simulation results | :html |
| `:risk` | Risk register | :html, :csv |

### Auto-Filters

For `:type :task` reports, use `:auto-filter` instead of writing lambda functions:

| Filter | Description |
|--------|-------------|
| `:critical` | Tasks with zero slack |
| `:scheduled` | Only scheduled tasks |
| `:milestones` | Only milestone tasks |
| `:incomplete` | Tasks < 100% complete |
| `:high-priority` | Priority > 800 |
| `:overdue` | Past end date and incomplete |

### Examples

```lisp
;; Gantt chart as HTML with embedded SVG
(defreport gantt "Project Timeline"
  :type :gantt
  :format :html
  :width 1200
  :height 600)

;; Gantt chart as standalone SVG
(defreport gantt-svg "Timeline SVG"
  :type :gantt
  :format :svg
  :width 1000)

;; Gantt data as JSON for external tools
(defreport gantt-json "Timeline Data"
  :type :gantt
  :format :json)

;; Critical path report (automatic filtering)
(defreport critical "Critical Path"
  :type :critical-path
  :format :html
  :columns (:name :start :end :slack))

;; Milestone report
(defreport milestones "Project Milestones"
  :type :milestone
  :format :html
  :columns (:name :start))

;; EVM status report
(defreport evm "EVM Status"
  :type :evm
  :format :html
  :columns (:name :pv :ev :sv)
  :include-summary t)

;; Monte Carlo simulation report
(defreport simulation "Schedule Risk Analysis"
  :type :simulation
  :format :html
  :trials 5000
  :percentiles (10 50 75 90 95)
  :include-histogram t)

;; Risk register sorted by risk score
(defreport risks "Risk Register"
  :type :risk
  :format :html
  :columns (:name :probability :impact :score :severity)
  :sort-by :score-desc)

;; Open risks only
(defreport open-risks "Active Risks"
  :type :risk
  :format :html
  :filter-status :open)

;; Using auto-filter instead of lambda
(defreport high-priority "High Priority Tasks"
  :type :task
  :format :html
  :columns (:name :start :end :priority)
  :auto-filter :high-priority)

;; Tasks on critical path using auto-filter
(defreport critical-auto "Critical Tasks"
  :type :task
  :format :html
  :columns (:name :slack)
  :auto-filter :critical)

;; Incomplete tasks
(defreport incomplete "Incomplete Tasks"
  :type :task
  :format :html
  :columns (:name :complete :end)
  :auto-filter :incomplete)
```

### Generating Reports

```lisp
;; Generate and save a single report
(save-project-report *current-project* 'gantt "output/gantt.html")

;; Generate all reports defined in the project
(generate-all-reports *current-project* "output/")

;; Get report content as string (for further processing)
(let ((html (generate-project-report *current-project* 'critical)))
  ;; do something with html string
  )
```

## Interactive Usage

### Start a REPL Session

```bash
sbcl
```

```lisp
;; Load the library
(ql:quickload :project-juggler)
(in-package :project-juggler)

;; Define a project interactively
(defproject demo "Demo Project"
  :start (date 2024 1 1)
  :end (date 2024 12 31)

  (defresource dev "Developer")

  (deftask task1 "First Task"
    :duration (duration 5 :days)
    :allocate (dev)))

;; Work with it
(finalize-project *current-project*)
(schedule *current-project*)

;; Query results
(maphash (lambda (id task)
           (format t "~A: ~A to ~A~%"
                   (task-name task)
                   (task-start task)
                   (task-end task)))
         (project-tasks *current-project*))

;; Save for later
(let ((session (make-instance 'session :project *current-project*)))
  (save-session session "~/my-projects/demo.lisp"))
```

## Loading Saved Projects

```lisp
;; Load a previously saved project
(ql:quickload :project-juggler)
(in-package :project-juggler)

(setf *current-session* (load-project-session "~/my-projects/demo.lisp"))
(setf *current-project* (session-project *current-session*))

;; Continue working with it
(format t "Loaded: ~A~%" (project-name *current-project*))
```

## Using in Your Own Lisp Applications

### As a Dependency

Add to your `.asd` file:

```lisp
(defsystem "my-app"
  :depends-on (:project-juggler)
  :components (...))
```

### In Your Code

```lisp
(defpackage :my-app
  (:use :cl :project-juggler)
  (:export ...))

(in-package :my-app)

(defun plan-sprint ()
  "Plan a 2-week sprint"
  (defproject sprint "Sprint 47"
    :start (date 2024 11 18)
    :end (date 2024 11 29)

    (defresource team "Dev Team")

    (deftask story1 "User Story 1"
      :effort (duration 20 :hours)
      :allocate (team))

    (deftask story2 "User Story 2"
      :effort (duration 30 :hours)
      :allocate (team)))

  (finalize-project *current-project*)
  (schedule *current-project*)

  *current-project*)
```

## Working with Calendars and Bookings

### Create Reusable Calendars

```lisp
;; In ~/my-projects/company-calendar.lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

(defvar *company-calendar*
  (let ((wh (make-instance 'working-hours
                          :days '(:monday :tuesday :wednesday :thursday :friday)
                          :start-time "09:00"
                          :end-time "17:00"))
        (cal (make-instance 'calendar
                           :id 'company
                           :name "Company Calendar"
                           :working-hours wh)))
    ;; Add 2024 holidays
    (add-holiday cal (date 2024 1 1) "New Year's Day")
    (add-holiday cal (date 2024 7 4) "Independence Day")
    (add-holiday cal (date 2024 11 28) "Thanksgiving")
    (add-holiday cal (date 2024 12 25) "Christmas")
    cal))
```

### Use Calendar in Projects

```lisp
;; In ~/my-projects/q4-project.lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

;; Load your company calendar
(load "~/my-projects/company-calendar.lisp")

;; Use it in calculations
(let ((start (date 2024 11 18))
      (end (date 2024 12 31)))
  (format t "Working hours in Q4: ~A~%"
          (working-hours-between start end *company-calendar*)))
```

## Time Tracking Workflow

### 1. Plan the Project

```bash
# Create project-plan.lisp
```

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

(defproject feature-x "Feature X Development"
  :start (date 2024 11 18)
  :end (date 2024 12 31)

  (defresource alice "Alice (Senior Dev)" :efficiency 1.5)
  (defresource bob "Bob (Mid Dev)" :efficiency 1.0)

  (deftask backend "Backend API"
    :effort (duration 80 :hours)
    :allocate (alice bob))

  (deftask frontend "Frontend"
    :effort (duration 60 :hours)
    :allocate (alice)))

(finalize-project *current-project*)
(schedule *current-project*)

;; Create baseline
(let ((baseline (create-baseline *current-project* :name "Original Plan")))
  (set-project-baseline *current-project* baseline))

;; Save project
(let ((session (make-instance 'session :project *current-project*)))
  (save-session session "feature-x-project.lisp"))
```

### 2. Track Time Daily

```bash
# Create track-time.lisp
```

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

;; Load the project
(setf *current-session* (load-project-session "feature-x-project.lisp"))
(setf *current-project* (session-project *current-session*))

;; Record today's work
(let ((backend-task (gethash 'backend (project-tasks *current-project*)))
      (alice (gethash 'alice (project-resources *current-project*)))
      (bob (gethash 'bob (project-resources *current-project*))))

  ;; Alice worked 8 hours
  (add-booking backend-task alice
               (date 2024 11 18 9 0 0)
               (duration 8 :hours))

  ;; Bob worked 6 hours
  (add-booking backend-task bob
               (date 2024 11 18 9 0 0)
               (duration 6 :hours))

  ;; Auto-update completion
  (update-task-completion-from-bookings backend-task)

  (format t "Backend: ~A% complete~%" (task-complete backend-task)))

;; Save updated project
(let ((session (make-instance 'session :project *current-project*)))
  (save-session session "feature-x-project.lisp"))
```

### 3. Generate Reports

```bash
# Create report-status.lisp
```

```lisp
(ql:quickload :project-juggler)
(in-package :project-juggler)

;; Load project
(setf *current-session* (load-project-session "feature-x-project.lisp"))
(setf *current-project* (session-project *current-session*))

;; EVM analysis
(let ((pv (calculate-planned-value *current-project* (local-time:now)))
      (ev (calculate-earned-value *current-project*))
      (spi (calculate-spi *current-project* (local-time:now))))
  (format t "~%Project Status:~%")
  (format t "  Planned: ~A%~%" pv)
  (format t "  Actual: ~A%~%" ev)
  (format t "  SPI: ~,2F~%~%" spi))

;; Generate HTML report
(quick-task-report *current-project*
  :format :html
  :columns '(:id :name :complete :start :end))
```

## Best Practices

### 1. Project Organization

```
~/projects/
├── calendars/
│   └── company-2024.lisp       # Reusable calendar
├── templates/
│   └── sprint-template.lisp    # Project templates
└── active/
    ├── sprint-47.lisp
    ├── infrastructure-upgrade.lisp
    └── website-redesign.lisp
```

### 2. Version Control

```bash
# Your projects should be version controlled
cd ~/projects/active
git init
git add sprint-47.lisp
git commit -m "Initial sprint planning"

# Project Juggler is just a library - keep it separate
```

### 3. Team Sharing

```lisp
;; Projects are just .lisp files - share via Git
;; Team members can load and modify them:

(ql:quickload :project-juggler)
(in-package :project-juggler)

(setf *current-session*
      (load-project-session "/path/to/shared/project.lisp"))

;; Make changes
;; ...

;; Save back
(save-session *current-session* "/path/to/shared/project.lisp")
```

## Troubleshooting

### "System not found"

```lisp
;; If (ql:quickload :project-juggler) fails:

;; 1. Check if it's in quicklisp local-projects
(probe-file "~/quicklisp/local-projects/project-juggler/")

;; 2. If not, symlink or clone it there
;; 3. Try again
(ql:quickload :project-juggler)
```

### "Package does not exist"

```lisp
;; Always load before using
(ql:quickload :project-juggler)
(in-package :project-juggler)  ; Or use :pj nickname
```

### Projects Not Finding Files

```lisp
;; Use absolute paths or expand home directory
(load-project-session
  (merge-pathnames "projects/my-project.lisp"
                   (user-homedir-pathname)))

;; Or use probe-file to check
(probe-file "~/projects/my-project.lisp")
```

## Summary

**Key Points:**
- ✅ Install once, use anywhere
- ✅ Create project files in your own directories
- ✅ Projects are version-controllable .lisp files
- ✅ Library is a dependency, not a workspace
- ✅ Use `(ql:quickload :project-juggler)` from any directory
- ✅ Save/load projects from anywhere on your filesystem

**The Library Way:**
```
[Project Juggler Library]  ← Installed once via Quicklisp
         ↓ ↓ ↓
[Your Project 1]  [Your Project 2]  [Your Project 3]
  (any dir)          (any dir)          (any dir)
```

**Not This Way:**
```
[Project Juggler Directory]
    └── Your projects inside here ← ❌ Wrong!
```
