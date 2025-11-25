# Project Juggler Templates

This directory contains templates you can copy to create your own projects.

## Quick Start

### 1. Copy the Template

```bash
# Copy to your own directory
cp templates/project-template.lisp ~/my-projects/my-project.lisp
```

### 2. Edit Your Project

```bash
# Open in your favorite editor
nano ~/my-projects/my-project.lisp
# or
vim ~/my-projects/my-project.lisp
# or
emacs ~/my-projects/my-project.lisp
```

### 3. Customize

Edit these sections:
- Project name and dates
- Resources (team members)
- Tasks and dependencies
- Reports you want to generate

### 4. Run

```bash
cd ~/my-projects
sbcl --script my-project.lisp
```

This will:
- Schedule your project
- Analyze the critical path
- Check for resource conflicts
- Generate HTML and CSV reports
- Save the project for later

## Template Overview

The template includes:

### ✅ Complete Project Structure
- Working calendar with holidays
- Resource definitions
- Task definitions with examples:
  - Fixed duration tasks
  - Effort-based tasks
  - Nested subtasks
  - Milestones
- Report definitions (HTML and CSV)

### ✅ Analysis Features
- Project scheduling
- Critical path identification
- Resource conflict detection
- Baseline creation

### ✅ Optional Features (Commented)
- Time tracking with bookings
- EVM metrics calculation

### ✅ Reports Generated
- `project-summary.html` - All tasks overview
- `project-tasks.csv` - CSV for spreadsheets
- `project-critical.html` - Critical path only
- `project-resources.html` - Resource utilization

### ✅ Project Persistence
- Auto-saves project to `my-project-saved.lisp`
- Can be loaded and modified later

## Usage Patterns

### Pattern 1: One-Time Planning

Use for quick project analysis:

```bash
cp templates/project-template.lisp ~/sprint-planning.lisp
# Edit sprint-planning.lisp
sbcl --script ~/sprint-planning.lisp
# Review generated reports
```

### Pattern 2: Ongoing Tracking

Use for projects you'll update over time:

```bash
cp templates/project-template.lisp ~/q4-initiative.lisp
# Edit and run initially
sbcl --script ~/q4-initiative.lisp

# Later, load and update:
sbcl
(ql:quickload :project-juggler)
(in-package :project-juggler)
(setf *current-session* (load-project-session "~/q4-initiative-saved.lisp"))
# Make changes, track time, etc.
```

### Pattern 3: Team Collaboration

Use with version control:

```bash
cd ~/team-projects
git init
cp ~/project-juggler/templates/project-template.lisp ./product-launch.lisp
# Edit product-launch.lisp
git add product-launch.lisp
git commit -m "Initial project plan"
git push

# Team members can clone and run:
git clone <repo>
cd team-projects
sbcl --script product-launch.lisp
```

## Customization Tips

### Add More Holidays

```lisp
(add-holiday *my-calendar* (date 2024 7 4) "Independence Day")
(add-holiday *my-calendar* (date 2024 11 28) "Thanksgiving")
```

### Change Working Hours

```lisp
;; 4-day work week
(defvar *my-calendar*
  (let ((wh (make-instance 'working-hours
                          :days '(:monday :tuesday :wednesday :thursday)
                          :start-time "08:00"
                          :end-time "17:00"))
        ...))
```

### Add Custom Reports

```lisp
(defreport high-priority "High Priority Tasks"
  :type :task
  :format :html
  :columns (:id :name :priority :start :end)
  :filter (lambda (task) (> (task-priority task) 800))
  :sort-by (lambda (a b) (> (task-priority a) (task-priority b))))
```

### Track Actual Time

Uncomment the bookings section and add your actual work:

```lisp
(let ((task (gethash 'task1 (project-tasks *current-project*)))
      (resource (gethash 'person1 (project-resources *current-project*))))
  (add-booking task resource
               (date 2024 1 15 9 0 0)
               (duration 8 :hours))
  (update-task-completion-from-bookings task))
```

## Need Help?

- See [USAGE.md](../USAGE.md) for detailed usage guide
- See [TUTORIAL.md](../TUTORIAL.md) for step-by-step learning
- See [examples/](../examples/) for more complex examples
- Check [README.md](../README.md) for API reference

## License

Templates are provided as-is. Customize freely for your projects!
