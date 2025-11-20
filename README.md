# Project Juggler

**A modern, text-first project management system written in Common Lisp**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Tests: 389/389](https://img.shields.io/badge/tests-389%2F389%20passing-brightgreen)](tests/)
[![Common Lisp](https://img.shields.io/badge/language-Common%20Lisp-blue)](https://common-lisp.net/)

Project Juggler is a TaskJuggler-inspired project management tool that brings powerful scheduling and tracking capabilities to Common Lisp. Define your projects in a clean, expressive DSL, schedule them with industry-standard algorithms, and track progress with Earned Value Management.

## ✨ Features

### 🎯 Core Capabilities
- **Declarative DSL** - Define projects, tasks, and resources using intuitive Lisp macros
- **Dual Scheduling Algorithms**
  - TaskJuggler-style heuristic scheduling for optimal resource allocation
  - Critical Path Method (CPM) for mathematical slack-based analysis
- **Earned Value Management (EVM)** - Track project performance with PV, EV, SV, and SPI metrics
- **Resource Management** - Allocate resources, detect over-allocation, calculate utilization
- **Interactive REPL** - Modify projects on-the-fly with full undo/redo support

### 📊 Reporting & Tracking
- **HTML Reports** - Professional, styled HTML output with task and resource views
- **CSV Export** - RFC 4180 compliant CSV for spreadsheet integration
- **Gantt Chart Data** - Structured data ready for visualization
- **Baseline Comparison** - Snapshot and compare project states over time
- **Critical Path Analysis** - Identify tasks that impact project completion

### 🔧 Developer Features
- **Session Management** - Save and load project state with full fidelity
- **Namespace System** - Organize large projects into modular components
- **Comprehensive Validation** - Circular dependency detection, constraint checking
- **Type Safety** - Rich temporal types (dates, durations, intervals)
- **100% Test Coverage** - 389 tests ensure reliability

## 📦 Installation

### Prerequisites
- **SBCL** (Steel Bank Common Lisp) 2.0 or later
- **Quicklisp** (Common Lisp package manager)

### Quick Start

```bash
# Clone the repository
git clone https://github.com/yourusername/project-juggler.git
cd project-juggler

# Load with Quicklisp
sbcl
```

```lisp
;; In SBCL REPL:
(push (truename ".") asdf:*central-registry*)
(ql:quickload :project-juggler)
(in-package :project-juggler)
```

### Running Tests

```bash
# Run all tests
sbcl --script run-tests.lisp

# Expected output: 389/389 tests passing
```

## 🚀 Quick Example

```lisp
(use-package :project-juggler)

;; Define a web application project
(defproject web-app "Web Application Launch"
  :start (date 2024 3 1)
  :end (date 2024 6 30)

  ;; Define resources
  (defresource dev-team "Development Team"
    :efficiency 1.0
    :rate 100.0)

  (defresource qa-team "QA Team"
    :efficiency 0.9
    :rate 80.0)

  ;; Define tasks with dependencies
  (deftask design "UI/UX Design"
    :duration (duration 2 :weeks)
    :allocate (dev-team)
    :priority 900)

  (deftask implementation "Feature Implementation"
    :duration (duration 6 :weeks)
    :depends-on (design)
    :allocate (dev-team)
    :priority 800

    (deftask frontend "Frontend Development"
      :duration (duration 4 :weeks))

    (deftask backend "Backend Development"
      :duration (duration 4 :weeks)))

  (deftask testing "QA Testing"
    :duration (duration 2 :weeks)
    :depends-on (implementation)
    :allocate (qa-team)
    :priority 900)

  (deftask launch "Production Launch"
    :milestone t
    :depends-on (testing)))

;; Finalize and schedule the project
(finalize-project *current-project*)
(schedule *current-project*)

;; Find critical path
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical Path:~%")
  (dolist (task critical-tasks)
    (format t "  - ~A (slack: ~A days)~%"
            (task-name task)
            (task-slack task))))

;; Generate HTML report
(let ((report (make-instance 'task-report
                :id 'summary
                :title "Project Summary"
                :format :html
                :columns '(:id :name :start :end :duration :priority))))
  (with-open-file (out "report.html" :direction :output
                                      :if-exists :supersede)
    (write-string (generate-report report *current-project*) out)))
```

## 📖 Documentation

### Core Concepts

#### Projects
Projects are the top-level container defined with `defproject`:

```lisp
(defproject my-project "Project Name"
  :start (date 2024 1 1)
  :end (date 2024 12 31)

  ;; Tasks and resources go here
  )
```

#### Tasks
Tasks represent work to be done:

```lisp
(deftask task-id "Task Name"
  :duration (duration 5 :days)        ; How long it takes
  :effort (duration 40 :hours)        ; Work effort required
  :depends-on (other-task)            ; Dependencies
  :allocate (resource1 resource2)     ; Resource allocation
  :priority 800                       ; Scheduling priority (0-1000)
  :milestone t                        ; Is this a milestone?
  :start (date 2024 3 1)              ; Fixed start date (optional)

  ;; Subtasks
  (deftask subtask1 "Subtask" ...))
```

#### Resources
Resources can be people, equipment, or materials:

```lisp
(defresource resource-id "Resource Name"
  :efficiency 1.0    ; Productivity factor (0.0-1.0+)
  :rate 100.0)       ; Cost per hour
```

#### Dependencies
Tasks can depend on other tasks:

```lisp
(deftask task2 "Task 2"
  :depends-on (task1)              ; Single dependency
  :depends-on (task1 task2 task3)) ; Multiple dependencies
```

Dependencies default to "finish-start" (task2 starts after task1 finishes).

### Temporal Types

#### Dates
```lisp
;; Create dates
(date 2024 3 15)                     ; March 15, 2024
(date 2024 3 15 14 30 0)             ; With time: 2:30 PM

;; Date arithmetic
(date+ my-date (duration 5 :days))   ; Add 5 days
(date- end-date start-date)          ; Get duration between dates

;; Date comparison
(date< date1 date2)
(date= date1 date2)
(date>= date1 date2)
```

#### Durations
```lisp
;; Create durations
(duration 5 :days)
(duration 3 :weeks)
(duration 40 :hours)

;; Convert durations
(duration-in-hours (duration 2 :days))   ; => 48
(duration-in-days (duration 3 :weeks))   ; => 21
```

#### Intervals
```lisp
;; Create intervals
(let ((interval (make-instance 'interval
                  :start (date 2024 3 1)
                  :end (date 2024 3 15))))
  (interval-duration-days interval))      ; => 14
```

### Scheduling

#### Finalize and Schedule
```lisp
;; Always finalize before scheduling
(finalize-project *current-project*)  ; Validates and resolves references
(schedule *current-project*)          ; Assigns start/end dates to tasks
```

#### Critical Path Analysis
```lisp
;; Calculate critical path
(forward-pass *current-project*)      ; Calculate early start/finish
(backward-pass *current-project*)     ; Calculate late start/finish
(calculate-slack *current-project*)   ; Calculate slack/float

;; Get critical tasks (zero slack)
(critical-path *current-project*)     ; Returns list of critical tasks
```

### Earned Value Management (EVM)

Track project performance against baseline:

```lisp
;; Create baseline snapshot
(let ((baseline (create-baseline *current-project* :name "Initial Plan")))
  (set-project-baseline *current-project* baseline))

;; Update task completion
(setf (task-complete (gethash 'task1 (project-tasks *current-project*))) 50)

;; Calculate EVM metrics
(let ((pv (calculate-planned-value *current-project* (local-time:now)))
      (ev (calculate-earned-value *current-project*))
      (sv (calculate-schedule-variance *current-project* (local-time:now)))
      (spi (calculate-spi *current-project* (local-time:now))))
  (format t "Planned Value: ~A%~%" pv)
  (format t "Earned Value: ~A%~%" ev)
  (format t "Schedule Variance: ~A%~%" sv)
  (format t "Schedule Performance Index: ~A~%" spi)
  (when (< spi 1.0)
    (format t "Project is behind schedule!~%")))
```

**EVM Metrics:**
- **PV (Planned Value)**: Percentage that should be complete by now
- **EV (Earned Value)**: Percentage actually complete
- **SV (Schedule Variance)**: EV - PV (positive = ahead, negative = behind)
- **SPI (Schedule Performance Index)**: EV / PV (>1.0 = ahead, <1.0 = behind)

### Resource Management

Detect over-allocation:

```lisp
;; Find resource over-allocations
(let ((overallocations (detect-resource-overallocations *current-project*)))
  (dolist (oa overallocations)
    (format t "Resource ~A is overallocated on ~A (load: ~A)~%"
            (overallocation-resource-id oa)
            (overallocation-date oa)
            (overallocation-load oa))))
```

### Reporting

#### HTML Reports
```lisp
(let ((report (make-instance 'task-report
                :id 'summary
                :title "Task Summary"
                :format :html
                :columns '(:id :name :start :end :duration :priority)
                :filter (lambda (task) (> (task-priority task) 500))
                :sort-by (lambda (a b) (date< (task-start a) (task-start b))))))
  (generate-report report *current-project*))
```

#### CSV Export
```lisp
(let ((report (make-instance 'task-report
                :id 'export
                :title "Task Export"
                :format :csv
                :columns '(:id :name :start :end :duration))))
  (with-open-file (out "tasks.csv" :direction :output :if-exists :supersede)
    (write-string (generate-report report *current-project*) out)))
```

#### Gantt Chart Data
```lisp
;; Generate structured data for Gantt visualization
(let ((gantt-data (generate-gantt-data *current-project*)))
  ;; Each entry is a plist:
  ;; (:id task-id :name "Task Name" :start date :end date :dependencies (dep1 dep2))
  (dolist (entry gantt-data)
    (format t "~A: ~A to ~A~%"
            (getf entry :name)
            (getf entry :start)
            (getf entry :end))))
```

### Session Management

#### Save and Load Projects
```lisp
;; Save project to file
(save-session *current-session* "my-project.lisp")

;; Load project from file
(setf *current-session* (load-project-session "my-project.lisp"))
```

#### Undo/Redo
```lisp
;; Make changes
(add-task-to-session 'new-task "New Task" :duration (duration 5 :days))

;; Undo the change
(undo)

;; Redo the change
(redo)
```

## 🏗️ Architecture

### Design Principles

1. **Text-First**: Projects defined in human-readable Lisp DSL
2. **Separation of Concerns**: Heuristic scheduling separate from CPM analysis
3. **Immutable Baselines**: Project snapshots for reliable EVM tracking
4. **Type Safety**: Rich temporal types prevent common errors
5. **Test-Driven**: 389 tests ensure correctness

### Key Components

```
project-juggler/
├── src/
│   ├── core/           # Domain model, types, classes
│   ├── dsl/            # Project definition macros
│   ├── namespace/      # Modular organization
│   ├── validation/     # Constraint checking
│   ├── scheduling/     # TaskJuggler heuristic + CPM
│   ├── session/        # Save/load, undo/redo
│   ├── tracking/       # EVM, baselines, bookings
│   └── reporting/      # HTML, CSV, Gantt
└── tests/              # Comprehensive test suite
```

## 📚 Examples

See the [`examples/`](examples/) directory for complete project examples:

- **Web Application Project** - Multi-team software development
- **Construction Project** - Resource-constrained scheduling
- **Research Project** - Milestone-driven academic project

## 🤝 Contributing

Contributions are welcome! This project follows strict TDD methodology:

1. **Write tests first** - No code without tests
2. **Run tests** - Ensure they fail initially
3. **Implement** - Write minimal code to pass
4. **Refactor** - Improve clarity while keeping tests green
5. **Document** - Update README and docstrings

### Running Tests

```bash
sbcl --script run-tests.lisp
```

### Code Style

- Use descriptive names
- Add docstrings to all public functions
- Keep functions focused and small
- Follow existing patterns in the codebase

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **TaskJuggler** - Inspiration for heuristic scheduling approach
- **Critical Path Method** - Classic project management technique
- **Earned Value Management** - PMI standard for project tracking

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/yourusername/project-juggler/issues)
- **Documentation**: See [TUTORIAL.md](TUTORIAL.md) for step-by-step guide
- **Examples**: Check [`examples/`](examples/) directory

## 🗺️ Roadmap

Core implementation is complete! Future enhancements:

- [ ] Resource leveling algorithms
- [ ] Monte Carlo simulation for risk analysis
- [ ] Gantt chart rendering (HTML5 Canvas/SVG)
- [ ] Web-based UI
- [ ] Import/export TaskJuggler format
- [ ] Calendar integration (working hours, holidays)

---

**Built with ❤️ in Common Lisp**

*Project Juggler - Because managing projects should be elegant*
