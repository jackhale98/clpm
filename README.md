# CLAPS - Common Lisp Automated Project Scheduling

**A modern, text-first project management system written in Common Lisp**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Tests: 1226/1226](https://img.shields.io/badge/tests-1226%2F1226%20passing-brightgreen)](tests/)
[![CI](https://github.com/yourusername/claps/workflows/CI/badge.svg)](https://github.com/yourusername/claps/actions)
[![Common Lisp](https://img.shields.io/badge/language-Common%20Lisp-blue)](https://common-lisp.net/)

CLAPS is a TaskJuggler-inspired project management tool that brings powerful scheduling and tracking capabilities to Common Lisp. Define your projects in a clean, expressive DSL, schedule them with industry-standard algorithms, and track progress with Earned Value Management.

## Features

### Core Capabilities
- **Declarative DSL** - Define projects, tasks, resources, and reports using intuitive Lisp macros
- **Dual Scheduling Algorithms**
  - TaskJuggler-style heuristic scheduling for optimal resource allocation
  - Critical Path Method (CPM) for mathematical slack-based analysis (automatic)
  - **Effort-based scheduling** with resource efficiency calculations
- **Working Time Calendars** - Define working hours, holidays, and calculate calendar-aware durations
- **Declarative Time Tracking** - Record actual work with `defbooking`/`deftimesheet` macros
- **Actual vs Planned Tracking** - `:actual-start`, `:actual-end`, `:complete` properties on tasks
- **Earned Value Management (EVM)** - Track project performance with PV, EV, SV, and SPI metrics
- **Resource Management** - Allocate resources, detect over-allocation, calculate utilization
- **Interactive REPL** - Modify projects on-the-fly with full undo/redo support

### Command-Line Interface
- **Process projects** - `claps project.lisp` to finalize, schedule, and generate reports
- **Quick analysis** - `--summary`, `--critical-path`, `--resources`, `--milestones`
- **EVM tracking** - `--evm` with optional `--status-date`
- **Risk analysis** - `--simulate` for Monte Carlo simulation
- **Scenario comparison** - `--scenarios` and `--compare`
- **JSON output** - Add `--json` to any command for machine-readable output

### Reporting & Tracking
- **defreport DSL** - Define reports alongside tasks with filtering and sorting
- **HTML Reports** - Professional, styled HTML output with task and resource views
- **CSV Export** - RFC 4180 compliant CSV for spreadsheet integration
- **Gantt Chart Data** - Structured data ready for visualization
- **Scenario Comparison** - TaskJuggler-style scenarios with comparison reports
- **Summary Tasks (Phases)** - Automatic aggregation of subtask values
- **Critical Path Analysis** - Automatically calculated during scheduling

### Developer Features
- **Session Management** - Save and load project state with full fidelity
- **Modular Includes** - Split projects across files with `include`, namespace prefixing
- **Comprehensive Validation** - Circular dependency detection, constraint checking
- **Type Safety** - Rich temporal types (dates, durations, intervals)
- **Monte Carlo Simulation** - Quantitative schedule risk analysis with PERT
- **Dynamic Scenario Management** - Create, copy, and modify scenarios at runtime
- **100% Test Coverage** - 1226 tests ensure reliability
- **CI/CD** - GitHub Actions for testing and releases

## Installation

### Prerequisites
- **SBCL** (Steel Bank Common Lisp) 2.0 or later
- **Quicklisp** (Common Lisp package manager)

### Install as a Library (Recommended for Users)

```bash
# Option 1: Install to Quicklisp local-projects (works from any directory)
cd ~/quicklisp/local-projects/
git clone https://github.com/yourusername/claps.git

# Option 2: Or symlink your development copy
ln -s /path/to/your/claps ~/quicklisp/local-projects/
```

Now from **any directory** on your system:

```bash
sbcl
```

```lisp
;; Load the library
(ql:quickload :claps)
(in-package :claps)

;; Start creating projects!
```

### For Development/Testing (Inside the Repository)

```bash
# Clone and work inside the repository
git clone https://github.com/yourusername/claps.git
cd claps
sbcl
```

```lisp
;; Load from current directory
(push (truename ".") asdf:*central-registry*)
(ql:quickload :claps)
(in-package :claps)
```

**See [USAGE.md](USAGE.md) for detailed usage patterns and real-world examples.**

### Verification

After installation, verify it works:

```bash
sbcl --eval "(ql:quickload :claps)" --eval "(format t \"CLAPS loaded successfully!~%\")" --quit
```

### Running Tests

```bash
# Run all tests
sbcl --script run-tests.lisp

# Expected output: 1225/1225 tests passing
```

## Command-Line Interface

Build the CLI executable:

```bash
./scripts/build-claps.sh
```

### Usage

```bash
claps project.lisp                     # Process and generate all reports
claps project.lisp --summary           # Quick project overview
claps project.lisp --critical-path     # Show critical path
claps project.lisp --resources         # Show resource utilization
claps project.lisp --evm               # Show earned value metrics
claps project.lisp --simulate          # Run Monte Carlo simulation
claps --help                           # Show usage
```

### CLI Options

| Option | Description |
|--------|-------------|
| `<file.lisp>` | Project file (positional, required for most commands) |
| `--output-dir DIR` | Output directory for reports |
| `--report ID` | Generate specific report only |
| `--validate` | Validate without scheduling |
| `--critical-path` | Print critical path tasks |
| `--scenarios` | List available scenarios |
| `--compare S1 S2` | Compare two scenarios |
| `--summary` | Quick project overview |
| `--resources` | Show resource utilization |
| `--overallocations` | Show resource conflicts |
| `--evm` | Show earned value metrics |
| `--status-date DATE` | Status date for EVM (default: today) |
| `--simulate` | Run Monte Carlo simulation |
| `--trials N` | Number of simulation trials (default: 1000) |
| `--milestones` | Show milestone timeline |
| `--repl` | Start interactive REPL after loading |
| `--quiet` | Suppress informational output |
| `--json` | Output in JSON format |
| `--help` | Show usage |
| `--version` | Show version |

### Interactive REPL Mode

The `--repl` flag starts an interactive Common Lisp REPL after loading your project:

```bash
claps project.lisp --repl
```

This gives you full access to SBCL's debugger and inspector, allowing you to:
- Explore and modify the project interactively
- Run what-if analyses
- Debug scheduling issues with full stack traces
- Hot-reload your project file after making changes

```
================================================================================
CLAPS Interactive REPL
================================================================================

Project loaded: Company Website Redesign
Tasks: 6  Resources: 3  Scenarios: (PLAN DELAYED)

Useful variables:
  claps:*current-project*  - The loaded project

Quick commands:
  (claps:critical-path claps:*current-project*)
  (claps:list-scenarios claps:*current-project*)
  (claps:detect-resource-overallocations claps:*current-project*)
  (claps:run-monte-carlo-simulation claps:*current-project* :trials 1000)

To reload and reschedule after changes:
  (claps/cli:reload-project)

Type (quit) or Ctrl-D to exit.
================================================================================

* (length (critical-path *current-project*))
4
* (simulation-percentile (run-monte-carlo-simulation *current-project*) 90)
82.3
```

### Example Output

```
$ claps project.lisp --summary

CLAPS - Project Summary
=======================
Project: Company Website Redesign
Period:  2024-03-01 to 2024-05-31

Tasks:      6 total, 6 scheduled, 1 milestones
Resources:  3 defined
Scenarios:  plan, delayed

Progress:   45% complete

Critical Path: 4 tasks
  Requirements -> Design -> Frontend -> Deployment
```

## Quick Example

Create a file `~/my-projects/website.lisp`:

```lisp
;; Load the library (works from any directory!)
(ql:quickload :claps :silent t)
(in-package :claps)

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
    :depends-on (testing))

  ;; Define reports alongside tasks
  (defreport summary "Project Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration :priority)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

  (defreport critical-only "Critical Path Tasks"
    :type :task
    :format :html
    :columns (:id :name :start :end :slack)
    :filter (lambda (task) (and (task-slack task) (zerop (task-slack task))))))

;; Finalize and schedule the project
(finalize-project *current-project*)
(schedule *current-project*)  ; Automatically calculates critical path!

;; View critical path
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical Path:~%")
  (dolist (task critical-tasks)
    (format t "  - ~A (slack: ~A days)~%"
            (task-name task)
            (task-slack task))))

;; Generate reports using DSL-defined reports
(save-project-report *current-project* 'summary "report.html")
(save-project-report *current-project* 'critical-only "critical.html")
```

Run it from anywhere:

```bash
cd ~/my-projects
sbcl --script website.lisp
# Creates report.html and critical.html in current directory
```

Or use the CLI:

```bash
claps website.lisp --summary
claps website.lisp --critical-path --json
```

**Note:** The examples in `examples/` directory are for demonstration and testing within the repository. For real-world usage, see [USAGE.md](USAGE.md).

## Documentation

See [USAGE.md](USAGE.md) for comprehensive documentation including:

- Projects, Tasks, Resources, and Dependencies
- Temporal Types (Dates, Durations, Intervals)
- Scheduling and Critical Path Analysis
- Earned Value Management (EVM)
- TaskJuggler-style Scenarios
- Monte Carlo Simulation
- Resource Management
- Reporting

## Architecture

### Design Principles

1. **Text-First**: Projects defined in human-readable Lisp DSL
2. **Separation of Concerns**: Heuristic scheduling separate from CPM analysis
3. **Scenario-Based EVM**: First scenario is baseline for tracking
4. **Type Safety**: Rich temporal types prevent common errors
5. **Calendar-Aware**: Working time calendars for realistic scheduling
6. **Test-Driven**: 1225 tests ensure correctness

### Key Components

```
claps/
├── src/
│   ├── core/           # Domain model, types, classes
│   ├── dsl/            # Project definition macros
│   ├── namespace/      # Modular organization
│   ├── validation/     # Constraint checking
│   ├── scheduling/     # TaskJuggler + CPM + calendars
│   ├── session/        # Save/load, undo/redo
│   ├── tracking/       # EVM, scenarios, bookings
│   ├── reporting/      # HTML, CSV, Gantt
│   ├── risk/           # Risk register, Monte Carlo simulation
│   └── cli/            # Command-line interface
├── tests/              # 1226 comprehensive tests
├── examples/           # Example projects
└── scripts/            # Build scripts
```

## Examples

### Repository Examples (For Learning)

The [`examples/`](examples/) directory contains demonstration projects:

- **time-tracking-project.lisp** - Calendars + bookings + EVM integration
- **simple-project.lisp** - Website redesign with scenarios (6 tasks, demonstrates basics)
- **web-application.lisp** - Complex SaaS platform with scenarios (40+ tasks, multiple teams)
- **effort-scheduling.lisp** - Effort-based scheduling with resource efficiency
- **monte-carlo-example.lisp** - Schedule risk analysis with PERT simulation
- **modular-project/** - Multi-file project with includes and namespace prefixing

Run from repository root: `sbcl --script examples/simple-project.lisp`

Or with CLI: `claps examples/simple-project.lisp --summary`

### Real-World Usage

For using CLAPS in your own projects, see **[USAGE.md](USAGE.md)** which shows:
- Installing as a proper library
- Creating project files in your own directories
- Loading from anywhere on your system
- Version controlling your projects
- Team collaboration patterns
- Integration with your applications

## Declarative Time Tracking

CLAPS supports TaskJuggler-style declarative time tracking. Project files are pure data that can be edited by hand.

### In Task Definitions

```lisp
(deftask backend "Backend Development"
  :effort (duration 80 :hours)
  :complete 50                            ; Progress percentage
  :actual-start (date 2024 11 11)         ; When work actually began
  :actual-end (date 2024 11 20)           ; When work finished
  :bookings ((alice (date 2024 11 11) 8)  ; Inline time entries
             (bob (date 2024 11 12) 6)))
```

### Separate Timesheet Files

Keep project schedules clean by putting time entries in separate files:

```lisp
;; timesheets/november-2024.lisp

;; Batch style
(defbookings
  (backend alice (date 2024 11 18) 8)
  (backend bob (date 2024 11 18) 6))

;; Timesheet style (grouped by resource)
(deftimesheet alice
  (backend (date 2024 11 18) 8)
  (backend (date 2024 11 19) 8))
```

## Contributing

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

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Acknowledgments

- **TaskJuggler** - Inspiration for heuristic scheduling approach
- **Critical Path Method** - Classic project management technique
- **Earned Value Management** - PMI standard for project tracking

## Support

- **Issues**: [GitHub Issues](https://github.com/yourusername/claps/issues)
- **Documentation**: See [USAGE.md](USAGE.md) for detailed guide
- **Examples**: Check [`examples/`](examples/) directory

## Roadmap

Core implementation is complete! Recent additions:

- [x] Calendar integration (working hours, holidays)
- [x] Actual time tracking with bookings
- [x] Monte Carlo simulation for risk analysis
- [x] Dynamic scenario management
- [x] Summary task aggregation (phases)
- [x] Scenario comparison reports
- [x] Command-line interface (CLI)

Future enhancements:

- [ ] Resource leveling algorithms
- [ ] Gantt chart rendering (HTML5 Canvas/SVG)
- [ ] Web-based UI
- [ ] Import/export TaskJuggler format

---

**Built with Common Lisp**

*CLAPS - Common Lisp Automated Project Scheduling*
