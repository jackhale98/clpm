# TaskJuggler Conversion & Implementation Plan

## Part 1: Feature Mapping Analysis

### TaskJuggler tutorial.tjp → Project Juggler DSL

#### Direct Mappings (Already Supported)

| TaskJuggler | Project Juggler | Notes |
|-------------|-----------------|-------|
| `project acso "Name" 2002-01-16 +4m` | `(defproject acso "Name" :start (date 2002 1 16) :end (date 2002 5 16))` | Date arithmetic needed |
| `task id "Name"` | `(deftask id "Name")` | Exact match |
| `effort 20d` | `:effort (duration 20 :days)` | Supported |
| `depends !spec` | `:depends-on (spec)` | Relative refs with ! not needed |
| `allocate dev1, dev2` | `:allocate (dev1 dev2)` | Exact match |
| `resource id "Name"` | `(defresource id "Name")` | Exact match |
| `rate 350.0` | `:rate 350.0` | Exact match |
| `priority 1000` | `:priority 1000` | Exact match |
| `complete 95` | `:complete 95` | Exact match |
| `milestone t` | `:milestone t` | Exact match |

#### Partial Support (Need Enhancement)

| TaskJuggler | Project Juggler | Gap |
|-------------|-----------------|-----|
| `scenario plan "Plan" { scenario delayed }` | Baseline system exists | Need scenario DSL |
| `leaves holiday "Name" date` | Calendar system exists | Need global holiday API |
| `leaves annual 2002-02-01 - 2002-02-05` | Not implemented | Need resource leave tracking |
| `limits { dailymax 6.4h }` | Not implemented | Need resource allocation limits |

#### Missing Features (Require Implementation)

| TaskJuggler | Status | Priority |
|-------------|--------|----------|
| `flags team` | Not implemented | Low - filtering feature |
| `macro name [body]` | Lisp has better solution | N/A - use defun |
| `journalentry date "title" { author, alert, summary, details }` | Not implemented | Medium - project tracking |
| `chargeset account` | Not implemented | Low - accounting feature |
| `charge amount onstart` | Not implemented | Low - accounting feature |
| `account name { subaccounts }` | Not implemented | Low - accounting feature |
| `balance cost rev` | Not implemented | Low - P&L analysis |
| `note "text"` | Not implemented | Low - documentation |
| `managers boss` | Not implemented | Low - hierarchy |
| `responsible dev1` | Not implemented | Medium - ownership |
| `extend resource { text Phone }` | Not implemented | Low - custom fields |
| `delayed:effort 40d` | Need scenario support | High - what-if analysis |
| `delayed:start date` | Need scenario support | High - what-if analysis |

### Current Project Juggler Advantages Over TaskJuggler

1. **True Lisp Integration**
   - Full programmability within project files
   - Use any Common Lisp function in project definitions
   - Runtime modification via REPL

2. **Interactive Development**
   - Live project manipulation with undo/redo
   - Session management with save/restore
   - No recompile/reload cycle

3. **Modern Features Already Built**
   - Earned Value Management (EVM)
   - Actual time tracking with bookings
   - Working time calendars
   - Critical Path Method (CPM) automatic calculation

4. **Embedded Library**
   - Can be used as a library in any Common Lisp application
   - Direct programmatic access to project data
   - No need for external process or file parsing

## Part 2: Tutorial Conversion

### Original TaskJuggler Structure

```taskjuggler
project acso "Accounting Software" 2002-01-16 +4m {
  scenario plan "Plan" { scenario delayed "Delayed" }
}

resource boss "Paul Henry Bullock" { rate 480 }
resource dev "Developers" {
  resource dev1 "Paul Smith" { rate 350.0 }
  resource dev2 "Sébastien Bono" { rate 390.0 }
  resource dev3 "Klaus Müller" { rate 390.0, leaves annual 2002-02-01 - 2002-02-05 }
}
resource test "Peter Murphy" { rate 310.0, limits { dailymax 6.4h } }
resource doc "Dim Sung" { rate 300.0, leaves annual 2002-03-11 - 2002-03-16 }

task AcSo "Accounting Software" {
  task spec "Specification" {
    effort 20d
    allocate dev1, dev2, dev3
    depends !deliveries.start
  }

  task software "Software Development" {
    priority 1000
    depends !spec

    task database "Database coupling" { effort 20d, allocate dev1, dev2 }
    task gui "Graphical User Interface" { effort 35d, depends !database, !backend, allocate dev2, dev3 }
    task backend "Back-End Functions" { effort 30d, complete 95, depends !database, allocate dev1, dev2 }
  }

  task test "Software testing" {
    task alpha "Alpha Test" { effort 1w, depends !!software, allocate test, dev2 }
    task beta "Beta Test" { effort 4w, depends !alpha, allocate test, dev1 }
  }

  task manual "Manual" { effort 10w, depends !deliveries.start, allocate doc, dev3 }

  task deliveries "Milestones" {
    task start "Project start" { start ${projectstart}, delayed:start 2002-01-20 }
    task prev "Technology Preview" { depends !!software.backend }
    task beta "Beta version" { depends !!test.alpha }
    task done "Ship Product to Customer" { depends !!test.beta, !!manual }
  }
}
```

### Converted Project Juggler DSL

See: `test-data/tutorial-converted.lisp` (to be created)

### Key Conversion Decisions

1. **Scenarios → Baselines**: Use baseline system for plan vs. actual
2. **Leaves → Calendar**: Integrate with calendar system
3. **Limits → Resource constraints**: New feature needed
4. **Journal entries**: Skip for now, document as notes in comments
5. **Accounting features**: Skip - not core PM functionality

## Part 3: Strategic Common Lisp Advantages & How to Leverage Them

### What Makes Project Juggler Unique

#### 1. First-Class Functions & Closures

**TaskJuggler Limitation:**
```taskjuggler
macro allocate_developers [
  allocate dev1
  allocate dev2
  allocate dev3
]
```

**Project Juggler Power:**
```lisp
;; Define reusable allocation strategies as functions
(defun allocate-team (team-name resources &key (priority 500))
  (loop for res in resources
        collect `(:allocate ,res :priority ,priority)))

;; Use in project definition
(defproject my-project "My Project"
  :start (date 2024 1 1)

  ;; Programmatically generate team resources
  ,@(loop for i from 1 to 10
          collect `(defresource ,(intern (format nil "DEV~D" i))
                     ,(format nil "Developer ~D" i)
                     :rate ,(+ 80 (random 50))
                     :efficiency ,(+ 0.8 (random 0.4))))

  ;; Programmatically generate tasks
  ,@(loop for sprint from 1 to 4
          collect `(deftask ,(intern (format nil "SPRINT~D" sprint))
                     ,(format nil "Sprint ~D" sprint)
                     :duration (duration 2 :weeks)
                     ,@(when (> sprint 1)
                         `(:depends-on (,(intern (format nil "SPRINT~D" (1- sprint)))))))))
```

**Benefits:**
- Generate complex project structures programmatically
- Reuse common patterns across projects
- Template projects with parameters
- Calculate allocations based on business logic

#### 2. Runtime Introspection & Modification

**Unique Capability:**
```lisp
;; In REPL during project execution:
(in-package :project-juggler)

;; Load and modify project on the fly
(ql:quickload :my-project)

;; Check current state
(gethash 'task1 (project-tasks *current-project*))

;; Modify task dynamically
(setf (task-priority (gethash 'task1 (project-tasks *current-project*))) 900)

;; Re-schedule without reloading
(schedule *current-project*)

;; Undo if needed
(undo)

;; Save modified state
(save-session *current-session* "modified-plan.lisp")
```

**TaskJuggler:** Must edit file → reload → recompile entire project

#### 3. Embedded in Applications

**Use Case: Project Management Dashboard**
```lisp
(defun web-api-get-critical-tasks (project-id)
  "REST API endpoint returning critical path tasks"
  (let ((project (load-project-by-id project-id)))
    (finalize-project project)
    (schedule project)
    (json:encode-json-to-string
      (mapcar (lambda (task)
                (list :id (task-id task)
                      :name (task-name task)
                      :slack (task-slack task)
                      :start (task-start task)
                      :end (task-end task)))
              (critical-path project)))))

;; Use in web framework
(hunchentoot:define-easy-handler (critical-tasks :uri "/api/projects/:id/critical")
    ((id :parameter-type 'integer))
  (setf (hunchentoot:content-type*) "application/json")
  (web-api-get-critical-tasks id))
```

**TaskJuggler:** External process, file-based, harder to embed

#### 4. Advanced DSL Customization with Macros

**Example: Domain-Specific Extensions**
```lisp
;; Define custom macro for agile sprints
(defmacro defsprint (id name duration &key backlog-items velocity team)
  "Define an agile sprint with automatic task breakdown"
  `(deftask ,id ,name
     :duration (duration ,duration :weeks)
     :priority 900

     ,@(loop for item in backlog-items
             for i from 1
             when (<= (getf item :points) velocity)
             collect
             `(deftask ,(intern (format nil "~A-ITEM~D" id i))
                ,(getf item :title)
                :effort (duration ,(* (getf item :points) 8) :hours)
                :allocate ,team))

     (deftask ,(intern (format nil "~A-RETROSPECTIVE" id))
       "Sprint Retrospective"
       :duration (duration 2 :hours)
       :allocate ,team
       :milestone t)))

;; Use it
(defproject agile-project "Agile Development"
  :start (date 2024 1 1)

  (defresource dev-team "Development Team" :efficiency 1.2)

  (defsprint sprint1 "Sprint 1" 2
    :velocity 20
    :team (dev-team)
    :backlog-items ((:title "User Authentication" :points 8)
                    (:title "Dashboard UI" :points 5)
                    (:title "API Integration" :points 5))))
```

#### 5. Type System & Validation

**Project Juggler has rich temporal types:**
```lisp
;; Date arithmetic with validation
(date+ (date 2024 1 1) (duration 5 :days))  ; => date object

;; Type checking prevents errors
(duration 5 :invalid-unit)  ; => ERROR at compile time

;; Calendar-aware operations
(working-hours-between (date 2024 11 18)
                       (date 2024 11 25)
                       *company-calendar*)
```

**TaskJuggler:** String-based dates, runtime errors

### Recommended Strategic Direction

#### Short Term: Core PM Features (Next 3-6 months)

**Priority 1: Resource Leveling**
- Automatic resource load balancing
- Resolve over-allocations
- Optimize resource utilization

**Priority 2: Scenario Analysis (What-If)**
- Multiple parallel scenarios
- Compare plans (optimistic/pessimistic/likely)
- Track variance between scenarios

**Priority 3: Enhanced Reporting**
- Gantt chart export (JSON/SVG/Canvas)
- Resource histograms
- Burndown charts

#### Medium Term: Lisp Advantages (6-12 months)

**Priority 4: Template System**
- Project templates as functions
- Parameterized project generation
- Pattern library

**Priority 5: Interactive Tools**
- Web-based REPL interface
- Real-time project modification
- Collaborative editing

**Priority 6: Embedded Use Cases**
- REST API wrapper
- Database integration (PostgreSQL)
- Event-driven updates

#### Long Term: Differentiation (12+ months)

**Priority 7: AI/ML Integration**
- Estimate prediction from historical data
- Risk analysis via Monte Carlo
- Resource optimization via ML

**Priority 8: Real-Time Collaboration**
- Multi-user editing
- Conflict resolution
- Change tracking

**Priority 9: Domain Extensions**
- Construction industry DSL
- Software development (agile) DSL
- Research project DSL

## Part 4: Implementation Plans (TDD Approach)

### Feature 1: Resource Leveling

#### Overview
Automatically balance resource workload to prevent over-allocation by shifting non-critical tasks.

#### Algorithm: Minimum Slack Heuristic
1. Calculate slack for all tasks (already have via CPM)
2. Identify over-allocated periods for each resource
3. Sort tasks by slack (ascending - least flexible first)
4. For over-allocated periods:
   - Find tasks with slack > 0
   - Shift tasks forward in time until allocation acceptable
   - Respect dependencies
5. Verify schedule still valid

#### Test Plan (TDD)

**File:** `tests/scheduling/test-resource-leveling.lisp`

```lisp
(in-package :project-juggler-tests)

;;; Test 1: Detect Over-Allocation
(define-test test-detect-resource-overallocation ()
  "Verify we can detect when a resource is allocated >100%"
  (let* ((project (make-test-project))
         (dev (make-instance 'resource :id 'dev :name "Developer"))
         (task1 (make-instance 'task :id 't1 :effort (duration 8 :hours)))
         (task2 (make-instance 'task :id 't2 :effort (duration 8 :hours))))

    ;; Both tasks same day = 16 hours for 1 resource = overallocation
    (setf (task-start task1) (date 2024 1 1))
    (setf (task-start task2) (date 2024 1 1))
    (push dev (task-allocated-resources task1))
    (push dev (task-allocated-resources task2))

    (let ((overallocations (detect-resource-overallocations project)))
      (assert-true (> (length overallocations) 0))
      (assert-equal 'dev (overallocation-resource-id (first overallocations))))))

;;; Test 2: Level Resources - Simple Case
(define-test test-level-resources-simple ()
  "Shift one task to resolve over-allocation"
  (let* ((project (make-test-project-with-overallocation)))
    (level-resources project)

    ;; After leveling, no overallocations should exist
    (assert-equal 0 (length (detect-resource-overallocations project)))

    ;; Non-critical task should be shifted
    (let ((t1-start (task-start (gethash 't1 (project-tasks project))))
          (t2-start (task-start (gethash 't2 (project-tasks project)))))
      (assert-false (date= t1-start t2-start)))))

;;; Test 3: Level Resources - Preserve Critical Path
(define-test test-level-resources-preserve-critical-path ()
  "Critical tasks should not be moved during leveling"
  (let* ((project (make-test-project-with-critical-path))
         (critical-tasks-before (critical-path project)))

    ;; Force overallocation on non-critical task
    (add-overallocation-to-non-critical-task project)

    (level-resources project)

    ;; Critical path should be unchanged
    (let ((critical-tasks-after (critical-path project)))
      (assert-equal (length critical-tasks-before)
                    (length critical-tasks-after))
      (dolist (task critical-tasks-before)
        (assert-true (member task critical-tasks-after))))))

;;; Test 4: Level Resources - Complex Dependencies
(define-test test-level-resources-respect-dependencies ()
  "Ensure task dependencies are maintained after leveling"
  (let* ((project (make-complex-project-with-dependencies)))
    (level-resources project)

    ;; Verify all dependencies still satisfied
    (maphash (lambda (id task)
               (declare (ignore id))
               (dolist (dep (task-dependencies task))
                 (assert-true (date<= (task-end dep) (task-start task)))))
             (project-tasks project))))

;;; Test 5: Level Resources - Cannot Level
(define-test test-level-resources-impossible ()
  "Handle case where leveling is impossible (all tasks critical)"
  (let* ((project (make-all-critical-project)))
    (handler-case
        (level-resources project)
      (resource-leveling-impossible (e)
        (assert-true (typep e 'resource-leveling-impossible)))
      (:no-error ()
        (fail "Should have raised resource-leveling-impossible error")))))
```

#### Implementation Plan

**Phase 1: Detection (Week 1)**
- File: `src/scheduling/resource-leveling.lisp`
- Functions:
  - `detect-resource-overallocations`
  - `calculate-resource-load-by-date`
  - `resource-allocated-tasks-on-date`

**Phase 2: Leveling Algorithm (Week 2)**
- Functions:
  - `level-resources`
  - `find-tasks-with-slack`
  - `shift-task-forward`
  - `validate-leveled-schedule`

**Phase 3: Integration (Week 3)**
- Add `:level-resources` option to `schedule`
- Update reports to show resource utilization
- Documentation

### Feature 2: Scenario Analysis (What-If)

#### Overview
Support multiple scenarios with different parameters to compare outcomes.

#### Design

```lisp
;; Define scenarios
(defscenario plan "Original Plan"
  :confidence 0.5)

(defscenario optimistic "Best Case"
  :confidence 0.1
  :parent plan)

(defscenario pessimistic "Worst Case"
  :confidence 0.3
  :parent plan)

;; Tasks can have scenario-specific values
(deftask development "Development Phase"
  :duration (duration 4 :weeks)
  :optimistic:duration (duration 3 :weeks)
  :pessimistic:duration (duration 6 :weeks)
  :allocate (team))

;; Schedule each scenario
(schedule project :scenario 'plan)
(schedule project :scenario 'optimistic)
(schedule project :scenario 'pessimistic)

;; Compare
(compare-scenarios project '(plan optimistic pessimistic))
```

#### Test Plan

**File:** `tests/scheduling/test-scenarios.lisp`

```lisp
(define-test test-define-scenario ()
  "Define multiple scenarios"
  (let ((project (make-test-project)))
    (add-scenario project 'plan "Plan" :confidence 0.5)
    (add-scenario project 'delayed "Delayed" :confidence 0.3)

    (assert-equal 2 (length (project-scenarios project)))
    (assert-true (has-scenario-p project 'plan))
    (assert-true (has-scenario-p project 'delayed))))

(define-test test-scenario-specific-values ()
  "Tasks can have different values per scenario"
  (let* ((project (make-test-project))
         (task (make-instance 'task :id 'dev :name "Development")))

    (set-scenario-value task 'plan :duration (duration 4 :weeks))
    (set-scenario-value task 'delayed :duration (duration 6 :weeks))

    (assert-equal 4 (duration-in-weeks
                     (get-scenario-value task 'plan :duration)))
    (assert-equal 6 (duration-in-weeks
                     (get-scenario-value task 'delayed :duration)))))

(define-test test-schedule-scenario ()
  "Schedule project with specific scenario"
  (let ((project (make-test-project-with-scenarios)))
    (schedule project :scenario 'plan)
    (let ((plan-end (project-end-date project 'plan)))

      (schedule project :scenario 'delayed)
      (let ((delayed-end (project-end-date project 'delayed)))

        (assert-true (date< plan-end delayed-end))))))

(define-test test-compare-scenarios ()
  "Generate comparison report between scenarios"
  (let* ((project (make-test-project-with-scenarios))
         (comparison (compare-scenarios project '(plan delayed))))

    (assert-true (listp comparison))
    (assert-true (> (length comparison) 0))

    ;; Check comparison contains task variance
    (let ((task-comparison (find 'dev comparison :key #'getf :test #'eq)))
      (assert-true task-comparison)
      (assert-true (getf task-comparison :plan-end))
      (assert-true (getf task-comparison :delayed-end))
      (assert-true (getf task-comparison :variance)))))
```

#### Implementation Plan

**Phase 1: Scenario Infrastructure (Week 1)**
- File: `src/core/scenarios.lisp`
- Classes: `scenario`
- Functions:
  - `add-scenario`
  - `has-scenario-p`
  - `set-scenario-value`
  - `get-scenario-value`

**Phase 2: Scheduling with Scenarios (Week 2)**
- Modify `schedule` to accept `:scenario` parameter
- Store results per scenario
- Update task/resource classes for scenario storage

**Phase 3: Comparison & Reporting (Week 3)**
- Functions:
  - `compare-scenarios`
  - `scenario-variance-report`
- Update HTML reports for scenario comparison

### Feature 3: Gantt Chart Export

#### Overview
Export project schedule as Gantt chart data in multiple formats.

#### Formats to Support
1. **JSON** - For web visualization (D3.js, Chart.js)
2. **SVG** - Standalone vector graphic
3. **HTML5 Canvas** - Interactive web component
4. **CSV** - Import to spreadsheet tools

#### Test Plan

**File:** `tests/reporting/test-gantt-export.lisp`

```lisp
(define-test test-export-gantt-json ()
  "Export Gantt data as JSON"
  (let* ((project (make-test-project-scheduled))
         (json (export-gantt project :format :json)))

    (assert-true (stringp json))
    (let ((data (json:decode-json-from-string json)))
      (assert-true (listp data))
      (assert-true (> (length data) 0))

      ;; Verify structure
      (let ((first-task (first data)))
        (assert-true (assoc :id first-task))
        (assert-true (assoc :name first-task))
        (assert-true (assoc :start first-task))
        (assert-true (assoc :end first-task))
        (assert-true (assoc :dependencies first-task))))))

(define-test test-export-gantt-svg ()
  "Export Gantt as SVG"
  (let* ((project (make-test-project-scheduled))
         (svg (export-gantt project :format :svg)))

    (assert-true (stringp svg))
    (assert-true (search "<svg" svg))
    (assert-true (search "</svg>" svg))

    ;; Verify contains task rectangles
    (assert-true (search "<rect" svg))

    ;; Verify contains text labels
    (assert-true (search "<text" svg))))

(define-test test-export-gantt-custom-options ()
  "Export with custom styling and options"
  (let* ((project (make-test-project-scheduled))
         (json (export-gantt project
                             :format :json
                             :include-resources t
                             :include-milestones t
                             :date-format :iso8601)))

    (let ((data (json:decode-json-from-string json)))
      (let ((first-task (first data)))
        (assert-true (assoc :resources first-task))
        (assert-true (assoc :milestone first-task))))))
```

#### Implementation Plan

**Phase 1: JSON Export (Week 1)**
- File: `src/reporting/gantt.lisp`
- Functions:
  - `export-gantt`
  - `gantt-task-to-json`
  - `format-gantt-date`

**Phase 2: SVG Export (Week 2)**
- Functions:
  - `generate-gantt-svg`
  - `svg-timeline`
  - `svg-task-bar`
  - `svg-dependency-arrow`

**Phase 3: HTML5 Canvas (Week 3)**
- Generate HTML with embedded Canvas JavaScript
- Interactive features (zoom, pan, tooltips)

## Part 5: Testing Strategy

### Overall Testing Approach

1. **Write tests first** (TDD)
2. **One feature at a time**
3. **Integration tests after unit tests**
4. **Manual validation against TaskJuggler**

### Test Data
- Use tutorial.tjp converted project
- Create synthetic projects for edge cases
- Build regression test suite

### Validation Metrics
- Schedule end dates (±1 day tolerance)
- Critical path identification (100% match)
- Resource utilization (±5% tolerance)
- Dependency satisfaction (100% match)

## Part 6: Timeline

### Month 1: Validation & Foundation
- Week 1: Convert tutorial.tjp, run comparison
- Week 2: Document gaps, validate algorithms
- Week 3: Resource leveling tests
- Week 4: Resource leveling implementation

### Month 2: Scenarios & What-If
- Week 1-2: Scenario infrastructure & scheduling
- Week 3: Comparison & reporting
- Week 4: Integration & documentation

### Month 3: Visualization & Polish
- Week 1: JSON/CSV export
- Week 2: SVG generation
- Week 3: HTML5 Canvas
- Week 4: Documentation & examples

## Part 7: Success Criteria

### Tutorial Comparison
- [  ] Scheduled within ±1 day of TaskJuggler
- [  ] Same critical path identified
- [  ] Resource allocations match
- [  ] Handles all dependencies correctly

### New Features
- [  ] Resource leveling resolves >90% of conflicts
- [  ] Scenarios allow 3+ parallel analyses
- [  ] Gantt export works in 3+ formats
- [  ] All features >95% test coverage

### Strategic Position
- [  ] Demonstrate 3+ use cases TaskJuggler can't do
- [  ] Show embedded library usage
- [  ] Prove REPL-driven development advantage
- [  ] Document Common Lisp benefits with examples

## Next Steps

1. **Convert tutorial.tjp** → `test-data/tutorial-converted.lisp`
2. **Run and compare** → Document in `test-data/COMPARISON_RESULTS.md`
3. **Write tests** → `tests/scheduling/test-resource-leveling.lisp`
4. **Implement features** → Following TDD discipline
5. **Validate continuously** → Against TaskJuggler outputs
