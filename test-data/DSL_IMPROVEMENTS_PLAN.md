# DSL Improvements for User-Friendliness

## Goal
Make Project Juggler accessible to non-Lisp programmers while maintaining full power for advanced users.

## Current State vs. Desired State

### Current DSL (Lisp-heavy)
```lisp
(defproject my-project "My Project"
  :start (date 2024 1 15)
  :end (date 2024 6 30)

  (defresource dev "Developer"
    :rate 100.0
    :efficiency 1.0)

  (deftask task1 "Task 1"
    :effort (duration 5 :days)
    :allocate (dev)
    :depends-on (task0)))
```

### Desired DSL (User-friendly)
```lisp
(defproject my-project "My Project"
  :start "2024-01-15"
  :end "2024-06-30"

  (defresource dev "Developer"
    :rate 100)  ; defaults to daily rate, efficiency 1.0

  (deftask task1 "Task 1"
    :effort "5 days"
    :allocate dev  ; single resource, no list needed
    :depends-on task0))  ; single dependency, no list needed
```

### Advanced Usage (Still Available)
```lisp
;; Power users can still use full Lisp
(defproject generated-project "Generated"
  :start "2024-01-01"
  :end "2024-12-31"

  ;; Programmatically generate resources
  ,@(loop for i from 1 to 10
          collect `(defresource ,(intern (format nil "DEV~D" i))
                     ,(format nil "Developer ~D" i)
                     :rate ,(+ 80 (random 40))))

  ;; Programmatically generate tasks
  ,@(loop for month from 1 to 12
          collect `(deftask ,(intern (format nil "SPRINT~D" month))
                     ,(format nil "Sprint ~D" month)
                     :duration "2 weeks"
                     ,@(when (> month 1)
                         `(:depends-on ,(intern (format nil "SPRINT~D" (1- month))))))))
```

## Proposed Improvements

### 1. String Date Parsing

**Problem:** `(date 2024 1 15)` requires knowing Lisp function call syntax

**Solution:** Accept ISO 8601 date strings

```lisp
;; All of these should work:
:start "2024-01-15"
:start "2024-01-15T09:00"
:start "2024-01-15T09:00:00"
:start (date 2024 1 15)  ; Advanced users can still use this
```

**Implementation:**
```lisp
(defun parse-date-flexible (date-expr)
  "Parse date from string or date object"
  (typecase date-expr
    (string (parse-iso-8601-date date-expr))
    (pj-date date-expr)
    (t (error "Invalid date format: ~A. Use \"YYYY-MM-DD\" or (date y m d)" date-expr))))
```

### 2. String Duration Parsing

**Problem:** `(duration 5 :days)` requires knowing keyword syntax

**Solution:** Accept natural language duration strings

```lisp
;; All of these should work:
:duration "5 days"
:duration "2 weeks"
:duration "3 months"
:effort "40 hours"
:effort "1 week"
:effort (duration 5 :days)  ; Advanced users can still use this
```

**Supported formats:**
- "N days" / "N day"
- "N weeks" / "N week"
- "N months" / "N month"
- "N hours" / "N hour"
- "N d" / "N w" / "N mo" / "N h" (short forms)

**Implementation:**
```lisp
(defun parse-duration-flexible (duration-expr)
  "Parse duration from string or duration object"
  (typecase duration-expr
    (string (parse-duration-string duration-expr))
    (duration duration-expr)
    (t (error "Invalid duration: ~A. Use \"N days\" or (duration n :days)" duration-expr))))

(defun parse-duration-string (str)
  "Parse strings like '5 days', '2 weeks', '40 hours'"
  (let ((parts (split-string str #\Space)))
    (when (< (length parts) 2)
      (error "Duration must be 'N unit' like '5 days'"))
    (let ((number (parse-integer (first parts)))
          (unit (string-downcase (second parts))))
      (cond
        ((member unit '("day" "days" "d") :test #'string=)
         (duration number :days))
        ((member unit '("week" "weeks" "w") :test #'string=)
         (duration (* number 5) :days))
        ((member unit '("month" "months" "mo") :test #'string=)
         (duration (* number 20) :days))  ; Approximation: 4 weeks
        ((member unit '("hour" "hours" "h") :test #'string=)
         (duration number :hours))
        (t (error "Unknown duration unit: ~A. Use days, weeks, months, hours" unit))))))
```

### 3. Flexible Resource/Task References

**Problem:** Single items require wrapping in lists

**Solution:** Auto-detect single vs. multiple

```lisp
;; Current (requires lists):
:allocate (dev)
:depends-on (task1)

;; Improved (lists optional for single items):
:allocate dev
:allocate (dev1 dev2 dev3)
:depends-on task1
:depends-on (task1 task2)
```

**Implementation:**
```lisp
(defun normalize-to-list (value)
  "Ensure value is a list. Single symbols become single-item lists."
  (cond
    ((null value) nil)
    ((listp value) value)
    ((symbolp value) (list value))
    (t (list value))))
```

### 4. Parent Task Auto-Duration

**Problem:** Parent tasks error if no duration/effort specified

**Solution:** Automatically calculate from subtasks

```lisp
;; This should work:
(deftask phase1 "Phase 1"
  (deftask task1 "Task 1" :duration "5 days")
  (deftask task2 "Task 2" :duration "3 days" :depends-on task1))

;; Phase1 duration auto-calculated as 8 days (5 + 3 in sequence)
```

**Implementation:** Calculate in scheduler, not in DSL macro

### 5. Sensible Defaults

**Problem:** Too many required parameters

**Solution:** Provide smart defaults

```lisp
(defresource dev "Developer"
  :rate 100)
  ;; Auto-defaults:
  ;; :efficiency 1.0
  ;; :rate-type :daily

(deftask task1 "Task 1"
  :duration "5 days")
  ;; Auto-defaults:
  ;; :priority 500
  ;; :milestone nil
```

### 6. Alternative Simple Syntax (Optional)

**For truly simple projects, offer an even simpler macro:**

```lisp
(simple-project "Website Redesign" "2024-01-01" "2024-06-30"

  ;; Resources: name rate
  (:resources
    ("Senior Dev" 150)
    ("Junior Dev" 80)
    ("Designer" 100))

  ;; Tasks: name duration [depends-on] [resources]
  (:tasks
    ("Requirements" "2 weeks" nil ("Senior Dev" "Designer"))
    ("Design" "3 weeks" "Requirements" ("Designer"))
    ("Frontend" "4 weeks" "Design" ("Senior Dev" "Junior Dev"))
    ("Backend" "4 weeks" "Design" ("Senior Dev"))
    ("Testing" "2 weeks" ("Frontend" "Backend") ("Senior Dev" "Junior Dev"))
    ("Launch" milestone "Testing")))
```

This expands to full `defproject` syntax.

## Implementation Plan

### Phase 1: String Parsing (Week 1)

**Files to Create:**
- `src/core/parse-dates.lisp` - Date string parsing
- `src/core/parse-durations.lisp` - Duration string parsing
- `tests/core/test-parse-dates.lisp`
- `tests/core/test-parse-durations.lisp`

**Test First:**
```lisp
(in-package :project-juggler-tests)

;;; Date Parsing Tests
(define-test test-parse-iso-date ()
  (let ((date (parse-date-flexible "2024-01-15")))
    (assert-true (typep date 'pj-date))
    (assert-equal 2024 (date-year date))
    (assert-equal 1 (date-month date))
    (assert-equal 15 (date-day date))))

(define-test test-parse-iso-datetime ()
  (let ((date (parse-date-flexible "2024-01-15T14:30:00")))
    (assert-equal 14 (date-hour date))
    (assert-equal 30 (date-minute date))))

(define-test test-parse-date-object ()
  (let ((date-obj (date 2024 1 15)))
    (assert-eq date-obj (parse-date-flexible date-obj))))

;;; Duration Parsing Tests
(define-test test-parse-duration-days ()
  (let ((dur (parse-duration-flexible "5 days")))
    (assert-equal 5 (duration-in-days dur))))

(define-test test-parse-duration-weeks ()
  (let ((dur (parse-duration-flexible "2 weeks")))
    (assert-equal 10 (duration-in-days dur))))

(define-test test-parse-duration-hours ()
  (let ((dur (parse-duration-flexible "40 hours")))
    (assert-equal 40 (duration-in-hours dur))))

(define-test test-parse-duration-object ()
  (let ((dur-obj (duration 5 :days)))
    (assert-eq dur-obj (parse-duration-flexible dur-obj))))
```

### Phase 2: DSL Macro Updates (Week 1)

**Files to Modify:**
- `src/dsl/defproject.lisp` - Update to use parse-date-flexible
- `src/dsl/deftask.lisp` - Update to use parse-duration-flexible, normalize-to-list
- `src/dsl/defresource.lisp` - Add defaults

**Changes to deftask:**
```lisp
(defmacro deftask (id name &body body)
  ;; ... existing parsing ...

  ;; Update parsing to use flexible parsers:
  (when effort-expr
    (setf effort-expr `(parse-duration-flexible ,effort-expr)))

  (when duration-expr
    (setf duration-expr `(parse-duration-flexible ,duration-expr)))

  (when start-expr
    (setf start-expr `(parse-date-flexible ,start-expr)))

  (when end-expr
    (setf end-expr `(parse-date-flexible ,end-expr)))

  ;; Normalize lists:
  (when depends-on-list
    (setf depends-on-list `(normalize-to-list ',depends-on-list)))

  (when allocate-list
    (setf allocate-list `(normalize-to-list ',allocate-list)))

  ;; ... rest of macro ...
)
```

**Tests:**
```lisp
(define-test test-deftask-string-duration ()
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (deftask t1 "Task 1" :duration "5 days"))

  (let ((task (gethash 't1 (project-tasks *current-project*))))
    (assert-equal 5 (duration-in-days (task-duration task)))))

(define-test test-deftask-string-date ()
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (deftask t1 "Task 1" :start "2024-02-01" :duration "5 days"))

  (let ((task (gethash 't1 (project-tasks *current-project*))))
    (assert-equal 2 (date-month (task-start task)))))

(define-test test-deftask-single-dependency ()
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (deftask t1 "Task 1" :duration "5 days")
    (deftask t2 "Task 2" :duration "3 days" :depends-on t1))

  (let ((task (gethash 't2 (project-tasks *current-project*))))
    (assert-equal 1 (length (task-dependencies task)))))

(define-test test-deftask-single-resource ()
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (defresource dev "Dev" :rate 100)
    (deftask t1 "Task 1" :duration "5 days" :allocate dev))

  (let ((task (gethash 't1 (project-tasks *current-project*))))
    (assert-equal 1 (length (task-allocated-resources task)))))
```

### Phase 3: Add Missing Keywords (Week 1)

**Add :complete to deftask:**
```lisp
;; In deftask macro:
(complete-expr nil)

;; In case statement:
(:complete (setf complete-expr value))

;; In make-instance:
,@(when complete-expr `(:complete ,complete-expr))
```

**Test:**
```lisp
(define-test test-deftask-complete ()
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (deftask t1 "Task 1" :duration "5 days" :complete 75))

  (let ((task (gethash 't1 (project-tasks *current-project*))))
    (assert-equal 75 (task-complete task))))
```

### Phase 4: Parent Task Auto-Duration (Week 1)

**Modify scheduler:**
```lisp
(defun calculate-parent-task-durations (project)
  "Calculate duration for parent tasks based on subtask span"
  (maphash (lambda (id task)
             (declare (ignore id))
             (when (and (task-subtasks task)
                        (not (task-duration task))
                        (not (task-effort task)))
               ;; Calculate from subtasks
               (let ((earliest-start nil)
                     (latest-end nil))
                 (dolist (subtask (task-subtasks task))
                   (when (task-scheduled-p subtask)
                     (when (or (null earliest-start)
                               (date< (task-start subtask) earliest-start))
                       (setf earliest-start (task-start subtask)))
                     (when (or (null latest-end)
                               (date> (task-end subtask) latest-end))
                       (setf latest-end (task-end subtask)))))
                 (when (and earliest-start latest-end)
                   (setf (task-start task) earliest-start)
                   (setf (task-end task) latest-end)
                   (setf (task-duration task)
                         (interval-duration
                          (make-instance 'interval
                                         :start earliest-start
                                         :end latest-end)))))))
           (project-tasks project)))
```

**Test:**
```lisp
(define-test test-parent-task-auto-duration ()
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (deftask parent "Parent"
      (deftask child1 "Child 1" :duration "5 days")
      (deftask child2 "Child 2" :duration "3 days" :depends-on child1)))

  (finalize-project *current-project*)
  (schedule *current-project*)

  (let ((parent (gethash 'parent (project-tasks *current-project*))))
    (assert-true (task-scheduled-p parent))
    (assert-equal 8 (duration-in-days (task-duration parent)))))
```

### Phase 5: Documentation & Examples (Week 1)

**Create beginner-friendly examples:**

**examples/simple-website.lisp:**
```lisp
(ql:quickload :project-juggler :silent t)
(in-package :project-juggler)

(defproject website "Simple Website Redesign"
  :start "2024-01-15"
  :end "2024-06-30"

  ;; Team
  (defresource designer "Sarah (Designer)" :rate 100)
  (defresource developer "John (Developer)" :rate 120)
  (defresource qa "Maria (QA)" :rate 80)

  ;; Planning
  (deftask requirements "Gather Requirements"
    :duration "1 week"
    :allocate designer)

  ;; Design
  (deftask wireframes "Create Wireframes"
    :duration "2 weeks"
    :depends-on requirements
    :allocate designer)

  (deftask mockups "Design Mockups"
    :duration "2 weeks"
    :depends-on wireframes
    :allocate designer)

  ;; Development
  (deftask frontend "Frontend Development"
    :duration "4 weeks"
    :depends-on mockups
    :allocate developer)

  (deftask backend "Backend Development"
    :duration "3 weeks"
    :depends-on mockups
    :allocate developer)

  ;; Testing & Launch
  (deftask testing "QA Testing"
    :duration "2 weeks"
    :depends-on (frontend backend)
    :allocate qa)

  (deftask launch "Website Launch"
    :milestone t
    :depends-on testing)

  ;; Report
  (defreport timeline "Project Timeline"
    :type :task
    :format :html
    :columns (:name :start :end :duration)))

;; Schedule and report
(finalize-project *current-project*)
(schedule *current-project*)

(format t "Project: ~A~%" (project-name *current-project*))
(format t "Duration: ~A to ~A~%"
        (project-start *current-project*)
        (project-end *current-project*))
(format t "Critical Path: ~A tasks~%"
        (length (critical-path *current-project*)))

(save-project-report *current-project* 'timeline "website-timeline.html")
```

## Migration Guide for Existing Projects

**Old Style:**
```lisp
(defproject old "Old Style"
  :start (date 2024 1 15)
  :end (date 2024 6 30)

  (defresource dev "Dev"
    :rate 100.0
    :efficiency 1.0)

  (deftask task1 "Task"
    :effort (duration 5 :days)
    :allocate (dev)
    :depends-on (task0)))
```

**New Style (backward compatible):**
```lisp
(defproject new "New Style"
  :start "2024-01-15"
  :end "2024-06-30"

  (defresource dev "Dev"
    :rate 100)

  (deftask task1 "Task"
    :effort "5 days"
    :allocate dev
    :depends-on task0))
```

**Both work!** Old projects don't break.

## Benefits

### For Beginners:
- No need to learn Lisp function syntax
- Natural language durations
- ISO dates (familiar format)
- Fewer parentheses
- Smart defaults

### For Advanced Users:
- Full Lisp still available
- Programmatic generation
- Runtime modification
- Custom functions
- REPL integration

### For Everyone:
- Less typing
- More readable
- Fewer errors
- Better examples
- Gradual learning curve

## Success Criteria

- [  ] String dates work in all DSL macros
- [  ] String durations work in all DSL macros
- [  ] Single items don't require lists
- [  ] Parent tasks auto-calculate duration
- [  ] All existing examples still work
- [  ] New simple examples created
- [  ] Documentation updated
- [  ] Migration guide written
- [  ] 100% backward compatibility
- [  ] All tests pass (existing + new)

## Timeline

**Week 1 (DSL Improvements):**
- Days 1-2: String parsing (dates & durations) + tests
- Days 3-4: DSL macro updates + tests
- Day 5: Parent task auto-duration + tests

**Then proceed to features:**
- Weeks 2-3: Resource Leveling (from original plan)
- Weeks 4-5: Scenario Analysis (from original plan)
- Weeks 6-7: Gantt Export (from original plan)

Total: 7 weeks to complete all improvements + features
