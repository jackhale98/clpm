# PROJECT JUGGLER - Complete Implementation Plan
## Modern TaskJuggler Replacement in Common Lisp

**Version:** 1.0  
**Target Audience:** AI Agent implementing the system  
**Methodology:** Test-Driven Development (TDD)  
**Language:** Common Lisp  
**Paradigm:** DSL using macros, CLOS for domain objects

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [System Architecture](#system-architecture)
3. [TaskJuggler Algorithm Analysis](#taskjuggler-algorithm-analysis)
4. [Task Reference System](#task-reference-system)
5. [Implementation Phases](#implementation-phases)
6. [Test-Driven Development Guide](#test-driven-development-guide)
7. [Critical Features](#critical-features)
8. [Pitfalls and Solutions](#pitfalls-and-solutions)
9. [Code Organization](#code-organization)
10. [Examples and Test Cases](#examples-and-test-cases)

---

## Executive Summary

### Project Goals

Create a modern, powerful project management DSL in Common Lisp that:
- Matches/exceeds TaskJuggler 3 capabilities
- Uses text-first, git-friendly S-expression files
- Provides interactive REPL workflow
- Follows PMP (Project Management Professional) best practices
- Supports enterprise-scale projects (1000+ tasks, 100+ resources)

### Key Design Decisions

1. **Task References**: Symbol-based with namespace prefixes for included files (like Lisp packages)
2. **Scheduling**: Heuristic-based (like TaskJuggler), NOT optimization-based
3. **State Management**: Session-based with change tracking
4. **File Format**: Pure S-expressions for version control
5. **Architecture**: CLOS for domain objects, macros for DSL

### Success Criteria

- Load 1000+ task project in <5 seconds
- Schedule 1000+ tasks in <30 seconds
- All references resolve correctly
- REPL workflow is intuitive
- Generate professional reports (HTML, CSV, PDF)
- Pass 200+ test cases

---

## System Architecture

### Core Components

```
┌─────────────────────────────────────────────────────────────┐
│                      PROJECT JUGGLER                         │
├─────────────────────────────────────────────────────────────┤
│  DSL Layer (Macros)                                         │
│  ├─ defproject, deftask, defresource, defaccount           │
│  ├─ depends-on, allocate, booking                          │
│  └─ defreport, defscenario                                 │
├─────────────────────────────────────────────────────────────┤
│  Domain Model (CLOS)                                        │
│  ├─ project, task, resource, account                       │
│  ├─ scenario, allocation, dependency                       │
│  └─ shift, calendar, booking                               │
├─────────────────────────────────────────────────────────────┤
│  Scheduling Engine                                          │
│  ├─ Critical path calculation                              │
│  ├─ Resource allocation (heuristic)                        │
│  ├─ Path criticalness computation                          │
│  └─ ASAP/ALAP scheduling                                   │
├─────────────────────────────────────────────────────────────┤
│  Session Management                                         │
│  ├─ Load/Save project files                                │
│  ├─ Change tracking & undo/redo                            │
│  ├─ Reference resolution                                   │
│  └─ Validation framework                                   │
├─────────────────────────────────────────────────────────────┤
│  Reporting Engine                                           │
│  ├─ Task reports (Gantt, etc)                              │
│  ├─ Resource reports                                        │
│  ├─ Earned Value Management                                │
│  └─ Export formats (HTML, CSV, PDF)                        │
└─────────────────────────────────────────────────────────────┘
```

### Package Structure

```lisp
(defpackage #:project-juggler
  (:use #:cl)
  (:nicknames #:pj)
  (:export
   ;; Core forms
   #:defproject #:deftask #:defresource #:defaccount
   
   ;; Accessors
   #:project #:task #:resource
   
   ;; Time utilities
   #:date #:duration #:interval
   
   ;; Dependencies
   #:depends-on #:allocate
   
   ;; Scheduling
   #:schedule #:critical-path
   
   ;; Session
   #:load-project-session #:save-session
   
   ;; Reports
   #:generate-report))
```

---

## TaskJuggler Algorithm Analysis

### Scheduling Algorithm (From TaskJuggler Source)

TaskJuggler uses a **heuristic-based** scheduling algorithm, NOT an optimization algorithm. This is critical to understand.

#### Key Insights from TaskJuggler Internals:

1. **Not Optimization-Based**: TaskJuggler doesn't search solution space - that would take hours for real projects

2. **Heuristic Approach**: Uses priority-based heuristic to decide resource allocation

3. **Sorting Criteria**: Tasks sorted by:
   - Priority (user-defined, high to low)
   - Path Criticalness (high to low)
   - Index (low to high)

4. **Ready State**: Task can only be scheduled when "ready" (start date known)

5. **Resource Criticalness**: Statistical measure of resource availability
   - Criticalness > 1.0 means statistically some tasks won't get resources
   - Based on total requested allocations vs available work time

6. **Path Criticalness**: For each task, sum criticalness along longest dependency chain
   - Favors tasks with critical resources
   - Favors tasks with long dependency chains
   - Keeps critical paths short

7. **Task Criticalness**: 
   - For milestones: Based on priority (0-1000 maps to 0-2.0)
   - For effort tasks: effort × average resource criticalness

#### Scheduling Process:

```
1. Calculate resource criticalness for all leaf resources
   - Sum all effort allocations per resource
   - Compare to available work time
   - Resource criticalness = requested / available

2. Calculate task criticalness for all tasks
   - Milestones: priority / 500.0
   - Effort tasks: effort × avg(resource criticalness)

3. Calculate path criticalness for all tasks
   - For each dependency path through task
   - Sum criticalness of all tasks in path
   - Task's path criticalness = max of all paths

4. Sort leaf tasks by:
   - Priority (high → low)
   - Path criticalness (high → low)
   - Index (low → high)

5. Schedule loop:
   WHILE unscheduled tasks exist:
     - Find first ready task
     - Schedule task completely (allocate resources for all time slots)
     - Propagate dates to dependent tasks
     - Mark task as scheduled
     - Continue until all scheduled or error

6. Ready state determination:
   - ASAP tasks: start date must be known
   - ALAP tasks: end date must be known
```

#### ASAP vs ALAP:

- **ASAP** (As Soon As Possible): Schedule from start to end
  - Uses task start date or depends-on earliest end
  - Most common mode

- **ALAP** (As Late As Possible): Schedule from end to start
  - Uses task end date or precedes latest start
  - Use carefully - can cause priority inversion

#### Resource Allocation (Per Time Slot):

```
For each time slot in task duration:
  For each allocation in task:
    candidates = get candidate resources from allocation
    Sort candidates by selection mode:
      - minAllocationProbability (default)
      - maxLoad
      - minLoad
      - order
      - random
    
    For each candidate:
      IF resource available at this time slot:
        IF resource capacity not exceeded:
          IF task limits not exceeded:
            Allocate resource to time slot
            BREAK
```

#### Implementation Notes:

1. **Time Slots**: TaskJuggler works in discrete time slots (typically 1 hour)
2. **No Backtracking**: Once scheduled, tasks don't get rescheduled
3. **Deterministic**: Same input = same output (unless using random selection)
4. **Incremental**: Can add constraints and re-schedule

---

## Task Reference System

### User's Preferred Approach: Symbol-Based with Namespace Prefixes

Based on user feedback, we'll use **symbol-based references** with **namespace prefixes** for included files, similar to Common Lisp's package system.

### Design

```lisp
;; Main project file: project.lisp
(defproject acso "Accounting Software" ...
  ;; Tasks in main file use bare symbols
  (deftask requirements "Requirements")
  (deftask software "Software"
    (:depends-on requirements))  ; Direct symbol reference
  
  ;; Include external file with namespace prefix
  (include "infrastructure.lisp" :as infra)
  
  ;; Reference task from included file
  (deftask deployment "Deployment"
    (:depends-on software infra:servers)))  ; Prefixed reference

;; infrastructure.lisp
(in-namespace infra)  ; Declare namespace

(deftask servers "Server Setup" ...)
(deftask network "Network Config" 
  (:depends-on servers))  ; Within namespace, use bare symbols
```

### Implementation Strategy

#### 1. Namespace System

```lisp
(defvar *current-namespace* nil
  "Current namespace for task/resource definitions")

(defvar *namespace-registry* (make-hash-table :test 'equal)
  "Maps namespace names to namespace objects")

(defclass namespace ()
  ((name :initarg :name :reader namespace-name)
   (tasks :initform (make-hash-table :test 'eq) :accessor namespace-tasks)
   (resources :initform (make-hash-table :test 'eq) :accessor namespace-resources)
   (source-file :initarg :source-file :reader namespace-source-file)))

(defmacro in-namespace (name)
  "Set current namespace for subsequent definitions"
  `(setf *current-namespace* 
         (or (gethash ',name *namespace-registry*)
             (setf (gethash ',name *namespace-registry*)
                   (make-instance 'namespace 
                                  :name ',name
                                  :source-file *load-pathname*)))))

(defmacro include (filespec &key as)
  "Include file with optional namespace prefix"
  `(let ((*current-namespace* 
           ,(if as
                `(or (gethash ',as *namespace-registry*)
                     (setf (gethash ',as *namespace-registry*)
                           (make-instance 'namespace :name ',as)))
                '*current-namespace*)))
     (load ,filespec)))
```

#### 2. Qualified Symbol Resolution

```lisp
(defun resolve-task-reference (ref &optional (project *current-project*))
  "Resolve possibly-qualified symbol to task
  
  Examples:
    requirements        → current namespace
    infra:servers      → infra namespace
    /acso/requirements → absolute path (fallback)"
  (typecase ref
    (symbol
     (let* ((name (symbol-name ref))
            (colon-pos (position #\: name)))
       (if colon-pos
           ;; Qualified: namespace:task
           (let* ((ns-name (subseq name 0 colon-pos))
                  (task-name (subseq name (1+ colon-pos)))
                  (namespace (gethash (intern ns-name :keyword) 
                                    *namespace-registry*)))
             (unless namespace
               (error "Unknown namespace: ~A" ns-name))
             (or (gethash (intern task-name) (namespace-tasks namespace))
                 (error "Task not found in namespace ~A: ~A" ns-name task-name)))
           ;; Unqualified: current namespace
           (or (gethash ref (namespace-tasks *current-namespace*))
               ;; Fallback: search in default namespace
               (gethash ref (namespace-tasks 
                              (gethash nil *namespace-registry*)))
               (error "Task not found: ~A" ref)))))
    
    (string
     ;; Absolute path like "/acso/requirements"
     (resolve-absolute-path ref project))
    
    (t
     (error "Invalid task reference: ~A" ref))))
```

#### 3. Enhanced Task Registration

```lisp
(defun register-task (task &optional (namespace *current-namespace*))
  "Register task in namespace and project"
  (let ((id (task-id task)))
    ;; Check for duplicates in namespace
    (when (gethash id (namespace-tasks namespace))
      (cerror "Replace existing task"
              "Task ~A already exists in namespace ~A" 
              id (namespace-name namespace)))
    
    ;; Register in namespace
    (setf (gethash id (namespace-tasks namespace)) task)
    
    ;; Register in project with qualified name
    (let ((qualified-id (if (namespace-name namespace)
                            (intern (format nil "~A:~A" 
                                          (namespace-name namespace) 
                                          id))
                            id)))
      (setf (gethash qualified-id (project-tasks *current-project*)) task))
    
    task))
```

### Advantages of This Approach

1. ✅ **Natural for Lisp users**: Familiar package-like system
2. ✅ **Git-friendly**: Moving files doesn't break references if namespace stays same
3. ✅ **No UUIDs needed**: Human-readable references
4. ✅ **Explicit imports**: Clear where external references come from
5. ✅ **Refactor-safe**: Rename within namespace doesn't affect external refs
6. ✅ **Simple**: No complex path resolution, WBS codes, etc.

### Example Usage

```lisp
;; main.lisp
(defproject company "Company Projects" ...
  
  ;; Include shared resources with namespace
  (include "resources/team.lisp" :as team)
  (include "resources/equipment.lisp" :as equip)
  
  ;; Main project tasks
  (deftask phase1 "Phase 1"
    (:allocate team:alice team:bob)  ; Reference by namespace
    
    (deftask requirements "Requirements"
      (:allocate team:pm))
    
    (deftask design "Design"
      (:depends-on requirements)
      (:allocate team:alice equip:workstation)))
  
  ;; Include another project
  (include "infrastructure/setup.lisp" :as infra)
  
  (deftask integration "Integration"
    (:depends-on phase1 infra:servers)))

;; resources/team.lisp
(in-namespace team)

(defresource alice "Alice Smith" ...)
(defresource bob "Bob Johnson" ...)
(defresource pm "Project Manager" ...)

;; infrastructure/setup.lisp  
(in-namespace infra)

(deftask servers "Setup Servers" ...)
(deftask network "Configure Network"
  (:depends-on servers))  ; Within namespace, no prefix needed
```

---

## Implementation Phases

### Phase 0: Setup & Foundation (Week 1)

**Deliverable**: Working ASDF system with test framework

**Tasks**:
1. Setup ASDF system definition
2. Setup test framework (FiveAM or similar)
3. Create package structure
4. Setup CI/CD (optional but recommended)

**Test First**:
```lisp
(test basic-package-loads
  "Package loads without errors"
  (is (find-package :project-juggler)))

(test can-create-basic-types
  "Can create date, duration"
  (is (date 2024 1 1))
  (is (duration 5 :days)))
```

**Implementation**:
```lisp
;;;; project-juggler.asd
(defsystem "project-juggler"
  :description "Modern TaskJuggler replacement in Common Lisp"
  :version "1.0.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (#:local-time
               #:cl-ppcre
               #:alexandria
               #:split-sequence)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "types")
                 (:file "classes")
                 ...))))

(defsystem "project-juggler/tests"
  :depends-on (#:project-juggler
               #:fiveam)
  :components ((:module "tests"
                :serial t
                :components
                ((:file "test-package")
                 (:file "test-types")
                 ...))))
```

### Phase 1: Core Types & Data Structures (Week 1-2)

**Deliverable**: Working temporal types and CLOS classes

#### 1.1: Temporal Types (TDD)

**Test First**:
```lisp
(test date-creation
  "Can create dates"
  (let ((d (date 2024 3 15)))
    (is (= 2024 (date-year d)))
    (is (= 3 (date-month d)))
    (is (= 15 (date-day d)))))

(test duration-creation
  "Can create durations in various units"
  (is (duration 5 :days))
  (is (duration 40 :hours))
  (is (duration 2 :weeks)))

(test duration-conversion
  "Can convert durations"
  (let ((d (duration 2 :weeks)))
    (is (= 14 (duration-in-days d)))
    (is (= 336 (duration-in-hours d)))))

(test date-arithmetic
  "Can add/subtract durations from dates"
  (let ((d1 (date 2024 3 15))
        (dur (duration 5 :days)))
    (let ((d2 (date+ d1 dur)))
      (is (= 20 (date-day d2))))))

(test interval-creation
  "Can create and query intervals"
  (let ((i (interval (date 2024 3 1) (date 2024 3 31))))
    (is (= 30 (interval-duration-days i)))
    (is (contains-date-p i (date 2024 3 15)))
    (is (not (contains-date-p i (date 2024 4 1))))))
```

**Implementation**:
```lisp
;;;; src/types.lisp

(defclass pj-date ()
  ((timestamp :initarg :timestamp 
              :reader date-timestamp
              :documentation "Internal timestamp representation"))
  (:documentation "Project Juggler date"))

(defun date (year month day &optional (hour 0) (minute 0) (second 0))
  "Create a date"
  (make-instance 'pj-date
                 :timestamp (local-time:encode-timestamp 
                              0 second minute hour day month year)))

(defun date-year (date)
  (local-time:timestamp-year (date-timestamp date)))

(defun date-month (date)
  (local-time:timestamp-month (date-timestamp date)))

(defun date-day (date)
  (local-time:timestamp-day (date-timestamp date)))

(defclass duration ()
  ((value :initarg :value :reader duration-value)
   (unit :initarg :unit :reader duration-unit))
  (:documentation "Duration in various units"))

(defun duration (value unit)
  "Create duration"
  (make-instance 'duration :value value :unit unit))

(defgeneric duration-in-days (duration)
  (:documentation "Convert duration to days"))

(defmethod duration-in-days ((dur duration))
  (ecase (duration-unit dur)
    (:minutes (/ (duration-value dur) 60 24))
    (:hours (/ (duration-value dur) 24))
    (:days (duration-value dur))
    (:weeks (* (duration-value dur) 7))
    (:months (* (duration-value dur) 30))  ; Approximate
    (:years (* (duration-value dur) 365)))) ; Approximate

(defun date+ (date duration)
  "Add duration to date"
  (let ((days (duration-in-days duration)))
    (make-instance 'pj-date
                   :timestamp (local-time:timestamp+ 
                                (date-timestamp date)
                                days :day))))

(defclass interval ()
  ((start :initarg :start :reader interval-start)
   (end :initarg :end :reader interval-end))
  (:documentation "Time interval"))

(defun interval (start end)
  "Create interval"
  (make-instance 'interval :start start :end end))

(defun interval-duration-days (interval)
  "Duration of interval in days"
  (local-time:timestamp-difference 
    (date-timestamp (interval-end interval))
    (date-timestamp (interval-start interval))
    :day))
```

#### 1.2: Core CLOS Classes (TDD)

**Test First**:
```lisp
(test create-project
  "Can create a project"
  (let ((p (make-instance 'project
                          :id 'test
                          :name "Test Project"
                          :start (date 2024 1 1)
                          :end (date 2024 12 31))))
    (is (eq 'test (project-id p)))
    (is (string= "Test Project" (project-name p)))))

(test create-task
  "Can create a task"
  (let ((project (make-test-project))
        (task (make-instance 'task
                            :id 't1
                            :name "Task 1"
                            :project project)))
    (is (eq 't1 (task-id task)))
    (is (eq project (task-project task)))))

(test task-hierarchy
  "Tasks can have parent/child relationships"
  (let* ((project (make-test-project))
         (parent (make-instance 'task :id 'parent :name "Parent" :project project))
         (child (make-instance 'task :id 'child :name "Child" :project project :parent parent)))
    (is (eq parent (task-parent child)))
    (push child (task-subtasks parent))
    (is (member child (task-subtasks parent)))))
```

**Implementation**:
```lisp
;;;; src/classes.lisp

(defclass project ()
  ((id :initarg :id :reader project-id)
   (name :initarg :name :reader project-name)
   (start :initarg :start :reader project-start)
   (end :initarg :end :reader project-end)
   (tasks :initform (make-hash-table :test 'eq) :accessor project-tasks)
   (resources :initform (make-hash-table :test 'eq) :accessor project-resources)
   (scenarios :initform nil :accessor project-scenarios)
   (current-scenario :initform 'plan :accessor project-current-scenario))
  (:documentation "A project"))

(defclass task ()
  ((id :initarg :id :reader task-id)
   (name :initarg :name :reader task-name)
   (project :initarg :project :reader task-project)
   (parent :initarg :parent :initform nil :accessor task-parent)
   (subtasks :initform nil :accessor task-subtasks)
   (dependencies :initform nil :accessor task-dependencies)
   (allocations :initform nil :accessor task-allocations)
   (effort :initarg :effort :initform nil :accessor task-effort)
   (duration :initarg :duration :initform nil :accessor task-duration)
   (start :initarg :start :initform nil :accessor task-start)
   (end :initarg :end :initform nil :accessor task-end)
   (priority :initarg :priority :initform 500 :accessor task-priority)
   (milestone :initarg :milestone :initform nil :accessor task-milestone-p)
   (complete :initarg :complete :initform 0 :accessor task-complete)
   
   ;; Scheduling state
   (scheduled :initform nil :accessor task-scheduled-p)
   (criticalness :initform 0.0 :accessor task-criticalness)
   (path-criticalness :initform 0.0 :accessor task-path-criticalness))
  (:documentation "A task"))

(defclass resource ()
  ((id :initarg :id :reader resource-id)
   (name :initarg :name :reader resource-name)
   (project :initarg :project :reader resource-project)
   (parent :initarg :parent :initform nil :accessor resource-parent)
   (efficiency :initarg :efficiency :initform 1.0 :accessor resource-efficiency)
   (rate :initarg :rate :initform nil :accessor resource-rate)
   (limits :initform nil :accessor resource-limits)
   
   ;; Scheduling state
   (criticalness :initform 0.0 :accessor resource-criticalness)
   (allocated-effort :initform 0.0 :accessor resource-allocated-effort)
   (available-effort :initform 0.0 :accessor resource-available-effort))
  (:documentation "A resource"))

(defclass dependency ()
  ((source :initarg :source :reader dependency-source)
   (target-ref :initarg :target-ref :reader dependency-target-ref)
   (target :initform nil :accessor dependency-target)
   (type :initarg :type :initform :finish-start :reader dependency-type)
   (gap-duration :initarg :gap :initform nil :reader dependency-gap))
  (:documentation "A dependency between tasks"))

(defclass allocation ()
  ((task :initarg :task :reader allocation-task)
   (resource-refs :initarg :resources :reader allocation-resource-refs)
   (resources :initform nil :accessor allocation-resources)
   (mandatory :initarg :mandatory :initform nil :reader allocation-mandatory-p))
  (:documentation "Resource allocation to task"))
```

### Phase 2: Namespace & Reference System (Week 2)

**Deliverable**: Working namespace system with reference resolution

**Test First**:
```lisp
(test namespace-creation
  "Can create and register namespaces"
  (let ((ns (make-instance 'namespace :name 'test-ns)))
    (setf (gethash 'test-ns *namespace-registry*) ns)
    (is (eq ns (gethash 'test-ns *namespace-registry*)))))

(test task-registration-in-namespace
  "Tasks registered in namespace"
  (let* ((*current-namespace* (make-instance 'namespace :name 'test))
         (task (make-instance 'task :id 't1 :name "Task 1")))
    (register-task task)
    (is (eq task (gethash 't1 (namespace-tasks *current-namespace*))))))

(test qualified-reference-resolution
  "Can resolve qualified task references"
  (let* ((ns1 (make-instance 'namespace :name 'ns1))
         (ns2 (make-instance 'namespace :name 'ns2))
         (task1 (make-instance 'task :id 't1 :name "Task 1"))
         (task2 (make-instance 'task :id 't2 :name "Task 2")))
    (setf (gethash 'ns1 *namespace-registry*) ns1)
    (setf (gethash 'ns2 *namespace-registry*) ns2)
    (setf (gethash 't1 (namespace-tasks ns1)) task1)
    (setf (gethash 't2 (namespace-tasks ns2)) task2)
    
    ;; Should resolve qualified reference
    (let ((*current-namespace* ns1))
      (is (eq task2 (resolve-task-reference 'ns2:t2))))))

(test include-with-namespace
  "Include file sets up namespace correctly"
  ;; Create temp file with task definitions
  (with-temp-file (file "(in-namespace test)
                         (deftask t1 \"Task 1\")")
    (include file :as test)
    (is (gethash 'test *namespace-registry*))))
```

**Implementation**: See [Task Reference System](#task-reference-system) section above

### Phase 3: DSL Macros (Week 2-3)

**Deliverable**: Working defproject, deftask, defresource macros

**Test First**:
```lisp
(test defproject-basic
  "defproject creates project"
  (let ((project (defproject test "Test" (date 2024 1 1) (date 2024 12 31))))
    (is (project-p project))
    (is (eq 'test (project-id project)))))

(test deftask-basic
  "deftask creates task"
  (with-test-project
    (let ((task (deftask t1 "Task 1"
                  (:effort (duration 5 :days)))))
      (is (task-p task))
      (is (eq 't1 (task-id task)))
      (is (equal (duration 5 :days) (task-effort task))))))

(test deftask-hierarchy
  "deftask creates hierarchy"
  (with-test-project
    (deftask parent "Parent"
      (deftask child1 "Child 1")
      (deftask child2 "Child 2"))
    (let ((parent (find-task 'parent)))
      (is (= 2 (length (task-subtasks parent)))))))

(test depends-on-basic
  "depends-on creates dependencies"
  (with-test-project
    (deftask t1 "Task 1")
    (deftask t2 "Task 2"
      (:depends-on t1))
    (let ((t2 (find-task 't2)))
      (is (= 1 (length (task-dependencies t2)))))))

(test depends-on-qualified
  "depends-on with namespace prefix"
  (with-test-project
    (include "external.lisp" :as ext)  ; Contains ext:t1
    (deftask t2 "Task 2"
      (:depends-on ext:t1))
    (is (find-dependency-to (find-task 't2) 'ext:t1))))
```

**Implementation**:
```lisp
;;;; src/dsl/defproject.lisp

(defmacro defproject (id name start end &body options)
  "Define a project"
  `(let* ((project (make-instance 'project
                                  :id ',id
                                  :name ,name
                                  :start ,start
                                  :end ,end))
          (*current-project* project)
          (*current-namespace* (or (gethash nil *namespace-registry*)
                                  (setf (gethash nil *namespace-registry*)
                                        (make-instance 'namespace :name nil)))))
     ,@(mapcar #'process-project-option options)
     (setf (gethash ',id *project-registry*) project)
     project))

;;;; src/dsl/deftask.lisp

(defmacro deftask (id name &body body)
  "Define a task"
  (let ((task-var (gensym "TASK")))
    `(let ((,task-var (make-instance 'task
                                     :id ',id
                                     :name ,name
                                     :project *current-project*
                                     :parent *current-task*)))
       ;; Register task
       (register-task ,task-var)
       
       ;; Add to parent if exists
       (when *current-task*
         (push ,task-var (task-subtasks *current-task*)))
       
       ;; Process body with task as current
       (let ((*current-task* ,task-var))
         ,@(process-task-body body))
       
       ,task-var)))

(defun process-task-body (body)
  "Process task body forms"
  (loop for form in body
        collect (cond
                  ((and (listp form) (keywordp (first form)))
                   ;; Keyword form like (:effort ...)
                   (process-task-attribute form))
                  ((and (listp form) (eq (first form) 'deftask))
                   ;; Nested task
                   form)
                  (t
                   (error "Invalid task body form: ~A" form)))))

(defun process-task-attribute (form)
  "Process task attribute form"
  (destructuring-bind (keyword &rest args) form
    (ecase keyword
      (:effort
       `(setf (task-effort *current-task*) ,(first args)))
      (:duration
       `(setf (task-duration *current-task*) ,(first args)))
      (:start
       `(setf (task-start *current-task*) ,(first args)))
      (:end
       `(setf (task-end *current-task*) ,(first args)))
      (:priority
       `(setf (task-priority *current-task*) ,(first args)))
      (:depends-on
       `(progn
          ,@(loop for ref in args
                  collect `(add-dependency-by-reference *current-task* ',ref))))
      (:allocate
       `(progn
          ,@(loop for ref in args
                  collect `(add-allocation-by-reference *current-task* ',ref))))
      (:milestone
       `(setf (task-milestone-p *current-task*) ,(first args))))))

;;;; src/dsl/dependencies.lisp

(defun add-dependency-by-reference (task ref)
  "Add dependency with unresolved reference"
  (let ((dep (make-instance 'dependency
                           :source task
                           :target-ref ref
                           :type :finish-start)))
    (push dep (task-dependencies task))
    dep))
```

### Phase 4: Reference Resolution & Validation (Week 3)

**Deliverable**: All references resolve, circular dependencies detected

**Test First**:
```lisp
(test resolve-simple-reference
  "Resolve reference in same namespace"
  (with-test-project
    (deftask t1 "Task 1")
    (deftask t2 "Task 2" (:depends-on t1))
    (finalize-project *current-project*)
    (let ((dep (first (task-dependencies (find-task 't2)))))
      (is (eq (find-task 't1) (dependency-target dep))))))

(test resolve-qualified-reference
  "Resolve namespace-qualified reference"
  (with-test-project
    (include "external.lisp" :as ext)
    (deftask t2 "Task 2" (:depends-on ext:t1))
    (finalize-project *current-project*)
    (is (dependency-target (first (task-dependencies (find-task 't2)))))))

(test detect-circular-dependency
  "Detect circular dependencies"
  (with-test-project
    (deftask t1 "Task 1" (:depends-on t3))
    (deftask t2 "Task 2" (:depends-on t1))
    (deftask t3 "Task 3" (:depends-on t2))
    (signals circular-dependency-error
      (finalize-project *current-project*))))

(test unresolved-reference-error
  "Error on unresolved reference"
  (with-test-project
    (deftask t1 "Task 1" (:depends-on nonexistent))
    (signals reference-error
      (finalize-project *current-project*))))
```

**Implementation**:
```lisp
;;;; src/validation.lisp

(defun finalize-project (project)
  "Resolve all references and validate"
  ;; Step 1: Resolve all task references
  (resolve-all-task-references project)
  
  ;; Step 2: Resolve all resource references
  (resolve-all-resource-references project)
  
  ;; Step 3: Detect circular dependencies
  (detect-circular-dependencies project)
  
  ;; Step 4: Build dependency graph
  (build-dependency-graph project)
  
  ;; Step 5: Calculate available resource time
  (calculate-resource-availability project)
  
  t)

(defun resolve-all-task-references (project)
  "Resolve all task dependencies"
  (loop for task being the hash-values of (project-tasks project)
        do (loop for dep in (task-dependencies task)
                 unless (dependency-target dep)
                 do (setf (dependency-target dep)
                         (resolve-task-reference 
                           (dependency-target-ref dep)
                           project)))))

(defun detect-circular-dependencies (project)
  "Detect cycles in dependency graph using DFS"
  (let ((visited (make-hash-table))
        (rec-stack (make-hash-table))
        (cycles nil))
    
    (labels ((visit (task path)
               (setf (gethash task visited) t)
               (setf (gethash task rec-stack) t)
               
               (dolist (dep (task-dependencies task))
                 (let ((target (dependency-target dep)))
                   (cond
                     ((gethash target rec-stack)
                      ;; Cycle found
                      (push (append path (list target)) cycles))
                     ((not (gethash target visited))
                      (visit target (append path (list task)))))))
               
               (setf (gethash task rec-stack) nil)))
      
      ;; Visit all tasks
      (loop for task being the hash-values of (project-tasks project)
            unless (gethash task visited)
            do (visit task nil)))
    
    (when cycles
      (error 'circular-dependency-error
             :cycles cycles
             :message "Circular dependencies detected"))))
```

### Phase 5: Scheduling Algorithm (Week 3-4)

**Deliverable**: Working ASAP scheduling with resource allocation

This is the **most complex** part. Follow TaskJuggler's algorithm carefully.

**Test First**:
```lisp
(test schedule-single-task
  "Schedule single task with start date"
  (with-test-project
    (deftask t1 "Task 1"
      (:start (date 2024 3 1))
      (:duration (duration 5 :days)))
    (schedule *current-project*)
    (let ((task (find-task 't1)))
      (is (date= (date 2024 3 1) (task-start task)))
      (is (date= (date 2024 3 6) (task-end task))))))

(test schedule-dependent-tasks
  "Schedule tasks with dependencies"
  (with-test-project
    (deftask t1 "Task 1"
      (:start (date 2024 3 1))
      (:duration (duration 5 :days)))
    (deftask t2 "Task 2"
      (:depends-on t1)
      (:duration (duration 3 :days)))
    (schedule *current-project*)
    (is (date= (date 2024 3 6) (task-start (find-task 't2))))
    (is (date= (date 2024 3 9) (task-end (find-task 't2))))))

(test schedule-with-effort-and-resources
  "Schedule effort-based task with resource allocation"
  (with-test-project
    (defresource dev "Developer")
    (deftask t1 "Task 1"
      (:start (date 2024 3 1))
      (:effort (duration 40 :hours))  ; 5 days at 8h/day
      (:allocate dev))
    (schedule *current-project*)
    (is (date= (date 2024 3 6) (task-end (find-task 't1))))))

(test schedule-critical-path
  "Calculate critical path"
  (with-test-project
    (deftask t1 "Task 1"
      (:start (date 2024 3 1))
      (:duration (duration 5 :days)))
    (deftask t2 "Task 2"
      (:depends-on t1)
      (:duration (duration 3 :days)))
    (deftask t3 "Task 3"
      (:depends-on t1)
      (:duration (duration 10 :days)))  ; Longer path
    (deftask t4 "Task 4"
      (:depends-on t2 t3)
      (:duration (duration 2 :days)))
    (schedule *current-project*)
    (let ((cp (critical-path *current-project*)))
      ;; t1 -> t3 -> t4 is critical path
      (is (member (find-task 't1) cp))
      (is (member (find-task 't3) cp))
      (is (member (find-task 't4) cp))
      (is (not (member (find-task 't2) cp))))))
```

**Implementation** (following TaskJuggler):
```lisp
;;;; src/scheduling/scheduler.lisp

(defun schedule (project &optional (scenario 'plan))
  "Schedule the project using TaskJuggler's heuristic algorithm"
  
  ;; Step 1: Calculate resource criticalness
  (calculate-resource-criticalness project scenario)
  
  ;; Step 2: Calculate task criticalness
  (calculate-task-criticalness project scenario)
  
  ;; Step 3: Calculate path criticalness
  (calculate-path-criticalness project scenario)
  
  ;; Step 4: Sort tasks by priority
  (let ((sorted-tasks (sort-tasks-for-scheduling project scenario)))
    
    ;; Step 5: Schedule loop
    (loop while (some (lambda (task) (not (task-scheduled-p task))) 
                     sorted-tasks)
          do (let ((ready-task (find-ready-task sorted-tasks)))
               (unless ready-task
                 (error "No ready task found - project cannot be scheduled"))
               
               ;; Schedule this task completely
               (schedule-task ready-task scenario)
               
               ;; Mark as scheduled
               (setf (task-scheduled-p ready-task) t)
               
               ;; Propagate dates to dependent tasks
               (propagate-dates ready-task scenario)))))

(defun calculate-resource-criticalness (project scenario)
  "Calculate criticalness for all resources"
  (loop for resource being the hash-values of (project-resources project)
        do (let* ((allocated (resource-allocated-effort resource))
                  (available (resource-available-effort resource))
                  (criticalness (if (zerop available)
                                    0.0
                                    (/ allocated available))))
             (setf (resource-criticalness resource) criticalness))))

(defun calculate-task-criticalness (project scenario)
  "Calculate criticalness for all tasks"
  (loop for task being the hash-values of (project-tasks project)
        do (if (task-milestone-p task)
               ;; Milestone criticalness based on priority
               (setf (task-criticalness task)
                     (/ (task-priority task) 500.0))
               
               ;; Effort task criticalness
               (when (and (task-effort task) (task-allocations task))
                 (let ((avg-resource-criticalness
                         (/ (loop for alloc in (task-allocations task)
                                  sum (loop for res in (allocation-resources alloc)
                                            sum (resource-criticalness res)))
                            (length (task-allocations task)))))
                   (setf (task-criticalness task)
                         (* (duration-in-hours (task-effort task))
                            avg-resource-criticalness)))))))

(defun calculate-path-criticalness (project scenario)
  "Calculate path criticalness for all tasks"
  (let ((memo (make-hash-table)))
    (labels ((compute (task)
               (or (gethash task memo)
                   (setf (gethash task memo)
                         (+ (task-criticalness task)
                            (loop for dep in (task-dependencies task)
                                  maximize (compute (dependency-target dep))))))))
      
      (loop for task being the hash-values of (project-tasks project)
            do (setf (task-path-criticalness task) (compute task))))))

(defun sort-tasks-for-scheduling (project scenario)
  "Sort tasks by priority, path criticalness, index"
  (let ((tasks (loop for task being the hash-values of (project-tasks project)
                     when (not (task-subtasks task))  ; Only leaf tasks
                     collect task)))
    (sort tasks
          (lambda (a b)
            (or (> (task-priority a) (task-priority b))
                (and (= (task-priority a) (task-priority b))
                     (> (task-path-criticalness a) 
                        (task-path-criticalness b)))
                (and (= (task-priority a) (task-priority b))
                     (= (task-path-criticalness a) 
                        (task-path-criticalness b))
                     (< (task-index a) (task-index b))))))))

(defun find-ready-task (tasks)
  "Find first task that is ready for scheduling"
  (find-if #'task-ready-p tasks))

(defun task-ready-p (task)
  "Check if task is ready for scheduling"
  (and (not (task-scheduled-p task))
       ;; Start date must be known (from explicit start or dependencies)
       (or (task-start task)
           (every #'task-scheduled-p 
                  (mapcar #'dependency-target (task-dependencies task))))))

(defun schedule-task (task scenario)
  "Schedule a single task completely"
  (cond
    ((task-milestone-p task)
     ;; Milestone: use dependency end dates
     (schedule-milestone task scenario))
    
    ((task-duration task)
     ;; Duration-based: simple date arithmetic
     (schedule-duration-task task scenario))
    
    ((task-effort task)
     ;; Effort-based: allocate resources
     (schedule-effort-task task scenario))
    
    (t
     (error "Task ~A has no duration or effort" (task-name task)))))

(defun schedule-milestone (task scenario)
  "Schedule a milestone task"
  (let ((start (or (task-start task)
                   (compute-earliest-start task))))
    (setf (task-start task) start)
    (setf (task-end task) start)))

(defun schedule-duration-task (task scenario)
  "Schedule task with fixed duration"
  (let* ((start (or (task-start task)
                    (compute-earliest-start task)))
         (duration (task-duration task))
         (end (date+ start duration)))
    (setf (task-start task) start)
    (setf (task-end task) end)))

(defun schedule-effort-task (task scenario)
  "Schedule task with effort and resource allocation"
  (let* ((start (or (task-start task)
                    (compute-earliest-start task)))
         (effort-hours (duration-in-hours (task-effort task)))
         (allocations (task-allocations task))
         (current-date start)
         (remaining-effort effort-hours))
    
    ;; Allocate resources time slot by time slot
    (loop while (> remaining-effort 0)
          do (let ((allocated-this-slot 0))
               ;; Try to allocate each resource for this time slot
               (dolist (alloc allocations)
                 (dolist (resource (allocation-resources alloc))
                   (when (resource-available-at-p resource current-date)
                     (let ((capacity (resource-capacity-at resource current-date)))
                       (incf allocated-this-slot capacity)
                       (record-allocation resource task current-date capacity)))))
               
               ;; Advance time
               (decf remaining-effort allocated-this-slot)
               (setf current-date (date+ current-date (duration 1 :day)))))
    
    (setf (task-start task) start)
    (setf (task-end task) current-date)))

(defun compute-earliest-start (task)
  "Compute earliest start based on dependencies"
  (if (null (task-dependencies task))
      (project-start (task-project task))
      (loop for dep in (task-dependencies task)
            maximize (dependency-target-end dep))))

(defun propagate-dates (task scenario)
  "Propagate dates to dependent tasks"
  ;; Find all tasks that depend on this task
  (loop for other-task being the hash-values of (project-tasks (task-project task))
        when (some (lambda (dep) 
                     (eq task (dependency-target dep)))
                   (task-dependencies other-task))
        do (update-task-earliest-start other-task)))
```

### Phase 6: Critical Path Calculation (Week 4)

**Test First**:
```lisp
(test critical-path-simple
  "Critical path in simple linear project"
  (with-test-project
    (deftask t1 "T1" (:start (date 2024 3 1)) (:duration (duration 5 :days)))
    (deftask t2 "T2" (:depends-on t1) (:duration (duration 3 :days)))
    (deftask t3 "T3" (:depends-on t2) (:duration (duration 2 :days)))
    (schedule *current-project*)
    (let ((cp (critical-path *current-project*)))
      (is (= 3 (length cp)))
      (is (every (lambda (t) (member t cp)) (list 't1 't2 't3))))))

(test critical-path-branching
  "Critical path with branching"
  (with-test-project
    (deftask t1 "T1" (:start (date 2024 3 1)) (:duration (duration 5 :days)))
    (deftask t2a "T2a" (:depends-on t1) (:duration (duration 3 :days)))
    (deftask t2b "T2b" (:depends-on t1) (:duration (duration 10 :days)))  ; Critical
    (deftask t3 "T3" (:depends-on t2a t2b) (:duration (duration 1 :day)))
    (schedule *current-project*)
    (let ((cp (critical-path *current-project*)))
      ;; Path should be t1 -> t2b -> t3
      (is (member 't2b (mapcar #'task-id cp)))
      (is (not (member 't2a (mapcar #'task-id cp)))))))
```

**Implementation**:
```lisp
;;;; src/scheduling/critical-path.lisp

(defun critical-path (project &optional (scenario 'plan))
  "Calculate the critical path(s) of the project"
  ;; Critical path = path(s) with zero slack
  (let ((all-paths (find-all-paths project))
        (critical-paths nil)
        (max-duration 0))
    
    ;; Find maximum duration
    (dolist (path all-paths)
      (let ((duration (path-duration path)))
        (when (> duration max-duration)
          (setf max-duration duration))))
    
    ;; Paths with duration = max are critical
    (dolist (path all-paths)
      (when (= (path-duration path) max-duration)
        (push path critical-paths)))
    
    ;; Return unique tasks on critical paths
    (remove-duplicates 
      (loop for path in critical-paths
            append path))))

(defun find-all-paths (project)
  "Find all paths from start to end tasks"
  (let ((start-tasks (find-start-tasks project))
        (end-tasks (find-end-tasks project))
        (all-paths nil))
    
    (dolist (start start-tasks)
      (dolist (end end-tasks)
        (setf all-paths 
              (append all-paths 
                     (find-paths-between start end)))))
    all-paths))

(defun find-paths-between (start-task end-task)
  "Find all paths between two tasks"
  (let ((paths nil))
    (labels ((dfs (current-task path)
               (cond
                 ((eq current-task end-task)
                  ;; Reached end, save path
                  (push (reverse (cons current-task path)) paths))
                 
                 ((member current-task path)
                  ;; Cycle detected, stop
                  nil)
                 
                 (t
                  ;; Continue to successors
                  (let ((successors (task-successors current-task)))
                    (dolist (succ successors)
                      (dfs succ (cons current-task path))))))))
      
      (dfs start-task nil))
    paths))

(defun task-successors (task)
  "Get tasks that depend on this task"
  (loop for other-task being the hash-values of (project-tasks (task-project task))
        when (some (lambda (dep) (eq task (dependency-target dep)))
                   (task-dependencies other-task))
        collect other-task))

(defun path-duration (path)
  "Calculate total duration of path"
  (loop for task in path
        sum (task-duration-days task)))

(defun task-duration-days (task)
  "Get task duration in days"
  (if (and (task-start task) (task-end task))
      (local-time:timestamp-difference 
        (date-timestamp (task-end task))
        (date-timestamp (task-start task))
        :day)
      0))
```

### Phase 7: Session Management (Week 4-5)

**Test First**:
```lisp
(test load-project-session
  "Can load project into session"
  (with-temp-file (file "(defproject test \"Test\" ...)")
    (let ((session (load-project-session file)))
      (is (session-p session))
      (is (project-p (session-project session))))))

(test session-change-tracking
  "Changes are tracked in session"
  (with-test-session
    (add-task-interactive 'nil "New Task")
    (is (= 1 (length (session-changes *current-session*))))))

(test session-undo-redo
  "Can undo and redo changes"
  (with-test-session
    (add-task-interactive 'nil "Task 1")
    (let ((task (find-task 'task-1)))
      (is task)
      (undo)
      (is (not (find-task 'task-1)))
      (redo)
      (is (find-task 'task-1)))))

(test save-project-session
  "Can save project back to file"
  (with-temp-file (file "(defproject test \"Test\" ...)")
    (let ((session (load-project-session file)))
      (add-task-interactive 'nil "New Task")
      (save-session)
      ;; Reload and verify
      (let ((session2 (load-project-session file)))
        (is (find-task 'new-task (session-project session2)))))))
```

**Implementation**: See earlier implementation guide sections

### Phase 8: Reporting (Week 5-6)

**Test First**:
```lisp
(test generate-task-report
  "Can generate task report"
  (with-test-project
    (deftask t1 "Task 1" ...)
    (schedule *current-project*)
    (let ((report (generate-report 
                    (make-instance 'task-report
                                  :format :html))))
      (is (stringp report))
      (is (search "Task 1" report)))))

(test generate-resource-report
  "Can generate resource report"
  (with-test-project
    (defresource dev "Developer")
    (deftask t1 "Task 1" (:allocate dev))
    (schedule *current-project*)
    (let ((report (generate-report
                    (make-instance 'resource-report
                                  :format :html))))
      (is (search "Developer" report)))))

(test generate-gantt-chart
  "Can generate Gantt chart data"
  (with-test-project
    (deftask t1 "T1" (:start (date 2024 3 1)) (:duration (duration 5 :days)))
    (deftask t2 "T2" (:depends-on t1) (:duration (duration 3 :days)))
    (schedule *current-project*)
    (let ((gantt-data (generate-gantt-data *current-project*)))
      (is (= 2 (length gantt-data))))))
```

**Implementation**:
```lisp
;;;; src/reporting/reports.lisp

(defclass report ()
  ((id :initarg :id :reader report-id)
   (title :initarg :title :reader report-title)
   (format :initarg :format :reader report-format)
   (columns :initarg :columns :reader report-columns)
   (filter :initarg :filter :initform nil :reader report-filter)
   (sort-by :initarg :sort-by :initform nil :reader report-sort-by))
  (:documentation "Base report class"))

(defclass task-report (report)
  ()
  (:documentation "Task-focused report"))

(defclass resource-report (report)
  ()
  (:documentation "Resource-focused report"))

(defgeneric generate-report (report project)
  (:documentation "Generate report output"))

(defmethod generate-report ((report task-report) project)
  "Generate task report"
  (let* ((tasks (collect-tasks-for-report report project))
         (sorted-tasks (sort-tasks-for-report tasks report)))
    (ecase (report-format report)
      (:html (generate-html-task-report sorted-tasks report))
      (:csv (generate-csv-task-report sorted-tasks report))
      (:pdf (generate-pdf-task-report sorted-tasks report)))))

(defun collect-tasks-for-report (report project)
  "Collect tasks matching report filter"
  (let ((all-tasks (loop for task being the hash-values of (project-tasks project)
                         collect task)))
    (if (report-filter report)
        (remove-if-not (report-filter report) all-tasks)
        all-tasks)))

(defun generate-html-task-report (tasks report)
  "Generate HTML task report"
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%<html>~%<head>~%")
    (format s "<title>~A</title>~%" (report-title report))
    (format s "</head>~%<body>~%")
    (format s "<h1>~A</h1>~%" (report-title report))
    (format s "<table border='1'>~%")
    
    ;; Header
    (format s "<tr>~%")
    (dolist (col (report-columns report))
      (format s "<th>~A</th>~%" (column-title col)))
    (format s "</tr>~%")
    
    ;; Rows
    (dolist (task tasks)
      (format s "<tr>~%")
      (dolist (col (report-columns report))
        (format s "<td>~A</td>~%" (format-column-value col task)))
      (format s "</tr>~%"))
    
    (format s "</table>~%</body>~%</html>~%")))
```

### Phase 9: Advanced Features (Week 6-8)

#### 9.1: Earned Value Management

**Test First**:
```lisp
(test calculate-planned-value
  "Calculate planned value (PV)"
  (with-test-project
    ;; Setup project with costs
    (schedule *current-project*)
    (let ((pv (calculate-planned-value *current-project* (date 2024 3 15))))
      (is (numberp pv))
      (is (>= pv 0)))))

(test calculate-earned-value
  "Calculate earned value (EV) based on completion"
  (with-test-project
    (deftask t1 "Task 1" 
      (:effort (duration 10 :days))
      (:complete 50))
    (schedule *current-project*)
    (let ((ev (calculate-earned-value *current-project* (date 2024 3 15))))
      (is (numberp ev)))))

(test calculate-spi-cpi
  "Calculate schedule and cost performance indices"
  (with-test-project
    ;; Setup and schedule
    (schedule *current-project*)
    (let ((metrics (calculate-evm-metrics *current-project* (date 2024 3 15))))
      (is (numberp (evm-spi metrics)))
      (is (numberp (evm-cpi metrics))))))
```

#### 9.2: Resource Leveling

**Test First**:
```lisp
(test detect-overallocation
  "Detect when resource is overallocated"
  (with-test-project
    (defresource dev "Developer" (:limits (:daily-max (duration 8 :hours))))
    (deftask t1 "Task 1" (:allocate dev) (:effort (duration 10 :hours)))
    (deftask t2 "Task 2" (:allocate dev) (:effort (duration 10 :hours)))
    ;; Both scheduled on same day would overallocate
    (schedule *current-project*)
    (let ((overalloc (find-overallocations *current-project*)))
      (is (not (null overalloc))))))

(test level-resources
  "Resource leveling resolves overallocations"
  (with-test-project
    (defresource dev "Developer" (:limits (:daily-max (duration 8 :hours))))
    (deftask t1 "Task 1" (:allocate dev) (:effort (duration 10 :hours)))
    (deftask t2 "Task 2" (:allocate dev) (:effort (duration 10 :hours)))
    (schedule *current-project*)
    (level-resources *current-project*)
    (let ((overalloc (find-overallocations *current-project*)))
      (is (null overalloc)))))
```

#### 9.3: Risk Management

**Test First**:
```lisp
(test define-risk
  "Can define project risk"
  (with-test-project
    (let ((risk (make-instance 'risk
                              :id 'tech-debt
                              :description "Technical debt accumulation"
                              :probability 0.7
                              :impact-schedule (duration 2 :weeks))))
      (is (risk-p risk))
      (is (= 0.7 (risk-probability risk))))))

(test risk-impact-on-schedule
  "Risk can impact schedule"
  (with-test-project
    (deftask t1 "Task 1" ...)
    (add-risk 'tech-debt ...)
    (let ((baseline-end (schedule *current-project*))
          (risk-end (schedule-with-risks *current-project*)))
      (is (date> risk-end baseline-end)))))
```

---

## Test-Driven Development Guide

### TDD Cycle

```
┌─────────────────────────────────────────┐
│                                         │
│   1. RED: Write failing test           │
│      ↓                                  │
│   2. GREEN: Make it pass                │
│      ↓                                  │
│   3. REFACTOR: Clean up                 │
│      ↓                                  │
│   4. Repeat                             │
│                                         │
└─────────────────────────────────────────┘
```

### Test Organization

```lisp
;;;; tests/test-package.lisp
(defpackage #:project-juggler-tests
  (:use #:cl #:fiveam #:project-juggler)
  (:export #:run-all-tests))

(in-package #:project-juggler-tests)

(def-suite project-juggler-suite
  :description "All Project Juggler tests")

(def-suite types-suite 
  :in project-juggler-suite
  :description "Temporal types tests")

(def-suite scheduling-suite
  :in project-juggler-suite
  :description "Scheduling algorithm tests")

;;;; tests/test-types.lisp
(in-package #:project-juggler-tests)

(in-suite types-suite)

(test date-creation
  "Test date creation"
  (let ((d (date 2024 3 15)))
    (is (= 2024 (date-year d)))
    (is (= 3 (date-month d)))
    (is (= 15 (date-day d)))))

;;;; Run tests
(defun run-all-tests ()
  (run! 'project-juggler-suite))
```

### Test Utilities

```lisp
;;;; tests/test-utils.lisp

(defmacro with-test-project (&body body)
  "Execute body with temporary test project"
  `(let* ((*current-project* (make-test-project))
          (*current-namespace* (make-test-namespace))
          (*namespace-registry* (make-hash-table :test 'eq)))
     ,@body))

(defun make-test-project ()
  "Create a minimal test project"
  (make-instance 'project
                 :id 'test
                 :name "Test Project"
                 :start (date 2024 1 1)
                 :end (date 2024 12 31)))

(defmacro with-temp-file ((var content) &body body)
  "Create temporary file with content"
  `(let ((,var (make-pathname :name (format nil "temp-~A" (gensym))
                              :type "lisp")))
     (with-open-file (out ,var :direction :output :if-exists :supersede)
       (write-string ,content out))
     (unwind-protect
          (progn ,@body)
       (delete-file ,var))))

(defun date= (d1 d2)
  "Test date equality"
  (and (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-day d1) (date-day d2))))
```

### Testing Best Practices

1. **Test one thing per test**
2. **Use descriptive test names**
3. **Arrange-Act-Assert pattern**
4. **Test edge cases**
5. **Test error conditions**
6. **Keep tests independent**
7. **Use test fixtures/utilities**
8. **Aim for >90% code coverage**

---

## Critical Features

### 1. Working Time & Calendars

**Implementation**:
```lisp
(defclass working-hours ()
  ((days :initarg :days :reader working-hours-days)
   (hours :initarg :hours :reader working-hours-hours))
  (:documentation "Working hours specification"))

(defclass calendar ()
  ((id :initarg :id :reader calendar-id)
   (working-hours :initarg :working-hours :reader calendar-working-hours)
   (holidays :initform nil :accessor calendar-holidays)
   (timezone :initarg :timezone :initform :utc :reader calendar-timezone))
  (:documentation "Calendar with working time and holidays"))

(defun working-day-p (date calendar)
  "Check if date is a working day"
  (and (not (holiday-p date calendar))
       (working-hours-on-day-p date (calendar-working-hours calendar))))

(defun working-hours-between (start end calendar)
  "Calculate working hours between two dates"
  (let ((hours 0)
        (current start))
    (loop while (date< current end)
          do (when (working-day-p current calendar)
               (incf hours (working-hours-on-date current calendar)))
             (setf current (date+ current (duration 1 :day))))
    hours))
```

### 2. Multiple Scenarios

**Implementation**:
```lisp
(defclass scenario ()
  ((id :initarg :id :reader scenario-id)
   (name :initarg :name :reader scenario-name)
   (parent :initarg :parent :initform nil :accessor scenario-parent)
   (enabled :initarg :enabled :initform t :accessor scenario-enabled-p))
  (:documentation "Project scenario (plan, actual, etc)"))

(defmacro for-scenario (scenario-id &body body)
  "Execute body for specific scenario"
  `(let ((*current-scenario* ',scenario-id))
     ,@body))

;; Usage:
(deftask software "Software"
  (:effort (duration 120 :days))
  
  (:for-scenario actual
    (:effort (duration 135 :days))
    (:complete 75)))
```

### 3. Bookings (Actual Time Tracking)

**Implementation**:
```lisp
(defclass booking ()
  ((resource :initarg :resource :reader booking-resource)
   (task :initarg :task :reader booking-task)
   (start :initarg :start :reader booking-start)
   (end :initarg :end :reader booking-end)
   (amount :initarg :amount :reader booking-amount))
  (:documentation "Actual work booking"))

(defun add-booking (task resource start end-or-duration)
  "Record actual work"
  (let* ((end (if (duration-p end-or-duration)
                  (date+ start end-or-duration)
                  end-or-duration))
         (booking (make-instance 'booking
                                :task task
                                :resource resource
                                :start start
                                :end end)))
    (push booking (task-bookings task))
    (push booking (resource-bookings resource))
    booking))
```

---

## Pitfalls and Solutions

### Pitfall 1: Global State

**Problem**: Heavy use of special variables makes testing hard

**Solution**: Use session-based state, bind dynamically in tests

```lisp
;; BAD
(defvar *current-project* nil)  ; Global mutable state

;; BETTER
(defvar *current-session* nil)  ; Session encapsulates state

(defclass session ()
  ((project :accessor session-project)
   (current-task :accessor session-current-task)
   ...))
```

### Pitfall 2: Circular Dependencies

**Problem**: Easy to create, hard to debug

**Solution**: Detect early during finalization

```lisp
(defun finalize-project (project)
  ;; MUST detect circles before scheduling
  (detect-circular-dependencies project)
  ...)
```

### Pitfall 3: Reference Resolution

**Problem**: References fail silently, errors at wrong time

**Solution**: Fail fast during finalization with good error messages

```lisp
(defun resolve-task-reference (ref)
  (or (actual-resolution ref)
      (error 'reference-error
             :message "Cannot resolve: ~A~@
                      Available: ~{~A~^, ~}"
             ref
             (available-task-ids))))
```

### Pitfall 4: Performance with Large Projects

**Problem**: Naive algorithms slow for 1000+ tasks

**Solution**: Use hash tables, memoization, efficient data structures

```lisp
;; BAD: O(n) lookup
(find-if (lambda (t) (eq id (task-id t))) task-list)

;; GOOD: O(1) lookup
(gethash id task-hash-table)
```

### Pitfall 5: Time Zone Issues

**Problem**: Dates can shift unexpectedly

**Solution**: Always use explicit time zones, use local-time library

```lisp
(defun date (year month day &key (timezone :utc))
  (local-time:encode-timestamp 0 0 0 12 day month year
                               :timezone timezone))
```

### Pitfall 6: Floating Point in Dates

**Problem**: Rounding errors accumulate

**Solution**: Use integer arithmetic for time slots

```lisp
;; Use seconds or minutes as base unit, convert to days
(defun duration-to-seconds (duration)
  (* (duration-value duration)
     (ecase (duration-unit duration)
       (:minutes 60)
       (:hours 3600)
       (:days 86400))))
```

### Pitfall 7: Memory Leaks in Large Projects

**Problem**: Keeping too much data in memory

**Solution**: Clear caches after scheduling

```lisp
(defmethod finalize-scheduling ((project project))
  ;; Clear temporary data structures
  (setf (project-scheduling-cache project) nil)
  ...)
```

### Pitfall 8: Macro Hygiene

**Problem**: Variable capture in macros

**Solution**: Use gensym, careful expansion

```lisp
;; BAD
(defmacro deftask (id name &body body)
  `(let ((task (make-instance 'task ...)))  ; 'task' could be captured
     ...))

;; GOOD
(defmacro deftask (id name &body body)
  (let ((task-var (gensym "TASK")))  ; Unique variable
    `(let ((,task-var (make-instance 'task ...)))
       ...)))
```

---

## Code Organization

```
project-juggler/
├── project-juggler.asd              # ASDF system definition
├── README.md
├── LICENSE
│
├── src/
│   ├── package.lisp                 # Package definition
│   │
│   ├── core/
│   │   ├── types.lisp              # Date, duration, interval
│   │   ├── classes.lisp            # CLOS classes
│   │   ├── protocols.lisp          # Generic functions
│   │   └── errors.lisp             # Error conditions
│   │
│   ├── namespace/
│   │   ├── namespace.lisp          # Namespace system
│   │   ├── references.lisp         # Reference resolution
│   │   └── include.lisp            # Include mechanism
│   │
│   ├── dsl/
│   │   ├── defproject.lisp         # Project definition macro
│   │   ├── deftask.lisp            # Task definition macro
│   │   ├── defresource.lisp        # Resource definition macro
│   │   ├── defaccount.lisp         # Account definition macro
│   │   ├── dependencies.lisp       # depends-on, etc
│   │   └── allocations.lisp        # allocate macro
│   │
│   ├── scheduling/
│   │   ├── scheduler.lisp          # Main scheduling algorithm
│   │   ├── critical-path.lisp      # Critical path calculation
│   │   ├── criticalness.lisp       # Criticalness calculations
│   │   ├── resource-allocation.lisp # Resource allocation
│   │   └── calendars.lisp          # Working time
│   │
│   ├── session/
│   │   ├── session.lisp            # Session management
│   │   ├── changes.lisp            # Change tracking
│   │   ├── persistence.lisp        # Load/save
│   │   └── undo-redo.lisp          # Undo/redo
│   │
│   ├── validation/
│   │   ├── validation.lisp         # Validation framework
│   │   ├── finalization.lisp       # Project finalization
│   │   └── circular-deps.lisp      # Circular dependency detection
│   │
│   ├── tracking/
│   │   ├── baseline.lisp           # Baseline management
│   │   ├── evm.lisp                # Earned value
│   │   ├── bookings.lisp           # Actual time tracking
│   │   └── scenarios.lisp          # Scenario management
│   │
│   ├── reporting/
│   │   ├── reports.lisp            # Report generation
│   │   ├── task-reports.lisp       # Task-focused reports
│   │   ├── resource-reports.lisp   # Resource reports
│   │   ├── gantt.lisp              # Gantt chart
│   │   ├── formats/
│   │   │   ├── html.lisp           # HTML output
│   │   │   ├── csv.lisp            # CSV output
│   │   │   └── pdf.lisp            # PDF output
│   │   └── templates/              # Report templates
│   │
│   ├── risk/
│   │   ├── risk.lisp               # Risk management
│   │   └── simulation.lisp         # Monte Carlo
│   │
│   └── utils/
│       ├── helpers.lisp            # Utility functions
│       └── macros.lisp             # Utility macros
│
├── tests/
│   ├── test-package.lisp
│   ├── test-utils.lisp             # Test utilities
│   │
│   ├── core/
│   │   ├── test-types.lisp
│   │   ├── test-classes.lisp
│   │   └── test-protocols.lisp
│   │
│   ├── namespace/
│   │   ├── test-namespace.lisp
│   │   └── test-references.lisp
│   │
│   ├── dsl/
│   │   ├── test-defproject.lisp
│   │   ├── test-deftask.lisp
│   │   └── test-dependencies.lisp
│   │
│   ├── scheduling/
│   │   ├── test-scheduler.lisp
│   │   ├── test-critical-path.lisp
│   │   └── test-resource-allocation.lisp
│   │
│   ├── session/
│   │   ├── test-session.lisp
│   │   └── test-undo-redo.lisp
│   │
│   └── integration/
│       ├── test-simple-project.lisp
│       ├── test-complex-project.lisp
│       └── test-performance.lisp
│
├── examples/
│   ├── simple.lisp                 # Simple example
│   ├── software-dev.lisp           # Software project
│   ├── construction.lisp           # Construction project
│   ├── research.lisp               # Research project
│   └── resources/
│       ├── team.lisp               # Shared resources
│       └── equipment.lisp
│
└── docs/
    ├── manual.md                   # User manual
    ├── api.md                      # API reference
    ├── tutorial.md                 # Tutorial
    └── algorithms.md               # Algorithm documentation
```

---

## Examples and Test Cases

### Example 1: Simple Software Project

```lisp
;;;; examples/simple.lisp

(defproject simple "Simple Project" 
            (date 2024 3 1) (date 2024 6 1)
  
  (:scenarios
    (plan "Plan")
    (actual "Actual" :parent plan))
  
  (:tracking-scenario actual))

(defresource team "Team"
  (defresource dev1 "Developer 1"
    (:rate (money 150 :per-day)))
  
  (defresource dev2 "Developer 2"
    (:rate (money 140 :per-day))))

(deftask simple "Simple Project"
  (deftask requirements "Requirements"
    (:start (date 2024 3 1))
    (:duration (duration 1 :week))
    (:allocate dev1))
  
  (deftask development "Development"
    (:depends-on requirements)
    (:effort (duration 40 :hours))
    (:allocate dev1 dev2))
  
  (deftask testing "Testing"
    (:depends-on development)
    (:duration (duration 3 :days))
    (:allocate dev2)))

;; Generate report
(defreport overview :task-report
  (:title "Project Overview")
  (:formats (:html))
  (:columns name start end duration)
  (:output "reports/overview.html"))
```

### Example 2: Using Includes

```lisp
;;;; examples/main-project.lisp

(defproject company "Company Project"
            (date 2024 1 1) (date 2024 12 31)
  
  ;; Include shared resources
  (include "resources/team.lisp" :as team)
  (include "resources/equipment.lisp" :as equip)
  
  (deftask phase1 "Phase 1"
    (deftask requirements "Requirements"
      (:allocate team:pm))
    
    (deftask design "Design"
      (:depends-on requirements)
      (:allocate team:architect equip:workstation))))

;;;; examples/resources/team.lisp

(in-namespace team)

(defresource pm "Project Manager"
  (:rate (money 200 :per-day)))

(defresource architect "Software Architect"
  (:rate (money 180 :per-day)))
```

### Example 3: Complex Project with Scenarios

```lisp
;;;; examples/complex.lisp

(defproject acso "Accounting Software"
            (date 2002 1 16) (date 2002 5 16)
  
  (:scenarios
    (plan "Original Plan")
    (actual "Actual Progress" :parent plan)
    (optimistic "Best Case" :parent plan)
    (pessimistic "Worst Case" :parent plan))
  
  (:tracking-scenario actual)
  (:now (date 2002 3 5)))

(defresource team "Team"
  (defresource alice "Alice"
    (:rate (money 150 :per-day))
    (:efficiency 1.2)))

(deftask acso "Project"
  (deftask software "Software"
    (:effort (duration 120 :days))
    
    ;; Scenario-specific values
    (:for-scenario actual
      (:effort (duration 135 :days))
      (:complete 60))
    
    (:for-scenario optimistic
      (:effort (duration 100 :days)))
    
    (:for-scenario pessimistic
      (:effort (duration 150 :days)))
    
    (deftask backend "Backend"
      (:effort (duration 40 :days))
      (:allocate alice))))
```

---

## Performance Benchmarks

### Target Performance

```
Project Size        | Load Time  | Schedule Time | Memory Usage
--------------------|------------|---------------|-------------
Small (10 tasks)    | < 0.1s     | < 0.1s       | < 10 MB
Medium (100 tasks)  | < 1s       | < 1s         | < 50 MB
Large (1000 tasks)  | < 5s       | < 30s        | < 200 MB
XL (10000 tasks)    | < 30s      | < 5 min      | < 1 GB
```

### Performance Test Cases

```lisp
(test performance-large-project
  "Performance with 1000 tasks"
  (let ((start-time (get-internal-real-time)))
    (let ((project (generate-large-project 1000)))
      (finalize-project project)
      (schedule project)
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
        (is (< elapsed 30) "Scheduling 1000 tasks took ~A seconds" elapsed)))))

(defun generate-large-project (n-tasks)
  "Generate project with N tasks for testing"
  (let ((project (defproject perf "Performance Test"
                              (date 2024 1 1) (date 2025 1 1))))
    (loop for i from 1 to n-tasks
          do (deftask (intern (format nil "T~D" i))
                     (format nil "Task ~D" i)
               (:duration (duration 5 :days))
               (when (> i 1)
                 (:depends-on (intern (format nil "T~D" (1- i)))))))
    project))
```

---

## Final Checklist

### Before Starting Implementation

- [ ] Read and understand TaskJuggler algorithm
- [ ] Understand TDD methodology
- [ ] Setup development environment
- [ ] Choose Common Lisp implementation (SBCL recommended)
- [ ] Install dependencies (local-time, alexandria, etc)

### Phase Completion Criteria

- [ ] Phase 0: All packages load, test framework works
- [ ] Phase 1: All type tests pass (>20 tests)
- [ ] Phase 2: All namespace tests pass (>15 tests)
- [ ] Phase 3: All DSL tests pass (>30 tests)
- [ ] Phase 4: All validation tests pass (>20 tests)
- [ ] Phase 5: All scheduling tests pass (>40 tests)
- [ ] Phase 6: Critical path tests pass (>10 tests)
- [ ] Phase 7: Session management tests pass (>15 tests)
- [ ] Phase 8: Reporting tests pass (>20 tests)
- [ ] Phase 9: Advanced feature tests pass (>30 tests)

### Final Acceptance Criteria

- [ ] >200 tests passing
- [ ] >90% code coverage
- [ ] Load 1000 task project in <5 seconds
- [ ] Schedule 1000 task project in <30 seconds
- [ ] All references resolve correctly
- [ ] No circular dependencies allowed
- [ ] Generate HTML, CSV reports
- [ ] REPL workflow documented and working
- [ ] Examples run without errors
- [ ] Documentation complete

---

## Resources

### TaskJuggler Documentation
- [TaskJuggler Manual](https://taskjuggler.org/tj3/manual/)
- [TaskJuggler Internals](https://taskjuggler.org/tj3/manual/TaskJuggler_Internals.html)
- [TaskJuggler Source](https://github.com/taskjuggler/TaskJuggler)

### Common Lisp Resources
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [CLOS Tutorial](https://clos-tutorial.com)
- [Macro Writing](http://www.lispworks.com/documentation/HyperSpec/)

### Project Management
- [PMBOK Guide](https://www.pmi.org/pmbok-guide-standards)
- [Critical Path Method](https://en.wikipedia.org/wiki/Critical_path_method)
- [Earned Value Management](https://en.wikipedia.org/wiki/Earned_value_management)

---

## Success Tips for AI Agent

1. **Follow TDD strictly** - Write test first, every time
2. **Start simple** - Get basic cases working before complex ones
3. **Read TaskJuggler source** - When in doubt, see how they did it
4. **Test edge cases** - Empty projects, single tasks, circular deps
5. **Keep code clean** - Refactor after each phase
6. **Document as you go** - Docstrings for all functions
7. **Use the REPL** - Test interactively during development
8. **Profile early** - Don't wait until performance is a problem
9. **Ask questions** - If something is unclear, ask for clarification
10. **Commit often** - Small, atomic commits with good messages

---

## Contact & Support

For questions during implementation:
- Review this document thoroughly first
- Check TaskJuggler documentation
- Look at provided examples
- Examine test cases for expected behavior

**Good luck with the implementation!**
