;;;; src/scheduling/scheduler.lisp
;;;; Main scheduling algorithm (TaskJuggler heuristic)

(in-package #:project-juggler)

;;; Forward declaration for PERT duration calculation (defined in pert.lisp)
(declaim (ftype (function (t) t) pert-duration-for-scheduling))

;;; ============================================================================
;;; Main Scheduling Entry Point
;;; ============================================================================

(defun schedule (project &optional (scenario :plan))
  "Schedule the project using a simple dependency-based algorithm.

   For each task in dependency order:
   1. If task has fixed start date, use it
   2. Otherwise, start after all dependencies complete
   3. Calculate end date based on duration or effort
   4. Mark task as scheduled

   After scheduling, automatically calculates critical path using CPM."
  (declare (ignore scenario))

  ;; Reset scheduling state for all tasks before rescheduling
  ;; This ensures dependent tasks get updated when predecessors change
  (reset-scheduling-state project)

  ;; Get tasks in dependency order (topological sort)
  (let ((sorted-tasks (topological-sort-tasks project)))

    ;; Schedule each task in order
    (dolist (task sorted-tasks)
      (schedule-task task project))

    ;; Automatically calculate critical path
    (calculate-critical-path project)

    t))

(defun reset-scheduling-state (project)
  "Reset scheduling state for tasks that don't have a user-defined fixed start.
   Tasks with a fixed start date (set via :start in deftask) keep their start.
   Tasks without a fixed start have their computed dates cleared for rescheduling."
  (maphash (lambda (id task)
             (declare (ignore id))
             ;; Clear scheduled flag for all tasks
             (setf (task-scheduled-p task) nil)
             ;; Clear computed dates for tasks without a fixed start constraint
             ;; A task has a fixed start if it has a start-constraint of type :mso
             ;; or if it was defined with :start in deftask
             ;; We preserve the start date only if it was explicitly set
             ;; and has not been scheduled yet (i.e., this is first scheduling)
             ;; For rescheduling, we need to check if start was dependency-computed
             ;;
             ;; Simple heuristic: if start equals project start and task has no deps,
             ;; it's likely a fixed start. Otherwise clear it for recalculation.
             (unless (task-has-fixed-start-p task project)
               (setf (task-start task) nil)
               (setf (task-end task) nil))
             ;; Always clear CPM slots for recalculation
             (setf (task-early-start task) nil)
             (setf (task-early-finish task) nil)
             (setf (task-late-start task) nil)
             (setf (task-late-finish task) nil)
             (setf (task-slack task) nil))
           (project-tasks project)))

(defun task-has-fixed-start-p (task project)
  "Check if a task has a user-defined fixed start date.
   Returns T if the task should keep its start date during rescheduling."
  (declare (ignore project))
  ;; A task has a fixed start if:
  ;; 1. It has a :mso (must-start-on) constraint, OR
  ;; 2. It has a start date AND no dependencies (root task with explicit start)
  (or (and (task-start-constraint task)
           (eq :mso (constraint-type (task-start-constraint task))))
      (and (task-start task)
           (null (task-dependencies task)))))

(defun calculate-critical-path (project)
  "Convenience function that performs all CPM analysis steps.

   Calculates:
   - Forward pass (early start/finish)
   - Backward pass (late start/finish)
   - Slack for all tasks

   After calling this, use (critical-path project) to get critical tasks."
  (forward-pass project)
  (backward-pass project)
  (calculate-slack project)
  t)

;;; ============================================================================
;;; Task Scheduling
;;; ============================================================================

(defun schedule-task (task project)
  "Schedule a single task based on its type and dependencies"
  (declare (ignore project))

  (cond
    ;; Milestone: end date = start date
    ((task-milestone-p task)
     (let* ((proposed-start (or (task-start task)
                                (calculate-earliest-start task)))
            (start (apply-start-constraint task proposed-start)))
       (setf (task-start task) start)
       (setf (task-end task) start)
       (setf (task-scheduled-p task) t)))

    ;; Task with duration
    ((task-duration task)
     (let* ((proposed-start (or (task-start task)
                                (calculate-earliest-start task)))
            (start (apply-start-constraint task proposed-start))
            (duration (task-duration task))
            (proposed-end (date+ start duration)))
       ;; Apply finish constraint
       (multiple-value-bind (adjusted-start adjusted-end)
           (apply-finish-constraint task proposed-end duration)
         (if adjusted-start
             (progn
               (setf (task-start task) adjusted-start)
               (setf (task-end task) adjusted-end))
             (progn
               (setf (task-start task) start)
               (setf (task-end task) proposed-end))))
       (setf (task-scheduled-p task) t)))

    ;; Task with effort (resource-based scheduling)
    ((task-effort task)
     (let* ((proposed-start (or (task-start task)
                                (calculate-earliest-start task)))
            (start (apply-start-constraint task proposed-start))
            (duration (calculate-duration-from-effort task))
            (proposed-end (date+ start duration)))
       ;; Apply finish constraint
       (multiple-value-bind (adjusted-start adjusted-end)
           (apply-finish-constraint task proposed-end duration)
         (if adjusted-start
             (progn
               (setf (task-start task) adjusted-start)
               (setf (task-end task) adjusted-end))
             (progn
               (setf (task-start task) start)
               (setf (task-end task) proposed-end))))
       (setf (task-scheduled-p task) t)))

    ;; Task with PERT estimate (use expected duration)
    ((task-estimate task)
     (let* ((proposed-start (or (task-start task)
                                (calculate-earliest-start task)))
            (start (apply-start-constraint task proposed-start))
            (duration (pert-duration-for-scheduling task))
            (proposed-end (date+ start duration)))
       ;; Apply finish constraint
       (multiple-value-bind (adjusted-start adjusted-end)
           (apply-finish-constraint task proposed-end duration)
         (if adjusted-start
             (progn
               (setf (task-start task) adjusted-start)
               (setf (task-end task) adjusted-end))
             (progn
               (setf (task-start task) start)
               (setf (task-end task) proposed-end))))
       (setf (task-scheduled-p task) t)))

    ;; Parent task with subtasks but no duration/effort
    ;; Calculate dates from subtask span
    ((task-subtasks task)
     (calculate-parent-dates-from-subtasks task)
     (setf (task-scheduled-p task) t))

    ;; No duration or effort - error
    (t
     (error "Task ~A has neither duration nor effort" (task-id task)))))

(defun calculate-parent-dates-from-subtasks (parent-task)
  "Calculate parent task start/end dates from subtask span.

   Parent start = earliest subtask start
   Parent end = latest subtask end"
  (let ((earliest-start nil)
        (latest-end nil))

    (dolist (subtask (task-subtasks parent-task))
      (let ((sub-start (task-start subtask))
            (sub-end (task-end subtask)))
        ;; Track earliest start
        (when sub-start
          (if (null earliest-start)
              (setf earliest-start sub-start)
              (when (date< sub-start earliest-start)
                (setf earliest-start sub-start))))
        ;; Track latest end
        (when sub-end
          (if (null latest-end)
              (setf latest-end sub-end)
              (when (date> sub-end latest-end)
                (setf latest-end sub-end))))))

    ;; Set parent task dates
    (when earliest-start
      (setf (task-start parent-task) earliest-start))
    (when latest-end
      (setf (task-end parent-task) latest-end))))

(defun calculate-earliest-start (task)
  "Calculate earliest start date based on dependencies.

   Handles all dependency types:
   - FS (Finish-to-Start): dependent starts when predecessor finishes
   - SS (Start-to-Start): dependent starts when predecessor starts
   - FF (Finish-to-Finish): dependent finishes when predecessor finishes
   - SF (Start-to-Finish): dependent finishes when predecessor starts

   For FF and SF dependencies, this function calculates the required start
   date based on the end constraint and task duration."
  (if (null (task-dependencies task))
      ;; No dependencies: use project start
      (project-start (task-project task))
      ;; Has dependencies: calculate start based on dependency types
      (let ((latest-start nil)
            (task-dur (or (task-duration task)
                          (when (task-effort task)
                            (duration (duration-in-days (task-effort task)) :days))
                          (duration 1 :days))))  ; Default to 1 day if unknown
        (dolist (dep (task-dependencies task))
          (let* ((target (dependency-target dep))
                 (dep-type (dependency-type dep))
                 (lag (dependency-gap dep))
                 (constraint-start nil))
            (when target
              ;; Calculate constraint start based on dependency type
              (setf constraint-start
                    (case dep-type
                      ;; Finish-to-Start: start after predecessor finishes
                      (:fs
                       (let ((base (task-end target)))
                         (if lag (date+ base lag) base)))

                      ;; Start-to-Start: start when predecessor starts
                      (:ss
                       (let ((base (task-start target)))
                         (if lag (date+ base lag) base)))

                      ;; Finish-to-Finish: finish when predecessor finishes
                      ;; Calculate start = predecessor-end + lag - our-duration
                      (:ff
                       (let* ((base (task-end target))
                              (required-end (if lag (date+ base lag) base)))
                         ;; Work backward from required end
                         (date+ required-end (duration (- (duration-in-days task-dur)) :days))))

                      ;; Start-to-Finish: finish when predecessor starts
                      ;; Calculate start = predecessor-start + lag - our-duration
                      (:sf
                       (let* ((base (task-start target))
                              (required-end (if lag (date+ base lag) base)))
                         ;; Work backward from required end
                         (date+ required-end (duration (- (duration-in-days task-dur)) :days))))

                      ;; Default to FS behavior
                      (t
                       (let ((base (task-end target)))
                         (if lag (date+ base lag) base)))))

              ;; Track latest (most constraining) start date
              (when constraint-start
                (if (null latest-start)
                    (setf latest-start constraint-start)
                    (when (date> constraint-start latest-start)
                      (setf latest-start constraint-start)))))))

        (or latest-start (project-start (task-project task))))))

(defun calculate-duration-from-effort (task)
  "Calculate actual duration for an effort-based task considering resource efficiency
   and allocation percentages.

   Formula: duration = effort / effective_efficiency

   Where effective_efficiency is the sum of (efficiency × percent/100) for all allocated
   resources. If a resource is allocated at 50%, it contributes half its efficiency.

   Examples:
     - Task with 10 days effort, 1 resource at 1.0 efficiency, 100% => 10 days duration
     - Task with 10 days effort, 1 resource at 2.0 efficiency, 100% => 5 days duration
     - Task with 10 days effort, 1 resource at 1.0 efficiency, 50% => 20 days duration
     - Task with 10 days effort, no resources => 10 days duration (warning issued)"
  (let ((effort (task-effort task))
        (allocations (task-allocations task)))
    (if (null allocations)
        (progn
          (warn "Task ~A has effort but no resource allocations - treating effort as duration"
                (task-id task))
          effort)
        ;; Calculate total effective efficiency from allocated resources
        (let ((total-efficiency 0.0))
          (dolist (allocation allocations)
            (dolist (resource (allocation-resources allocation))
              (let* ((efficiency (resource-efficiency resource))
                     (percent (get-allocation-percent-for-resource allocation resource))
                     (effective (/ (* efficiency percent) 100.0)))
                (incf total-efficiency effective))))

          (if (zerop total-efficiency)
              (progn
                (warn "Task ~A has resources with zero total efficiency - treating effort as duration"
                      (task-id task))
                effort)
              ;; Calculate duration = effort / efficiency
              ;; Need to convert to same time units
              (let ((effort-days (duration-in-days effort)))
                (duration (ceiling (/ effort-days total-efficiency)) :days)))))))

;;; ============================================================================
;;; Topological Sort
;;; ============================================================================

(defun topological-sort-tasks (project)
  "Sort tasks in dependency order using depth-first search.

   Ensures:
   1. Dependencies are scheduled before dependents
   2. Subtasks are scheduled before their parent tasks"
  (let ((sorted nil)
        (visited (make-hash-table :test 'eq))
        (in-progress (make-hash-table :test 'eq)))

    (labels ((visit (task)
               (cond
                 ((gethash task visited) nil)  ; Already visited
                 ((gethash task in-progress)   ; Cycle detected
                  (error "Circular dependency detected involving task ~A" (task-id task)))
                 (t
                  (setf (gethash task in-progress) t)

                  ;; Visit all dependencies first
                  (dolist (dep (task-dependencies task))
                    (let ((target (dependency-target dep)))
                      (when target
                        (visit target))))

                  ;; Visit all subtasks before the parent
                  (dolist (subtask (task-subtasks task))
                    (visit subtask))

                  ;; Mark as visited and add to sorted list
                  (setf (gethash task visited) t)
                  (setf (gethash task in-progress) nil)
                  (push task sorted)))))

      ;; Only visit root-level tasks (those without parents)
      ;; Subtasks will be visited through their parents
      (loop for task being the hash-values of (project-tasks project)
            when (null (task-parent task))
            do (visit task))

      ;; Return tasks in correct order (dependencies first, subtasks before parents)
      (nreverse sorted))))
