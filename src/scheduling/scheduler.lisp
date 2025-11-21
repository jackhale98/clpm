;;;; src/scheduling/scheduler.lisp
;;;; Main scheduling algorithm (TaskJuggler heuristic)

(in-package #:project-juggler)

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

  ;; Get tasks in dependency order (topological sort)
  (let ((sorted-tasks (topological-sort-tasks project)))

    ;; Schedule each task in order
    (dolist (task sorted-tasks)
      (schedule-task task project))

    ;; Automatically calculate critical path
    (calculate-critical-path project)

    t))

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
     (let ((start (or (task-start task)
                      (calculate-earliest-start task))))
       (setf (task-start task) start)
       (setf (task-end task) start)
       (setf (task-scheduled-p task) t)))

    ;; Task with duration
    ((task-duration task)
     (let ((start (or (task-start task)
                      (calculate-earliest-start task))))
       (setf (task-start task) start)
       (setf (task-end task) (date+ start (task-duration task)))
       (setf (task-scheduled-p task) t)))

    ;; Task with effort (resource-based scheduling)
    ((task-effort task)
     (let* ((start (or (task-start task)
                       (calculate-earliest-start task)))
            (duration (calculate-duration-from-effort task)))
       (setf (task-start task) start)
       (setf (task-end task) (date+ start duration))
       (setf (task-scheduled-p task) t)))

    ;; No duration or effort - error
    (t
     (error "Task ~A has neither duration nor effort" (task-id task)))))

(defun calculate-earliest-start (task)
  "Calculate earliest start date based on dependencies"
  (if (null (task-dependencies task))
      ;; No dependencies: use project start
      (project-start (task-project task))
      ;; Has dependencies: start after all dependencies finish
      (let ((latest-end nil))
        (dolist (dep (task-dependencies task))
          (let* ((target (dependency-target dep))
                 (target-end (when target (task-end target))))
            (when target-end
              (if (null latest-end)
                  (setf latest-end target-end)
                  (when (date> target-end latest-end)
                    (setf latest-end target-end))))))
        (or latest-end (project-start (task-project task))))))

(defun calculate-duration-from-effort (task)
  "Calculate actual duration for an effort-based task considering resource efficiency.

   Formula: duration = effort / total_efficiency

   Where total_efficiency is the sum of efficiency values of all allocated resources.
   If no resources are allocated, uses efficiency of 1.0 (treat effort as duration).

   Examples:
     - Task with 10 days effort, 1 resource at 1.0 efficiency => 10 days duration
     - Task with 10 days effort, 1 resource at 2.0 efficiency => 5 days duration
     - Task with 10 days effort, 2 resources at 1.0 efficiency => 5 days duration
     - Task with 10 days effort, no resources => 10 days duration (warning issued)"
  (let ((effort (task-effort task))
        (allocations (task-allocations task)))
    (if (null allocations)
        (progn
          (warn "Task ~A has effort but no resource allocations - treating effort as duration"
                (task-id task))
          effort)
        ;; Calculate total efficiency from allocated resources
        (let ((total-efficiency 0.0))
          (dolist (allocation allocations)
            (dolist (resource (allocation-resources allocation))
              (incf total-efficiency (resource-efficiency resource))))

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
  "Sort tasks in dependency order using depth-first search"
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

                  ;; Mark as visited and add to sorted list
                  (setf (gethash task visited) t)
                  (setf (gethash task in-progress) nil)
                  (push task sorted)))))

      ;; Visit all tasks
      (loop for task being the hash-values of (project-tasks project)
            do (visit task))

      ;; Return tasks in correct order (dependencies first)
      (nreverse sorted))))
