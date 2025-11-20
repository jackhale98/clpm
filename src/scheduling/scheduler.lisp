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
   4. Mark task as scheduled"
  (declare (ignore scenario))

  ;; Get tasks in dependency order (topological sort)
  (let ((sorted-tasks (topological-sort-tasks project)))

    ;; Schedule each task in order
    (dolist (task sorted-tasks)
      (schedule-task task project))

    t))

;;; ============================================================================
;;; Task Scheduling
;;; ============================================================================

(defun schedule-task (task project)
  "Schedule a single task based on its type and dependencies"
  (declare (ignore project))

  (cond
    ;; Milestone: end date = start date
    ((task-milestone-p task)
     (unless (task-start task)
       (error "Milestone ~A has no start date" (task-id task)))
     (setf (task-end task) (task-start task))
     (setf (task-scheduled-p task) t))

    ;; Task with duration
    ((task-duration task)
     (let ((start (or (task-start task)
                      (calculate-earliest-start task))))
       (setf (task-start task) start)
       (setf (task-end task) (date+ start (task-duration task)))
       (setf (task-scheduled-p task) t)))

    ;; Task with effort (TODO: implement resource-based scheduling)
    ((task-effort task)
     (let ((start (or (task-start task)
                      (calculate-earliest-start task))))
       (setf (task-start task) start)
       ;; For now, treat effort as duration
       (setf (task-end task) (date+ start (task-effort task)))
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
