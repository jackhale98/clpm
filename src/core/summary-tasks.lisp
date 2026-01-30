;;;; src/core/summary-tasks.lisp
;;;; Summary task (phase) aggregation
;;;;
;;;; Summary tasks are parent tasks that contain subtasks.
;;;; Their values (duration, effort, start, end) are automatically
;;;; calculated from their children.

(in-package #:claps)

;;; ============================================================================
;;; Summary Task Detection
;;; ============================================================================

(defun summary-task-p (task)
  "Check if a task is a summary task (has subtasks)."
  (not (null (task-subtasks task))))

(defun leaf-task-p (task)
  "Check if a task is a leaf task (has no subtasks)."
  (null (task-subtasks task)))

;;; ============================================================================
;;; Summary Task Aggregation
;;; ============================================================================

(defun aggregate-summary-task (task)
  "Calculate summary task values from its subtasks.

   For a summary task:
   - Duration spans from earliest subtask start to latest subtask end
   - Effort is the sum of all subtask efforts
   - Start is the earliest subtask start
   - End is the latest subtask end
   - Complete is the weighted average of subtask completion"
  (when (summary-task-p task)
    (let ((subtasks (task-subtasks task))
          (earliest-start nil)
          (latest-end nil)
          (total-effort-days 0)
          (total-weighted-complete 0)
          (total-effort-weight 0))

      ;; Recursively aggregate nested summary tasks first
      (dolist (subtask subtasks)
        (when (summary-task-p subtask)
          (aggregate-summary-task subtask)))

      ;; Calculate aggregates from subtasks
      (dolist (subtask subtasks)
        ;; Track earliest start
        (let ((start (task-start subtask)))
          (when (and start (or (null earliest-start) (date< start earliest-start)))
            (setf earliest-start start)))

        ;; Track latest end
        (let ((end (task-end subtask)))
          (when (and end (or (null latest-end) (date> end latest-end)))
            (setf latest-end end)))

        ;; Sum efforts
        (let ((effort (task-effort subtask)))
          (when effort
            (let ((days (duration-in-days effort)))
              (incf total-effort-days days)
              ;; For weighted completion average
              (incf total-effort-weight days)
              (incf total-weighted-complete (* days (or (task-complete subtask) 0))))))

        ;; If no effort, use duration for weighting
        (unless (task-effort subtask)
          (let ((duration (task-duration subtask)))
            (when duration
              (let ((days (duration-in-days duration)))
                (incf total-effort-weight days)
                (incf total-weighted-complete (* days (or (task-complete subtask) 0))))))))

      ;; Update summary task values
      (when earliest-start
        (setf (task-start task) earliest-start))

      (when latest-end
        (setf (task-end task) latest-end))

      ;; Calculate duration from start/end span
      (when (and earliest-start latest-end)
        (let ((days (days-between earliest-start latest-end)))
          (setf (task-duration task) (duration days :days))))

      ;; Set aggregated effort
      (when (> total-effort-days 0)
        (setf (task-effort task) (duration total-effort-days :days)))

      ;; Calculate weighted completion percentage
      (when (> total-effort-weight 0)
        (setf (task-complete task)
              (round (/ total-weighted-complete total-effort-weight))))

      task)))

(defun aggregate-all-summary-tasks (project)
  "Aggregate all summary tasks in a project.
   Should be called after scheduling."
  (maphash (lambda (id task)
             (declare (ignore id))
             ;; Only aggregate top-level summary tasks
             ;; (nested ones are handled recursively)
             (when (and (summary-task-p task)
                        (null (task-parent task)))
               (aggregate-summary-task task)))
           (project-tasks project)))

;;; ============================================================================
;;; Summary Task Validation
;;; ============================================================================

(defun validate-summary-task (task)
  "Validate that a summary task is properly structured.
   Summary tasks should not have their own effort/duration set manually."
  (when (summary-task-p task)
    (let ((warnings nil))
      ;; Warn if summary task has explicit duration (will be overwritten)
      (when (task-duration task)
        (push (format nil "Summary task ~A has explicit duration that will be calculated from subtasks"
                      (task-id task))
              warnings))

      ;; Warn if summary task has explicit effort (will be overwritten)
      (when (task-effort task)
        (push (format nil "Summary task ~A has explicit effort that will be calculated from subtasks"
                      (task-id task))
              warnings))

      ;; Check that subtasks exist
      (when (null (task-subtasks task))
        (push (format nil "Summary task ~A has no subtasks"
                      (task-id task))
              warnings))

      warnings)))

;;; ============================================================================
;;; Summary Task Helpers
;;; ============================================================================

(defun get-all-subtasks (task)
  "Get all subtasks recursively (flattened)."
  (let ((result nil))
    (labels ((collect (parent-task)
               (dolist (subtask (task-subtasks parent-task))
                 (push subtask result)
                 (collect subtask))))
      (collect task))
    (nreverse result)))

(defun get-leaf-tasks (task)
  "Get all leaf tasks under a summary task."
  (remove-if #'summary-task-p (get-all-subtasks task)))

(defun summary-task-progress (task)
  "Get detailed progress information for a summary task."
  (when (summary-task-p task)
    (let* ((all-subtasks (get-all-subtasks task))
           (leaf-tasks (get-leaf-tasks task))
           (total-tasks (length all-subtasks))
           (completed-tasks (count-if (lambda (tsk) (= (or (task-complete tsk) 0) 100))
                                      leaf-tasks))
           (in-progress-tasks (count-if (lambda (tsk)
                                          (let ((c (or (task-complete tsk) 0)))
                                            (and (> c 0) (< c 100))))
                                        leaf-tasks))
           (not-started-tasks (count-if (lambda (tsk) (= (or (task-complete tsk) 0) 0))
                                        leaf-tasks)))
      (list :total-subtasks total-tasks
            :leaf-tasks (length leaf-tasks)
            :completed completed-tasks
            :in-progress in-progress-tasks
            :not-started not-started-tasks
            :overall-complete (task-complete task)))))
