;;;; src/tracking/baseline.lisp
;;;; Baseline creation and management

(in-package #:project-juggler)

;;; ============================================================================
;;; Baseline Creation
;;; ============================================================================

(defun create-baseline (project &key (name "Baseline") (date (local-time:now)))
  "Create a baseline snapshot of the current project state.

   Captures task start/end dates, durations, and priorities for later comparison."

  (let ((baseline (make-instance 'baseline
                                :name name
                                :date date)))
    ;; Snapshot all tasks
    (loop for task being the hash-values of (project-tasks project)
          do (let ((baseline-task (make-instance 'baseline-task
                                                 :id (task-id task)
                                                 :name (task-name task)
                                                 :start (task-start task)
                                                 :end (task-end task)
                                                 :duration (task-duration task)
                                                 :priority (task-priority task))))
               (setf (gethash (task-id task) (baseline-tasks baseline))
                     baseline-task)))
    baseline))

(defun set-project-baseline (project baseline)
  "Set the baseline for a project"
  (setf (project-baseline project) baseline))
