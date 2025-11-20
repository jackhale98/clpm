;;;; src/reporting/gantt.lisp
;;;; Gantt chart data generation

(in-package #:project-juggler)

;;; ============================================================================
;;; Gantt Chart Data Generation
;;; ============================================================================

(defun generate-gantt-data (project)
  "Generate Gantt chart data structure for project.

   Returns a list of plists, each representing a task:
   (:id :name :start :end :dependencies)"

  (let ((tasks (loop for task being the hash-values of (project-tasks project)
                    collect task)))
    ;; Sort tasks by start date
    (setf tasks (sort tasks #'date< :key #'task-start))

    ;; Build data structure
    (mapcar #'task-to-gantt-entry tasks)))

(defun task-to-gantt-entry (task)
  "Convert a task to a Gantt chart entry (plist)"
  (list :id (task-id task)
        :name (task-name task)
        :start (task-start task)
        :end (task-end task)
        :dependencies (get-dependency-ids task)))

(defun get-dependency-ids (task)
  "Get list of task IDs that this task depends on"
  (mapcar (lambda (dep)
           (task-id (dependency-target dep)))
         (task-dependencies task)))
