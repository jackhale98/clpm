;;;; src/scheduling/critical-path.lisp
;;;; CPM Critical Path Method (slack-based)
;;;;
;;;; This calculates the TRUE critical path using the Critical Path Method:
;;;; - Forward pass: Calculate Early Start (ES) and Early Finish (EF)
;;;; - Backward pass: Calculate Late Start (LS) and Late Finish (LF)
;;;; - Slack: LS - ES (or LF - EF)
;;;; - Critical path: Tasks with zero slack

(in-package #:project-juggler)

;;; ============================================================================
;;; Forward Pass (Early Start / Early Finish)
;;; ============================================================================

(defun forward-pass (project)
  "Calculate Early Start and Early Finish for all tasks.

   ES = max(EF of all predecessors)
   EF = ES + duration"

  ;; Get tasks in dependency order (topological sort)
  (let ((sorted-tasks (topological-sort-tasks project)))

    ;; Calculate ES and EF for each task in dependency order
    (dolist (task sorted-tasks)
      (calculate-early-dates task))))

(defun calculate-early-dates (task)
  "Calculate Early Start and Early Finish for a single task"
  (let ((early-start (calculate-early-start-date task)))
    (setf (task-early-start task) early-start)
    (setf (task-early-finish task) (task-end task))))

(defun calculate-early-start-date (task)
  "Calculate Early Start date based on predecessors"
  (if (null (task-dependencies task))
      ;; No dependencies: ES = scheduled start
      (task-start task)
      ;; Has dependencies: ES = max(EF of all predecessors)
      (let ((latest-finish nil))
        (dolist (dep (task-dependencies task))
          (let* ((predecessor (dependency-target dep))
                 (pred-ef (when predecessor (task-early-finish predecessor))))
            (when pred-ef
              (if (null latest-finish)
                  (setf latest-finish pred-ef)
                  (when (date> pred-ef latest-finish)
                    (setf latest-finish pred-ef))))))
        (or latest-finish (task-start task)))))

;;; ============================================================================
;;; Backward Pass (Late Start / Late Finish)
;;; ============================================================================

(defun backward-pass (project)
  "Calculate Late Start and Late Finish for all tasks.

   LF = min(LS of all successors), or project completion date for final tasks
   LS = LF - duration

   For CPM, we use the latest Early Finish among all tasks as the reference
   point for the backward pass. This ensures the longest path has zero slack."

  ;; Find the latest Early Finish among all tasks (project completion point)
  (let ((project-completion-date nil))
    (loop for task being the hash-values of (project-tasks project)
          for ef = (task-early-finish task)
          when ef
          do (if (null project-completion-date)
                 (setf project-completion-date ef)
                 (when (date> ef project-completion-date)
                   (setf project-completion-date ef))))

    ;; Get tasks in reverse dependency order
    (let ((sorted-tasks (reverse (topological-sort-tasks project))))

      ;; Calculate LS and LF for each task in reverse order
      (dolist (task sorted-tasks)
        (calculate-late-dates task project project-completion-date)))))

(defun calculate-late-dates (task project project-completion-date)
  "Calculate Late Start and Late Finish for a single task"
  (let* ((late-finish (calculate-late-finish-date task project project-completion-date))
         (duration-days (date-difference-days (task-end task) (task-start task)))
         (late-start (date- late-finish (duration duration-days :days))))
    (setf (task-late-finish task) late-finish)
    (setf (task-late-start task) late-start)))

(defun calculate-late-finish-date (task project project-completion-date)
  "Calculate Late Finish date based on successors"
  (let ((successors (find-task-successors task project)))
    (if (null successors)
        ;; No successors: LF = project completion date (latest EF among all tasks)
        project-completion-date
        ;; Has successors: LF = min(LS of all successors)
        (let ((earliest-start nil))
          (dolist (successor successors)
            (let ((succ-ls (task-late-start successor)))
              (when succ-ls
                (if (null earliest-start)
                    (setf earliest-start succ-ls)
                    (when (date< succ-ls earliest-start)
                      (setf earliest-start succ-ls))))))
          (or earliest-start project-completion-date)))))

(defun find-task-successors (task project)
  "Find all tasks that depend on this task"
  (let ((successors nil))
    (loop for other-task being the hash-values of (project-tasks project)
          do (dolist (dep (task-dependencies other-task))
               (when (eq task (dependency-target dep))
                 (push other-task successors))))
    successors))

;;; ============================================================================
;;; Slack Calculation
;;; ============================================================================

(defun calculate-slack (project)
  "Calculate slack (float) for all tasks.

   Slack = LS - ES (or equivalently LF - EF)

   Tasks with zero slack are on the critical path."

  (loop for task being the hash-values of (project-tasks project)
        do (let* ((es (task-early-start task))
                  (ls (task-late-start task))
                  (slack-duration (when (and es ls)
                                    (date-difference-days ls es))))
             (setf (task-slack task) (or slack-duration 0)))))

(defun date-difference-days (date1 date2)
  "Calculate difference between two dates in days"
  (let ((diff (local-time:timestamp-difference
               (date-timestamp date1)
               (date-timestamp date2))))
    (round (/ diff 86400))))

;;; ============================================================================
;;; Critical Path Identification
;;; ============================================================================

(defun critical-path (project)
  "Identify and return tasks on the critical path.

   The critical path consists of all tasks with zero slack.
   These are the tasks that, if delayed, will delay the entire project."

  ;; Run CPM analysis
  (forward-pass project)
  (backward-pass project)
  (calculate-slack project)

  ;; Return tasks with zero slack
  (let ((critical-tasks nil))
    (loop for task being the hash-values of (project-tasks project)
          when (and (task-slack task) (zerop (task-slack task)))
          do (push task critical-tasks))
    critical-tasks))
