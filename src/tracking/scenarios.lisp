;;;; src/tracking/scenarios.lisp
;;;; What-if scenario management

(in-package #:project-juggler)

;;; ============================================================================
;;; Scenario Class (Enhanced)
;;; ============================================================================

;;; Note: Base scenario class is defined in core/classes.lisp
;;; Here we add slots and functionality for what-if analysis

(defclass what-if-scenario ()
  ((name :initarg :name :reader scenario-name)
   (project :initarg :project :reader scenario-project)
   (description :initarg :description :initform nil :accessor scenario-description)
   (created-at :initarg :created-at :initform (get-universal-time) :reader scenario-created-at)
   ;; Task modifications: hash of task-id -> plist of modified properties
   (task-modifications :initform (make-hash-table :test 'eq) :accessor scenario-task-modifications)
   ;; Snapshot of original task data at scenario creation
   (task-snapshots :initform (make-hash-table :test 'eq) :accessor scenario-task-snapshots)
   ;; Additional dependencies: list of (dependent-id predecessor-id type lag)
   (added-dependencies :initform nil :accessor scenario-added-dependencies)
   ;; Removed dependencies: list of (dependent-id predecessor-id)
   (removed-dependencies :initform nil :accessor scenario-removed-dependencies)
   ;; Scheduled task data for this scenario: hash of task-id -> scheduled plist
   (scheduled-data :initform (make-hash-table :test 'eq) :accessor scenario-scheduled-data))
  (:documentation "What-if scenario for project analysis"))

;;; ============================================================================
;;; Scenario Creation
;;; ============================================================================

(defun create-scenario (project name &key description)
  "Create a new what-if scenario based on current project state.
   Takes a snapshot of all task data and schedules it."
  (let ((scenario (make-instance 'what-if-scenario
                                :name name
                                :project project
                                :description description)))
    ;; Snapshot all tasks
    (maphash (lambda (id task)
               (setf (gethash id (scenario-task-snapshots scenario))
                     (snapshot-task task)))
             (project-tasks project))
    ;; Register scenario with project
    (push scenario (project-scenarios project))
    ;; Schedule the scenario to populate scheduled-data
    (schedule-scenario scenario)
    scenario))

(defun snapshot-task (task)
  "Create a snapshot of task properties for a scenario."
  (list :id (task-id task)
        :name (task-name task)
        :start (task-start task)
        :end (task-end task)
        :duration (task-duration task)
        :effort (task-effort task)
        :priority (task-priority task)
        :complete (task-complete task)
        :dependencies (mapcar (lambda (dep)
                               (list (dependency-target-ref dep)
                                     (dependency-type dep)
                                     (dependency-gap dep)))
                             (task-dependencies task))))

;;; ============================================================================
;;; Scenario Modification
;;; ============================================================================

(defun scenario-modify-task (scenario task-id &key duration effort priority start end)
  "Modify task properties in a scenario."
  (let ((mods (gethash task-id (scenario-task-modifications scenario))))
    (when duration (setf (getf mods :duration) duration))
    (when effort (setf (getf mods :effort) effort))
    (when priority (setf (getf mods :priority) priority))
    (when start (setf (getf mods :start) start))
    (when end (setf (getf mods :end) end))
    (setf (gethash task-id (scenario-task-modifications scenario)) mods)))

(defun scenario-add-dependency (scenario dependent-id predecessor-id &key (type :fs) lag)
  "Add a dependency in a scenario."
  (push (list dependent-id predecessor-id type lag)
        (scenario-added-dependencies scenario)))

(defun scenario-remove-dependency (scenario dependent-id predecessor-id)
  "Remove a dependency in a scenario."
  (push (list dependent-id predecessor-id)
        (scenario-removed-dependencies scenario)))

;;; ============================================================================
;;; Scenario Scheduling
;;; ============================================================================

(defun schedule-scenario (scenario)
  "Schedule the project with scenario modifications applied."
  (let* ((project (scenario-project scenario))
         (snapshots (scenario-task-snapshots scenario))
         (modifications (scenario-task-modifications scenario)))
    ;; Clear previous scheduled data
    (clrhash (scenario-scheduled-data scenario))

    ;; Build effective task data combining snapshots and modifications
    (maphash (lambda (id snapshot)
               (let ((mods (gethash id modifications))
                     (effective (copy-list snapshot)))
                 ;; Apply modifications
                 (when mods
                   (loop for (key val) on mods by #'cddr
                         do (setf (getf effective key) val)))
                 ;; Store in scheduled-data (we'll update start/end after scheduling)
                 (setf (gethash id (scenario-scheduled-data scenario)) effective)))
             snapshots)

    ;; Simple scheduling: process tasks in dependency order
    (let ((sorted-ids (scenario-topological-sort scenario)))
      (dolist (task-id sorted-ids)
        (schedule-scenario-task scenario task-id)))

    scenario))

(defun scenario-topological-sort (scenario)
  "Sort task IDs in dependency order for scenario scheduling."
  (let ((data (scenario-scheduled-data scenario))
        (sorted nil)
        (visited (make-hash-table :test 'eq))
        (added-deps (scenario-added-dependencies scenario))
        (removed-deps (scenario-removed-dependencies scenario)))

    (labels ((get-deps (task-id)
               ;; Get effective dependencies for task
               (let* ((task-data (gethash task-id data))
                      (base-deps (getf task-data :dependencies))
                      (result nil))
                 ;; Add base dependencies (unless removed)
                 (dolist (dep base-deps)
                   (let ((pred-id (first dep)))
                     (unless (member (list task-id pred-id) removed-deps :test #'equal)
                       (push pred-id result))))
                 ;; Add scenario dependencies
                 (dolist (added added-deps)
                   (when (eq task-id (first added))
                     (push (second added) result)))
                 result))

             (visit (task-id)
               (unless (gethash task-id visited)
                 ;; Visit dependencies first
                 (dolist (dep-id (get-deps task-id))
                   (visit dep-id))
                 (setf (gethash task-id visited) t)
                 (push task-id sorted))))

      (maphash (lambda (id data)
                 (declare (ignore data))
                 (visit id))
               data))

    (nreverse sorted)))

(defun schedule-scenario-task (scenario task-id)
  "Schedule a single task within a scenario."
  (let* ((project (scenario-project scenario))
         (data (gethash task-id (scenario-scheduled-data scenario)))
         (mods (gethash task-id (scenario-task-modifications scenario)))
         (duration (or (getf data :duration)
                      (when (getf data :effort)
                        (duration (duration-in-days (getf data :effort)) :days))
                      (duration 1 :days)))
         ;; Only use fixed-start if explicitly modified in scenario
         (fixed-start (getf mods :start))
         (earliest-start (scenario-earliest-start scenario task-id)))

    ;; Calculate start - only use fixed start if explicitly set in modifications
    (let ((start (or fixed-start earliest-start (project-start project))))
      (setf (getf data :start) start)
      (setf (getf data :end) (date+ start duration))
      (setf (gethash task-id (scenario-scheduled-data scenario)) data))))

(defun scenario-earliest-start (scenario task-id)
  "Calculate earliest start based on scenario dependencies."
  (let ((data (scenario-scheduled-data scenario))
        (added-deps (scenario-added-dependencies scenario))
        (removed-deps (scenario-removed-dependencies scenario))
        (task-data (gethash task-id (scenario-scheduled-data scenario)))
        (latest-end nil))

    ;; Check base dependencies
    (let ((base-deps (getf task-data :dependencies)))
      (dolist (dep base-deps)
        (let ((pred-id (first dep)))
          (unless (member (list task-id pred-id) removed-deps :test #'equal)
            (let ((pred-data (gethash pred-id data)))
              (when pred-data
                (let ((pred-end (getf pred-data :end)))
                  (when pred-end
                    (if (null latest-end)
                        (setf latest-end pred-end)
                        (when (date> pred-end latest-end)
                          (setf latest-end pred-end)))))))))))

    ;; Check added dependencies
    (dolist (added added-deps)
      (when (eq task-id (first added))
        (let* ((pred-id (second added))
               (pred-data (gethash pred-id data)))
          (when pred-data
            (let ((pred-end (getf pred-data :end)))
              (when pred-end
                (if (null latest-end)
                    (setf latest-end pred-end)
                    (when (date> pred-end latest-end)
                      (setf latest-end pred-end)))))))))

    latest-end))

;;; ============================================================================
;;; Scenario Data Access
;;; ============================================================================

(defun get-scenario-task-data (scenario task-id)
  "Get the scheduled task data for a task in a scenario."
  (gethash task-id (scenario-scheduled-data scenario)))

(defun scenario-end-date (scenario)
  "Get the latest end date across all tasks in a scenario."
  (let ((latest nil))
    (maphash (lambda (id data)
               (declare (ignore id))
               (let ((end (getf data :end)))
                 (when end
                   (if (null latest)
                       (setf latest end)
                       (when (date> end latest)
                         (setf latest end))))))
             (scenario-scheduled-data scenario))
    latest))

(defun scenario-total-duration (scenario)
  "Calculate total project duration for a scenario in days."
  (let ((project (scenario-project scenario))
        (end-date (scenario-end-date scenario)))
    (if (and end-date (project-start project))
        (let ((start-ts (local-time:timestamp-to-unix (date-timestamp (project-start project))))
              (end-ts (local-time:timestamp-to-unix (date-timestamp end-date))))
          (truncate (/ (- end-ts start-ts) 86400)))
        0)))

(defun scenario-total-cost (scenario)
  "Calculate total cost for a scenario."
  (let ((total 0.0))
    (maphash (lambda (id data)
               (declare (ignore id))
               (let* ((effort-dur (getf data :effort))
                      (effort-days (if effort-dur (duration-in-days effort-dur) 0))
                      ;; Simplified: assume average rate of 100 if not tracked
                      (rate 100.0))
                 (incf total (* effort-days rate))))
             (scenario-scheduled-data scenario))
    total))

;;; ============================================================================
;;; Scenario Management
;;; ============================================================================

(defun list-scenarios (project)
  "List all scenario names for a project."
  (mapcar #'scenario-name (project-scenarios project)))

(defun get-scenario (project name)
  "Get a scenario by name."
  (find name (project-scenarios project)
        :key #'scenario-name
        :test #'string=))

(defun delete-scenario (project name)
  "Delete a scenario by name."
  (setf (project-scenarios project)
        (remove name (project-scenarios project)
                :key #'scenario-name
                :test #'string=)))

;;; ============================================================================
;;; Scenario Comparison
;;; ============================================================================

(defun compare-scenarios (scenario1 scenario2)
  "Compare two scenarios and return difference statistics."
  (let ((name1 (scenario-name scenario1))
        (name2 (scenario-name scenario2)))
    (list (intern (format nil "~A-DURATION" (string-upcase name1)) :keyword)
          (scenario-total-duration scenario1)
          (intern (format nil "~A-DURATION" (string-upcase name2)) :keyword)
          (scenario-total-duration scenario2)
          (intern (format nil "~A-COST" (string-upcase name1)) :keyword)
          (scenario-total-cost scenario1)
          (intern (format nil "~A-COST" (string-upcase name2)) :keyword)
          (scenario-total-cost scenario2)
          (intern (format nil "~A-END" (string-upcase name1)) :keyword)
          (scenario-end-date scenario1)
          (intern (format nil "~A-END" (string-upcase name2)) :keyword)
          (scenario-end-date scenario2))))

;;; ============================================================================
;;; Scenario Summary
;;; ============================================================================

(defun scenario-summary (scenario)
  "Get summary statistics for a scenario."
  (list :name (scenario-name scenario)
        :total-duration (scenario-total-duration scenario)
        :total-cost (scenario-total-cost scenario)
        :end-date (scenario-end-date scenario)
        :task-count (hash-table-count (scenario-scheduled-data scenario))))

(defun scenario-critical-path (scenario)
  "Get the critical path for a scenario (tasks with zero slack)."
  ;; Simplified: return all tasks in dependency order
  ;; Full implementation would calculate slack
  (let ((sorted-ids (scenario-topological-sort scenario))
        (result nil))
    (dolist (id sorted-ids)
      (push id result))
    (nreverse result)))
