;;;; src/tracking/scenarios.lisp
;;;; TaskJuggler-style scenarios
;;;;
;;;; Key design (matching TaskJuggler):
;;;; - Scenarios declared at project level: :scenarios (plan delayed)
;;;; - Tasks store values per scenario: :delayed/effort (duration 40 :days)
;;;; - First scenario is the baseline
;;;; - No separate "baseline" concept - just scenarios

(in-package #:project-juggler)

;;; ============================================================================
;;; Scenario Utilities
;;; ============================================================================

(defun get-scenario (project scenario-id)
  "Get a scenario by ID from the project."
  (find scenario-id (project-scenarios project) :key #'scenario-id))

(defun list-scenarios (project)
  "List all scenario IDs in the project."
  (mapcar #'scenario-id (project-scenarios project)))

(defun baseline-scenario (project)
  "Get the baseline scenario (first one declared)."
  (first (project-scenarios project)))

(defun baseline-scenario-id (project)
  "Get the baseline scenario ID."
  (let ((baseline (baseline-scenario project)))
    (when baseline (scenario-id baseline))))

;;; ============================================================================
;;; Task Value Access for Scenarios
;;; ============================================================================

(defun task-scheduled-values-for-scenario (task scenario-id)
  "Get all scheduled values for a task in a specific scenario.
   Returns a plist with :start :end :duration :effort :complete.
   Falls back to base values for any not overridden."
  (list :start (task-start-for-scenario task scenario-id)
        :end (task-end-for-scenario task scenario-id)
        :duration (task-duration-for-scenario task scenario-id)
        :effort (task-effort-for-scenario task scenario-id)
        :complete (task-complete-for-scenario task scenario-id)))

;;; ============================================================================
;;; Scenario Comparison
;;; ============================================================================

(defun compare-scenarios (project scenario-id-1 scenario-id-2)
  "Compare two scenarios, returning a summary of differences.
   Returns a plist with :duration-1 :duration-2 :effort-1 :effort-2 :end-1 :end-2."
  (let ((total-dur-1 0)
        (total-dur-2 0)
        (total-eff-1 0)
        (total-eff-2 0)
        (end-1 nil)
        (end-2 nil))
    (maphash (lambda (id task)
               (declare (ignore id))
               (let ((dur-1 (task-duration-for-scenario task scenario-id-1))
                     (dur-2 (task-duration-for-scenario task scenario-id-2))
                     (eff-1 (task-effort-for-scenario task scenario-id-1))
                     (eff-2 (task-effort-for-scenario task scenario-id-2))
                     (e-1 (task-end-for-scenario task scenario-id-1))
                     (e-2 (task-end-for-scenario task scenario-id-2)))
                 (when dur-1 (incf total-dur-1 (duration-in-days dur-1)))
                 (when dur-2 (incf total-dur-2 (duration-in-days dur-2)))
                 (when eff-1 (incf total-eff-1 (duration-in-days eff-1)))
                 (when eff-2 (incf total-eff-2 (duration-in-days eff-2)))
                 (when (and e-1 (or (null end-1) (date> e-1 end-1)))
                   (setf end-1 e-1))
                 (when (and e-2 (or (null end-2) (date> e-2 end-2)))
                   (setf end-2 e-2))))
             (project-tasks project))
    (list :duration-1 total-dur-1
          :duration-2 total-dur-2
          :effort-1 total-eff-1
          :effort-2 total-eff-2
          :end-1 end-1
          :end-2 end-2)))

(defun compare-task-scenarios (task scenario-id-1 scenario-id-2)
  "Compare a single task's values between two scenarios."
  (let ((dur-1 (task-duration-for-scenario task scenario-id-1))
        (dur-2 (task-duration-for-scenario task scenario-id-2))
        (eff-1 (task-effort-for-scenario task scenario-id-1))
        (eff-2 (task-effort-for-scenario task scenario-id-2)))
    (list :duration-1 (when dur-1 (duration-in-days dur-1))
          :duration-2 (when dur-2 (duration-in-days dur-2))
          :effort-1 (when eff-1 (duration-in-days eff-1))
          :effort-2 (when eff-2 (duration-in-days eff-2)))))

;;; ============================================================================
;;; Scenario Summary
;;; ============================================================================

(defun scenario-summary (project scenario-id)
  "Get summary statistics for a scenario."
  (let ((total-duration 0)
        (total-effort 0)
        (end-date nil)
        (task-count 0))
    (maphash (lambda (id task)
               (declare (ignore id))
               (incf task-count)
               (let ((dur (task-duration-for-scenario task scenario-id))
                     (eff (task-effort-for-scenario task scenario-id))
                     (end (task-end-for-scenario task scenario-id)))
                 (when dur (incf total-duration (duration-in-days dur)))
                 (when eff (incf total-effort (duration-in-days eff)))
                 (when (and end (or (null end-date) (date> end end-date)))
                   (setf end-date end))))
             (project-tasks project))
    (list :total-duration total-duration
          :total-effort total-effort
          :end-date end-date
          :task-count task-count)))
