;;;; src/dsl/defscenario.lisp
;;;; defscenario macro for creating named scenarios/baselines
;;;;
;;;; In Project Juggler, scenarios and baselines are the same concept.
;;;; A scenario is a named snapshot of project task values that can be
;;;; compared against other scenarios.

(in-package #:claps)

;;; ============================================================================
;;; defscenario Macro
;;; ============================================================================

(defmacro defscenario (id name &key (project '*current-project*) description)
  "Define a new scenario (baseline) for the project.

   A scenario captures the current task values (effort, duration, start, end)
   and can be used for what-if analysis or as a baseline for EVM tracking.

   Usage:
     ;; Create a scenario from current project state
     (defscenario initial-plan \"Initial Plan\"
       :description \"Original estimates before scope changes\")

     ;; Create scenario explicitly on a project
     (defscenario optimistic \"Optimistic Estimate\"
       :project my-project)

   Arguments:
     id          - Symbol identifier for the scenario
     name        - Human-readable name
     :project    - Project to add scenario to (default: *current-project*)
     :description - Optional description of the scenario"
  `(add-scenario ,project ',id ,name :description ,description))

(defun add-scenario (project id name &key description)
  "Add a new scenario to a project, capturing current task values.

   Returns the newly created scenario."
  (let ((scenario (make-instance 'scenario
                                 :id id
                                 :name name)))
    ;; Store current task values for this scenario
    (maphash (lambda (task-id task)
               (declare (ignore task-id))
               ;; Snapshot current values into scenario-values
               (setf (gethash id (task-scenario-values task))
                     (list :effort (task-effort task)
                           :duration (task-duration task)
                           :start (task-start task)
                           :end (task-end task)
                           :complete (task-complete task))))
             (project-tasks project))

    ;; Add scenario to project's scenario list
    (setf (project-scenarios project)
          (append (project-scenarios project) (list scenario)))

    ;; Store description if provided
    (when description
      (setf (scenario-description scenario) description))

    scenario))

(defun remove-scenario (project id)
  "Remove a scenario from a project.
   Cannot remove the first (baseline) scenario."
  (when (eq id (baseline-scenario-id project))
    (error "Cannot remove the baseline scenario (~A)" id))

  ;; Remove from project's scenario list
  (setf (project-scenarios project)
        (remove id (project-scenarios project) :key #'scenario-id))

  ;; Remove scenario values from all tasks
  (maphash (lambda (task-id task)
             (declare (ignore task-id))
             (remhash id (task-scenario-values task)))
           (project-tasks project)))

(defun copy-scenario (project source-id new-id new-name)
  "Create a copy of an existing scenario with a new ID and name.
   Useful for creating what-if variations."
  (let ((new-scenario (make-instance 'scenario
                                     :id new-id
                                     :name new-name)))
    ;; Copy task values from source scenario
    (maphash (lambda (task-id task)
               (declare (ignore task-id))
               (let ((source-values (gethash source-id (task-scenario-values task))))
                 (when source-values
                   (setf (gethash new-id (task-scenario-values task))
                         (copy-list source-values)))))
             (project-tasks project))

    ;; Add to project
    (setf (project-scenarios project)
          (append (project-scenarios project) (list new-scenario)))

    new-scenario))

(defun set-scenario-value (project scenario-id task-id property value)
  "Set a specific value for a task in a scenario.

   Example:
     (set-scenario-value *project* 'delayed 'backend :effort (duration 40 :days))"
  (let ((task (gethash task-id (project-tasks project))))
    (unless task
      (error "Task ~A not found in project" task-id))
    (let ((scenario-plist (gethash scenario-id (task-scenario-values task))))
      (if scenario-plist
          (setf (getf (gethash scenario-id (task-scenario-values task)) property) value)
          (setf (gethash scenario-id (task-scenario-values task))
                (list property value))))
    value))

;;; ============================================================================
;;; Scenario Description Extension
;;; ============================================================================

;; Add description slot to scenario class if not present
(defmethod scenario-description ((scenario scenario))
  "Get scenario description."
  (if (slot-boundp scenario 'description)
      (slot-value scenario 'description)
      nil))

(defmethod (setf scenario-description) (value (scenario scenario))
  "Set scenario description."
  (setf (slot-value scenario 'description) value))
