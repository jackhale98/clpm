;;;; src/session/persistence.lisp
;;;; Project loading and saving

(in-package #:project-juggler)

;;; ============================================================================
;;; Load Project Session
;;; ============================================================================

(defun load-project-session (file-path)
  "Load a project from file into a new session.

   Returns a SESSION object with the loaded project.
   The project is finalized and scheduled automatically."

  ;; Reset global state
  (setf *namespace-registry* (make-hash-table :test 'eq))
  (setf *project-registry* (make-hash-table :test 'eq))
  (setf *current-namespace* (make-instance 'namespace :name nil))
  (setf (gethash nil *namespace-registry*) *current-namespace*)

  ;; Load the project file
  (load file-path)

  ;; Get the loaded project (should be in *current-project*)
  (unless *current-project*
    (error "No project defined in file: ~A" file-path))

  (let ((project *current-project*))
    ;; Finalize and schedule
    (finalize-project project)
    (schedule project)

    ;; Create and return session
    (make-instance 'session :project project)))

;;; ============================================================================
;;; Save Session
;;; ============================================================================

(defun save-session (session file-path)
  "Save a session's project back to file.

   Writes the project definition with all current state to the file."

  (with-open-file (out file-path
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (write-project-to-stream (session-project session) out)))

(defun write-project-to-stream (project stream)
  "Write project definition to stream as S-expressions"

  (format stream ";;;; Project: ~A~%~%" (project-name project))
  (format stream "(defproject ~A ~S~%"
          (project-id project)
          (project-name project))
  (format stream "  :start (date ~D ~D ~D)~%"
          (date-year (project-start project))
          (date-month (project-start project))
          (date-day (project-start project)))
  (format stream "  :end (date ~D ~D ~D)~%~%"
          (date-year (project-end project))
          (date-month (project-end project))
          (date-day (project-end project)))

  ;; Write resources
  (loop for resource being the hash-values of (project-resources project)
        do (write-resource-to-stream resource stream))

  ;; Write tasks (only root tasks, subtasks handled recursively)
  (let ((root-tasks (loop for task being the hash-values of (project-tasks project)
                         when (null (task-parent task))
                         collect task)))
    ;; Sort by index for consistent output
    (setf root-tasks (sort root-tasks #'< :key #'task-index))
    (dolist (task root-tasks)
      (write-task-to-stream task stream 2)))

  (format stream ")~%"))

(defun write-resource-to-stream (resource stream &optional (indent 2))
  "Write resource definition to stream"
  (format stream "~VT(defresource ~A ~S"
          indent
          (resource-id resource)
          (resource-name resource))

  (when (resource-efficiency resource)
    (format stream "~%~VT:efficiency ~A"
            (+ indent 2)
            (resource-efficiency resource)))

  (when (resource-rate resource)
    (format stream "~%~VT:rate ~A"
            (+ indent 2)
            (resource-rate resource)))

  (format stream ")~%~%"))

(defun write-task-to-stream (task stream &optional (indent 2))
  "Write task definition to stream (recursively for subtasks)"
  (format stream "~VT(deftask ~A ~S~%"
          indent
          (task-id task)
          (task-name task))

  ;; Write task attributes
  (when (task-start task)
    (format stream "~VT:start (date ~D ~D ~D)~%"
            (+ indent 2)
            (date-year (task-start task))
            (date-month (task-start task))
            (date-day (task-start task))))

  (when (task-end task)
    (format stream "~VT:end (date ~D ~D ~D)~%"
            (+ indent 2)
            (date-year (task-end task))
            (date-month (task-end task))
            (date-day (task-end task))))

  (when (task-duration task)
    (let ((dur (task-duration task)))
      (format stream "~VT:duration (duration ~A :~(~A~))~%"
              (+ indent 2)
              (duration-value dur)
              (duration-unit dur))))

  (when (task-effort task)
    (let ((eff (task-effort task)))
      (format stream "~VT:effort (duration ~A :~(~A~))~%"
              (+ indent 2)
              (duration-value eff)
              (duration-unit eff))))

  (when (/= 500 (task-priority task))
    (format stream "~VT:priority ~A~%"
            (+ indent 2)
            (task-priority task)))

  (when (task-milestone-p task)
    (format stream "~VT:milestone t~%"
            (+ indent 2)))

  ;; Write dependencies
  (when (task-dependencies task)
    (format stream "~VT:depends-on (~{~A~^ ~})~%"
            (+ indent 2)
            (mapcar (lambda (dep)
                     (task-id (dependency-target dep)))
                   (task-dependencies task))))

  ;; Write allocations
  (when (task-allocations task)
    (format stream "~VT:allocate (~{~A~^ ~})~%"
            (+ indent 2)
            (mapcar (lambda (alloc)
                     (mapcar #'resource-id (allocation-resources alloc)))
                   (task-allocations task))))

  ;; Write subtasks (if any)
  (when (task-subtasks task)
    (format stream "~%")
    (let ((sorted-subtasks (sort (copy-list (task-subtasks task))
                                #'< :key #'task-index)))
      (dolist (subtask sorted-subtasks)
        (write-task-to-stream subtask stream (+ indent 2)))))

  (format stream "~VT)~%~%" indent))
