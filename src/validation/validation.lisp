;;;; src/validation/validation.lisp
;;;; Project validation and finalization

(in-package #:project-juggler)

;;; ============================================================================
;;; Main Finalization Entry Point
;;; ============================================================================

(defun finalize-project (project)
  "Finalize project: resolve all references, validate constraints, detect cycles.

   This function must be called before scheduling. It:
   1. Resolves all task dependency references
   2. Resolves all resource allocation references
   3. Detects circular dependencies using DFS
   4. Validates task constraints
   5. Validates date ranges

   Signals errors on validation failures."

  ;; Step 1: Resolve all task references
  (resolve-all-task-references project)

  ;; Step 2: Resolve all resource references
  (resolve-all-resource-references project)

  ;; Step 3: Detect circular dependencies
  (detect-circular-dependencies project)

  ;; Step 4: Validate task constraints
  (validate-task-constraints project)

  ;; Step 5: Validate date ranges
  (validate-date-ranges project)

  ;; Return t on success
  t)

;;; ============================================================================
;;; Reference Resolution
;;; ============================================================================

(defun resolve-all-task-references (project)
  "Resolve all task dependency references in the project"
  (loop for task being the hash-values of (project-tasks project)
        do (loop for dep in (task-dependencies task)
                 unless (dependency-target dep)
                 do (handler-case
                        (setf (dependency-target dep)
                              (resolve-task-reference
                                (dependency-target-ref dep)
                                project))
                      (reference-error (e)
                        (error 'reference-error
                               :message (format nil "Cannot resolve task reference ~A in task ~A: ~A"
                                              (dependency-target-ref dep)
                                              (task-id task)
                                              (error-message e))))))))

(defun resolve-all-resource-references (project)
  "Resolve all resource allocation references in the project"
  (loop for task being the hash-values of (project-tasks project)
        do (loop for alloc in (task-allocations task)
                 unless (allocation-resources alloc)
                 do (let ((resolved-resources nil))
                      (dolist (ref (allocation-resource-refs alloc))
                        (handler-case
                            (push (resolve-resource-reference ref project)
                                  resolved-resources)
                          (reference-error (e)
                            (error 'reference-error
                                   :message (format nil "Cannot resolve resource reference ~A in task ~A: ~A"
                                                  ref
                                                  (task-id task)
                                                  (error-message e))))))
                      (setf (allocation-resources alloc)
                            (nreverse resolved-resources))))))

;;; ============================================================================
;;; Circular Dependency Detection
;;; ============================================================================

(defun detect-circular-dependencies (project)
  "Detect cycles in dependency graph using depth-first search.
   Signals circular-dependency-error if cycles are found."

  (let ((visited (make-hash-table :test 'eq))
        (rec-stack (make-hash-table :test 'eq))
        (cycles nil))

    (labels ((visit (task path)
               "Visit task in DFS, maintaining recursion stack"
               (setf (gethash task visited) t)
               (setf (gethash task rec-stack) t)

               ;; Visit all dependencies
               (dolist (dep (task-dependencies task))
                 (let ((target (dependency-target dep)))
                   (when target  ; Only check if reference was resolved
                     (cond
                       ;; Cycle detected: target is in recursion stack
                       ((gethash target rec-stack)
                        (let ((cycle-path (append path (list task target))))
                          (push cycle-path cycles)))

                       ;; Not yet visited: recurse
                       ((not (gethash target visited))
                        (visit target (append path (list task))))))))

               ;; Remove from recursion stack
               (setf (gethash task rec-stack) nil)))

      ;; Visit all tasks in the project
      (loop for task being the hash-values of (project-tasks project)
            unless (gethash task visited)
            do (visit task nil)))

    ;; If cycles found, signal error
    (when cycles
      (error 'circular-dependency-error
             :cycles cycles
             :message (format nil "Circular dependencies detected: ~{~A~^, ~}"
                            (mapcar #'format-cycle-path cycles))))))

(defun format-cycle-path (cycle)
  "Format cycle path for error message"
  (format nil "~{~A~^ → ~}"
          (mapcar (lambda (task)
                   (if (task-p task)
                       (task-id task)
                       task))
                  cycle)))

;;; ============================================================================
;;; Constraint Validation
;;; ============================================================================

(defun validate-task-constraints (project)
  "Validate task constraints (effort XOR duration, milestone rules, etc.)"
  (loop for task being the hash-values of (project-tasks project)
        do (validate-single-task-constraints task)))

(defun validate-single-task-constraints (task)
  "Validate constraints for a single task"

  ;; Rule 1: Task cannot have both effort and duration
  (when (and (task-effort task) (task-duration task))
    (error 'validation-error
           :message (format nil "Task ~A has both effort and duration - only one allowed"
                          (task-id task))))

  ;; Rule 2: Milestone cannot have effort
  (when (and (task-milestone-p task) (task-effort task))
    (error 'validation-error
           :message (format nil "Milestone task ~A cannot have effort"
                          (task-id task))))

  ;; Rule 3: Milestone should not have duration (warning only)
  (when (and (task-milestone-p task) (task-duration task))
    (warn "Milestone task ~A has duration - milestones typically have zero duration"
          (task-id task)))

  ;; Rule 4: Task with effort should have allocations (warning only)
  (when (and (task-effort task)
             (null (task-allocations task)))
    (warn "Task ~A has effort but no resource allocations - it may not schedule correctly"
          (task-id task))))

(defun validate-date-ranges (project)
  "Validate that task dates are within project date range"
  (let ((project-start (project-start project))
        (project-end (project-end project)))

    (loop for task being the hash-values of (project-tasks project)
          do (progn
               ;; Check task start date
               (when (task-start task)
                 (when (date< (task-start task) project-start)
                   (error 'validation-error
                          :message (format nil "Task ~A start date (~A) is before project start (~A)"
                                         (task-id task)
                                         (format-date (task-start task))
                                         (format-date project-start))))

                 (when (date> (task-start task) project-end)
                   (error 'validation-error
                          :message (format nil "Task ~A start date (~A) is after project end (~A)"
                                         (task-id task)
                                         (format-date (task-start task))
                                         (format-date project-end)))))

               ;; Check task end date
               (when (task-end task)
                 (when (date< (task-end task) project-start)
                   (error 'validation-error
                          :message (format nil "Task ~A end date (~A) is before project start (~A)"
                                         (task-id task)
                                         (format-date (task-end task))
                                         (format-date project-start))))

                 (when (date> (task-end task) project-end)
                   (error 'validation-error
                          :message (format nil "Task ~A end date (~A) is after project end (~A)"
                                         (task-id task)
                                         (format-date (task-end task))
                                         (format-date project-end)))))))))

(defun format-date (date)
  "Format date for display in error messages"
  (format nil "~4,'0D-~2,'0D-~2,'0D"
          (date-year date)
          (date-month date)
          (date-day date)))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun task-p (obj)
  "Check if object is a task"
  (typep obj 'task))
