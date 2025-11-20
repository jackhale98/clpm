;;;; src/scheduling/criticalness.lisp
;;;; TaskJuggler-style heuristic criticalness calculations
;;;;
;;;; NOTE: This is TaskJuggler's HEURISTIC criticalness used for scheduling priority.
;;;; True critical path (slack-based) is calculated AFTER scheduling in critical-path.lisp

(in-package #:project-juggler)

;;; ============================================================================
;;; Resource Criticalness (TaskJuggler Heuristic)
;;; ============================================================================

(defun calculate-resource-criticalness (project &optional (scenario :plan))
  "Calculate criticalness for all resources.

   Resource criticalness = allocated effort / available effort

   Criticalness > 1.0 means resource is statistically over-allocated.
   This is used as a heuristic to prioritize tasks during scheduling."
  (declare (ignore scenario))

  (loop for resource being the hash-values of (project-resources project)
        do (let* ((allocated (resource-allocated-effort resource))
                  (available (resource-available-effort resource))
                  (criticalness (if (zerop available)
                                    0.0  ; Zero available = 0.0, not infinity
                                    (/ allocated available))))
             (setf (resource-criticalness resource) criticalness))))

;;; ============================================================================
;;; Task Criticalness (TaskJuggler Heuristic)
;;; ============================================================================

(defun calculate-task-criticalness (project &optional (scenario :plan))
  "Calculate criticalness for all tasks.

   For milestones: criticalness = priority / 500.0
   For effort tasks: criticalness = effort (hours) × avg(resource criticalness)

   Higher criticalness = higher priority during scheduling."
  (declare (ignore scenario))

  (loop for task being the hash-values of (project-tasks project)
        do (cond
             ;; Milestone criticalness based on priority
             ((task-milestone-p task)
              (setf (task-criticalness task)
                    (/ (task-priority task) 500.0)))

             ;; Effort task criticalness
             ((and (task-effort task) (task-allocations task))
              (let* ((effort-hours (float (duration-in-hours (task-effort task))))
                     (resource-sum 0.0)
                     (resource-count 0))

                ;; Sum criticalness of all allocated resources
                (dolist (alloc (task-allocations task))
                  (dolist (resource (allocation-resources alloc))
                    (incf resource-sum (resource-criticalness resource))
                    (incf resource-count)))

                ;; Task criticalness = effort × average resource criticalness
                (let ((avg-resource-criticalness
                       (if (zerop resource-count)
                           0.0
                           (/ resource-sum resource-count))))
                  (setf (task-criticalness task)
                        (* effort-hours avg-resource-criticalness)))))

             ;; Default: low criticalness
             (t
              (setf (task-criticalness task) 0.0)))))

;;; ============================================================================
;;; Path Criticalness (TaskJuggler Heuristic)
;;; ============================================================================

(defun calculate-path-criticalness (project &optional (scenario :plan))
  "Calculate path criticalness for all tasks.

   Path criticalness = task criticalness + max(dependency path criticalness)

   This accumulates criticalness along dependency chains, favoring tasks
   on long chains of critical resources. Used to prioritize scheduling."
  (declare (ignore scenario))

  (let ((memo (make-hash-table :test 'eq)))

    (labels ((compute-path-criticalness (task)
               "Recursively compute path criticalness with memoization"
               (or (gethash task memo)
                   (setf (gethash task memo)
                         (+ (task-criticalness task)
                            ;; Add max of all dependency paths
                            (if (task-dependencies task)
                                (loop for dep in (task-dependencies task)
                                      for target = (dependency-target dep)
                                      when target
                                      maximize (compute-path-criticalness target))
                                0.0))))))

      ;; Compute and store path criticalness for all tasks
      (loop for task being the hash-values of (project-tasks project)
            do (setf (task-path-criticalness task)
                     (compute-path-criticalness task))))))

