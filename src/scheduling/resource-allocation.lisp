;;;; src/scheduling/resource-allocation.lisp
;;;; Resource allocation and leveling

(in-package #:project-juggler)

;;; ============================================================================
;;; Overallocation Class
;;; ============================================================================

(defclass overallocation ()
  ((resource-id :initarg :resource-id :reader overallocation-resource-id)
   (resource :initarg :resource :reader overallocation-resource)
   (date :initarg :date :reader overallocation-date)
   (load :initarg :load :reader overallocation-load)
   (tasks :initarg :tasks :reader overallocation-tasks))
  (:documentation "Represents a resource over-allocation on a specific date"))

;;; ============================================================================
;;; Resource Load Calculation
;;; ============================================================================

(defun calculate-resource-load (resource project start-date end-date)
  "Calculate the daily load for a resource within a date range.

   Returns a list of (date . load) pairs where load is a float
   (1.0 = 100% allocated, 2.0 = 200% overallocated, etc.)"
  (let ((result nil)
        (current-date start-date))

    ;; Iterate through each day
    (loop while (not (date> current-date end-date))
          do (let ((daily-load (calculate-daily-load resource project current-date)))
               (push (cons current-date daily-load) result))
             (setf current-date (date+ current-date (duration 1 :days))))

    (nreverse result)))

(defun calculate-daily-load (resource project date)
  "Calculate the load on a resource for a specific date.

   Returns the sum of allocations (1.0 per full-time task)."
  (let ((load 0.0))
    (loop for task being the hash-values of (project-tasks project)
          do (when (and (task-start task)
                       (task-end task)
                       (not (date< date (task-start task)))
                       (date< date (task-end task))
                       (resource-allocated-to-task-p resource task))
               (incf load 1.0)))
    load))

(defun resource-allocated-to-task-p (resource task)
  "Check if a resource is allocated to a task"
  (dolist (allocation (task-allocations task))
    (dolist (allocated-resource (allocation-resources allocation))
      (when (eq resource allocated-resource)
        (return-from resource-allocated-to-task-p t))))
  nil)

;;; ============================================================================
;;; Over-allocation Detection
;;; ============================================================================

(defun detect-resource-overallocations (project)
  "Detect all resource over-allocations in a project.

   Returns a list of OVERALLOCATION objects."
  (let ((overallocations nil)
        (start-date (project-start project))
        (end-date (project-end project)))

    ;; Check each resource
    (loop for resource being the hash-values of (project-resources project)
          do (let ((load-data (calculate-resource-load resource project start-date end-date)))
               ;; Find days where load > 1.0
               (dolist (day-load load-data)
                 (when (> (cdr day-load) 1.0)
                   (let ((tasks (tasks-using-resource-on-date resource project (car day-load))))
                     (push (make-instance 'overallocation
                                         :resource-id (resource-id resource)
                                         :resource resource
                                         :date (car day-load)
                                         :load (cdr day-load)
                                         :tasks tasks)
                           overallocations))))))

    ;; Return unique overallocations (one per resource, not per day)
    (remove-duplicates overallocations :key #'overallocation-resource-id)))

(defun tasks-using-resource-on-date (resource project date)
  "Get all tasks using a resource on a specific date"
  (let ((tasks nil))
    (loop for task being the hash-values of (project-tasks project)
          do (when (and (task-start task)
                       (task-end task)
                       (not (date< date (task-start task)))
                       (date< date (task-end task))
                       (resource-allocated-to-task-p resource task))
               (push task tasks)))
    tasks))

;;; ============================================================================
;;; Resource Leveling
;;; ============================================================================

(defun level-resources (project)
  "Level resources by shifting tasks to resolve over-allocations.

   Algorithm:
   1. Detect over-allocations
   2. For each over-allocated resource:
      a. Get all tasks using that resource
      b. Sort tasks by slack (descending - most flexible first)
      c. Shift tasks with slack forward until allocation <= 100%
   3. Recalculate schedule
   4. Verify no dependencies violated"

  (let ((overallocations (detect-resource-overallocations project)))
    (when overallocations
      ;; Process each overallocation
      (dolist (overalloc overallocations)
        (level-resource-allocation project (overallocation-resource overalloc)))

      ;; Recalculate critical path after leveling
      (calculate-critical-path project))))

(defun level-resource-allocation (project resource)
  "Level allocations for a single resource"
  (let* ((start-date (project-start project))
         (end-date (project-end project))
         (load-data (calculate-resource-load resource project start-date end-date)))

    ;; Find over-allocated days and shift tasks
    (dolist (day-load load-data)
      (when (> (cdr day-load) 1.0)
        (resolve-overallocation-on-date project resource (car day-load))))))

(defun resolve-overallocation-on-date (project resource date)
  "Resolve over-allocation on a specific date by shifting tasks"
  (declare (ignore date))
  (let* ((tasks (tasks-using-resource-on-date resource project date))
         ;; Sort tasks: low priority first, then by slack (most slack first)
         ;; Tasks to be shifted will be taken from the sorted list, keeping the first one
         (sorted-tasks (sort (copy-list tasks)
                            (lambda (a b)
                              (let ((slack-a (or (task-slack a) 0))
                                    (slack-b (or (task-slack b) 0))
                                    (pri-a (task-priority a))
                                    (pri-b (task-priority b)))
                                ;; First compare by priority (higher priority stays)
                                ;; so lower priority comes first in the list to be shifted
                                (if (/= pri-a pri-b)
                                    (< pri-a pri-b)  ; Lower priority first (to shift)
                                    ;; Same priority: more slack first (to shift)
                                    (> slack-a slack-b)))))))

    ;; Now sorted-tasks has tasks to shift first at the beginning
    ;; We want to keep the LAST task (highest priority / least slack) in place
    ;; So shift from the front, but keep at least one task
    (let ((current-load (length tasks)))
      (when (> current-load 1)
        ;; The last task in sorted-tasks is the one we keep in place
        (let* ((task-to-keep (car (last sorted-tasks)))
               (tasks-to-shift (butlast sorted-tasks)))
          (dolist (task tasks-to-shift)
            (when (> current-load 1)
              ;; Shift task to start after the kept task ends
              (shift-task-past-other task task-to-keep project)
              (decf current-load))))))))

(defun shift-task-past-other (task other-task project)
  "Shift a task to start after another task ends to resolve over-allocation.

   The task will start at the end date of other-task, respecting dependencies."
  (let* ((new-start (task-end other-task))
         (duration-days (if (task-duration task)
                           (duration-value (task-duration task))
                           1))
         (new-end (date+ new-start (duration duration-days :days))))

    ;; Verify dependencies still valid
    (let ((min-start (calculate-earliest-valid-start task project)))
      (when (date< new-start min-start)
        (setf new-start min-start)
        (setf new-end (date+ new-start (duration duration-days :days)))))

    ;; Update task dates
    (setf (task-start task) new-start)
    (setf (task-end task) new-end)))

(defun shift-task-forward (task project)
  "Shift a task forward in time to resolve over-allocation.

   Moves the task to start after its current end date while respecting dependencies."
  (let* ((new-start (task-end task))
         (duration-days (if (task-duration task)
                           (duration-value (task-duration task))
                           1))
         (new-end (date+ new-start (duration duration-days :days))))

    ;; Verify dependencies still valid
    (let ((min-start (calculate-earliest-valid-start task project)))
      (when (date< new-start min-start)
        (setf new-start min-start)
        (setf new-end (date+ new-start (duration duration-days :days)))))

    ;; Update task dates
    (setf (task-start task) new-start)
    (setf (task-end task) new-end)))

(defun calculate-earliest-valid-start (task project)
  "Calculate the earliest valid start date respecting dependencies"
  (declare (ignore project))
  (if (null (task-dependencies task))
      (task-start task)
      (let ((latest-dep-end nil))
        (dolist (dep (task-dependencies task))
          (let ((target (dependency-target dep)))
            (when (and target (task-end target))
              (if (null latest-dep-end)
                  (setf latest-dep-end (task-end target))
                  (when (date> (task-end target) latest-dep-end)
                    (setf latest-dep-end (task-end target)))))))
        (or latest-dep-end (task-start task)))))
