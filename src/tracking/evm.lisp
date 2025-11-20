;;;; src/tracking/evm.lisp
;;;; Earned Value Management calculations

(in-package #:project-juggler)

;;; ============================================================================
;;; Planned Value (PV)
;;; ============================================================================

(defun calculate-planned-value (project status-date)
  "Calculate Planned Value at the given status date.

   PV is the percentage of work that should be complete by the status date
   according to the baseline schedule."

  (unless (project-baseline project)
    (error "Project has no baseline"))

  (let ((total-tasks 0)
        (total-pv 0.0))
    (loop for baseline-task being the hash-values of (baseline-tasks (project-baseline project))
          do (incf total-tasks)
             (let ((task-pv (calculate-task-planned-value baseline-task status-date)))
               (incf total-pv task-pv)))

    (if (zerop total-tasks)
        0.0
        (/ total-pv total-tasks))))

(defun calculate-task-planned-value (baseline-task status-date)
  "Calculate planned value for a single task"
  (let ((start (baseline-task-start baseline-task))
        (end (baseline-task-end baseline-task)))
    (cond
      ;; Task hasn't started yet
      ((date< status-date start) 0.0)

      ;; Task is complete
      ((date>= status-date end) 100.0)

      ;; Task is in progress
      (t (let* ((total-days (date-difference-days end start))
                (elapsed-days (date-difference-days status-date start))
                (percent (if (zerop total-days)
                            100.0
                            (* 100.0 (/ elapsed-days total-days)))))
           (max 0.0 (min 100.0 percent)))))))

;;; ============================================================================
;;; Earned Value (EV)
;;; ============================================================================

(defun calculate-earned-value (project)
  "Calculate Earned Value - percentage of work actually complete"
  (let ((total-tasks 0)
        (total-ev 0.0))
    (loop for task being the hash-values of (project-tasks project)
          do (incf total-tasks)
             (incf total-ev (task-complete task)))

    (if (zerop total-tasks)
        0.0
        (/ total-ev total-tasks))))

;;; ============================================================================
;;; Schedule Variance (SV) and Schedule Performance Index (SPI)
;;; ============================================================================

(defun calculate-schedule-variance (project status-date)
  "Calculate Schedule Variance (SV) = EV - PV

   Positive = ahead of schedule
   Negative = behind schedule"

  (let ((ev (calculate-earned-value project))
        (pv (calculate-planned-value project status-date)))
    (- ev pv)))

(defun calculate-spi (project status-date)
  "Calculate Schedule Performance Index (SPI) = EV / PV

   > 1.0 = ahead of schedule
   < 1.0 = behind schedule
   = 1.0 = on schedule"

  (let ((ev (calculate-earned-value project))
        (pv (calculate-planned-value project status-date)))
    (if (zerop pv)
        1.0  ; No work planned yet
        (/ ev pv))))

;;; ============================================================================
;;; Resource Over-Allocation Detection
;;; ============================================================================

(defun detect-resource-overallocations (project)
  "Detect resource over-allocations in the project.

   Returns a list of OVERALLOCATION objects for each detected issue."

  (let ((overallocations nil))
    (loop for resource being the hash-values of (project-resources project)
          do (let ((resource-overallocs (detect-resource-overallocation resource project)))
               (setf overallocations (append overallocations resource-overallocs))))
    overallocations))

(defun detect-resource-overallocation (resource project)
  "Detect over-allocation for a single resource"
  (let ((overallocations nil)
        (allocated-tasks (get-resource-allocated-tasks resource project)))

    ;; Check each day in the project
    (when (>= (length allocated-tasks) 2)
      ;; Simple check: if resource has 2+ tasks with overlapping dates
      (dolist (task1 allocated-tasks)
        (dolist (task2 allocated-tasks)
          (when (and (not (eq task1 task2))
                    (tasks-overlap-p task1 task2))
            ;; Found an over-allocation
            (let ((overlap-start (if (date> (task-start task1) (task-start task2))
                                    (task-start task1)
                                    (task-start task2))))
              (push (make-instance 'overallocation
                                  :resource-id (resource-id resource)
                                  :date overlap-start
                                  :load 2.0)  ; Simplified: assume 2x load
                    overallocations))))))

    ;; Return unique over-allocations
    (remove-duplicates overallocations :key #'overallocation-date :test #'date=)))

(defun get-resource-allocated-tasks (resource project)
  "Get all tasks allocated to a resource"
  (let ((tasks nil))
    (loop for task being the hash-values of (project-tasks project)
          do (dolist (alloc (task-allocations task))
               (when (member resource (allocation-resources alloc))
                 (push task tasks))))
    tasks))

(defun tasks-overlap-p (task1 task2)
  "Check if two tasks overlap in time"
  (and (task-start task1) (task-end task1)
       (task-start task2) (task-end task2)
       (not (or (date>= (task-start task1) (task-end task2))
                (date>= (task-start task2) (task-end task1))))))

(defun calculate-resource-load (resource date)
  "Calculate resource load on a specific date"
  (let ((load 0.0))
    ;; This is a simplified implementation
    ;; Count how many tasks this resource is working on at the given date
    (dolist (alloc (resource-allocations resource))
      (let ((task (allocation-task alloc)))
        (when (and (task-start task) (task-end task)
                  (date>= date (task-start task))
                  (date< date (task-end task)))
          (incf load 1.0))))
    load))

(defun resource-allocations (resource)
  "Get all allocations for a resource (helper function)"
  (let ((allocs nil))
    (loop for task being the hash-values of (project-tasks (resource-project resource))
          do (dolist (alloc (task-allocations task))
               (when (member resource (allocation-resources alloc))
                 (push alloc allocs))))
    allocs))

(defun date-difference-days (date1 date2)
  "Calculate difference between two dates in days"
  (let ((diff (local-time:timestamp-difference
               (date-timestamp date1)
               (date-timestamp date2))))
    (abs (round (/ diff 86400)))))
