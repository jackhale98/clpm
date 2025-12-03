;;;; src/scheduling/availability.lisp
;;;; Resource availability calculations

(in-package #:project-juggler)

;;; ============================================================================
;;; Leave/Vacation Checking
;;; ============================================================================

(defun resource-available-p (resource date)
  "Check if a resource is available on a specific date.
   Returns T if available, NIL if on leave."
  (not (resource-on-leave-p resource date)))

(defun resource-on-leave-p (resource date)
  "Check if a resource is on leave on a specific date."
  (let ((leaves (resource-leaves resource)))
    (when leaves
      (dolist (leave leaves)
        (when (date-in-leave-p date leave)
          (return-from resource-on-leave-p t))))
    nil))

(defun date-in-leave-p (date leave)
  "Check if a date falls within a leave period."
  (let ((leave-start (leave-start leave))
        (leave-end (leave-end leave)))
    (and (or (date= date leave-start)
             (date> date leave-start))
         (or (date= date leave-end)
             (date< date leave-end)))))

;;; ============================================================================
;;; Available Hours Calculation
;;; ============================================================================

(defun resource-available-hours (resource date)
  "Get the number of hours a resource can work on a specific date.
   Uses daily-limit if set, otherwise defaults to 8 hours."
  (let ((daily-limit (resource-daily-limit resource)))
    (if daily-limit
        (duration-in-hours daily-limit)
        8.0)))  ; Default 8 hours/day

(defun effective-available-hours (resource date)
  "Get effective available hours considering leaves and limits.
   Returns 0 if on leave, otherwise returns daily limit."
  (if (resource-on-leave-p resource date)
      0.0
      (resource-available-hours resource date)))

;;; ============================================================================
;;; Leave Conflict Detection
;;; ============================================================================

(defun detect-leave-conflicts (task)
  "Detect conflicts between a task's schedule and resource leaves.
   Returns a list of leave-conflict objects."
  (let ((conflicts nil)
        (task-start (task-start task))
        (task-end (task-end task)))
    (when (and task-start task-end)
      (dolist (allocation (task-allocations task))
        (dolist (resource (allocation-resources allocation))
          (dolist (leave (resource-leaves resource))
            (let ((overlap (calculate-date-overlap
                           task-start task-end
                           (leave-start leave) (leave-end leave))))
              (when overlap
                (push (make-instance 'leave-conflict
                                    :resource resource
                                    :leave leave
                                    :task task
                                    :overlap-start (first overlap)
                                    :overlap-end (second overlap))
                      conflicts)))))))
    (nreverse conflicts)))

(defun calculate-date-overlap (start1 end1 start2 end2)
  "Calculate the overlap between two date ranges.
   Returns (overlap-start overlap-end) or NIL if no overlap."
  (let ((overlap-start (if (date> start1 start2) start1 start2))
        (overlap-end (if (date< end1 end2) end1 end2)))
    (when (or (date< overlap-start overlap-end)
              (date= overlap-start overlap-end))
      (list overlap-start overlap-end))))

;;; ============================================================================
;;; Availability Summary
;;; ============================================================================

(defun availability-summary (resource start-date end-date)
  "Calculate availability summary for a resource over a date range.
   Returns a plist with:
   - :total-hours - total available hours in range
   - :leave-days - number of leave days
   - :working-days - number of working days"
  (let ((total-hours 0.0)
        (leave-days 0)
        (working-days 0)
        (current-date start-date))
    (loop while (or (date< current-date end-date)
                    (date= current-date end-date))
          do (let ((hours (effective-available-hours resource current-date)))
               (if (zerop hours)
                   (incf leave-days)
                   (progn
                     (incf working-days)
                     (incf total-hours hours)))
               (setf current-date (date+ current-date (duration 1 :days)))))
    (list :total-hours total-hours
          :leave-days leave-days
          :working-days working-days)))

;;; ============================================================================
;;; Allocation Percent Helpers
;;; ============================================================================

(defun get-allocation-percent-for-resource (allocation resource)
  "Get the allocation percentage for a specific resource.
   Returns the resource-specific percent or the default allocation percent."
  (let ((resource-percents (allocation-resource-percents allocation))
        (resource-ref (resource-id resource)))
    (if resource-percents
        (or (cdr (assoc resource-ref resource-percents :test #'eq))
            (allocation-percent allocation))
        (allocation-percent allocation))))

(defun calculate-effective-efficiency (task)
  "Calculate effective efficiency for a task considering allocation percentages.
   Returns the sum of (efficiency × percent/100) for all allocated resources."
  (let ((total-efficiency 0.0))
    (dolist (allocation (task-allocations task))
      (dolist (resource (allocation-resources allocation))
        (let ((efficiency (resource-efficiency resource))
              (percent (get-allocation-percent-for-resource allocation resource)))
          (incf total-efficiency (* efficiency (/ percent 100.0))))))
    (if (zerop total-efficiency)
        1.0  ; Avoid division by zero
        total-efficiency)))
