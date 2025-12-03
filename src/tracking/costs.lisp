;;;; src/tracking/costs.lisp
;;;; Cost tracking and calculation functions

(in-package #:project-juggler)

;;; ============================================================================
;;; Task Cost Calculations
;;; ============================================================================

(defun calculate-task-planned-cost (task &key (include-fixed nil))
  "Calculate the planned labor cost for a task.

   For effort-based tasks: cost = effort / num_resources × sum(resource_rates)
   For duration-based tasks: cost = duration × sum(resource_rates)

   If include-fixed is T, adds the task's fixed-cost."
  (let ((labor-cost 0.0)
        (allocations (task-allocations task)))

    (when allocations
      (let* ((resources (loop for alloc in allocations
                              append (allocation-resources alloc)))
             (num-resources (length resources))
             (total-rate (loop for r in resources
                               sum (or (resource-rate r) 0.0))))

        (cond
          ;; Effort-based task: divide effort among resources
          ((task-effort task)
           (let ((effort-days (duration-in-days (task-effort task))))
             ;; Each resource works effort/num_resources days
             ;; Cost = (effort/n) × rate1 + (effort/n) × rate2 + ...
             ;;      = (effort/n) × (rate1 + rate2 + ...)
             (when (> num-resources 0)
               (setf labor-cost (* (/ effort-days num-resources) total-rate)))))

          ;; Duration-based task: all resources work full duration
          ((task-duration task)
           (let ((duration-days (duration-in-days (task-duration task))))
             (setf labor-cost (* duration-days total-rate))))

          ;; Task with scheduled dates but no explicit duration
          ((and (task-start task) (task-end task))
           (let* ((start-ts (local-time:timestamp-to-unix (date-timestamp (task-start task))))
                  (end-ts (local-time:timestamp-to-unix (date-timestamp (task-end task))))
                  (duration-days (max 1 (truncate (/ (- end-ts start-ts) 86400)))))
             (setf labor-cost (* duration-days total-rate)))))))

    ;; Add fixed cost if requested
    (when (and include-fixed (slot-boundp task 'fixed-cost))
      (incf labor-cost (or (task-fixed-cost task) 0.0)))

    labor-cost))

(defun calculate-task-actual-cost (task)
  "Calculate actual cost from bookings for a task.

   Sums (booking-hours × resource-rate) for all bookings."
  (let ((total-cost 0.0))
    (dolist (booking (task-bookings task))
      (let* ((resource (booking-resource booking))
             (hours (booking-duration-hours booking))
             (daily-rate (or (resource-rate resource) 0.0))
             (hourly-rate (/ daily-rate 8.0)))  ; Assume 8-hour day
        (incf total-cost (* hours hourly-rate))))
    total-cost))

;;; ============================================================================
;;; Project Cost Calculations
;;; ============================================================================

(defun calculate-project-planned-cost (project)
  "Calculate total planned cost for the entire project.

   Sums planned costs for all leaf tasks (excludes parent tasks to avoid double-counting)."
  (let ((total 0.0))
    (maphash (lambda (id task)
               (declare (ignore id))
               ;; Only count leaf tasks
               (unless (task-subtasks task)
                 (incf total (calculate-task-planned-cost task :include-fixed t))))
             (project-tasks project))
    total))

(defun calculate-project-actual-cost (project)
  "Calculate total actual cost from all bookings in the project."
  (let ((total 0.0))
    (maphash (lambda (id task)
               (declare (ignore id))
               (incf total (calculate-task-actual-cost task)))
             (project-tasks project))
    total))

(defun project-budget-remaining (project)
  "Calculate remaining budget (budget - actual cost)."
  (let ((budget (or (project-budget project) 0.0))
        (actual (calculate-project-actual-cost project)))
    (- budget actual)))

;;; ============================================================================
;;; EVM Cost Metrics
;;; ============================================================================

(defun calculate-actual-cost (project)
  "Calculate Actual Cost (AC) - the actual cost of work performed.

   This is the same as calculate-project-actual-cost but named for EVM consistency."
  (calculate-project-actual-cost project))

(defun calculate-earned-value-cost (project)
  "Calculate Earned Value (EV) in cost terms.

   EV = sum of (task-complete% × task-planned-cost) for all tasks."
  (let ((total-ev 0.0))
    (maphash (lambda (id task)
               (declare (ignore id))
               (unless (task-subtasks task)
                 (let ((planned-cost (calculate-task-planned-cost task))
                       (complete-pct (/ (or (task-complete task) 0) 100.0)))
                   (incf total-ev (* planned-cost complete-pct)))))
             (project-tasks project))
    total-ev))

(defun calculate-cost-variance (project)
  "Calculate Cost Variance (CV = EV - AC).

   Positive CV = under budget
   Negative CV = over budget"
  (let ((ev (calculate-earned-value-cost project))
        (ac (calculate-actual-cost project)))
    (- ev ac)))

(defun calculate-cpi-cost (project)
  "Calculate Cost Performance Index (CPI = EV / AC).

   CPI > 1.0 = under budget (good)
   CPI = 1.0 = on budget
   CPI < 1.0 = over budget (bad)"
  (let ((ev (calculate-earned-value-cost project))
        (ac (calculate-actual-cost project)))
    (if (zerop ac)
        1.0  ; No actual cost yet, assume on budget
        (/ ev ac))))

(defun calculate-bac (project)
  "Calculate Budget at Completion (BAC).

   BAC = total planned cost for the project."
  (calculate-project-planned-cost project))

(defun calculate-eac (project)
  "Calculate Estimate at Completion (EAC = BAC / CPI).

   Forecasts total project cost based on current performance."
  (let ((bac (calculate-bac project))
        (cpi (calculate-cpi-cost project)))
    (if (zerop cpi)
        bac  ; Avoid division by zero
        (/ bac cpi))))

(defun calculate-etc (project)
  "Calculate Estimate to Complete (ETC = EAC - AC).

   Forecasts cost to complete remaining work."
  (let ((eac (calculate-eac project))
        (ac (calculate-actual-cost project)))
    (- eac ac)))

(defun calculate-vac (project)
  "Calculate Variance at Completion (VAC = BAC - EAC).

   Positive VAC = expected under budget
   Negative VAC = expected over budget"
  (let ((bac (calculate-bac project))
        (eac (calculate-eac project)))
    (- bac eac)))

;;; ============================================================================
;;; Cost Reports
;;; ============================================================================

(defun generate-cost-report (project &key (by :task))
  "Generate a cost report for the project.

   :by :task - Group costs by task
   :by :resource - Group costs by resource

   Returns a list of plists with cost information."
  (ecase by
    (:task (generate-cost-report-by-task project))
    (:resource (generate-cost-report-by-resource project))))

(defun generate-cost-report-by-task (project)
  "Generate cost report grouped by task."
  (let ((report '()))
    (maphash (lambda (id task)
               (unless (task-subtasks task)  ; Leaf tasks only
                 (push (list :task-id id
                             :task-name (task-name task)
                             :planned-cost (calculate-task-planned-cost task)
                             :actual-cost (calculate-task-actual-cost task)
                             :variance (- (calculate-task-planned-cost task)
                                         (calculate-task-actual-cost task)))
                       report)))
             (project-tasks project))
    (nreverse report)))

(defun generate-cost-report-by-resource (project)
  "Generate cost report grouped by resource."
  (let ((resource-costs (make-hash-table :test 'eq)))
    ;; Initialize all resources
    (maphash (lambda (id resource)
               (setf (gethash id resource-costs)
                     (list :resource-id id
                           :resource-name (resource-name resource)
                           :rate (resource-rate resource)
                           :planned-cost 0.0
                           :actual-cost 0.0)))
             (project-resources project))

    ;; Accumulate planned costs from allocations
    (maphash (lambda (id task)
               (declare (ignore id))
               (unless (task-subtasks task)
                 (let* ((allocations (task-allocations task))
                        (resources (loop for alloc in allocations
                                         append (allocation-resources alloc)))
                        (num-resources (max 1 (length resources))))
                   (dolist (resource resources)
                     (let* ((res-id (resource-id resource))
                            (entry (gethash res-id resource-costs))
                            (task-cost (calculate-task-planned-cost task))
                            (resource-share (/ task-cost num-resources)))
                       (when entry
                         (incf (getf entry :planned-cost) resource-share)))))))
             (project-tasks project))

    ;; Accumulate actual costs from bookings
    (maphash (lambda (id task)
               (declare (ignore id))
               (dolist (booking (task-bookings task))
                 (let* ((resource (booking-resource booking))
                        (res-id (resource-id resource))
                        (entry (gethash res-id resource-costs))
                        (hours (booking-duration-hours booking))
                        (hourly-rate (/ (or (resource-rate resource) 0.0) 8.0))
                        (booking-cost (* hours hourly-rate)))
                   (when entry
                     (incf (getf entry :actual-cost) booking-cost)))))
             (project-tasks project))

    ;; Convert to list
    (let ((report '()))
      (maphash (lambda (id entry)
                 (declare (ignore id))
                 (setf (getf entry :variance)
                       (- (getf entry :planned-cost) (getf entry :actual-cost)))
                 (push entry report))
               resource-costs)
      report)))
