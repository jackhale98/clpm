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
;;; Helper Functions
;;; ============================================================================
;;; Note: Resource over-allocation detection and leveling functions are in
;;; src/scheduling/resource-allocation.lisp

(defun date-difference-days (date1 date2)
  "Calculate difference between two dates in days"
  (let ((diff (local-time:timestamp-difference
               (date-timestamp date1)
               (date-timestamp date2))))
    (abs (round (/ diff 86400)))))
