;;;; src/tracking/evm.lisp
;;;; Earned Value Management calculations
;;;;
;;;; Uses TaskJuggler-style scenarios where the first scenario is the baseline.

(in-package #:project-juggler)

;;; ============================================================================
;;; Planned Value (PV)
;;; ============================================================================

(defun calculate-planned-value (project status-date &key scenario)
  "Calculate Planned Value at the given status date.

   PV is the percentage of work that should be complete by the status date
   according to the baseline schedule.

   SCENARIO - optional scenario ID. If not provided, uses baseline (first scenario)."

  (let* ((scenario-id (or scenario (baseline-scenario-id project)))
         (total-tasks 0)
         (total-pv 0.0))
    (unless scenario-id
      (error "Project has no scenarios"))

    (maphash (lambda (task-id task)
               (declare (ignore task-id))
               (incf total-tasks)
               (let ((start (task-start-for-scenario task scenario-id))
                     (end (task-end-for-scenario task scenario-id)))
                 (when (and start end)
                   (incf total-pv (calculate-task-pv start end status-date)))))
             (project-tasks project))

    (if (zerop total-tasks)
        0.0
        (/ total-pv total-tasks))))

(defun calculate-task-pv (start end status-date)
  "Calculate planned value for a single task given its start/end dates."
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
         (max 0.0 (min 100.0 percent))))))

;;; ============================================================================
;;; Earned Value (EV)
;;; ============================================================================

(defun calculate-earned-value (project &key scenario)
  "Calculate Earned Value - percentage of work actually complete.

   SCENARIO - optional scenario ID. If nil, uses current task completion values."
  (let ((scenario-id (or scenario (baseline-scenario-id project)))
        (total-tasks 0)
        (total-ev 0.0))
    (maphash (lambda (task-id task)
               (declare (ignore task-id))
               (incf total-tasks)
               (incf total-ev (task-complete-for-scenario task scenario-id)))
             (project-tasks project))

    (if (zerop total-tasks)
        0.0
        (/ total-ev total-tasks))))

;;; ============================================================================
;;; Schedule Variance (SV) and Schedule Performance Index (SPI)
;;; ============================================================================

(defun calculate-schedule-variance (project status-date &key scenario)
  "Calculate Schedule Variance (SV) = EV - PV

   Positive = ahead of schedule
   Negative = behind schedule

   SCENARIO - optional scenario ID for PV calculation."

  (let ((ev (calculate-earned-value project :scenario scenario))
        (pv (calculate-planned-value project status-date :scenario scenario)))
    (- ev pv)))

(defun calculate-spi (project status-date &key scenario)
  "Calculate Schedule Performance Index (SPI) = EV / PV

   > 1.0 = ahead of schedule
   < 1.0 = behind schedule
   = 1.0 = on schedule

   SCENARIO - optional scenario ID for PV calculation."

  (let ((ev (calculate-earned-value project :scenario scenario))
        (pv (calculate-planned-value project status-date :scenario scenario)))
    (if (zerop pv)
        1.0  ; No work planned yet
        (/ ev pv))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun date-difference-days (date1 date2)
  "Calculate difference between two dates in days"
  (let ((diff (local-time:timestamp-difference
               (date-timestamp date1)
               (date-timestamp date2))))
    (abs (round (/ diff 86400)))))
