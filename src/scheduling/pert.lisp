;;;; src/scheduling/pert.lisp
;;;; PERT three-point estimation calculations

(in-package #:project-juggler)

;;; ============================================================================
;;; Basic PERT Calculations
;;; ============================================================================

(defun pert-expected-duration (task)
  "Calculate PERT expected duration for a task.
   Formula: E = (O + 4M + P) / 6
   Returns duration in days."
  (let ((estimate (task-estimate task)))
    (if estimate
        (let ((o (duration-in-days (estimate-optimistic estimate)))
              (m (duration-in-days (estimate-likely estimate)))
              (p (duration-in-days (estimate-pessimistic estimate))))
          (/ (+ o (* 4 m) p) 6.0))
        ;; If no estimate, return effort or duration
        (or (when (task-effort task)
              (duration-in-days (task-effort task)))
            (when (task-duration task)
              (duration-in-days (task-duration task)))
            0.0))))

(defun pert-standard-deviation (task)
  "Calculate PERT standard deviation for a task.
   Formula: σ = (P - O) / 6
   Returns standard deviation in days."
  (let ((estimate (task-estimate task)))
    (if estimate
        (let ((o (duration-in-days (estimate-optimistic estimate)))
              (p (duration-in-days (estimate-pessimistic estimate))))
          (/ (- p o) 6.0))
        0.0)))

(defun pert-variance (task)
  "Calculate PERT variance for a task.
   Formula: σ² = ((P - O) / 6)²
   Returns variance in days²."
  (let ((sd (pert-standard-deviation task)))
    (* sd sd)))

;;; ============================================================================
;;; Confidence Intervals
;;; ============================================================================

(defun pert-confidence-interval (task confidence-level)
  "Calculate confidence interval for task duration.

   confidence-level: 68 (1σ), 95 (2σ), or 99 (3σ)
   Returns (min max) in days."
  (let* ((expected (pert-expected-duration task))
         (sd (pert-standard-deviation task))
         (multiplier (case confidence-level
                       (68 1.0)
                       (95 2.0)
                       (99 3.0)
                       (t (/ confidence-level 34.0)))))  ; Approximate
    (list (- expected (* sd multiplier))
          (+ expected (* sd multiplier)))))

;;; ============================================================================
;;; Project-Level PERT
;;; ============================================================================

(defun project-pert-expected-duration (project)
  "Calculate total expected duration for the project's critical path.
   Sums expected durations of all tasks on the critical path."
  (let ((critical-tasks (critical-path project)))
    (if critical-tasks
        (reduce #'+ critical-tasks :key #'pert-expected-duration :initial-value 0.0)
        ;; If no critical path, sum all leaf tasks
        (let ((total 0.0))
          (maphash (lambda (id task)
                     (declare (ignore id))
                     (unless (task-subtasks task)
                       (incf total (pert-expected-duration task))))
                   (project-tasks project))
          total))))

(defun project-pert-variance (project)
  "Calculate total variance for the project's critical path.
   Sums variances of all tasks on the critical path."
  (let ((critical-tasks (critical-path project)))
    (if critical-tasks
        (reduce #'+ critical-tasks :key #'pert-variance :initial-value 0.0)
        ;; If no critical path calculated, sum all leaf tasks
        (let ((total 0.0))
          (maphash (lambda (id task)
                     (declare (ignore id))
                     (unless (task-subtasks task)
                       (incf total (pert-variance task))))
                   (project-tasks project))
          total))))

(defun project-pert-standard-deviation (project)
  "Calculate standard deviation for the project's critical path.
   Formula: σ_project = √(Σσ²)"
  (sqrt (project-pert-variance project)))

(defun project-pert-confidence-interval (project confidence-level)
  "Calculate confidence interval for project completion.
   Returns (min max) in days."
  (let* ((expected (project-pert-expected-duration project))
         (sd (project-pert-standard-deviation project))
         (multiplier (case confidence-level
                       (68 1.0)
                       (95 2.0)
                       (99 3.0)
                       (t (/ confidence-level 34.0)))))
    (list (- expected (* sd multiplier))
          (+ expected (* sd multiplier)))))

;;; ============================================================================
;;; Probability Calculations
;;; ============================================================================

(defun standard-normal-cdf (z)
  "Approximate cumulative distribution function for standard normal distribution.
   Uses the error function approximation."
  (let ((a1  0.254829592)
        (a2 -0.284496736)
        (a3  1.421413741)
        (a4 -1.453152027)
        (a5  1.061405429)
        (p   0.3275911))
    (let* ((sign (if (< z 0) -1 1))
           (x (abs z))
           (t-val (/ 1.0 (+ 1.0 (* p x))))
           (y (- 1.0 (* (+ (* a1 t-val)
                          (* a2 (* t-val t-val))
                          (* a3 (* t-val t-val t-val))
                          (* a4 (* t-val t-val t-val t-val))
                          (* a5 (* t-val t-val t-val t-val t-val)))
                       (exp (- (* x x)))))))
      (* 0.5 (+ 1.0 (* sign y))))))

(defun probability-of-completion-by (task target-days)
  "Calculate probability that a task completes by target duration.
   Uses Z-score and standard normal distribution.
   Returns probability as decimal (0.0 to 1.0)."
  (let ((expected (pert-expected-duration task))
        (sd (pert-standard-deviation task)))
    (if (zerop sd)
        (if (<= expected target-days) 1.0 0.0)
        (let ((z (/ (- target-days expected) sd)))
          (standard-normal-cdf z)))))

(defun project-probability-of-completion-by (project target-days)
  "Calculate probability that the project completes by target duration."
  (let ((expected (project-pert-expected-duration project))
        (sd (project-pert-standard-deviation project)))
    (if (zerop sd)
        (if (<= expected target-days) 1.0 0.0)
        (let ((z (/ (- target-days expected) sd)))
          (standard-normal-cdf z)))))

;;; ============================================================================
;;; PERT-Aware Scheduling
;;; ============================================================================

(defun pert-duration-for-scheduling (task)
  "Get the duration to use for scheduling a PERT-estimated task.
   Uses ceiling of expected duration to be conservative."
  (let ((estimate (task-estimate task)))
    (if estimate
        (duration (ceiling (pert-expected-duration task)) :days)
        nil)))
