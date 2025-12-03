;;;; src/risk/simulation.lisp
;;;; Monte Carlo PERT simulation for project schedule risk analysis

(in-package #:project-juggler)

;;; ============================================================================
;;; Simulation Trial Class
;;; ============================================================================

(defclass simulation-trial ()
  ((trial-number :initarg :trial-number :reader trial-number)
   (project-duration :initarg :project-duration :accessor trial-project-duration)
   (task-durations :initarg :task-durations :reader trial-task-durations
                   :documentation "Hash table: task-id -> sampled duration in days")
   (task-end-dates :initarg :task-end-dates :reader trial-task-end-dates
                   :documentation "Hash table: task-id -> end date")
   (critical-path :initarg :critical-path :initform nil :accessor trial-critical-path)
   (end-date :initarg :end-date :accessor trial-end-date)
   (risk-occurrences :initarg :risk-occurrences :initform nil :accessor trial-risk-occurrences
                     :documentation "List of risk IDs that occurred in this trial"))
  (:documentation "Results from a single simulation trial"))

;;; ============================================================================
;;; Simulation Results Class
;;; ============================================================================

(defclass simulation-results ()
  ((project :initarg :project :reader simulation-project)
   (trials :initarg :trials :reader simulation-trials
           :documentation "List of simulation-trial objects")
   (trial-count :initarg :trial-count :reader simulation-trial-count)
   (durations :initarg :durations :reader simulation-durations
              :documentation "Sorted list of project durations")
   (mean :initarg :mean :reader simulation-mean)
   (std-dev :initarg :std-dev :reader simulation-std-dev)
   (min-duration :initarg :min :reader simulation-min)
   (max-duration :initarg :max :reader simulation-max)
   (risk-occurrence-counts :initarg :risk-occurrence-counts :initform nil
                           :reader simulation-risk-occurrence-counts
                           :documentation "Hash table: risk-id -> count of occurrences"))
  (:documentation "Aggregate results from Monte Carlo simulation"))

;;; ============================================================================
;;; Random Number Generation
;;; ============================================================================

(defun random-uniform ()
  "Generate a uniform random number in [0, 1)."
  (random 1.0))

;;; ============================================================================
;;; PERT Distribution Sampling
;;; ============================================================================

(defun sample-triangular (a m b)
  "Sample from a triangular distribution with min=a, mode=m, max=b.
   Uses inverse transform sampling."
  (let* ((u (random-uniform))
         (fc (/ (- m a) (- b a))))
    (if (< u fc)
        ;; Left side of triangle
        (+ a (sqrt (* u (- b a) (- m a))))
        ;; Right side of triangle
        (- b (sqrt (* (- 1.0 u) (- b a) (- b m)))))))

(defun sample-beta-pert (a m b &optional (lambda-param 4.0))
  "Sample from a Beta-PERT distribution.
   a = optimistic (minimum)
   m = most likely (mode)
   b = pessimistic (maximum)
   lambda-param = shape parameter (default 4 for standard PERT)

   Uses the relationship between PERT and Beta distribution."
  (when (= a b)
    (return-from sample-beta-pert a))
  (when (= a m b)
    (return-from sample-beta-pert a))
  (let* ((range (- b a)))
    (when (zerop range)
      (return-from sample-beta-pert a))
    (let* (;; Calculate Beta shape parameters using standard PERT formula
           (mu (/ (+ a (* lambda-param m) b) (+ lambda-param 2.0)))
           (denom-alpha (* (- m mu) range))
           (denom-beta (- mu a)))
      ;; Check for edge cases that could cause division by zero
      (when (or (zerop denom-alpha) (zerop denom-beta) (= m mu))
        (return-from sample-beta-pert (sample-triangular a m b)))
      (let* ((alpha (/ (* (- mu a) (+ (* 2.0 m) (- a) (- b))) denom-alpha))
             (beta (/ (* alpha (- b mu)) denom-beta)))
        ;; Ensure valid shape parameters
        (when (or (<= alpha 0.1) (<= beta 0.1) (> alpha 100) (> beta 100))
          ;; Fall back to triangular if Beta params are problematic
          (return-from sample-beta-pert (sample-triangular a m b)))
        ;; Sample from Beta distribution and scale to [a, b]
        (let ((y (sample-beta alpha beta)))
          (+ a (* y range)))))))

(defun sample-beta (alpha beta)
  "Sample from Beta(alpha, beta) distribution using gamma sampling."
  (let ((x (sample-gamma alpha))
        (y (sample-gamma beta)))
    (if (zerop (+ x y))
        0.5  ; Fallback for edge case
        (/ x (+ x y)))))

(defun sample-gamma (shape)
  "Sample from Gamma(shape, 1) distribution.
   Uses Marsaglia and Tsang's method for shape >= 1,
   and rejection method for shape < 1."
  (when (<= shape 0)
    (return-from sample-gamma 0.0))
  (if (< shape 1.0)
      ;; For shape < 1, use: Gamma(shape) = Gamma(shape+1) * U^(1/shape)
      (let ((u (max 1e-10 (random-uniform))))  ; Avoid 0^(1/shape) issues
        (* (sample-gamma (+ shape 1.0))
           (expt u (/ 1.0 shape))))
      ;; Marsaglia and Tsang's method for shape >= 1
      (let* ((d (- shape (/ 1.0 3.0)))
             (c (/ 1.0 (sqrt (max 1e-10 (* 9.0 d))))))  ; Avoid sqrt(0)
        (loop for attempts from 0 below 1000 do  ; Limit iterations
          (let* ((x (sample-standard-normal))
                 (v-base (+ 1.0 (* c x)))
                 (v (if (> v-base 0) (expt v-base 3) 0)))
            (when (> v 0)
              (let ((u (max 1e-10 (random-uniform))))
                (when (< (log u)
                         (+ (* 0.5 x x)
                            (- d)
                            (* d v)
                            (- (* d (log (max 1e-10 v))))))
                  (return-from sample-gamma (* d v))))))
          finally (return-from sample-gamma shape)))))

(defun sample-standard-normal ()
  "Sample from standard normal distribution using Box-Muller transform."
  (let ((u1 (max 1e-10 (random-uniform)))  ; Avoid log(0)
        (u2 (random-uniform)))
    (* (sqrt (* -2.0 (log u1)))
       (cos (* 2.0 pi u2)))))

(defun sample-pert-duration (task)
  "Sample a random duration for a task based on its PERT estimate.
   If no PERT estimate, returns the fixed duration/effort.
   Returns duration in days."
  (let ((estimate (task-estimate task)))
    (if estimate
        (let ((o (duration-in-days (estimate-optimistic estimate)))
              (m (duration-in-days (estimate-likely estimate)))
              (p (duration-in-days (estimate-pessimistic estimate))))
          ;; Use Beta-PERT distribution for more realistic sampling
          (sample-beta-pert o m p))
        ;; No PERT estimate - return fixed duration
        (or (when (task-duration task)
              (duration-in-days (task-duration task)))
            (when (task-effort task)
              (duration-in-days (task-effort task)))
            0.0))))

;;; ============================================================================
;;; Risk Sampling
;;; ============================================================================

(defun sample-risk-occurrence (risk)
  "Determine if a risk occurs in this trial.
   Returns T if risk occurs, NIL otherwise."
  (< (random-uniform) (risk-probability risk)))

(defun calculate-risk-schedule-impact (risk project sampled-durations)
  "Calculate the schedule impact if a risk occurs.
   Returns additional days to add to affected tasks."
  (let ((impact-days 0.0))
    (dolist (task-id (risk-tasks risk))
      (let ((base-duration (or (gethash task-id sampled-durations)
                               0.0)))
        (incf impact-days (* base-duration (risk-schedule-impact risk)))))
    impact-days))

;;; ============================================================================
;;; Single Simulation Trial
;;; ============================================================================

(defun run-simulation-trial (project trial-number &key include-risks)
  "Run a single Monte Carlo simulation trial.
   Returns a simulation-trial object with sampled durations and results."
  (let ((task-durations (make-hash-table :test 'eq))
        (task-end-dates (make-hash-table :test 'eq))
        (task-start-dates (make-hash-table :test 'eq))
        (project-start (project-start project))
        (risk-occurrences nil))

    ;; Sample durations for all tasks
    (maphash (lambda (id task)
               (setf (gethash id task-durations)
                     (sample-pert-duration task)))
             (project-tasks project))

    ;; Apply risk impacts if enabled
    (when include-risks
      (dolist (risk (project-risk-register project))
        (when (sample-risk-occurrence risk)
          (push (risk-id risk) risk-occurrences)
          ;; Add schedule impact to affected tasks
          (dolist (task-id (risk-tasks risk))
            (let* ((base-duration (gethash task-id task-durations))
                   (impact (* base-duration (risk-schedule-impact risk))))
              (when base-duration
                (incf (gethash task-id task-durations) impact)))))))

    ;; Schedule tasks in dependency order
    (let ((sorted-tasks (topological-sort-tasks project)))
      (dolist (task sorted-tasks)
        (let* ((task-id (task-id task))
               (dur-days (or (gethash task-id task-durations) 0.0))
               (earliest-start (calculate-trial-earliest-start
                                task task-end-dates project-start)))
          (setf (gethash task-id task-start-dates) earliest-start)
          ;; Use ceiling to convert float days to integer for duration
          (setf (gethash task-id task-end-dates)
                (date+ earliest-start (duration (ceiling dur-days) :days))))))

    ;; Find project end date (latest task end)
    (let ((project-end project-start))
      (maphash (lambda (id end-date)
                 (declare (ignore id))
                 (when (date> end-date project-end)
                   (setf project-end end-date)))
               task-end-dates)

      ;; Calculate project duration in days
      (let ((project-duration
              (/ (- (local-time:timestamp-to-unix (date-timestamp project-end))
                    (local-time:timestamp-to-unix (date-timestamp project-start)))
                 86400.0)))

        (make-instance 'simulation-trial
                      :trial-number trial-number
                      :project-duration project-duration
                      :task-durations task-durations
                      :task-end-dates task-end-dates
                      :end-date project-end
                      :risk-occurrences risk-occurrences)))))

(defun calculate-trial-earliest-start (task task-end-dates project-start)
  "Calculate earliest start for a task based on predecessor end dates."
  (let ((earliest project-start))
    (dolist (dep (task-dependencies task))
      (let* ((pred-id (dependency-target-ref dep))
             (pred-end (gethash pred-id task-end-dates)))
        (when (and pred-end (date> pred-end earliest))
          (setf earliest pred-end))))
    earliest))

(defun topological-sort-tasks (project)
  "Sort tasks in dependency order for simulation."
  (let ((sorted nil)
        (visited (make-hash-table :test 'eq)))
    (labels ((visit (task)
               (unless (gethash (task-id task) visited)
                 ;; Visit dependencies first
                 (dolist (dep (task-dependencies task))
                   (let ((pred (gethash (dependency-target-ref dep)
                                       (project-tasks project))))
                     (when pred
                       (visit pred))))
                 (setf (gethash (task-id task) visited) t)
                 (push task sorted))))
      (maphash (lambda (id task)
                 (declare (ignore id))
                 (visit task))
               (project-tasks project)))
    (nreverse sorted)))

;;; ============================================================================
;;; Monte Carlo Simulation
;;; ============================================================================

(defun run-monte-carlo-simulation (project &key (trials 1000))
  "Run Monte Carlo simulation with specified number of trials.
   Returns a simulation-results object with statistics."
  (let ((trial-results nil)
        (durations nil))

    ;; Run all trials
    (dotimes (i trials)
      (let ((trial (run-simulation-trial project (1+ i))))
        (push trial trial-results)
        (push (trial-project-duration trial) durations)))

    ;; Sort durations for percentile calculations
    (setf durations (sort durations #'<))

    ;; Calculate statistics
    (let* ((n (length durations))
           (sum (reduce #'+ durations))
           (mean (/ sum n))
           (variance (/ (reduce #'+ (mapcar (lambda (d) (expt (- d mean) 2)) durations))
                        n))
           (std-dev (sqrt variance))
           (min-dur (first durations))
           (max-dur (car (last durations))))

      (make-instance 'simulation-results
                    :project project
                    :trials (nreverse trial-results)
                    :trial-count n
                    :durations durations
                    :mean mean
                    :std-dev std-dev
                    :min min-dur
                    :max max-dur))))

(defun run-risk-simulation (project &key (trials 1000))
  "Run Monte Carlo simulation with risk integration.
   Returns simulation-results with risk occurrence tracking."
  (let ((trial-results nil)
        (durations nil)
        (risk-counts (make-hash-table :test 'eq)))

    ;; Initialize risk counts
    (dolist (risk (project-risk-register project))
      (setf (gethash (risk-id risk) risk-counts) 0))

    ;; Run all trials with risks
    (dotimes (i trials)
      (let ((trial (run-simulation-trial project (1+ i) :include-risks t)))
        (push trial trial-results)
        (push (trial-project-duration trial) durations)
        ;; Count risk occurrences
        (dolist (risk-id (trial-risk-occurrences trial))
          (incf (gethash risk-id risk-counts 0)))))

    ;; Sort durations
    (setf durations (sort durations #'<))

    ;; Calculate statistics
    (let* ((n (length durations))
           (sum (reduce #'+ durations))
           (mean (/ sum n))
           (variance (/ (reduce #'+ (mapcar (lambda (d) (expt (- d mean) 2)) durations))
                        n))
           (std-dev (sqrt variance))
           (min-dur (first durations))
           (max-dur (car (last durations))))

      (make-instance 'simulation-results
                    :project project
                    :trials (nreverse trial-results)
                    :trial-count n
                    :durations durations
                    :mean mean
                    :std-dev std-dev
                    :min min-dur
                    :max max-dur
                    :risk-occurrence-counts risk-counts))))

;;; ============================================================================
;;; Statistics Functions
;;; ============================================================================

(defun simulation-percentile (results percentile)
  "Get the specified percentile from simulation results.
   Percentile should be 0-100 (e.g., 90 for P90)."
  (let* ((durations (simulation-durations results))
         (n (length durations))
         (index (min (1- n)
                     (max 0
                          (floor (* n (/ percentile 100.0)))))))
    (nth index durations)))

(defun simulation-probability-of-completion (results target-days)
  "Calculate probability of completing within target days.
   Returns probability as decimal (0.0 to 1.0)."
  (let* ((durations (simulation-durations results))
         (count-below (count-if (lambda (d) (<= d target-days)) durations)))
    (/ count-below (float (length durations)))))

(defun simulation-histogram (results &key (bins 10))
  "Generate histogram data from simulation results.
   Returns list of (bin-min bin-max count) triplets."
  (let* ((durations (simulation-durations results))
         (min-dur (simulation-min results))
         (max-dur (simulation-max results))
         (range (- max-dur min-dur))
         (bin-width (if (zerop range) 1.0 (/ range bins)))
         (histogram nil))

    ;; Create bins
    (dotimes (i bins)
      (let* ((bin-min (+ min-dur (* i bin-width)))
             (bin-max (+ min-dur (* (1+ i) bin-width)))
             (count (count-if (lambda (d)
                               (and (>= d bin-min)
                                    (if (= i (1- bins))
                                        (<= d bin-max)  ; Include max in last bin
                                        (< d bin-max))))
                             durations)))
        (push (list bin-min bin-max count) histogram)))

    (nreverse histogram)))

(defun simulation-risk-occurrences (results risk-id)
  "Get the number of times a risk occurred across all trials."
  (let ((counts (simulation-risk-occurrence-counts results)))
    (if counts
        (gethash risk-id counts 0)
        0)))

;;; ============================================================================
;;; Summary Report
;;; ============================================================================

(defun simulation-summary (results)
  "Generate a summary plist of simulation results."
  (list :trial-count (simulation-trial-count results)
        :mean (simulation-mean results)
        :std-dev (simulation-std-dev results)
        :min (simulation-min results)
        :max (simulation-max results)
        :p10 (simulation-percentile results 10)
        :p50 (simulation-percentile results 50)
        :p75 (simulation-percentile results 75)
        :p90 (simulation-percentile results 90)
        :p95 (simulation-percentile results 95)))

