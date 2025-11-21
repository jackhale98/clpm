;;;; examples/effort-scheduling.lisp
;;;; Example: Effort-Based Scheduling
;;;;
;;;; This example demonstrates:
;;;; - Effort-based vs duration-based tasks
;;;; - Resource efficiency affecting task duration
;;;; - How multiple resources reduce task duration

;;; Load the project-juggler system
(require :asdf)
(push (truename "../") asdf:*central-registry*)

;; Load quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load dependencies and project-juggler
(ql:quickload :project-juggler :silent t)

(in-package :project-juggler)

;;; =============================================================================
;;; PROJECT DEFINITION
;;; =============================================================================

(defproject effort-demo "Effort-Based Scheduling Demo"
  :start (date 2024 6 1)
  :end (date 2024 9 30)

  ;;; Resources with different efficiency levels
  (defresource senior-dev "Senior Developer"
    :efficiency 1.5    ; 50% more productive
    :rate 150.0)

  (defresource mid-dev "Mid-level Developer"
    :efficiency 1.0    ; Baseline productivity
    :rate 100.0)

  (defresource junior-dev "Junior Developer"
    :efficiency 0.6    ; 60% as productive as baseline
    :rate 70.0)

  (defresource designer "Designer"
    :efficiency 1.0
    :rate 90.0)

  ;;; Duration-based tasks (not affected by resource efficiency)
  (deftask meeting "Kickoff Meeting"
    :duration (duration 1 :days)
    :allocate (senior-dev mid-dev junior-dev designer)
    :priority 1000)

  ;;; Effort-based tasks - duration varies by resource efficiency

  ;; Task 1: 10 days effort with senior dev (efficiency 1.5)
  ;; Actual duration: 10 / 1.5 = 7 days
  (deftask task1 "Feature A - Senior Developer"
    :effort (duration 10 :days)
    :depends-on (meeting)
    :allocate (senior-dev)
    :priority 900)

  ;; Task 2: 10 days effort with mid-level dev (efficiency 1.0)
  ;; Actual duration: 10 / 1.0 = 10 days
  (deftask task2 "Feature B - Mid-level Developer"
    :effort (duration 10 :days)
    :depends-on (meeting)
    :allocate (mid-dev)
    :priority 900)

  ;; Task 3: 10 days effort with junior dev (efficiency 0.6)
  ;; Actual duration: 10 / 0.6 = 17 days
  (deftask task3 "Feature C - Junior Developer"
    :effort (duration 10 :days)
    :depends-on (meeting)
    :allocate (junior-dev)
    :priority 900)

  ;; Task 4: 20 days effort with TWO developers (efficiencies: 1.5 + 1.0 = 2.5)
  ;; Actual duration: 20 / 2.5 = 8 days
  (deftask task4 "Feature D - Pair Programming"
    :effort (duration 20 :days)
    :depends-on (meeting)
    :allocate (senior-dev mid-dev)
    :priority 900)

  ;; Task 5: 15 days effort with ALL THREE developers (efficiencies: 1.5 + 1.0 + 0.6 = 3.1)
  ;; Actual duration: 15 / 3.1 = 5 days
  (deftask task5 "Integration - Full Team"
    :effort (duration 15 :days)
    :depends-on (task1 task2 task3 task4)
    :allocate (senior-dev mid-dev junior-dev)
    :priority 950)

  ;; Final milestone
  (deftask launch "Project Launch"
    :milestone t
    :depends-on (task5))

  ;;; Reports
  (defreport schedule-comparison "Effort vs Duration Comparison"
    :type :task
    :format :html
    :columns (:id :name :effort :start :end)))

;;; =============================================================================
;;; ANALYSIS AND REPORTING
;;; =============================================================================

(format t "~%╔══════════════════════════════════════════════════════════╗~%")
(format t "║     Effort-Based Scheduling Demonstration               ║~%")
(format t "╚══════════════════════════════════════════════════════════╝~%~%")

;; Finalize and schedule
(format t "▶ Finalizing project...~%")
(finalize-project *current-project*)

(format t "▶ Scheduling tasks...~%")
(schedule *current-project*)

(format t "✓ Project scheduled successfully!~%~%")

;; Display comparison
(format t "──────────────────────────────────────────────────────────~%")
(format t "EFFORT-BASED SCHEDULING COMPARISON~%")
(format t "──────────────────────────────────────────────────────────~%~%")

(format t "All effort-based tasks have 10 days of effort,~%")
(format t "but actual duration varies by resource efficiency:~%~%")

(labels ((show-task (id description)
           (let* ((task (gethash id (project-tasks *current-project*)))
                  (effort (task-effort task))
                  (duration-days (if effort
                                     (duration-in-days
                                      (calculate-duration-from-effort task))
                                     (duration-in-days (task-duration task))))
                  (allocations (task-allocations task))
                  (resources (loop for alloc in allocations
                                   append (allocation-resources alloc)))
                  (total-eff (reduce #'+ resources
                                    :key #'resource-efficiency
                                    :initial-value 0.0)))
             (format t "~A:~%" description)
             (format t "  Resources: ~{~A~^, ~}~%"
                     (mapcar (lambda (r)
                              (format nil "~A (eff: ~,1F)"
                                     (resource-name r)
                                     (resource-efficiency r)))
                            resources))
             (format t "  Total Efficiency: ~,1F~%" total-eff)
             (when effort
               (format t "  Effort: ~A days~%" (duration-in-days effort)))
             (format t "  Actual Duration: ~A days~%~%" duration-days))))

  (show-task 'task1 "Feature A - Senior Developer")
  (show-task 'task2 "Feature B - Mid-level Developer")
  (show-task 'task3 "Feature C - Junior Developer")
  (show-task 'task4 "Feature D - Pair Programming")
  (show-task 'task5 "Integration - Full Team"))

(format t "──────────────────────────────────────────────────────────~%")
(format t "KEY INSIGHTS~%")
(format t "──────────────────────────────────────────────────────────~%~%")

(format t "1. Senior developer (efficiency 1.5) completes 10 days effort in 7 days~%")
(format t "2. Mid-level developer (efficiency 1.0) completes 10 days effort in 10 days~%")
(format t "3. Junior developer (efficiency 0.6) completes 10 days effort in 17 days~%")
(format t "4. Two developers working together combine their efficiencies~%")
(format t "5. Duration-based tasks (meetings) are not affected by efficiency~%~%")

;; Generate report
(format t "──────────────────────────────────────────────────────────~%")
(format t "GENERATING REPORT~%")
(format t "──────────────────────────────────────────────────────────~%")

(save-project-report *current-project* 'schedule-comparison "effort-scheduling-report.html")
(format t "✓ Report: effort-scheduling-report.html~%~%")

(format t "──────────────────────────────────────────────────────────~%")
(format t "✓ EXAMPLE COMPLETE!~%")
(format t "──────────────────────────────────────────────────────────~%")
(format t "~%This example demonstrates how Project Juggler handles~%")
(format t "effort-based scheduling with varying resource efficiency.~%~%")

(format t "Try experimenting with:~%")
(format t "• Changing resource efficiency values~%")
(format t "• Adding more resources to tasks~%")
(format t "• Comparing effort vs duration for your use case~%~%")

(format t "══════════════════════════════════════════════════════════~%~%")
