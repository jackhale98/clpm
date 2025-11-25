;;;; project-template.lisp
;;;; Template for creating your own Project Juggler projects
;;;;
;;;; Copy this file to your own directory and customize it!
;;;; Example: cp templates/project-template.lisp ~/my-projects/my-project.lisp

;;; ============================================================================
;;; SETUP - Load Project Juggler
;;; ============================================================================

;; Load the library (works from any directory!)
;; Note: You must install Project Juggler first:
;;   cd ~/quicklisp/local-projects/
;;   git clone https://github.com/yourusername/project-juggler.git

(handler-case
    (ql:quickload :project-juggler :silent t)
  (error (e)
    (format t "~%ERROR: Could not load project-juggler.~%")
    (format t "~%Make sure it's installed in ~/quicklisp/local-projects/~%")
    (format t "~%Installation:~%")
    (format t "  cd ~/quicklisp/local-projects/~%")
    (format t "  git clone https://github.com/yourusername/project-juggler.git~%")
    (format t "~%Or symlink your development copy:~%")
    (format t "  ln -s /path/to/project-juggler ~/quicklisp/local-projects/~%~%")
    (format t "Error details: ~A~%~%" e)
    (uiop:quit 1)))

(in-package :project-juggler)

;;; ============================================================================
;;; OPTIONAL: Define Working Calendar
;;; ============================================================================

(defvar *my-calendar*
  (let ((wh (make-instance 'working-hours
                          :days '(:monday :tuesday :wednesday :thursday :friday)
                          :start-time "09:00"
                          :end-time "17:00"))
        (cal (make-instance 'calendar
                           :id 'my-calendar
                           :name "My Company Calendar"
                           :working-hours wh
                           :timezone :utc)))
    ;; Add your company holidays
    (add-holiday cal (date 2024 12 25) "Christmas")
    (add-holiday cal (date 2024 1 1) "New Year's Day")
    ;; Add more holidays as needed
    cal))

;;; ============================================================================
;;; PROJECT DEFINITION
;;; ============================================================================

(defproject my-project "My Project Name"
  :start (date 2024 1 1)
  :end (date 2024 12 31)

  ;;; ---------------------------------------------------------------------------
  ;;; Resources
  ;;; ---------------------------------------------------------------------------

  (defresource person1 "Team Member 1"
    :efficiency 1.0
    :rate 100.0)

  (defresource person2 "Team Member 2"
    :efficiency 1.2  ; 20% more productive
    :rate 120.0)

  ;; Add more resources as needed

  ;;; ---------------------------------------------------------------------------
  ;;; Tasks
  ;;; ---------------------------------------------------------------------------

  ;; Example: Task with fixed duration
  (deftask task1 "Planning Phase"
    :duration (duration 1 :weeks)
    :allocate (person1 person2)
    :priority 900)

  ;; Example: Task with effort (scales with resource efficiency)
  (deftask task2 "Implementation"
    :effort (duration 80 :hours)  ; 10 person-days
    :depends-on (task1)
    :allocate (person1)
    :priority 800)

  ;; Example: Nested tasks (subtasks)
  (deftask task3 "Testing Phase"
    :duration (duration 2 :weeks)
    :depends-on (task2)
    :priority 900

    (deftask task3a "Unit Testing"
      :duration (duration 1 :weeks)
      :allocate (person1))

    (deftask task3b "Integration Testing"
      :duration (duration 1 :weeks)
      :depends-on (task3a)
      :allocate (person2)))

  ;; Example: Milestone (zero duration)
  (deftask milestone1 "Phase 1 Complete"
    :milestone t
    :depends-on (task3))

  ;; Add more tasks as needed

  ;;; ---------------------------------------------------------------------------
  ;;; Reports
  ;;; ---------------------------------------------------------------------------

  (defreport summary "Project Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :duration :priority)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

  (defreport csv-export "CSV Export"
    :type :task
    :format :csv
    :columns (:id :name :start :end :duration :complete))

  (defreport critical "Critical Path"
    :type :task
    :format :html
    :columns (:id :name :start :end :slack)
    :filter (lambda (task) (and (task-slack task) (zerop (task-slack task)))))

  (defreport resources "Resource Report"
    :type :resource
    :format :html
    :columns (:id :name :efficiency :rate :criticalness)))

;;; ============================================================================
;;; ANALYSIS
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════╗~%")
(format t "║  ~A~%" (project-name *current-project*))
(format t "╚══════════════════════════════════════════════════╝~%~%")

;; Finalize and schedule
(format t "Scheduling project...~%")
(finalize-project *current-project*)
(schedule *current-project*)
(format t "✓ Scheduling complete!~%~%")

;; Display schedule
(format t "Project Schedule:~%")
(format t "  Start: ~A~%" (project-start *current-project*))
(format t "  End:   ~A~%" (project-end *current-project*))
(format t "  Tasks: ~A~%~%" (hash-table-count (project-tasks *current-project*)))

;; Critical path
(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical Path (~A tasks):~%" (length critical-tasks))
  (dolist (task critical-tasks)
    (format t "  • ~A~%" (task-name task)))
  (format t "~%"))

;; Resource over-allocation check
(let ((overallocations (detect-resource-overallocations *current-project*)))
  (if (null overallocations)
      (format t "✓ No resource conflicts~%~%")
      (progn
        (format t "⚠ Resource conflicts detected:~%")
        (dolist (oa overallocations)
          (format t "  • ~A on ~A~%"
                  (overallocation-resource-id oa)
                  (overallocation-date oa)))
        (format t "~%"))))

;;; ============================================================================
;;; CREATE BASELINE (for EVM tracking)
;;; ============================================================================

(let ((baseline (create-baseline *current-project* :name "Initial Plan")))
  (set-project-baseline *current-project* baseline)
  (format t "✓ Baseline created~%~%"))

;;; ============================================================================
;;; OPTIONAL: Record Actual Work (Bookings)
;;; ============================================================================

;; Uncomment and customize to track actual time:
;;
;; (let ((task (gethash 'task1 (project-tasks *current-project*)))
;;       (resource (gethash 'person1 (project-resources *current-project*))))
;;   ;; Record 8 hours of work
;;   (add-booking task resource
;;                (date 2024 1 15 9 0 0)
;;                (duration 8 :hours))
;;
;;   ;; Auto-calculate completion
;;   (update-task-completion-from-bookings task)
;;   (format t "Task completion: ~A%~%" (task-complete task)))

;;; ============================================================================
;;; OPTIONAL: EVM Metrics
;;; ============================================================================

;; Uncomment to see EVM metrics:
;;
;; (let ((pv (calculate-planned-value *current-project* (local-time:now)))
;;       (ev (calculate-earned-value *current-project*))
;;       (spi (calculate-spi *current-project* (local-time:now))))
;;   (format t "~%EVM Metrics:~%")
;;   (format t "  Planned Value: ~,1F%~%" pv)
;;   (format t "  Earned Value:  ~,1F%~%" ev)
;;   (format t "  SPI: ~,2F~%~%" spi))

;;; ============================================================================
;;; GENERATE REPORTS
;;; ============================================================================

(format t "Generating reports...~%")

;; Save reports (will be created in current directory)
(save-project-report *current-project* 'summary "project-summary.html")
(format t "  • project-summary.html~%")

(save-project-report *current-project* 'csv-export "project-tasks.csv")
(format t "  • project-tasks.csv~%")

(save-project-report *current-project* 'critical "project-critical.html")
(format t "  • project-critical.html~%")

(save-project-report *current-project* 'resources "project-resources.html")
(format t "  • project-resources.html~%")

;;; ============================================================================
;;; SAVE PROJECT
;;; ============================================================================

(let ((session (make-instance 'session :project *current-project*)))
  (save-session session "my-project-saved.lisp")
  (format t "~%✓ Project saved to: my-project-saved.lisp~%"))

;;; ============================================================================
;;; DONE!
;;; ============================================================================

(format t "~%╔══════════════════════════════════════════════════╗~%")
(format t "║  Project planning complete!                      ║~%")
(format t "╚══════════════════════════════════════════════════╝~%~%")

(format t "Next steps:~%")
(format t "  1. Open project-summary.html in your browser~%")
(format t "  2. Review the critical path~%")
(format t "  3. Check for resource conflicts~%")
(format t "  4. Track actual time with bookings~%")
(format t "  5. Monitor progress with EVM metrics~%~%")

;;; ============================================================================
;;; USAGE EXAMPLES
;;; ============================================================================

;; To run this file:
;;   sbcl --script my-project.lisp
;;
;; To load in a REPL for interactive work:
;;   sbcl
;;   (load "my-project.lisp")
;;
;; To load a saved project:
;;   (ql:quickload :project-juggler)
;;   (in-package :project-juggler)
;;   (setf *current-session* (load-project-session "my-project-saved.lisp"))
;;   (setf *current-project* (session-project *current-session*))
;;
;; To update task completion:
;;   (setf (task-complete (gethash 'task1 (project-tasks *current-project*))) 50)
;;
;; To view a task:
;;   (let ((task (gethash 'task1 (project-tasks *current-project*))))
;;     (format t "~A: ~A to ~A~%"
;;             (task-name task) (task-start task) (task-end task)))
