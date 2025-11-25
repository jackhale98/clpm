;;;; examples/time-tracking-project.lisp
;;;; Demonstrates working time calendars and actual time tracking with bookings

(require :asdf)
(in-package #:cl-user)

;; Add project root to ASDF registry
;; Works whether running from examples/ or project root
(let ((parent-dir (truename "../"))
      (current-dir (truename ".")))
  ;; Check if .asd file is in parent directory (running from examples/)
  (if (probe-file (merge-pathnames "project-juggler.asd" parent-dir))
      (push parent-dir asdf:*central-registry*)
      ;; Otherwise assume we're in project root
      (push current-dir asdf:*central-registry*)))

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load Project Juggler
(handler-case
    (asdf:load-system :project-juggler :verbose nil)
  (error (e)
    (format t "Error loading project-juggler: ~A~%" e)
    (format t "Please ensure all dependencies are installed.~%")
    (format t "Make sure you're running from either the project root or examples/ directory.~%")
    (uiop:quit 1)))

(in-package :project-juggler)

(format t "~%╔══════════════════════════════════════════════════════════════════════════════╗~%")
(format t "║                         TIME TRACKING PROJECT DEMO                           ║~%")
(format t "║                    Calendars + Bookings + EVM Integration                    ║~%")
(format t "╚══════════════════════════════════════════════════════════════════════════════╝~%~%")

;;;
;;; STEP 1: Create a Company Calendar
;;;

(format t "STEP 1: Creating Company Calendar~%")
(format t "════════════════════════════════════~%~%")

;; Define standard working hours (M-F, 9-5)
(defvar *working-hours*
  (make-instance 'working-hours
                :days '(:monday :tuesday :wednesday :thursday :friday)
                :start-time "09:00"
                :end-time "17:00"))

;; Create company calendar
(defvar *company-calendar*
  (make-instance 'calendar
                :id 'company-cal
                :name "Acme Corp Calendar"
                :working-hours *working-hours*
                :timezone :utc))

;; Add company holidays
(add-holiday *company-calendar* (date 2024 12 25) "Christmas Day")
(add-holiday *company-calendar* (date 2024 12 26) "Boxing Day")
(add-holiday *company-calendar* (date 2024 1 1) "New Year's Day")
(add-holiday *company-calendar* (date 2024 7 4) "Independence Day")
(add-holiday *company-calendar* (date 2024 11 28) "Thanksgiving")

(format t "✓ Calendar: ~A~%" (calendar-name *company-calendar*))
(format t "  Working days: Mon-Fri, ~A to ~A~%"
        (working-hours-start-time *working-hours*)
        (working-hours-end-time *working-hours*))
(format t "  Hours per day: ~A~%" (working-hours-per-day *working-hours*))
(format t "  Holidays defined: ~A~%~%" (length (calendar-holidays *company-calendar*)))

;; Demonstrate calendar features
(format t "Calendar Example: Week of Nov 18-22, 2024~%")
(let ((monday (date 2024 11 18))
      (friday (date 2024 11 22))
      (saturday (date 2024 11 16)))
  (format t "  Monday (18th) working day? ~A~%" (working-day-p monday *company-calendar*))
  (format t "  Saturday (16th) working day? ~A~%" (working-day-p saturday *company-calendar*))
  (format t "  Working hours Mon-Fri: ~A hours~%"
          (working-hours-between monday friday *company-calendar*)))

(format t "~%")

;;;
;;; STEP 2: Define Project with Effort-Based Tasks
;;;

(format t "STEP 2: Defining Mobile App Development Project~%")
(format t "════════════════════════════════════════════════~%~%")

(defproject mobile-app "Mobile App Development"
  :start (date 2024 11 18)  ; Monday
  :end (date 2024 12 31)

  ;; Resources with different efficiency levels
  (defresource senior-dev "Sarah (Senior Developer)"
    :efficiency 1.5
    :rate 150.0)

  (defresource mid-dev "Mike (Mid-level Developer)"
    :efficiency 1.0
    :rate 100.0)

  (defresource junior-dev "Jamie (Junior Developer)"
    :efficiency 0.7
    :rate 70.0)

  ;; Phase 1: Backend API (effort-based for accurate tracking)
  (deftask backend-api "Backend API Development"
    :effort (duration 80 :hours)  ; 10 person-days of work
    :allocate (senior-dev mid-dev)
    :priority 900)

  ;; Phase 2: Mobile UI (junior developer learning)
  (deftask mobile-ui "Mobile User Interface"
    :effort (duration 120 :hours)  ; 15 person-days of work
    :depends-on (backend-api)
    :allocate (junior-dev)
    :priority 800)

  ;; Phase 3: Integration
  (deftask integration "API Integration"
    :effort (duration 40 :hours)  ; 5 person-days
    :depends-on (mobile-ui)
    :allocate (mid-dev)
    :priority 900)

  ;; Phase 4: Testing
  (deftask testing "Testing & QA"
    :effort (duration 32 :hours)  ; 4 person-days
    :depends-on (integration)
    :allocate (senior-dev mid-dev)
    :priority 1000)

  ;; Launch milestone
  (deftask launch "App Launch"
    :milestone t
    :depends-on (testing))

  ;; Reports
  (defreport time-tracking "Time Tracking Report"
    :type :task
    :format :html
    :columns (:id :name :start :end :effort :complete)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b)))))

(format t "✓ Project defined: ~A~%" (project-name *current-project*))
(format t "  Tasks: ~A~%" (hash-table-count (project-tasks *current-project*)))
(format t "  Resources: ~A~%~%" (hash-table-count (project-resources *current-project*)))

;;;
;;; STEP 3: Schedule and Create Baseline
;;;

(format t "STEP 3: Scheduling Project~%")
(format t "═══════════════════════════~%~%")

(finalize-project *current-project*)
(schedule *current-project*)

;; Display schedule
(format t "Project Schedule:~%")
(maphash (lambda (id task)
           (when (task-scheduled-p task)
             (format t "  ~A: ~A to ~A~%"
                     (task-name task)
                     (task-start task)
                     (task-end task))))
         (project-tasks *current-project*))

;; Create baseline for EVM
(let ((baseline (create-baseline *current-project* :name "Initial Plan")))
  (set-project-baseline *current-project* baseline)
  (format t "~%✓ Baseline created for EVM tracking~%"))

(format t "~%")

;;;
;;; STEP 4: Simulate Work with Calendar-Aware Bookings
;;;

(format t "STEP 4: Recording Actual Work (Week 1)~%")
(format t "══════════════════════════════════════~%~%")

(let ((backend-task (gethash 'backend-api (project-tasks *current-project*)))
      (sarah (gethash 'senior-dev (project-resources *current-project*)))
      (mike (gethash 'mid-dev (project-resources *current-project*))))

  (format t "Recording work for Backend API Development...~%~%")

  ;; Monday, Nov 18
  (add-booking backend-task sarah (date 2024 11 18 9 0 0) (duration 8 :hours))
  (add-booking backend-task mike (date 2024 11 18 9 0 0) (duration 8 :hours))
  (format t "  Mon 11/18: Sarah (8h) + Mike (8h) = 16 hours~%")

  ;; Tuesday, Nov 19
  (add-booking backend-task sarah (date 2024 11 19 9 0 0) (duration 8 :hours))
  (add-booking backend-task mike (date 2024 11 19 9 0 0) (duration 8 :hours))
  (format t "  Tue 11/19: Sarah (8h) + Mike (8h) = 16 hours~%")

  ;; Wednesday, Nov 20
  (add-booking backend-task sarah (date 2024 11 20 9 0 0) (duration 8 :hours))
  (add-booking backend-task mike (date 2024 11 20 9 0 0) (duration 8 :hours))
  (format t "  Wed 11/20: Sarah (8h) + Mike (8h) = 16 hours~%")

  ;; Thursday, Nov 21
  (add-booking backend-task sarah (date 2024 11 21 9 0 0) (duration 6 :hours))
  (add-booking backend-task mike (date 2024 11 21 9 0 0) (duration 6 :hours))
  (format t "  Thu 11/21: Sarah (6h) + Mike (6h) = 12 hours~%")

  ;; Friday, Nov 22
  (add-booking backend-task sarah (date 2024 11 22 9 0 0) (duration 8 :hours))
  (add-booking backend-task mike (date 2024 11 22 9 0 0) (duration 8 :hours))
  (format t "  Fri 11/22: Sarah (8h) + Mike (8h) = 16 hours~%~%")

  ;; Calculate totals
  (let ((total-hours (total-booked-hours backend-task))
        (planned-hours (duration-in-hours (task-effort backend-task))))
    (format t "Week 1 Summary:~%")
    (format t "  Total hours booked: ~A~%" total-hours)
    (format t "  Planned effort: ~A hours~%" planned-hours)
    (format t "  Remaining: ~A hours~%~%" (- planned-hours total-hours))

    ;; Auto-calculate completion
    (update-task-completion-from-bookings backend-task)
    (format t "✓ Task auto-updated to ~A% complete~%" (task-complete backend-task))))

(format t "~%")

;;;
;;; STEP 5: Check Calendar Impact
;;;

(format t "STEP 5: Calendar-Aware Analysis~%")
(format t "═════════════════════════════════~%~%")

(let ((week-start (date 2024 11 18))
      (week-end (date 2024 11 25)))

  (format t "Analysis for Nov 18-22 (Mon-Fri):~%")
  (format t "  Calendar days: 7~%")
  (format t "  Working hours available: ~A (skips weekend)~%"
          (working-hours-between week-start week-end *company-calendar*))

  ;; Show weekend is not working
  (format t "~%Weekend check:~%")
  (format t "  Saturday (Nov 16): ~A hours~%"
          (working-hours-on-date (date 2024 11 16) *company-calendar*))
  (format t "  Sunday (Nov 17): ~A hours~%"
          (working-hours-on-date (date 2024 11 17) *company-calendar*))
  (format t "  Monday (Nov 18): ~A hours~%"
          (working-hours-on-date (date 2024 11 18) *company-calendar*)))

(format t "~%")

;;;
;;; STEP 6: EVM Metrics with Actual Bookings
;;;

(format t "STEP 6: Earned Value Management Analysis~%")
(format t "══════════════════════════════════════════~%~%")

(let ((status-date (date 2024 11 22))  ; End of week 1
      (pv (calculate-planned-value *current-project* (date 2024 11 22)))
      (ev (calculate-earned-value *current-project*))
      (sv (calculate-schedule-variance *current-project* (date 2024 11 22)))
      (spi (calculate-spi *current-project* (date 2024 11 22))))

  (format t "EVM Metrics as of ~A:~%" status-date)
  (format t "  Planned Value (PV):  ~,1F%~%" pv)
  (format t "  Earned Value (EV):   ~,1F% (from actual bookings)~%" ev)
  (format t "  Schedule Variance:   ~,1F%~%" sv)
  (format t "  Schedule Performance Index: ~,2F~%~%" spi)

  (cond
    ((> spi 1.0)
     (format t "  ✓ Status: Project is AHEAD of schedule!~%"))
    ((< spi 1.0)
     (format t "  ⚠ Status: Project is BEHIND schedule!~%"))
    (t
     (format t "  ✓ Status: Project is ON schedule!~%"))))

(format t "~%")

;;;
;;; STEP 7: Resource Utilization Report
;;;

(format t "STEP 7: Resource Utilization (Week 1)~%")
(format t "═════════════════════════════════════~%~%")

(format t "Individual resource hours:~%")
(maphash (lambda (id resource)
           (declare (ignore id))
           (let ((hours (total-booked-hours resource)))
             (when (> hours 0)
               (format t "  ~A: ~A hours~%"
                       (resource-name resource)
                       hours))))
         (project-resources *current-project*))

(format t "~%")

;;;
;;; STEP 8: Week 2 Planning with Calendar
;;;

(format t "STEP 8: Planning Week 2 with Calendar~%")
(format t "════════════════════════════════════════~%~%")

(let ((week2-start (date 2024 11 25))
      (week2-end (date 2024 11 29))  ; End before Thanksgiving
      (thanksgiving (date 2024 11 28)))

  (format t "Week 2 (Nov 25-29):~%")
  (format t "  Monday (25th) working? ~A~%" (working-day-p (date 2024 11 25) *company-calendar*))
  (format t "  Thursday (28th - Thanksgiving) working? ~A~%"
          (working-day-p thanksgiving *company-calendar*))
  (format t "  Friday (29th) working? ~A~%"
          (working-day-p (date 2024 11 29) *company-calendar*))

  (format t "~%  Available working hours: ~A~%"
          (working-hours-between week2-start week2-end *company-calendar*))
  (format t "  (Reduced due to Thanksgiving holiday)~%"))

(format t "~%")

;;;
;;; STEP 9: Generate Report
;;;

(format t "STEP 9: Generating Time Tracking Report~%")
(format t "═══════════════════════════════════════════~%~%")

(save-project-report *current-project* 'time-tracking "time-tracking-report.html")
(format t "✓ Report saved to: time-tracking-report.html~%~%")

;;;
;;; Summary
;;;

(format t "~%╔══════════════════════════════════════════════════════════════════════════════╗~%")
(format t "║                              DEMO SUMMARY                                    ║~%")
(format t "╚══════════════════════════════════════════════════════════════════════════════╝~%~%")

(format t "This demo showed:~%")
(format t "  ✓ Working time calendar with holidays~%")
(format t "  ✓ Weekend and holiday detection~%")
(format t "  ✓ Working hours calculations~%")
(format t "  ✓ Actual time tracking with bookings~%")
(format t "  ✓ Auto-calculated task completion~%")
(format t "  ✓ EVM metrics from real bookings~%")
(format t "  ✓ Resource utilization tracking~%")
(format t "  ✓ Calendar-aware project planning~%~%")

(format t "Key Takeaways:~%")
(format t "  • Calendars provide realistic scheduling (skipping weekends/holidays)~%")
(format t "  • Bookings give accurate progress tracking~%")
(format t "  • Completion % auto-calculated from actual work~%")
(format t "  • EVM metrics reflect real progress, not estimates~%")
(format t "  • Calendar-aware planning accounts for non-working days~%~%")

(format t "Next Steps:~%")
(format t "  1. View generated report: time-tracking-report.html~%")
(format t "  2. Try adding more bookings for week 2~%")
(format t "  3. Experiment with different working hours~%")
(format t "  4. Add your company's specific holidays~%~%")

(format t "═══════════════════════════════════════════════════════════════════════════════~%")
(format t "Demo complete! Calendar + Bookings = Realistic Project Tracking~%")
(format t "═══════════════════════════════════════════════════════════════════════════════~%~%")
