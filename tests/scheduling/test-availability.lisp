;;;; tests/scheduling/test-availability.lisp
;;;; Tests for resource availability (leaves, daily limits, percent allocation)

(in-package #:project-juggler-tests)

(def-suite availability-suite
  :in project-juggler-suite
  :description "Tests for resource availability")

(in-suite availability-suite)

;;; ============================================================================
;;; Leave/Vacation Tests
;;; ============================================================================

(test resource-leave-definition
  "Test defining resource with leave periods"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :leaves ((vacation (date 2024 7 1) (date 2024 7 14))))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      (is (not (null resource)))
      (is (= 1 (length (resource-leaves resource))))
      (let ((leave (first (resource-leaves resource))))
        (is (eq :vacation (leave-type leave)))))))

(test resource-multiple-leaves
  "Test resource with multiple leave periods"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :leaves ((vacation (date 2024 7 1) (date 2024 7 14))
               (holiday (date 2024 12 25) (date 2024 12 25))
               (sick (date 2024 3 15) (date 2024 3 17))))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      (is (= 3 (length (resource-leaves resource)))))))

(test resource-available-on-date
  "Test checking if resource is available on a specific date"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :leaves ((vacation (date 2024 7 1) (date 2024 7 14))))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Available before vacation
      (is (resource-available-p resource (date 2024 6 30)))
      ;; Not available during vacation
      (is (not (resource-available-p resource (date 2024 7 5))))
      ;; Not available on vacation start
      (is (not (resource-available-p resource (date 2024 7 1))))
      ;; Not available on vacation end
      (is (not (resource-available-p resource (date 2024 7 14))))
      ;; Available after vacation
      (is (resource-available-p resource (date 2024 7 15))))))

(test resource-leave-overlaps-task
  "Test detecting when leave overlaps with scheduled task"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :leaves ((vacation (date 2024 1 10) (date 2024 1 15))))
    (deftask task1 "Task 1"
      :start (date 2024 1 8)
      :duration (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*)))
          (task (gethash 'task1 (project-tasks *current-project*))))
      ;; Should detect overlap between task and leave
      (let ((conflicts (detect-leave-conflicts task)))
        (is (= 1 (length conflicts)))
        (is (eq resource (conflict-resource (first conflicts))))))))

;;; ============================================================================
;;; Daily/Weekly Limit Tests
;;; ============================================================================

(test resource-daily-limit-definition
  "Test defining resource with daily hour limit"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :daily-limit (duration 8 :hours))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      (is (not (null resource)))
      (let ((limit (resource-daily-limit resource)))
        (is (duration-p limit))
        (is (= 8 (duration-in-hours limit)))))))

(test resource-weekly-limit-definition
  "Test defining resource with weekly hour limit"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :weekly-limit (duration 40 :hours))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      (is (not (null resource)))
      (let ((limit (resource-weekly-limit resource)))
        (is (duration-p limit))
        (is (= 40 (duration-in-hours limit)))))))

(test resource-both-limits
  "Test resource with both daily and weekly limits"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :daily-limit (duration 6 :hours)
      :weekly-limit (duration 30 :hours))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      (is (= 6 (duration-in-hours (resource-daily-limit resource))))
      (is (= 30 (duration-in-hours (resource-weekly-limit resource)))))))

(test resource-available-hours-on-date
  "Test calculating available hours on a specific date"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :daily-limit (duration 8 :hours))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Should return daily limit on a working day
      (is (= 8 (resource-available-hours resource (date 2024 1 15)))))))

;;; ============================================================================
;;; Percent Allocation Tests
;;; ============================================================================

(test allocation-with-percent
  "Test allocating resource with percentage"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :allocate ((dev1 :percent 50)))
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (allocs (task-allocations task))
           (resource (gethash 'dev1 (project-resources *current-project*))))
      (is (= 1 (length allocs)))
      (let ((alloc (first allocs)))
        ;; Check the resource-specific percent
        (is (= 50 (get-allocation-percent-for-resource alloc resource)))))))

(test allocation-full-percent-default
  "Test that allocation defaults to 100%"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (allocs (task-allocations task)))
      (is (= 1 (length allocs)))
      (let ((alloc (first allocs)))
        (is (= 100 (allocation-percent alloc)))))))

(test allocation-multiple-resources-different-percent
  "Test multiple resources with different allocation percentages"
  (with-test-project
    (defresource dev1 "Developer 1" :rate 100.0)
    (defresource dev2 "Developer 2" :rate 120.0)
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :allocate ((dev1 :percent 100)
                 (dev2 :percent 50)))
    (finalize-project *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (allocs (task-allocations task)))
      (is (= 1 (length allocs)))  ; Single allocation object with multiple resources
      (let* ((alloc (first allocs))
             (resource-allocs (allocation-resource-percents alloc)))
        ;; Should have allocation info for both resources
        (is (= 2 (length resource-allocs)))))))

(test effort-calculation-with-percent-allocation
  "Test that effort calculation respects allocation percentage"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    ;; 10 days effort at 50% allocation = 20 days duration
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate ((dev1 :percent 50)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let* ((task (gethash 'task1 (project-tasks *current-project*)))
           (start (task-start task))
           (end (task-end task))
           ;; Calculate duration in days
           (start-ts (local-time:timestamp-to-unix (date-timestamp start)))
           (end-ts (local-time:timestamp-to-unix (date-timestamp end)))
           (duration-days (truncate (/ (- end-ts start-ts) 86400))))
      ;; Duration should be 20 days (10 days effort / 0.5 allocation)
      (is (= 20 duration-days)))))

(test cost-calculation-with-percent-allocation
  "Test that cost calculation respects allocation percentage"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)  ; $100/day
    ;; 10 days effort at 50% allocation = cost is still for 10 person-days
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate ((dev1 :percent 50)))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Cost = effort × rate = 10 days × $100/day = $1000
      (is (= 1000.0 (calculate-task-planned-cost task))))))

;;; ============================================================================
;;; Resource Calendar Tests
;;; ============================================================================

(test resource-calendar-override
  "Test resource with custom calendar"
  (with-test-project
    ;; Create a simple calendar using existing calendar class
    (let ((part-time-cal (make-instance 'pj:calendar
                                       :id 'part-time
                                       :working-hours (make-instance 'pj:working-hours
                                                                    :days '(:monday :tuesday :wednesday)
                                                                    :start-time "09:00"
                                                                    :end-time "17:00"))))
      (defresource dev1 "Developer"
        :rate 100.0
        :calendar part-time-cal)
      (finalize-project *current-project*)
      (let ((resource (gethash 'dev1 (project-resources *current-project*))))
        (is (not (null (resource-calendar resource))))
        (is (eq 'part-time (calendar-id (resource-calendar resource))))))))

;;; ============================================================================
;;; Combined Availability Tests
;;; ============================================================================

(test effective-availability-calculation
  "Test calculating effective availability considering all factors"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :daily-limit (duration 8 :hours)
      :leaves ((vacation (date 2024 7 1) (date 2024 7 5))))
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Normal day: full daily limit
      (is (= 8 (effective-available-hours resource (date 2024 6 15))))
      ;; During vacation: 0 hours
      (is (= 0 (effective-available-hours resource (date 2024 7 3)))))))

(test availability-summary-for-date-range
  "Test getting availability summary for a date range"
  (with-test-project
    (defresource dev1 "Developer"
      :rate 100.0
      :daily-limit (duration 8 :hours)
      :leaves ((vacation (date 2024 1 15) (date 2024 1 17))))  ; 3 days off
    (finalize-project *current-project*)
    (let ((resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Jan 10-20 is 11 days, minus 3 vacation days = 8 working days
      ;; At 8 hours/day = 64 available hours (assuming no weekends for simplicity)
      (let ((summary (availability-summary resource (date 2024 1 10) (date 2024 1 20))))
        (is (numberp (getf summary :total-hours)))
        (is (numberp (getf summary :leave-days)))))))
