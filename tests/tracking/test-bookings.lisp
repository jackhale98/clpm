;;;; tests/tracking/test-bookings.lisp
;;;; Tests for bookings (actual time tracking)

(in-package #:project-juggler-tests)

(def-suite bookings-suite
  :in project-juggler-suite
  :description "Bookings and actual time tracking tests")

(in-suite bookings-suite)

;;; Booking Class Tests

(test create-booking-basic
  "Can create a basic booking"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18))
           (end (date 2024 11 18 17 0 0))
           (booking (make-instance 'booking
                                   :resource resource
                                   :task task
                                   :start start
                                   :end end)))
      (is (not (null booking)))
      (is (booking-p booking))
      (is (eq resource (booking-resource booking)))
      (is (eq task (booking-task booking)))
      (is (date= start (booking-start booking)))
      (is (date= end (booking-end booking))))))

(test booking-with-amount
  "Can create booking with specific amount (hours)"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18))
           (end (date 2024 11 18 17 0 0))
           (booking (make-instance 'booking
                                   :resource resource
                                   :task task
                                   :start start
                                   :end end
                                   :amount 8.0)))
      (is (= 8.0 (booking-amount booking))))))

(test booking-calculates-duration
  "Booking calculates duration from start and end"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18 9 0 0))
           (end (date 2024 11 18 17 0 0))
           (booking (make-instance 'booking
                                   :resource resource
                                   :task task
                                   :start start
                                   :end end)))
      (is (= 8 (booking-duration-hours booking))))))

;;; Add Booking Tests

(test add-booking-to-task-and-resource
  "add-booking adds booking to both task and resource"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18 9 0 0))
           (end (date 2024 11 18 17 0 0)))
      (add-booking task resource start end)

      ;; Check task has booking
      (is (= 1 (length (task-bookings task))))
      (is (eq resource (booking-resource (first (task-bookings task)))))

      ;; Check resource has booking
      (is (= 1 (length (resource-bookings resource))))
      (is (eq task (booking-task (first (resource-bookings resource))))))))

(test add-booking-with-duration
  "add-booking can accept duration instead of end date"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18 9 0 0))
           (dur (duration 8 :hours)))
      (add-booking task resource start dur)

      (let ((booking (first (task-bookings task))))
        (is (not (null booking)))
        (is (= 8 (booking-duration-hours booking)))))))

(test add-multiple-bookings
  "Can add multiple bookings to same task"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(defresource dev2 "Developer 2"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource1 (resolve-resource-reference 'dev1))
           (resource2 (resolve-resource-reference 'dev2))
           (task (resolve-task-reference 'task1))
           (start1 (date 2024 11 18 9 0 0))
           (end1 (date 2024 11 18 17 0 0))
           (start2 (date 2024 11 19 9 0 0))
           (end2 (date 2024 11 19 17 0 0)))
      (add-booking task resource1 start1 end1)
      (add-booking task resource2 start2 end2)

      (is (= 2 (length (task-bookings task))))
      (is (= 1 (length (resource-bookings resource1))))
      (is (= 1 (length (resource-bookings resource2)))))))

;;; Booking Duration Calculations

(test booking-duration-in-hours
  "Can calculate booking duration in hours"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18 9 0 0))
           (end (date 2024 11 18 17 0 0))
           (booking (make-instance 'booking
                                   :resource resource
                                   :task task
                                   :start start
                                   :end end)))
      (is (= 8 (booking-duration-hours booking))))))

(test booking-duration-in-days
  "Can calculate booking duration in days"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1))
           (start (date 2024 11 18 9 0 0))
           (end (date 2024 11 19 9 0 0))
           (booking (make-instance 'booking
                                   :resource resource
                                   :task task
                                   :start start
                                   :end end)))
      (is (= 1 (booking-duration-days booking))))))

;;; Total Booked Hours Tests

(test calculate-total-booked-hours-for-task
  "Can calculate total booked hours for a task"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(defresource dev2 "Developer 2"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource1 (resolve-resource-reference 'dev1))
           (resource2 (resolve-resource-reference 'dev2))
           (task (resolve-task-reference 'task1)))
      (add-booking task resource1 (date 2024 11 18 9 0 0) (date 2024 11 18 17 0 0))
      (add-booking task resource2 (date 2024 11 19 9 0 0) (date 2024 11 19 17 0 0))

      (is (= 16 (total-booked-hours task))))))

(test calculate-total-booked-hours-for-resource
  "Can calculate total booked hours for a resource"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (eval '(deftask task2 "Task 2" :duration (duration 3 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task1 (resolve-task-reference 'task1))
           (task2 (resolve-task-reference 'task2)))
      (add-booking task1 resource (date 2024 11 18 9 0 0) (date 2024 11 18 17 0 0))
      (add-booking task2 resource (date 2024 11 19 9 0 0) (date 2024 11 19 17 0 0))

      (is (= 16 (total-booked-hours resource))))))

;;; Booking Date Range Tests

(test bookings-in-date-range
  "Can filter bookings by date range"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :duration (duration 5 :days)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1)))
      (add-booking task resource (date 2024 11 18 9 0 0) (date 2024 11 18 17 0 0))
      (add-booking task resource (date 2024 11 19 9 0 0) (date 2024 11 19 17 0 0))
      (add-booking task resource (date 2024 11 22 9 0 0) (date 2024 11 22 17 0 0))

      (let ((filtered (bookings-in-range task
                                        (date 2024 11 18)
                                        (date 2024 11 20))))
        (is (= 2 (length filtered)))))))

;;; Task Completion Based on Bookings

(test update-task-completion-from-bookings
  "Can calculate task completion percentage from bookings vs planned effort"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(deftask task1 "Task 1" :effort (duration 40 :hours)))
    (finalize-project *current-project*)

    (let* ((resource (resolve-resource-reference 'dev1))
           (task (resolve-task-reference 'task1)))
      ;; Book 20 hours of work
      (add-booking task resource (date 2024 11 18 9 0 0) (date 2024 11 18 17 0 0))
      (add-booking task resource (date 2024 11 19 9 0 0) (date 2024 11 19 13 0 0))

      ;; Update completion based on bookings
      (update-task-completion-from-bookings task)

      ;; 12 hours booked out of 40 planned = 30% complete
      (is (= 30 (task-complete task))))))

;;; Integration Tests

(test booking-full-integration
  "Full integration test with bookings, tasks, and resources"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))
    (eval '(defresource dev2 "Developer 2"))
    (eval '(deftask feature-a "Feature A" :effort (duration 80 :hours)))
    (eval '(deftask feature-b "Feature B" :effort (duration 40 :hours)))
    (finalize-project *current-project*)

    (let* ((dev1 (resolve-resource-reference 'dev1))
           (dev2 (resolve-resource-reference 'dev2))
           (feature-a (resolve-task-reference 'feature-a))
           (feature-b (resolve-task-reference 'feature-b)))

      ;; Book work for feature-a
      (add-booking feature-a dev1 (date 2024 11 18 9 0 0) (date 2024 11 18 17 0 0))
      (add-booking feature-a dev2 (date 2024 11 18 9 0 0) (date 2024 11 18 17 0 0))

      ;; Book work for feature-b
      (add-booking feature-b dev1 (date 2024 11 19 9 0 0) (date 2024 11 19 17 0 0))

      ;; Check totals
      (is (= 16 (total-booked-hours feature-a)))
      (is (= 8 (total-booked-hours feature-b)))
      (is (= 16 (total-booked-hours dev1)))
      (is (= 8 (total-booked-hours dev2)))

      ;; Check completion percentages
      (update-task-completion-from-bookings feature-a)
      (update-task-completion-from-bookings feature-b)
      (is (= 20 (task-complete feature-a)))  ; 16/80 = 20%
      (is (= 20 (task-complete feature-b))))))  ; 8/40 = 20%
