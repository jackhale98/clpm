;;;; src/tracking/bookings.lisp
;;;; Actual time tracking with bookings

(in-package #:project-juggler)

;;;
;;; Booking Predicates
;;;

(defun booking-p (obj)
  "Check if object is a booking"
  (typep obj 'booking))

;;;
;;; Booking Duration Calculations
;;;

(defun booking-duration-hours (booking)
  "Calculate duration of booking in hours"
  (let* ((start (booking-start booking))
         (end (booking-end booking))
         (seconds (local-time:timestamp-difference
                   (date-timestamp end)
                   (date-timestamp start))))
    (/ seconds 3600.0)))

(defun booking-duration-days (booking)
  "Calculate duration of booking in days"
  (let* ((start (booking-start booking))
         (end (booking-end booking))
         (seconds (local-time:timestamp-difference
                   (date-timestamp end)
                   (date-timestamp start))))
    (/ seconds 86400.0)))

;;;
;;; Add Booking
;;;

(defun add-booking (task resource start end-or-duration &key (amount nil))
  "Add a booking to a task and resource

  end-or-duration can be either:
    - A date (end date of booking)
    - A duration (duration to add to start date)

  Returns the created booking."
  (let* ((end (if (duration-p end-or-duration)
                  (date+ start end-or-duration)
                  end-or-duration))
         (booking (make-instance 'booking
                                :resource resource
                                :task task
                                :start start
                                :end end
                                :amount amount)))
    ;; Add booking to task
    (push booking (task-bookings task))

    ;; Add booking to resource
    (push booking (resource-bookings resource))

    booking))

;;;
;;; Total Booked Hours
;;;

(defun total-booked-hours (task-or-resource)
  "Calculate total booked hours for a task or resource"
  (let ((bookings (if (task-p task-or-resource)
                      (task-bookings task-or-resource)
                      (resource-bookings task-or-resource))))
    (reduce #'+ bookings
            :key #'booking-duration-hours
            :initial-value 0)))

;;;
;;; Booking Filtering
;;;

(defun bookings-in-range (task start-date end-date)
  "Get bookings for a task within a date range (inclusive)"
  (remove-if-not
   (lambda (booking)
     (and (date>= (booking-start booking) start-date)
          (date< (booking-start booking) end-date)))
   (task-bookings task)))

;;;
;;; Task Completion from Bookings
;;;

(defun update-task-completion-from-bookings (task)
  "Update task completion percentage based on booked hours vs planned effort

  If task has effort, calculates percentage as:
    completion = (total booked hours / effort in hours) * 100

  Caps at 100%."
  (when (task-effort task)
    (let* ((effort-hours (duration-in-hours (task-effort task)))
           (booked-hours (total-booked-hours task))
           (completion (min 100 (floor (* 100 (/ booked-hours effort-hours))))))
      (setf (task-complete task) completion)
      completion)))

;;;
;;; Exports
;;;

;; The following functions are now available:
;; - booking-p
;; - booking-duration-hours
;; - booking-duration-days
;; - add-booking
;; - total-booked-hours
;; - bookings-in-range
;; - update-task-completion-from-bookings
