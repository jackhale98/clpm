;;;; tests/scheduling/test-calendars.lisp
;;;; Tests for working time calendars

(in-package #:project-juggler-tests)

(def-suite calendar-suite
  :in scheduling-suite
  :description "Working time calendar tests")

(in-suite calendar-suite)

;;; Working Hours Tests

(test create-working-hours-basic
  "Can create basic working hours"
  (let ((wh (make-instance 'working-hours
                           :days '(:monday :tuesday :wednesday :thursday :friday)
                           :start-time "09:00"
                           :end-time "17:00")))
    (is (not (null wh)))
    (is (equal '(:monday :tuesday :wednesday :thursday :friday)
               (working-hours-days wh)))
    (is (string= "09:00" (working-hours-start-time wh)))
    (is (string= "17:00" (working-hours-end-time wh)))))

(test working-hours-default-8-hour-day
  "Default working hours are 8 hours per day"
  (let ((wh (make-instance 'working-hours
                           :days '(:monday :tuesday :wednesday :thursday :friday)
                           :start-time "09:00"
                           :end-time "17:00")))
    (is (= 8 (working-hours-per-day wh)))))

(test working-hours-custom-hours
  "Can create working hours with custom hours"
  (let ((wh (make-instance 'working-hours
                           :days '(:monday :tuesday :wednesday :thursday)
                           :start-time "08:00"
                           :end-time "16:00")))
    (is (= 8 (working-hours-per-day wh)))))

(test working-hours-half-day-friday
  "Can create working hours with half-day Friday"
  (let ((wh (make-instance 'working-hours
                           :days '(:monday :tuesday :wednesday :thursday)
                           :start-time "09:00"
                           :end-time "17:00"))
        (wh-friday (make-instance 'working-hours
                                  :days '(:friday)
                                  :start-time "09:00"
                                  :end-time "13:00")))
    (is (= 8 (working-hours-per-day wh)))
    (is (= 4 (working-hours-per-day wh-friday)))))

;;; Calendar Tests

(test create-calendar-basic
  "Can create a basic calendar"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :name "Standard Calendar"
                             :working-hours wh)))
    (is (not (null cal)))
    (is (eq 'standard (calendar-id cal)))
    (is (string= "Standard Calendar" (calendar-name cal)))
    (is (eq wh (calendar-working-hours cal)))))

(test calendar-with-timezone
  "Can create calendar with specific timezone"
  (let ((cal (make-instance 'calendar
                            :id 'us-eastern
                            :name "US Eastern"
                            :timezone :america/new_york)))
    (is (eq :america/new_york (calendar-timezone cal)))))

(test calendar-default-timezone-utc
  "Calendar defaults to UTC timezone"
  (let ((cal (make-instance 'calendar
                            :id 'default
                            :name "Default")))
    (is (eq :utc (calendar-timezone cal)))))

;;; Working Day Tests

(test working-day-monday-is-working
  "Monday is a working day with standard M-F schedule"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (monday (date 2024 11 18)))  ; Monday, November 18, 2024
    (is (working-day-p monday cal))))

(test working-day-saturday-not-working
  "Saturday is not a working day with standard M-F schedule"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (saturday (date 2024 11 16)))  ; Saturday, November 16, 2024
    (is (not (working-day-p saturday cal)))))

(test working-day-sunday-not-working
  "Sunday is not a working day with standard M-F schedule"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (sunday (date 2024 11 17)))  ; Sunday, November 17, 2024
    (is (not (working-day-p sunday cal)))))

;;; Holiday Tests

(test calendar-add-holiday
  "Can add holiday to calendar"
  (let ((cal (make-instance 'calendar
                            :id 'us
                            :name "US Calendar")))
    (add-holiday cal (date 2024 12 25) "Christmas")
    (is (= 1 (length (calendar-holidays cal))))))

(test calendar-holiday-is-not-working
  "Holiday is not a working day"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'us
                             :working-hours wh))
         (christmas (date 2024 12 25)))  ; Christmas 2024 (Wednesday)
    (add-holiday cal christmas "Christmas")
    (is (not (working-day-p christmas cal)))))

(test calendar-multiple-holidays
  "Can add multiple holidays"
  (let ((cal (make-instance 'calendar :id 'us)))
    (add-holiday cal (date 2024 12 25) "Christmas")
    (add-holiday cal (date 2024 7 4) "Independence Day")
    (add-holiday cal (date 2024 11 28) "Thanksgiving")
    (is (= 3 (length (calendar-holidays cal))))))

(test holiday-p-true-for-holiday
  "holiday-p returns true for holidays"
  (let ((cal (make-instance 'calendar :id 'us)))
    (add-holiday cal (date 2024 12 25) "Christmas")
    (is (holiday-p (date 2024 12 25) cal))))

(test holiday-p-false-for-non-holiday
  "holiday-p returns false for non-holidays"
  (let ((cal (make-instance 'calendar :id 'us)))
    (add-holiday cal (date 2024 12 25) "Christmas")
    (is (not (holiday-p (date 2024 12 26) cal)))))

;;; Working Hours Calculation Tests

(test working-hours-in-single-day
  "Calculate working hours in a single working day"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (monday-start (date 2024 11 18))
         (monday-end (date 2024 11 18)))
    (is (= 0 (working-hours-between monday-start monday-end cal)))))

(test working-hours-over-one-day
  "Calculate working hours over one complete working day"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (monday (date 2024 11 18))
         (tuesday (date 2024 11 19)))
    (is (= 8 (working-hours-between monday tuesday cal)))))

(test working-hours-over-week
  "Calculate working hours over a full week (M-F)"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (monday (date 2024 11 18))
         (next-monday (date 2024 11 25)))
    (is (= 40 (working-hours-between monday next-monday cal)))))

(test working-hours-skip-weekend
  "Working hours calculation skips weekends"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (friday (date 2024 11 22))
         (monday (date 2024 11 25)))
    ;; Friday to Monday = 1 working day (Friday), weekend doesn't count
    (is (= 8 (working-hours-between friday monday cal)))))

(test working-hours-skip-holiday
  "Working hours calculation skips holidays"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (tuesday (date 2024 12 24))
         (friday (date 2024 12 27)))
    ;; Add Christmas as holiday
    (add-holiday cal (date 2024 12 25) "Christmas")
    ;; Tue Dec 24 to Fri Dec 27 (exclusive) = Tue, Wed, Thu
    ;; Christmas (Wed Dec 25) is holiday, so only Tue and Thu count = 16 hours
    (is (= 16 (working-hours-between tuesday friday cal)))))

(test working-day-on-date
  "Calculate working hours on a specific date"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (monday (date 2024 11 18)))
    (is (= 8 (working-hours-on-date monday cal)))))

(test working-day-on-date-weekend
  "Working hours on weekend is zero"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (saturday (date 2024 11 16)))
    (is (= 0 (working-hours-on-date saturday cal)))))

(test working-day-on-date-holiday
  "Working hours on holiday is zero"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'standard
                             :working-hours wh))
         (christmas (date 2024 12 25)))
    (add-holiday cal christmas "Christmas")
    (is (= 0 (working-hours-on-date christmas cal)))))

;;; Date Day of Week Tests

(test date-day-of-week-monday
  "Can get day of week for Monday"
  (let ((monday (date 2024 11 18)))
    (is (eq :monday (date-day-of-week monday)))))

(test date-day-of-week-saturday
  "Can get day of week for Saturday"
  (let ((saturday (date 2024 11 16)))
    (is (eq :saturday (date-day-of-week saturday)))))

(test date-day-of-week-sunday
  "Can get day of week for Sunday"
  (let ((sunday (date 2024 11 17)))
    (is (eq :sunday (date-day-of-week sunday)))))

;;; Integration Tests

(test calendar-full-integration
  "Full integration test with calendar, holidays, and working hours"
  (let* ((wh (make-instance 'working-hours
                            :days '(:monday :tuesday :wednesday :thursday :friday)
                            :start-time "09:00"
                            :end-time "17:00"))
         (cal (make-instance 'calendar
                             :id 'company
                             :name "Company Calendar"
                             :working-hours wh
                             :timezone :utc)))

    ;; Add holidays
    (add-holiday cal (date 2024 12 25) "Christmas")
    (add-holiday cal (date 2024 1 1) "New Year's Day")

    ;; Test working days
    (is (working-day-p (date 2024 11 18) cal))  ; Monday
    (is (not (working-day-p (date 2024 11 16) cal)))  ; Saturday
    (is (not (working-day-p (date 2024 12 25) cal)))  ; Christmas

    ;; Test working hours calculation
    (let ((hours (working-hours-between (date 2024 11 18) (date 2024 11 22) cal)))
      ;; Mon-Fri = 5 days = 40 hours
      (is (= 32 hours)))))  ; Mon(18) to Fri(22) = 4 working days = 32 hours
