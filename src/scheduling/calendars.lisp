;;;; src/scheduling/calendars.lisp
;;;; Working time calendars

(in-package #:project-juggler)

;;;
;;; Working Hours Class
;;;

(defclass working-hours ()
  ((days :initarg :days
         :reader working-hours-days
         :initform '(:monday :tuesday :wednesday :thursday :friday)
         :documentation "List of working days as keywords")
   (start-time :initarg :start-time
               :reader working-hours-start-time
               :initform "09:00"
               :documentation "Start time as string HH:MM")
   (end-time :initarg :end-time
             :reader working-hours-end-time
             :initform "17:00"
             :documentation "End time as string HH:MM"))
  (:documentation "Working hours specification for a calendar"))

(defun working-hours-per-day (working-hours)
  "Calculate number of working hours per day"
  (let* ((start-time (working-hours-start-time working-hours))
         (end-time (working-hours-end-time working-hours))
         (start-hour (parse-time-string start-time))
         (end-hour (parse-time-string end-time)))
    (- end-hour start-hour)))

(defun parse-time-string (time-string)
  "Parse time string HH:MM to hours as decimal"
  (let* ((colon-pos (position #\: time-string))
         (hours (parse-integer (subseq time-string 0 colon-pos)))
         (minutes (parse-integer (subseq time-string (1+ colon-pos)))))
    (+ hours (/ minutes 60.0))))

;;;
;;; Calendar Class
;;;

(defclass calendar ()
  ((id :initarg :id
       :reader calendar-id
       :documentation "Calendar identifier")
   (name :initarg :name
         :reader calendar-name
         :initform ""
         :documentation "Calendar name")
   (working-hours :initarg :working-hours
                  :accessor calendar-working-hours
                  :initform nil
                  :documentation "Working hours specification")
   (holidays :initarg :holidays
             :accessor calendar-holidays
             :initform nil
             :documentation "List of holiday dates")
   (timezone :initarg :timezone
             :reader calendar-timezone
             :initform :utc
             :documentation "Calendar timezone"))
  (:documentation "Calendar with working time and holidays"))

;;;
;;; Holiday Management
;;;

(defun add-holiday (calendar date description)
  "Add a holiday to the calendar"
  (push (list :date date :description description)
        (calendar-holidays calendar)))

(defun holiday-p (date calendar)
  "Check if date is a holiday"
  (some (lambda (holiday)
          (date= (getf holiday :date) date))
        (calendar-holidays calendar)))

;;;
;;; Day of Week
;;;

(defun date-day-of-week (date)
  "Get day of week as keyword for a date"
  (let ((day-num (local-time:timestamp-day-of-week (date-timestamp date))))
    (case day-num
      (0 :sunday)
      (1 :monday)
      (2 :tuesday)
      (3 :wednesday)
      (4 :thursday)
      (5 :friday)
      (6 :saturday))))

;;;
;;; Working Day Checks
;;;

(defun working-day-p (date calendar)
  "Check if date is a working day (not holiday, not weekend)"
  (and (not (holiday-p date calendar))
       (working-hours-on-day-p date (calendar-working-hours calendar))))

(defun working-hours-on-day-p (date working-hours)
  "Check if a date falls on a working day of the week"
  (when working-hours
    (let ((day-of-week (date-day-of-week date)))
      (member day-of-week (working-hours-days working-hours)))))

;;;
;;; Working Hours Calculations
;;;

(defun working-hours-on-date (date calendar)
  "Calculate working hours on a specific date"
  (if (working-day-p date calendar)
      (working-hours-per-day (calendar-working-hours calendar))
      0))

(defun working-hours-between (start-date end-date calendar)
  "Calculate total working hours between two dates (exclusive of end-date)"
  (let ((hours 0)
        (current-date start-date))
    ;; Iterate through each day from start (inclusive) to end (exclusive)
    (loop while (date< current-date end-date)
          do (when (working-day-p current-date calendar)
               (incf hours (working-hours-per-day (calendar-working-hours calendar))))
             (setf current-date (date+ current-date (duration 1 :days))))
    hours))

;;;
;;; Predicates
;;;

(defun calendar-p (obj)
  "Check if object is a calendar"
  (typep obj 'calendar))

(defun working-hours-p (obj)
  "Check if object is working-hours"
  (typep obj 'working-hours))
