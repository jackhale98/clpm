;;;; src/core/types.lisp
;;;; Temporal types (to be implemented via TDD)

(in-package #:project-juggler)

;;; Special variables

(defvar *current-project* nil
  "Current project being defined")

(defvar *current-task* nil
  "Current task being defined")

(defvar *current-namespace* nil
  "Current namespace for task/resource definitions")

(defvar *current-session* nil
  "Current session")

(defvar *namespace-registry* (make-hash-table :test 'eq)
  "Maps namespace names to namespace objects")

(defvar *project-registry* (make-hash-table :test 'eq)
  "Maps project IDs to project objects")

;;; Temporal type implementations

(defclass pj-date ()
  ((timestamp :initarg :timestamp
              :reader date-timestamp
              :documentation "Internal timestamp representation using local-time"))
  (:documentation "Project Juggler date"))

(defun date (year month day &optional (hour 0) (minute 0) (second 0))
  "Create a date"
  (make-instance 'pj-date
                 :timestamp (local-time:encode-timestamp
                              0 second minute hour day month year)))

(defun date-year (date)
  "Get year from date"
  (local-time:timestamp-year (date-timestamp date)))

(defun date-month (date)
  "Get month from date"
  (local-time:timestamp-month (date-timestamp date)))

(defun date-day (date)
  "Get day from date"
  (local-time:timestamp-day (date-timestamp date)))

(defun date-hour (date)
  "Get hour from date"
  (local-time:timestamp-hour (date-timestamp date)))

(defun date-minute (date)
  "Get minute from date"
  (local-time:timestamp-minute (date-timestamp date)))

(defun date-second (date)
  "Get second from date"
  (local-time:timestamp-second (date-timestamp date)))

(defclass duration ()
  ((value :initarg :value :reader duration-value
          :documentation "Duration value")
   (unit :initarg :unit :reader duration-unit
         :documentation "Duration unit (:minutes :hours :days :weeks :months :years)"))
  (:documentation "Duration in various units"))

(defun duration (value unit)
  "Create duration"
  (make-instance 'duration :value value :unit unit))

(defun duration-p (obj)
  "Check if object is a duration"
  (typep obj 'duration))

(defmethod duration-in-seconds ((dur duration))
  "Convert duration to seconds"
  (let ((value (duration-value dur)))
    (ecase (duration-unit dur)
      (:seconds value)
      (:minutes (* value 60))
      (:hours (* value 3600))
      (:days (* value 86400))
      (:weeks (* value 604800))
      (:months (* value 2592000))  ; Approximate: 30 days
      (:years (* value 31536000))))) ; Approximate: 365 days

(defmethod duration-in-minutes ((dur duration))
  "Convert duration to minutes"
  (/ (duration-in-seconds dur) 60))

(defmethod duration-in-hours ((dur duration))
  "Convert duration to hours"
  (/ (duration-in-seconds dur) 3600))

(defmethod duration-in-days ((dur duration))
  "Convert duration to days"
  (let ((value (duration-value dur)))
    (ecase (duration-unit dur)
      (:seconds (/ value 86400.0))
      (:minutes (/ value 1440.0))
      (:hours (/ value 24.0))
      (:days value)
      (:weeks (* value 7))
      (:months (* value 30))  ; Approximate
      (:years (* value 365))))) ; Approximate

(defmethod duration-in-weeks ((dur duration))
  "Convert duration to weeks"
  (/ (duration-in-days dur) 7))

(defun date+ (date duration)
  "Add duration to date"
  (let* ((seconds (duration-in-seconds duration))
         (days (floor (/ seconds 86400)))
         (remaining-seconds (- seconds (* days 86400))))
    (make-instance 'pj-date
                   :timestamp (local-time:timestamp+
                                (local-time:timestamp+ (date-timestamp date)
                                                      days :day)
                                remaining-seconds :sec))))

(defun date- (date duration)
  "Subtract duration from date"
  (let* ((seconds (duration-in-seconds duration))
         (days (floor (/ seconds 86400)))
         (remaining-seconds (- seconds (* days 86400))))
    (make-instance 'pj-date
                   :timestamp (local-time:timestamp-
                                (local-time:timestamp- (date-timestamp date)
                                                      days :day)
                                remaining-seconds :sec))))

(defun date< (d1 d2)
  "Test if d1 is before d2"
  (local-time:timestamp< (date-timestamp d1) (date-timestamp d2)))

(defun date<= (d1 d2)
  "Test if d1 is before or equal to d2"
  (local-time:timestamp<= (date-timestamp d1) (date-timestamp d2)))

(defun date> (d1 d2)
  "Test if d1 is after d2"
  (local-time:timestamp> (date-timestamp d1) (date-timestamp d2)))

(defun date>= (d1 d2)
  "Test if d1 is after or equal to d2"
  (local-time:timestamp>= (date-timestamp d1) (date-timestamp d2)))

(defun date= (d1 d2)
  "Test date equality"
  (local-time:timestamp= (date-timestamp d1) (date-timestamp d2)))

(defclass interval ()
  ((start :initarg :start :reader interval-start
          :documentation "Interval start date")
   (end :initarg :end :reader interval-end
        :documentation "Interval end date"))
  (:documentation "Time interval"))

(defun interval (start end)
  "Create interval"
  (make-instance 'interval :start start :end end))

(defun interval-duration-days (interval)
  "Duration of interval in days"
  (let ((diff (local-time:timestamp-difference
                (date-timestamp (interval-end interval))
                (date-timestamp (interval-start interval)))))
    ;; Return integer days, rounded
    (round (/ diff 86400))))

(defun interval-duration-hours (interval)
  "Duration of interval in hours"
  (let ((diff (local-time:timestamp-difference
                (date-timestamp (interval-end interval))
                (date-timestamp (interval-start interval)))))
    (/ diff 3600.0)))

(defun contains-date-p (interval date)
  "Check if interval contains date"
  (and (date<= (interval-start interval) date)
       (date< date (interval-end interval))))

(defun overlaps-p (interval1 interval2)
  "Check if two intervals overlap"
  (or (contains-date-p interval1 (interval-start interval2))
      (contains-date-p interval1 (interval-end interval2))
      (contains-date-p interval2 (interval-start interval1))
      (contains-date-p interval2 (interval-end interval1))))
