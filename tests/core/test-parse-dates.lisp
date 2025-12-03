;;;; tests/core/test-parse-dates.lisp
;;;; Tests for flexible date parsing (strings and date objects)

(in-package :project-juggler-tests)

;;; =============================================================================
;;; ISO 8601 Date String Parsing Tests
;;; =============================================================================

(define-test test-parse-iso-date-basic ()
  "Parse basic ISO 8601 date string YYYY-MM-DD"
  (let ((date (parse-date-flexible "2024-01-15")))
    (assert-true (typep date 'pj-date))
    (assert-equal 2024 (date-year date))
    (assert-equal 1 (date-month date))
    (assert-equal 15 (date-day date))
    (assert-equal 0 (date-hour date))
    (assert-equal 0 (date-minute date))
    (assert-equal 0 (date-second date))))

(define-test test-parse-iso-date-with-time ()
  "Parse ISO 8601 date with time YYYY-MM-DDTHH:MM:SS"
  (let ((date (parse-date-flexible "2024-01-15T14:30:00")))
    (assert-equal 2024 (date-year date))
    (assert-equal 1 (date-month date))
    (assert-equal 15 (date-day date))
    (assert-equal 14 (date-hour date))
    (assert-equal 30 (date-minute date))
    (assert-equal 0 (date-second date))))

(define-test test-parse-iso-date-with-hour-minute ()
  "Parse ISO 8601 date with hour and minute YYYY-MM-DDTHH:MM"
  (let ((date (parse-date-flexible "2024-01-15T14:30")))
    (assert-equal 14 (date-hour date))
    (assert-equal 30 (date-minute date))
    (assert-equal 0 (date-second date))))

(define-test test-parse-iso-date-different-dates ()
  "Test parsing various valid dates"
  (let ((dates '("2024-12-31" "2024-01-01" "2024-06-15" "2025-03-20")))
    (dolist (date-str dates)
      (let ((date (parse-date-flexible date-str)))
        (assert-true (typep date 'pj-date)
                     "Failed to parse: ~A" date-str)))))

(define-test test-parse-iso-date-leading-zeros ()
  "Parse dates with leading zeros"
  (let ((date (parse-date-flexible "2024-03-05")))
    (assert-equal 3 (date-month date))
    (assert-equal 5 (date-day date))))

(define-test test-parse-iso-date-no-leading-zeros ()
  "Parse dates without leading zeros (if supported)"
  ;; Some systems accept "2024-3-5" format
  ;; We'll make this flexible
  (handler-case
      (let ((date (parse-date-flexible "2024-3-5")))
        (assert-equal 3 (date-month date))
        (assert-equal 5 (date-day date)))
    (error () :ok)))  ; It's OK if this format isn't supported

;;; =============================================================================
;;; Date Object Pass-Through Tests
;;; =============================================================================

(define-test test-parse-date-object-passthrough ()
  "Passing a date object should return it unchanged"
  (let* ((date-obj (date 2024 1 15))
         (parsed (parse-date-flexible date-obj)))
    (assert-eq date-obj parsed)))

(define-test test-parse-date-object-with-time ()
  "Pass through date object with time"
  (let* ((date-obj (date 2024 1 15 14 30 0))
         (parsed (parse-date-flexible date-obj)))
    (assert-eq date-obj parsed)))

;;; =============================================================================
;;; Error Handling Tests
;;; =============================================================================

(define-test test-parse-invalid-date-format ()
  "Invalid date format should raise an error"
  (handler-case
      (progn
        (parse-date-flexible "not-a-date")
        (fail "Should have raised an error for invalid date"))
    (error (e)
      (assert-true (search "Invalid date" (format nil "~A" e))))))

(define-test test-parse-invalid-date-numbers ()
  "Invalid date numbers should raise an error"
  (handler-case
      (progn
        (parse-date-flexible "2024-13-01")  ; Month 13 doesn't exist
        (fail "Should have raised an error for month 13"))
    (error ()
      :ok)))

(define-test test-parse-invalid-type ()
  "Invalid type should raise an error"
  (handler-case
      (progn
        (parse-date-flexible 123456)  ; Number, not string or date
        (fail "Should have raised an error for number"))
    (error (e)
      (assert-true (search "Invalid date" (format nil "~A" e))))))

(define-test test-parse-nil-date ()
  "Nil date should raise an error"
  (handler-case
      (progn
        (parse-date-flexible nil)
        (fail "Should have raised an error for nil"))
    (error ()
      :ok)))

;;; =============================================================================
;;; Edge Cases
;;; =============================================================================

(define-test test-parse-leap-year-date ()
  "Parse February 29 in leap year"
  (let ((date (parse-date-flexible "2024-02-29")))
    (assert-equal 2 (date-month date))
    (assert-equal 29 (date-day date))))

(define-test test-parse-year-boundaries ()
  "Parse dates at year boundaries"
  (let ((new-year (parse-date-flexible "2024-01-01"))
        (new-eve (parse-date-flexible "2024-12-31")))
    (assert-equal 1 (date-month new-year))
    (assert-equal 1 (date-day new-year))
    (assert-equal 12 (date-month new-eve))
    (assert-equal 31 (date-day new-eve))))

(define-test test-parse-midnight ()
  "Parse midnight time"
  (let ((date (parse-date-flexible "2024-01-15T00:00:00")))
    (assert-equal 0 (date-hour date))
    (assert-equal 0 (date-minute date))
    (assert-equal 0 (date-second date))))

(define-test test-parse-end-of-day ()
  "Parse end of day time"
  (let ((date (parse-date-flexible "2024-01-15T23:59:59")))
    (assert-equal 23 (date-hour date))
    (assert-equal 59 (date-minute date))
    (assert-equal 59 (date-second date))))

;;; =============================================================================
;;; Integration Tests
;;; =============================================================================

(define-test test-parse-date-in-defproject ()
  "Use string dates in defproject macro"
  (defproject test-proj "Test Project"
    :start "2024-01-15"
    :end "2024-06-30")

  (let ((start (project-start *current-project*))
        (end (project-end *current-project*)))
    (assert-equal 2024 (date-year start))
    (assert-equal 1 (date-month start))
    (assert-equal 15 (date-day start))
    (assert-equal 6 (date-month end))
    (assert-equal 30 (date-day end))))

(define-test test-parse-date-in-deftask ()
  "Use string dates in deftask macro"
  (defproject test-proj "Test" :start "2024-01-01" :end "2024-12-31"
    (deftask t1 "Task 1"
      :start "2024-02-15"
      :duration (duration 5 :days)))

  (let ((task (gethash 't1 (project-tasks *current-project*))))
    (assert-equal 2 (date-month (task-start task)))
    (assert-equal 15 (date-day (task-start task)))))

;;; =============================================================================
;;; Performance Tests
;;; =============================================================================

(define-test test-parse-date-performance ()
  "Parsing many dates should be reasonably fast"
  (let ((start-time (get-internal-real-time)))
    (dotimes (i 1000)
      (parse-date-flexible "2024-01-15"))
    (let ((elapsed (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)))
      ;; Should parse 1000 dates in less than 1 second
      (assert-true (< elapsed 1.0)
                   "Date parsing too slow: ~A seconds for 1000 parses" elapsed))))
