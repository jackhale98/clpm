;;;; tests/core/test-types.lisp
;;;; Tests for temporal types (Phase 0 basic tests, Phase 1 detailed tests)

(in-package #:project-juggler-tests)

(in-suite types-suite)

;;; Phase 0: Basic tests to verify system loads

(test basic-package-loads
  "Package loads without errors"
  (is (find-package :project-juggler))
  (is (find-package :pj)))

(test can-create-basic-types
  "Can create date and duration"
  (is (date 2024 1 1))
  (is (duration 5 :days)))

;;; Phase 1: Detailed TDD tests (to be written next)

(test date-creation
  "Can create dates"
  :depends-on (and basic-package-loads can-create-basic-types)
  (let ((d (date 2024 3 15)))
    (is (= 2024 (date-year d)))
    (is (= 3 (date-month d)))
    (is (= 15 (date-day d)))))

(test date-with-time
  "Can create dates with time"
  :depends-on date-creation
  (let ((d (date 2024 3 15 14 30 45)))
    (is (= 2024 (date-year d)))
    (is (= 3 (date-month d)))
    (is (= 15 (date-day d)))
    (is (= 14 (date-hour d)))
    (is (= 30 (date-minute d)))
    (is (= 45 (date-second d)))))

(test duration-creation
  "Can create durations in various units"
  :depends-on can-create-basic-types
  (is (duration 5 :days))
  (is (duration 40 :hours))
  (is (duration 2 :weeks))
  (is (duration 30 :minutes))
  (is (duration 3 :months))
  (is (duration 1 :years)))

(test duration-conversion-days
  "Can convert durations to days"
  :depends-on duration-creation
  (let ((d (duration 2 :weeks)))
    (is (= 14 (duration-in-days d))))
  (let ((d (duration 48 :hours)))
    (is (= 2 (duration-in-days d)))))

(test duration-conversion-hours
  "Can convert durations to hours"
  :depends-on duration-creation
  (let ((d (duration 2 :days)))
    (is (= 48 (duration-in-hours d))))
  (let ((d (duration 120 :minutes)))
    (is (= 2 (duration-in-hours d)))))

(test duration-conversion-minutes
  "Can convert durations to minutes"
  :depends-on duration-creation
  (let ((d (duration 2 :hours)))
    (is (= 120 (duration-in-minutes d)))))

(test duration-conversion-seconds
  "Can convert durations to seconds"
  :depends-on duration-creation
  (let ((d (duration 2 :minutes)))
    (is (= 120 (duration-in-seconds d)))))

(test date-arithmetic-addition
  "Can add durations to dates"
  :depends-on (and date-creation duration-creation)
  (let ((d1 (date 2024 3 15))
        (dur (duration 5 :days)))
    (let ((d2 (date+ d1 dur)))
      (is (= 20 (date-day d2)))
      (is (= 3 (date-month d2)))
      (is (= 2024 (date-year d2))))))

(test date-arithmetic-subtraction
  "Can subtract durations from dates"
  :depends-on (and date-creation duration-creation)
  (let ((d1 (date 2024 3 15))
        (dur (duration 5 :days)))
    (let ((d2 (date- d1 dur)))
      (is (= 10 (date-day d2)))
      (is (= 3 (date-month d2)))
      (is (= 2024 (date-year d2))))))

(test date-comparison
  "Can compare dates"
  :depends-on date-creation
  (let ((d1 (date 2024 3 15))
        (d2 (date 2024 3 20))
        (d3 (date 2024 3 15)))
    (is (date< d1 d2))
    (is (not (date< d2 d1)))
    (is (date<= d1 d2))
    (is (date<= d1 d3))
    (is (date> d2 d1))
    (is (not (date> d1 d2)))
    (is (date>= d2 d1))
    (is (date>= d1 d3))
    (is (date= d1 d3))
    (is (not (date= d1 d2)))))

(test interval-creation
  "Can create and query intervals"
  :depends-on date-creation
  (let ((i (interval (date 2024 3 1) (date 2024 3 31))))
    (is (= 30 (interval-duration-days i)))
    (is (contains-date-p i (date 2024 3 15)))
    (is (not (contains-date-p i (date 2024 4 1))))))

(test interval-overlap
  "Can detect interval overlap"
  :depends-on interval-creation
  (let ((i1 (interval (date 2024 3 1) (date 2024 3 15)))
        (i2 (interval (date 2024 3 10) (date 2024 3 20)))
        (i3 (interval (date 2024 3 20) (date 2024 3 30))))
    (is (overlaps-p i1 i2))
    (is (overlaps-p i2 i1))
    (is (not (overlaps-p i1 i3)))))
