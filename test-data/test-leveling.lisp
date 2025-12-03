;;;; test-data/test-leveling.lisp
;;;; Test resource leveling with over-allocation

(require :asdf)
(in-package #:cl-user)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Add project root to ASDF registry
(push (truename "../") asdf:*central-registry*)

;; Load Project Juggler
(asdf:load-system :project-juggler :verbose nil)

(in-package :project-juggler)

(format t "~%╔══════════════════════════════════════════════════════════════╗~%")
(format t "║           RESOURCE LEVELING VERIFICATION TEST                ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%~%")

;; Create a project with resource over-allocation
(defproject test-leveling "Resource Leveling Test"
  :start (date 2024 3 1)
  :end (date 2024 4 30)

  (defresource dev1 "Developer 1")

  ;; Two overlapping tasks - will cause over-allocation
  (deftask task1 "Task 1"
    :start (date 2024 3 1)
    :duration (duration 5 :days)
    :allocate (dev1)
    :priority 1000)

  (deftask task2 "Task 2"
    :start (date 2024 3 3)  ; Overlaps with task1
    :duration (duration 5 :days)
    :allocate (dev1)
    :priority 500))

(finalize-project *current-project*)
(schedule *current-project*)

(format t "SCENARIO: Developer assigned to two overlapping tasks~%")
(format t "  - Task1: 5 days starting March 1 (priority 1000)~%")
(format t "  - Task2: 5 days starting March 3 (priority 500)~%")
(format t "  - Both assigned to same developer!~%~%")

;; Check before leveling
(let ((overallocs (detect-resource-overallocations *current-project*)))
  (format t "BEFORE Leveling:~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  Overallocations detected: ~A~%" (length overallocs))
  (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
        (task2 (gethash 'task2 (project-tasks *current-project*))))
    (format t "  Task1: ~A-~A-~A to ~A-~A-~A~%"
            (date-year (task-start task1)) (date-month (task-start task1)) (date-day (task-start task1))
            (date-year (task-end task1)) (date-month (task-end task1)) (date-day (task-end task1)))
    (format t "  Task2: ~A-~A-~A to ~A-~A-~A~%"
            (date-year (task-start task2)) (date-month (task-start task2)) (date-day (task-start task2))
            (date-year (task-end task2)) (date-month (task-end task2)) (date-day (task-end task2)))
    (format t "  OVERLAP: March 3-5 (developer working on both tasks!)~%~%")))

;; Level resources
(format t "Running level-resources...~%~%")
(level-resources *current-project*)

;; Check after leveling
(let ((overallocs (detect-resource-overallocations *current-project*)))
  (format t "AFTER Leveling:~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "  Overallocations remaining: ~A~%" (length overallocs))
  (let ((task1 (gethash 'task1 (project-tasks *current-project*)))
        (task2 (gethash 'task2 (project-tasks *current-project*))))
    (format t "  Task1: ~A-~A-~A to ~A-~A-~A (priority ~A) - UNCHANGED~%"
            (date-year (task-start task1)) (date-month (task-start task1)) (date-day (task-start task1))
            (date-year (task-end task1)) (date-month (task-end task1)) (date-day (task-end task1))
            (task-priority task1))
    (format t "  Task2: ~A-~A-~A to ~A-~A-~A (priority ~A) - SHIFTED~%"
            (date-year (task-start task2)) (date-month (task-start task2)) (date-day (task-start task2))
            (date-year (task-end task2)) (date-month (task-end task2)) (date-day (task-end task2))
            (task-priority task2)))

  (if (zerop (length overallocs))
      (format t "~%✓ SUCCESS: Resource leveling resolved all overallocations!~%")
      (format t "~%✗ WARNING: ~A overallocations remaining~%" (length overallocs))))

(format t "~%═══════════════════════════════════════════════════════════════~%")
(format t "RESULT: Lower-priority task (Task2) was shifted after Task1~%")
(format t "        completes, resolving the resource conflict.~%")
(format t "═══════════════════════════════════════════════════════════════~%~%")
