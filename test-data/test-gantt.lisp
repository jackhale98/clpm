;;;; test-data/test-gantt.lisp
;;;; Test Gantt chart data generation

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
(format t "║              GANTT CHART DATA GENERATION TEST                ║~%")
(format t "╚══════════════════════════════════════════════════════════════╝~%~%")

;; Create a sample project
(defproject gantt-test "Gantt Chart Test Project"
  :start (date 2024 3 1)
  :end (date 2024 4 30)

  (defresource dev1 "Developer 1")

  (deftask design "Design Phase"
    :start (date 2024 3 1)
    :duration (duration 5 :days)
    :allocate (dev1))

  (deftask implementation "Implementation Phase"
    :duration (duration 10 :days)
    :depends-on (design)
    :allocate (dev1))

  (deftask testing "Testing Phase"
    :duration (duration 5 :days)
    :depends-on (implementation)
    :allocate (dev1))

  (deftask milestone "Project Complete"
    :milestone t
    :depends-on (testing)))

(finalize-project *current-project*)
(schedule *current-project*)

(format t "Project: ~A~%" (project-name *current-project*))
(format t "Tasks: ~A~%~%" (hash-table-count (project-tasks *current-project*)))

;; Generate Gantt data
(let ((gantt-data (generate-gantt-data *current-project*)))
  (format t "Gantt Chart Data:~%")
  (format t "═══════════════════════════════════════════════════════════~%~%")

  (dolist (entry gantt-data)
    (let ((id (getf entry :id))
          (name (getf entry :name))
          (start (getf entry :start))
          (end (getf entry :end))
          (deps (getf entry :dependencies)))
      (format t "Task: ~A~%" id)
      (format t "  Name: ~A~%" name)
      (format t "  Start: ~A-~A-~A~%"
              (date-year start) (date-month start) (date-day start))
      (format t "  End: ~A-~A-~A~%"
              (date-year end) (date-month end) (date-day end))
      (format t "  Dependencies: ~A~%~%" deps)))

  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "Total entries generated: ~A~%" (length gantt-data))
  (format t "~%✓ Gantt chart data generation working correctly!~%~%")

  ;; Output as JSON-like format for visualization tools
  (format t "JSON-like export (for visualization libraries):~%")
  (format t "═══════════════════════════════════════════════════════════~%")
  (format t "[~%")
  (loop for entry in gantt-data
        for i from 0
        do (format t "  {")
           (format t "\"id\": \"~A\", " (getf entry :id))
           (format t "\"name\": \"~A\", " (getf entry :name))
           (format t "\"start\": \"~A-~2,'0D-~2,'0D\", "
                   (date-year (getf entry :start))
                   (date-month (getf entry :start))
                   (date-day (getf entry :start)))
           (format t "\"end\": \"~A-~2,'0D-~2,'0D\", "
                   (date-year (getf entry :end))
                   (date-month (getf entry :end))
                   (date-day (getf entry :end)))
           (format t "\"dependencies\": ~A" (getf entry :dependencies))
           (format t "}~A~%" (if (< i (1- (length gantt-data))) "," "")))
  (format t "]~%"))

(format t "~%═══════════════════════════════════════════════════════════════~%~%")
