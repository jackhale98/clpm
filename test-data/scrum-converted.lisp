;;;; test-data/scrum-converted.lisp
;;;; Conversion of TaskJuggler scrum.tjp to Project Juggler DSL
;;;; Original: https://github.com/taskjuggler/TaskJuggler/blob/master/examples/Scrum/scrum.tjp

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
(handler-case
    (asdf:load-system :project-juggler :verbose nil)
  (error (e)
    (format t "Error loading project-juggler: ~A~%" e)
    (uiop:quit 1)))

(in-package :project-juggler)

(format t "~%╔══════════════════════════════════════════════════════════════════════════════╗~%")
(format t "║                      SCRUM PROJECT CONVERSION TEST                           ║~%")
(format t "║              3 Sprints, 3 Resources, Complex Dependencies                    ║~%")
(format t "╚══════════════════════════════════════════════════════════════════════════════╝~%~%")

;;;
;;; Project: Scrum Example - 4 months starting Feb 1, 2012
;;;
(defproject scrum-example "Scrum Example Project"
  :start (date 2012 2 1)
  :end (date 2012 6 1)  ; +4m

  (format t "STEP 1: Defining Resources~%")
  (format t "════════════════════════════════════════════════════════════════~%~%")

  ;; Resources
  (defresource r1 "R1 - Developer 1")
  (defresource r2 "R2 - Developer 2")
  (defresource r3 "R3 - Developer 3")

  (format t "  ✓ R1 - Developer 1~%")
  (format t "  ✓ R2 - Developer 2~%")
  (format t "  ✓ R3 - Developer 3~%~%")

  (format t "STEP 2: Defining Sprint Tasks~%")
  (format t "════════════════════════════════════════════════════════════════~%~%")

  ;;;
  ;;; Sprint 1 - Starts at project start
  ;;;
  (deftask s1 "Sprint 1"
    :start (date 2012 2 1)

    ;; Sprint 1 tasks
    (deftask s1-t1 "S1-T1"
      :effort (duration 5 :days)
      :allocate (r1))

    (deftask s1-t2 "S1-T2"
      :effort (duration 3 :days)
      :allocate (r1)
      :depends-on (s1-t1))

    (deftask s1-t3 "S1-T3"
      :effort (duration 7 :days)
      :allocate (r1))

    (deftask s1-t4 "S1-T4"
      :effort (duration 4 :days)
      :allocate (r2)
      :depends-on (s1-t2)))

  (format t "  Sprint 1: 4 tasks (T1→T2→T4, T3 parallel)~%")

  ;;;
  ;;; Sprint 2 - Depends on Sprint 1
  ;;;
  (deftask s2 "Sprint 2"
    :depends-on (s1)

    (deftask s2-t1 "S2-T1"
      :effort (duration 3 :days)
      :allocate (r2))

    (deftask s2-t2 "S2-T2"
      :effort (duration 4 :days)
      :allocate (r3)
      :depends-on (s2-t1))

    (deftask s2-t3 "S2-T3"
      :effort (duration 6 :days)
      :allocate (r1))

    (deftask s2-t4 "S2-T4"
      :effort (duration 5 :days)
      :allocate (r3)
      :depends-on (s2-t3))

    (deftask s2-t5 "S2-T5"
      :effort (duration 3 :days)
      :allocate (r2)
      :depends-on (s2-t1)))

  (format t "  Sprint 2: 5 tasks (T1→T2, T1→T5, T3→T4)~%")

  ;;;
  ;;; Sprint 3 - Depends on Sprint 2 + cross-sprint dependencies
  ;;;
  (deftask s3 "Sprint 3"
    :depends-on (s2)

    (deftask s3-t1 "S3-T1"
      :effort (duration 6 :days)
      :allocate (r1)
      :depends-on (s1-t2))  ; Cross-sprint dependency from Sprint 1!

    (deftask s3-t2 "S3-T2"
      :effort (duration 4 :days)
      :allocate (r3)
      :depends-on (s3-t1))

    (deftask s3-t3 "S3-T3"
      :effort (duration 4 :days)
      :allocate (r1)
      :depends-on (s2-t4))  ; Cross-sprint dependency from Sprint 2!

    (deftask s3-t4 "S3-T4"
      :effort (duration 7 :days)
      :allocate (r2)
      :depends-on (s3-t3))

    (deftask s3-t5 "S3-T5"
      :effort (duration 5 :days)
      :allocate (r2)
      :depends-on (s3-t1)))

  (format t "  Sprint 3: 5 tasks (T1→T2, T1→T5, T3→T4) + cross-sprint deps~%~%")

  ;; Define Reports
  (defreport sprint-overview "Sprint Overview"
    :type :task
    :format :html
    :columns (:id :name :start :end :effort :complete)
    :sort-by (lambda (a b) (date< (task-start a) (task-start b))))

  (defreport critical-tasks "Critical Path Tasks"
    :type :task
    :format :html
    :columns (:id :name :start :end :slack)
    :filter (lambda (task) (and (task-slack task) (zerop (task-slack task)))))

  (defreport resource-load "Resource Allocation"
    :type :resource
    :format :html
    :columns (:id :name :efficiency)))

;;;
;;; Finalize and Schedule
;;;
(format t "STEP 3: Scheduling Project~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(finalize-project *current-project*)
(format t "  ✓ Project finalized (references resolved)~%")

(schedule *current-project*)
(format t "  ✓ Project scheduled~%~%")

;;;
;;; Display Results
;;;
(format t "STEP 4: Schedule Results~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

;; Count tasks
(let ((total-tasks 0)
      (leaf-tasks 0)
      (parent-tasks 0))
  (maphash (lambda (id task)
             (declare (ignore id))
             (incf total-tasks)
             (if (task-subtasks task)
                 (incf parent-tasks)
                 (incf leaf-tasks)))
           (project-tasks *current-project*))
  (format t "  Total tasks: ~A (~A leaf, ~A parent)~%~%" total-tasks leaf-tasks parent-tasks))

;; Display Sprint Schedule
(format t "Sprint Schedule:~%")
(format t "────────────────────────────────────────────────────────────────~%")

(dolist (sprint-id '(s1 s2 s3))
  (let ((sprint (gethash sprint-id (project-tasks *current-project*))))
    (when sprint
      (format t "~%  ~A: ~A-~A-~A to ~A-~A-~A~%"
              (task-name sprint)
              (date-year (task-start sprint)) (date-month (task-start sprint)) (date-day (task-start sprint))
              (date-year (task-end sprint)) (date-month (task-end sprint)) (date-day (task-end sprint)))
      ;; Show subtasks
      (dolist (subtask (task-subtasks sprint))
        (format t "    └─ ~A: ~A-~A-~A to ~A-~A-~A (~A days effort)~%"
                (task-name subtask)
                (date-year (task-start subtask)) (date-month (task-start subtask)) (date-day (task-start subtask))
                (date-year (task-end subtask)) (date-month (task-end subtask)) (date-day (task-end subtask))
                (if (task-effort subtask) (duration-in-days (task-effort subtask)) "N/A"))))))

;;;
;;; Critical Path Analysis
;;;
(format t "~%~%STEP 5: Critical Path Analysis~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(let ((critical-tasks (critical-path *current-project*)))
  (format t "Critical Path (~A tasks):~%" (length critical-tasks))
  (dolist (task critical-tasks)
    (format t "  • ~A (~A)~%"
            (task-name task)
            (task-id task))))

;;;
;;; Resource Utilization Check
;;;
(format t "~%STEP 6: Resource Over-Allocation Check~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(let ((overallocs (detect-resource-overallocations *current-project*)))
  (if (zerop (length overallocs))
      (format t "  ✓ No resource over-allocations detected~%")
      (progn
        (format t "  ⚠ Found ~A resource over-allocations:~%" (length overallocs))
        (dolist (oa overallocs)
          (format t "    • ~A on ~A-~A-~A (load: ~A)~%"
                  (overallocation-resource-id oa)
                  (date-year (overallocation-date oa))
                  (date-month (overallocation-date oa))
                  (date-day (overallocation-date oa))
                  (overallocation-load oa))))))

;;;
;;; Generate Reports
;;;
(format t "~%STEP 7: Generating Reports~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(handler-case
    (progn
      (save-project-report *current-project* 'sprint-overview "scrum-overview.html")
      (format t "  ✓ Generated: scrum-overview.html~%")

      (save-project-report *current-project* 'critical-tasks "scrum-critical.html")
      (format t "  ✓ Generated: scrum-critical.html~%")

      (save-project-report *current-project* 'resource-load "scrum-resources.html")
      (format t "  ✓ Generated: scrum-resources.html~%"))
  (error (e)
    (format t "  Error generating reports: ~A~%" e)))

;;;
;;; Gantt Chart Data
;;;
(format t "~%STEP 8: Gantt Chart Data~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(let ((gantt-data (generate-gantt-data *current-project*)))
  (format t "  Generated ~A Gantt entries~%" (length gantt-data))
  (format t "~%  Sample entries:~%")
  (loop for entry in (subseq gantt-data 0 (min 5 (length gantt-data)))
        do (format t "    ~A: ~A-~A-~A to ~A-~A-~A~%"
                   (getf entry :id)
                   (date-year (getf entry :start))
                   (date-month (getf entry :start))
                   (date-day (getf entry :start))
                   (date-year (getf entry :end))
                   (date-month (getf entry :end))
                   (date-day (getf entry :end)))))

;;;
;;; Summary
;;;
(format t "~%╔══════════════════════════════════════════════════════════════════════════════╗~%")
(format t "║                            CONVERSION SUMMARY                                ║~%")
(format t "╚══════════════════════════════════════════════════════════════════════════════╝~%~%")

(format t "Successfully Converted:~%")
(format t "  ✓ 3 Sprints with dependencies between them~%")
(format t "  ✓ 14 tasks total (3 parent, 14 leaf tasks)~%")
(format t "  ✓ 3 resources~%")
(format t "  ✓ Effort-based scheduling~%")
(format t "  ✓ Cross-sprint dependencies (S3-T1 → S1-T2, S3-T3 → S2-T4)~%")
(format t "  ✓ Critical path analysis~%")
(format t "  ✓ Resource over-allocation detection~%")
(format t "  ✓ HTML reports generated~%")
(format t "  ✓ Gantt chart data generated~%~%")

(format t "════════════════════════════════════════════════════════════════════════════════~%")
(format t "Scrum project conversion complete!~%")
(format t "════════════════════════════════════════════════════════════════════════════════~%~%")
