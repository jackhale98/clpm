;;;; test-data/scrum-with-leveling.lisp
;;;; Scrum project with resource leveling applied

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

(format t "~%╔══════════════════════════════════════════════════════════════════════════════╗~%")
(format t "║              SCRUM PROJECT WITH RESOURCE LEVELING                            ║~%")
(format t "╚══════════════════════════════════════════════════════════════════════════════╝~%~%")

;;; Same project definition as before
(defproject scrum-example "Scrum Example Project"
  :start (date 2012 2 1)
  :end (date 2012 6 1)

  (defresource r1 "R1 - Developer 1")
  (defresource r2 "R2 - Developer 2")
  (defresource r3 "R3 - Developer 3")

  ;; Sprint 1
  (deftask s1 "Sprint 1"
    :start (date 2012 2 1)
    (deftask s1-t1 "S1-T1" :effort (duration 5 :days) :allocate (r1))
    (deftask s1-t2 "S1-T2" :effort (duration 3 :days) :allocate (r1) :depends-on (s1-t1))
    (deftask s1-t3 "S1-T3" :effort (duration 7 :days) :allocate (r1))
    (deftask s1-t4 "S1-T4" :effort (duration 4 :days) :allocate (r2) :depends-on (s1-t2)))

  ;; Sprint 2
  (deftask s2 "Sprint 2"
    :depends-on (s1)
    (deftask s2-t1 "S2-T1" :effort (duration 3 :days) :allocate (r2))
    (deftask s2-t2 "S2-T2" :effort (duration 4 :days) :allocate (r3) :depends-on (s2-t1))
    (deftask s2-t3 "S2-T3" :effort (duration 6 :days) :allocate (r1))
    (deftask s2-t4 "S2-T4" :effort (duration 5 :days) :allocate (r3) :depends-on (s2-t3))
    (deftask s2-t5 "S2-T5" :effort (duration 3 :days) :allocate (r2) :depends-on (s2-t1)))

  ;; Sprint 3
  (deftask s3 "Sprint 3"
    :depends-on (s2)
    (deftask s3-t1 "S3-T1" :effort (duration 6 :days) :allocate (r1) :depends-on (s1-t2))
    (deftask s3-t2 "S3-T2" :effort (duration 4 :days) :allocate (r3) :depends-on (s3-t1))
    (deftask s3-t3 "S3-T3" :effort (duration 4 :days) :allocate (r1) :depends-on (s2-t4))
    (deftask s3-t4 "S3-T4" :effort (duration 7 :days) :allocate (r2) :depends-on (s3-t3))
    (deftask s3-t5 "S3-T5" :effort (duration 5 :days) :allocate (r2) :depends-on (s3-t1))))

(finalize-project *current-project*)
(schedule *current-project*)

;;; Check BEFORE leveling
(format t "BEFORE Resource Leveling:~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(let ((overallocs-before (detect-resource-overallocations *current-project*)))
  (format t "  Over-allocations detected: ~A~%" (length overallocs-before))
  (dolist (oa overallocs-before)
    (format t "    • ~A on ~A-~A-~A (load: ~,1F = ~A tasks simultaneously!)~%"
            (overallocation-resource-id oa)
            (date-year (overallocation-date oa))
            (date-month (overallocation-date oa))
            (date-day (overallocation-date oa))
            (overallocation-load oa)
            (truncate (overallocation-load oa)))))

;;; Apply resource leveling
(format t "~%Applying level-resources...~%~%")
(level-resources *current-project*)

;;; Check AFTER leveling
(format t "AFTER Resource Leveling:~%")
(format t "════════════════════════════════════════════════════════════════~%~%")

(let ((overallocs-after (detect-resource-overallocations *current-project*)))
  (format t "  Over-allocations remaining: ~A~%" (length overallocs-after))
  (if (zerop (length overallocs-after))
      (format t "~%  ✓ SUCCESS: All resource conflicts resolved!~%")
      (progn
        (format t "~%  Remaining conflicts:~%")
        (dolist (oa overallocs-after)
          (format t "    • ~A on ~A-~A-~A (load: ~,1F)~%"
                  (overallocation-resource-id oa)
                  (date-year (overallocation-date oa))
                  (date-month (overallocation-date oa))
                  (date-day (overallocation-date oa))
                  (overallocation-load oa))))))

;;; Show revised schedule
(format t "~%~%Revised Sprint Schedule:~%")
(format t "════════════════════════════════════════════════════════════════~%")

(dolist (sprint-id '(s1 s2 s3))
  (let ((sprint (gethash sprint-id (project-tasks *current-project*))))
    (when sprint
      (format t "~%  ~A: ~A-~A-~A to ~A-~A-~A~%"
              (task-name sprint)
              (date-year (task-start sprint)) (date-month (task-start sprint)) (date-day (task-start sprint))
              (date-year (task-end sprint)) (date-month (task-end sprint)) (date-day (task-end sprint)))
      (dolist (subtask (sort (copy-list (task-subtasks sprint))
                             (lambda (a b) (date< (task-start a) (task-start b)))))
        (let* ((allocs (task-allocations subtask))
               (resource-names (when allocs
                                (mapcar (lambda (r) (resource-name r))
                                        (allocation-resources (first allocs))))))
          (format t "    ~A: ~A-~A-~A to ~A-~A-~A [~{~A~^, ~}]~%"
                  (task-name subtask)
                  (date-year (task-start subtask)) (date-month (task-start subtask)) (date-day (task-start subtask))
                  (date-year (task-end subtask)) (date-month (task-end subtask)) (date-day (task-end subtask))
                  resource-names))))))

(format t "~%~%════════════════════════════════════════════════════════════════════════════════~%")
(format t "Resource leveling test complete!~%")
(format t "════════════════════════════════════════════════════════════════════════════════~%~%")
