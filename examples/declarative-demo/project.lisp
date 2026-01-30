;;;; examples/declarative-demo/project.lisp
;;;;
;;;; A clean project schedule file - pure data, no imperative code.
;;;; Time tracking is in a separate file (timesheets.lisp).
;;;;
;;;; This demonstrates the TaskJuggler-style "file as object" approach.

;;; Load the claps system (skip if already loaded, e.g., via CLI)
(unless (find-package :claps)
  (let ((project-root (make-pathname :directory (butlast (butlast (pathname-directory *load-truename*))))))
    (push project-root asdf:*central-registry*))

  ;; Load quicklisp if available
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))

  ;; Load claps
  (ql:quickload :claps :silent t))

(in-package :claps)

(defproject demo "Declarative Demo Project"
  :start (date 2024 11 1)
  :end (date 2024 12 31)

  ;; Resources
  (defresource alice "Alice Developer"
    :efficiency 1.0)

  (defresource bob "Bob Developer"
    :efficiency 0.9)

  ;; Tasks - clean schedule definition
  (deftask design "System Design"
    :effort (duration 40 :hours)
    :allocate (alice)
    :complete 100                          ; Declarative completion
    :actual-start (date 2024 11 1)         ; Declarative actual dates
    :actual-end (date 2024 11 8))

  (deftask backend "Backend Development"
    :effort (duration 80 :hours)
    :allocate (alice bob)
    :depends-on (design)
    :complete 50
    :actual-start (date 2024 11 11)
    ;; Inline bookings option - can also be in separate file
    :bookings ((alice (date 2024 11 11) 8)
               (alice (date 2024 11 12) 8)
               (bob (date 2024 11 11) 6)
               (bob (date 2024 11 12) 6)))

  (deftask frontend "Frontend Development"
    :effort (duration 60 :hours)
    :allocate (bob)
    :depends-on (design)
    :complete 25)

  (deftask launch "Product Launch"
    :milestone t
    :depends-on (backend frontend))

  ;; Reports
  (defreport summary "Project Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :complete)))

;; Load timesheet data from separate file (optional)
;; (load "timesheets.lisp")

;; Finalize and schedule
(finalize-project *current-project*)
(schedule *current-project*)

;; Print summary
(format t "~%Project: ~A~%" (project-name *current-project*))
(format t "Tasks: ~D~%" (hash-table-count (project-tasks *current-project*)))
(maphash (lambda (id task)
           (declare (ignore id))
           (format t "  ~A: ~D%% complete~%"
                   (task-name task)
                   (or (task-complete task) 0))
           (when (task-actual-start task)
             (format t "    Actual: ~A to ~A~%"
                     (task-actual-start task)
                     (or (task-actual-end task) "ongoing"))))
         (project-tasks *current-project*))
