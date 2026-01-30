;;;; examples/modular-project/main.lisp
;;;;
;;;; Demonstrates modular project organization using include.
;;;; Similar to TaskJuggler's ability to split projects across files.
;;;;
;;;; This is the main project file that includes:
;;;;   - resources/team.lisp      - Team resource definitions
;;;;   - phases/backend.lisp      - Backend development tasks (namespaced)
;;;;   - phases/frontend.lisp     - Frontend development tasks (namespaced)
;;;;   - timesheets/november.lisp - Time tracking entries

;;; Load the claps system (skip if already loaded, e.g., via CLI)
(require :asdf)
(unless (find-package :claps)
  (in-package #:cl-user)

  ;; Add project root to ASDF registry
  (let ((project-root (make-pathname :directory (butlast (butlast (pathname-directory *load-truename*))))))
    (push project-root asdf:*central-registry*))

  ;; Load Quicklisp if available
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))

  ;; Load claps
  (handler-case
      (asdf:load-system :claps :verbose nil)
    (error (e)
      (format t "Error loading claps: ~A~%" e)
      (uiop:quit 1))))

(in-package :claps)

(format t "~%============================================================~%")
(format t "        MODULAR PROJECT ORGANIZATION DEMO~%")
(format t "============================================================~%~%")

;;; ============================================================================
;;; Main Project Definition
;;; ============================================================================

(defproject saas-platform "SaaS Platform Development"
  :start (date 2024 11 1)
  :end (date 2025 3 31)

  ;; Include team resources from separate file
  (include "resources/team.lisp")

  ;; Include backend tasks with namespace prefix
  ;; All task IDs will be prefixed: backend/api, backend/database, etc.
  (include "phases/backend.lisp" :namespace backend)

  ;; Include frontend tasks with namespace prefix
  (include "phases/frontend.lisp" :namespace frontend)

  ;; Integration phase (defined in main file)
  (deftask integration "System Integration"
    :effort (duration 40 :hours)
    :depends-on (backend/api frontend/ui)
    :allocate (fullstack-dev))

  ;; Launch milestone
  (deftask launch "Platform Launch"
    :milestone t
    :depends-on (integration))

  ;; Reports
  (defreport summary "Project Summary"
    :type :task
    :format :html
    :columns (:id :name :start :end :complete)))

;; Include time tracking from separate file (optional)
;; Uncomment to load actual time entries:
;; (include "timesheets/november.lisp")

;;; ============================================================================
;;; Finalize and Schedule
;;; ============================================================================

(format t "Loading modular project...~%")
(format t "  - Main project file: main.lisp~%")
(format t "  - Resources from: resources/team.lisp~%")
(format t "  - Backend tasks from: phases/backend.lisp (namespace: backend)~%")
(format t "  - Frontend tasks from: phases/frontend.lisp (namespace: frontend)~%~%")

(finalize-project *current-project*)
(schedule *current-project*)

;;; ============================================================================
;;; Display Results
;;; ============================================================================

(format t "Project: ~A~%" (project-name *current-project*))
(format t "Period: ~A to ~A~%~%"
        (project-start *current-project*)
        (project-end *current-project*))

(format t "Resources (~D total):~%"
        (hash-table-count (project-resources *current-project*)))
(maphash (lambda (id resource)
           (format t "  ~A: ~A~%"
                   id (resource-name resource)))
         (project-resources *current-project*))

(format t "~%Tasks (~D total):~%"
        (hash-table-count (project-tasks *current-project*)))
(let ((tasks nil))
  (maphash (lambda (id task)
             (declare (ignore id))
             (push task tasks))
           (project-tasks *current-project*))
  (dolist (task (sort tasks #'date< :key #'task-start))
    (if (task-milestone-p task)
        (format t "  [M] ~A~%" (task-name task))
        (format t "  ~A: ~A~%"
                (task-id task)
                (task-name task)))))

(format t "~%============================================================~%")
(format t "Benefits of modular organization:~%")
(format t "  - Team resources defined separately (easy to update)~%")
(format t "  - Backend/Frontend phases in separate files~%")
(format t "  - Namespace prefixes prevent ID conflicts~%")
(format t "  - Time tracking kept separate from schedule~%")
(format t "  - Each file can be version controlled independently~%")
(format t "============================================================~%~%")
