;;;; tests/test-utils.lisp
;;;; Test utilities and helper functions

(in-package #:project-juggler-tests)

;;; Test fixtures and utilities

(defmacro with-test-project (&body body)
  "Execute body with temporary test project"
  `(let* ((*current-project* (make-test-project))
          (*current-namespace* (make-test-namespace))
          (*namespace-registry* (make-hash-table :test 'eq))
          (*project-registry* (make-hash-table :test 'eq)))
     (setf (gethash nil *namespace-registry*) *current-namespace*)
     (setf (gethash 'test *project-registry*) *current-project*)
     ,@body))

(defun make-test-project ()
  "Create a minimal test project with default scenario"
  (let ((project (make-instance 'project
                                :id 'test
                                :name "Test Project"
                                :start (date 2024 1 1)
                                :end (date 2024 12 31))))
    ;; Create default 'plan scenario (TaskJuggler-style)
    (setf (project-scenarios project)
          (list (make-instance 'scenario :id 'plan :name "PLAN")))
    (setf (project-current-scenario project) 'plan)
    project))

(defun make-test-namespace ()
  "Create a test namespace"
  (make-instance 'namespace :name nil))

(defmacro with-test-session (&body body)
  "Execute body with temporary test session"
  `(let* ((*namespace-registry* (make-hash-table :test 'eq))
          (*project-registry* (make-hash-table :test 'eq))
          (*current-namespace* (make-instance 'namespace :name nil))
          (*current-project* (make-test-project-with-tasks))
          (*current-session* (make-instance 'session :project *current-project*)))
     (setf (gethash nil *namespace-registry*) *current-namespace*)
     (finalize-project *current-project*)
     (schedule *current-project*)
     ,@body))

(defun make-test-session ()
  "Create a test session"
  (let* ((*namespace-registry* (make-hash-table :test 'eq))
         (*project-registry* (make-hash-table :test 'eq))
         (*current-namespace* (make-instance 'namespace :name nil))
         (*current-project* (make-test-project-with-tasks)))
    (setf (gethash nil *namespace-registry*) *current-namespace*)
    (finalize-project *current-project*)
    (schedule *current-project*)
    (make-instance 'session :project *current-project*)))

(defun make-test-project-with-tasks ()
  "Create a test project with tasks"
  (let* ((project (make-instance 'project
                                :id 'test
                                :name "Test Project"
                                :start (date 2024 3 1)
                                :end (date 2024 12 31)))
         (t1 (make-instance 'task
                           :id 't1
                           :name "Task 1"
                           :project project
                           :start (date 2024 3 1)
                           :duration (duration 5 :days)))
         (t2 (make-instance 'task
                           :id 't2
                           :name "Task 2"
                           :project project
                           :duration (duration 3 :days)))
         (dep (make-instance 'dependency
                            :source t2
                            :target-ref 't1
                            :target t1)))
    ;; Create default 'plan scenario (TaskJuggler-style)
    (setf (project-scenarios project)
          (list (make-instance 'scenario :id 'plan :name "PLAN")))
    (setf (project-current-scenario project) 'plan)
    (setf (gethash 't1 (project-tasks project)) t1)
    (setf (gethash 't2 (project-tasks project)) t2)
    (setf (task-dependencies t2) (list dep))
    project))

(defmacro with-temp-file ((var content) &body body)
  "Create temporary file with content"
  `(let ((,var (make-pathname :name (format nil "temp-~A" (gensym))
                              :type "lisp"
                              :defaults (user-homedir-pathname))))
     (with-open-file (out ,var :direction :output :if-exists :supersede)
       (write-string ,content out))
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,var)
         (delete-file ,var)))))

(defun date= (d1 d2)
  "Test date equality"
  (and (= (date-year d1) (date-year d2))
       (= (date-month d1) (date-month d2))
       (= (date-day d1) (date-day d2))))

(defun date< (d1 d2)
  "Test if d1 is before d2"
  (local-time:timestamp< (date-timestamp d1) (date-timestamp d2)))

(defun date> (d1 d2)
  "Test if d1 is after d2"
  (local-time:timestamp> (date-timestamp d1) (date-timestamp d2)))

(defun find-task (id &optional (project *current-project*))
  "Find task by ID in project"
  (gethash id (project-tasks project)))

(defun find-resource (id &optional (project *current-project*))
  "Find resource by ID in project"
  (gethash id (project-resources project)))

(defmacro with-temp-project-file ((var) &body body)
  "Create temporary project file for testing"
  `(with-temp-file (,var "(defproject test-project \"Test Project\"
  :start (date 2024 3 1)
  :end (date 2024 12 31)

  (deftask t1 \"Task 1\"
    :duration (duration 5 :days))

  (deftask t2 \"Task 2\"
    :depends-on (t1)
    :duration (duration 3 :days)))")
     ,@body))
