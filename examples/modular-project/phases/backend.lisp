;;;; examples/modular-project/phases/backend.lisp
;;;;
;;;; Backend development phase tasks.
;;;; This file is included with :namespace backend, so all task IDs
;;;; will be prefixed: api -> backend/api, database -> backend/database, etc.
;;;;
;;;; This allows multiple teams to use simple task names without conflicts.

(in-package :claps)

;;; ============================================================================
;;; Backend Phase Tasks
;;; ============================================================================

;; Note: These tasks will get prefixed IDs when included with :namespace backend

;; Database comes first (no dependencies)
(deftask database "Database Schema & ORM"
  :effort (duration 40 :hours)
  :allocate (backend-dev)
  :priority 900)

;; API development (parallel to database)
(deftask api "REST API Development"
  :effort (duration 60 :hours)
  :allocate (backend-dev)
  :priority 800)

;; Auth depends on database - using full prefixed name since we're in namespace
;; Note: Within a namespace, dependencies need the full prefixed name
(deftask auth "Authentication System"
  :effort (duration 30 :hours)
  :depends-on (backend/database)
  :allocate (backend-dev)
  :priority 850)

;; Testing depends on both api and auth
(deftask testing "Backend Testing"
  :effort (duration 20 :hours)
  :depends-on (backend/api backend/auth)
  :allocate (qa-engineer)
  :priority 700)
