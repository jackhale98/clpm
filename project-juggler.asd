;;;; project-juggler.asd
;;;; ASDF system definition for Project Juggler

(defsystem "project-juggler"
  :description "Modern TaskJuggler replacement in Common Lisp"
  :version "1.0.0"
  :author "Project Juggler Team"
  :license "MIT"
  :depends-on (#:local-time
               #:cl-ppcre
               #:alexandria
               #:split-sequence)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:module "core"
                  :serial t
                  :components
                  ((:file "types")
                   (:file "classes")
                   (:file "protocols")
                   (:file "errors")))
                 (:module "namespace"
                  :serial t
                  :components
                  ((:file "namespace")
                   (:file "references")
                   (:file "include")))
                 (:module "dsl"
                  :serial t
                  :components
                  ((:file "defproject")
                   (:file "deftask")
                   (:file "defresource")
                   (:file "defaccount")
                   (:file "dependencies")
                   (:file "allocations")
                   (:file "defreport")))
                 (:module "validation"
                  :serial t
                  :components
                  ((:file "validation")
                   (:file "circular-deps")
                   (:file "finalization")))
                 (:module "scheduling"
                  :serial t
                  :components
                  ((:file "scheduler")
                   (:file "criticalness")
                   (:file "critical-path")
                   (:file "resource-allocation")
                   (:file "calendars")))
                 (:module "session"
                  :serial t
                  :components
                  ((:file "session")
                   (:file "changes")
                   (:file "persistence")
                   (:file "undo-redo")))
                 (:module "tracking"
                  :serial t
                  :components
                  ((:file "baseline")
                   (:file "evm")
                   (:file "bookings")
                   (:file "scenarios")))
                 (:module "reporting"
                  :serial t
                  :components
                  ((:file "reports")
                   (:file "task-reports")
                   (:file "resource-reports")
                   (:file "gantt")
                   (:file "helpers")
                   (:module "formats"
                    :serial t
                    :components
                    ((:file "html")
                     (:file "csv")
                     (:file "pdf")))))
                 (:module "risk"
                  :serial t
                  :components
                  ((:file "risk")
                   (:file "simulation")))
                 (:module "utils"
                  :serial t
                  :components
                  ((:file "helpers")
                   (:file "macros"))))))
  :in-order-to ((test-op (test-op "project-juggler/tests"))))

(defsystem "project-juggler/tests"
  :description "Test suite for Project Juggler"
  :depends-on (#:project-juggler
               #:fiveam)
  :serial t
  :components ((:module "tests"
                :serial t
                :components
                ((:file "test-package")
                 (:file "test-utils")
                 (:module "core"
                  :serial t
                  :components
                  ((:file "test-types")
                   (:file "test-classes")
                   (:file "test-protocols")))
                 (:module "namespace"
                  :serial t
                  :components
                  ((:file "test-namespace")
                   (:file "test-references")))
                 (:module "dsl"
                  :serial t
                  :components
                  ((:file "test-defproject")
                   (:file "test-deftask")
                   (:file "test-dependencies")))
                 (:module "validation"
                  :serial t
                  :components
                  ((:file "test-validation")))
                 (:module "scheduling"
                  :serial t
                  :components
                  ((:file "test-scheduler")
                   (:file "test-critical-path")
                   (:file "test-resource-allocation")))
                 (:module "session"
                  :serial t
                  :components
                  ((:file "test-session")
                   (:file "test-undo-redo")))
                 (:module "reporting"
                  :serial t
                  :components
                  ((:file "test-reports")))
                 (:module "tracking"
                  :serial t
                  :components
                  ((:file "test-evm")))
                 (:module "integration"
                  :serial t
                  :components
                  ((:file "test-simple-project")
                   (:file "test-complex-project")
                   (:file "test-performance"))))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* '#:project-juggler-suite
                                                    '#:project-juggler-tests))))
