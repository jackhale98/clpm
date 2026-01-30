;;;; claps.asd
;;;; ASDF system definition for CLAPS - Common Lisp Automated Project Scheduling

(defsystem "claps"
  :description "Common Lisp Automated Project Scheduling"
  :version "1.0.0"
  :author "CLAPS Team"
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
                   (:file "errors")
                   (:file "summary-tasks")))
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
                   (:file "defreport")
                   (:file "defscenario")
                   (:file "defbooking")))
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
                   (:file "calendars")
                   (:file "availability")
                   (:file "pert")))
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
                   (:file "costs")
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
                 (:module "constraints"
                  :serial t
                  :components
                  ((:file "constraints")))
                 (:module "utils"
                  :serial t
                  :components
                  ((:file "helpers")
                   (:file "macros"))))))
  :in-order-to ((test-op (test-op "claps/tests"))))

(defsystem "claps/cli"
  :description "CLI interface for CLAPS"
  :version "1.0.0"
  :author "CLAPS Team"
  :license "MIT"
  :depends-on (#:claps)
  :serial t
  :components ((:module "src"
                :components
                ((:module "cli"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "args")
                   (:file "output")
                   (:file "commands")
                   (:file "main")))))))

(defsystem "claps/tests"
  :description "Test suite for CLAPS"
  :depends-on (#:claps
               #:claps/cli
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
                   (:file "test-protocols")
                   (:file "test-summary-tasks")))
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
                   (:file "test-dependencies")
                   (:file "test-defscenario")))
                 (:module "validation"
                  :serial t
                  :components
                  ((:file "test-validation")))
                 (:module "scheduling"
                  :serial t
                  :components
                  ((:file "test-scheduler")
                   (:file "test-critical-path")
                   (:file "test-resource-allocation")
                   (:file "test-calendars")
                   (:file "test-dependency-types")
                   (:file "test-availability")
                   (:file "test-pert")))
                 (:module "session"
                  :serial t
                  :components
                  ((:file "test-session")
                   (:file "test-undo-redo")))
                 (:module "reporting"
                  :serial t
                  :components
                  ((:file "test-reports")
                   (:file "test-enhanced-reports")))
                 (:module "tracking"
                  :serial t
                  :components
                  ((:file "test-evm")
                   (:file "test-bookings")
                   (:file "test-costs")
                   (:file "test-baseline-scenarios")))
                (:module "scenarios"
                  :serial t
                  :components
                  ((:file "test-scenarios")))
                (:module "risk"
                  :serial t
                  :components
                  ((:file "test-risk")
                   (:file "test-simulation")))
                (:module "constraints"
                  :serial t
                  :components
                  ((:file "test-constraints")))
                 (:module "integration"
                  :serial t
                  :components
                  ((:file "test-simple-project")
                   (:file "test-complex-project")
                   (:file "test-performance")))
                 (:module "cli"
                  :serial t
                  :components
                  ((:file "test-cli-package")
                   (:file "test-args")
                   (:file "test-output")
                   (:file "test-commands")
                   (:file "test-integration"))))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run!
                                       (find-symbol* '#:claps-suite
                                                    '#:claps-tests))))
