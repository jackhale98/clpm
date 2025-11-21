;;;; run-tests.lisp
;;;; Script to load and run tests

(require :asdf)

(in-package #:cl-user)

;; Add current directory to ASDF registry
(push (truename ".") asdf:*central-registry*)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load dependencies
(format t "~%Loading dependencies...~%")
(ql:quickload :local-time :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :split-sequence :silent t)
(ql:quickload :fiveam :silent t)

;; Load project-juggler
(format t "Loading project-juggler...~%")
(asdf:load-system :project-juggler :verbose nil)

;; Load tests
(format t "Loading project-juggler/tests...~%")
(asdf:load-system :project-juggler/tests :verbose nil)

;; Run tests
(format t "~%Running tests...~%~%")
(in-package #:project-juggler-tests)
(run! 'types-suite)
(format t "~%")
(run! 'classes-suite)
(format t "~%")
(run! 'namespace-suite)
(format t "~%")
(run! 'dsl-suite)
(format t "~%")
(run! 'validation-suite)
(format t "~%")
(run! 'scheduling-suite)
(format t "~%")
(run! 'session-suite)
(format t "~%")
(run! 'reporting-suite)
(format t "~%")
(run! 'evm-suite)
(format t "~%")
(run! 'calendar-suite)
(format t "~%")
(run! 'bookings-suite)

;; Exit
(uiop:quit 0)
