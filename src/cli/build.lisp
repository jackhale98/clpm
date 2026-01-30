;;;; src/cli/build.lisp
;;;; Build script for creating CLAPS executable

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
(format t "Loading dependencies...~%")
(ql:quickload :local-time :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :split-sequence :silent t)

;; Load CLAPS and CLI
(format t "Loading CLAPS...~%")
(asdf:load-system :claps :verbose nil)
(asdf:load-system :claps/cli :verbose nil)

;; Build the executable
(format t "Building executable...~%")

(defun build-claps (&optional (output-path "claps"))
  "Build the CLAPS CLI executable"
  #+sbcl
  (sb-ext:save-lisp-and-die output-path
                            :toplevel #'claps/cli:run-cli
                            :executable t
                            :compression t
                            :purify t)
  #-sbcl
  (error "Building executables is only supported on SBCL"))

;; Run build if this is the main script
(build-claps)
