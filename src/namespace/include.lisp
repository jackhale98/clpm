;;;; src/namespace/include.lisp
;;;; Include functionality for modular project organization

(in-package #:claps)

;;; ============================================================================
;;; Include Macro
;;; ============================================================================
;;;
;;; Allows splitting project definitions across multiple files:
;;;
;;;   ;; Include resources from another file
;;;   (include "resources/developers.lisp")
;;;
;;;   ;; Include with namespace prefix (prefixes all IDs)
;;;   (include "subproject/backend.lisp" :namespace backend)
;;;
;;;   ;; Include relative to current file
;;;   (include "calendars/holidays.lisp")
;;;
;;; Included files can contain:
;;;   - defresource definitions
;;;   - defcalendar definitions
;;;   - deftask definitions
;;;   - defbookings / deftimesheet
;;;   - defreport definitions
;;;   - Other includes (nested)
;;;
;;; ============================================================================

(defvar *include-stack* nil
  "Stack of currently loading files (for detecting circular includes)")

(defvar *include-base-path* nil
  "Base path for resolving relative includes")

(defvar *current-namespace-prefix* nil
  "Current namespace prefix for included definitions")

(defun resolve-include-path (path)
  "Resolve an include path relative to the current file or base path."
  (let ((pathname (pathname path)))
    (if (uiop:absolute-pathname-p pathname)
        pathname
        ;; Resolve relative to current file or base path
        (let ((base (or *include-base-path*
                        (and *load-truename*
                             (make-pathname :directory (pathname-directory *load-truename*)))
                        *default-pathname-defaults*)))
          (merge-pathnames pathname base)))))

(defun check-circular-include (path)
  "Check for circular includes and signal an error if detected."
  (let ((resolved (namestring (truename path))))
    (when (member resolved *include-stack* :test #'string=)
      (error "Circular include detected: ~A~%Include stack: ~{~A~^ -> ~}"
             path (reverse *include-stack*)))))

(defmacro include (path &key namespace)
  "Include another file in the context of the current project.

   Usage:
     ;; Simple include
     (include \"resources/team.lisp\")

     ;; Include with namespace prefix
     (include \"backend/tasks.lisp\" :namespace backend)

   The included file is loaded with:
     - *current-project* bound to the including project
     - Paths resolved relative to the including file

   With :namespace, all task and resource IDs defined in the included
   file will be prefixed (e.g., task 'api becomes 'backend/api).

   Keywords:
     :namespace - Symbol prefix for IDs defined in the included file"
  `(include-file ,path :namespace ',namespace))

(defun include-file (path &key namespace)
  "Load an include file in the current project context."
  (let* ((resolved-path (resolve-include-path path))
         (*include-base-path* (make-pathname :directory (pathname-directory resolved-path))))

    ;; Check file exists
    (unless (probe-file resolved-path)
      (error "Include file not found: ~A (resolved from ~A)" resolved-path path))

    ;; Check for circular includes
    (check-circular-include resolved-path)

    ;; Load the file
    ;; Note: We use setf/unwind-protect instead of let binding because
    ;; SBCL's load with compilation doesn't properly inherit dynamic bindings
    (let ((old-stack *include-stack*)
          (old-prefix *current-namespace-prefix*))
      (unwind-protect
           (progn
             (setf *include-stack* (cons (namestring (truename resolved-path)) old-stack))
             (setf *current-namespace-prefix* namespace)
             (load resolved-path :verbose nil :print nil))
        (setf *include-stack* old-stack)
        (setf *current-namespace-prefix* old-prefix)))

    ;; Return path for informational purposes
    resolved-path))

;;; ============================================================================
;;; Namespace Prefix Support
;;; ============================================================================

(defun apply-namespace-prefix (id)
  "Apply the current namespace prefix to an ID if one is set.
   Uses uppercase symbol names and interns in the CLAPS package for consistency."
  (if *current-namespace-prefix*
      (intern (string-upcase
               (format nil "~A/~A"
                       (symbol-name *current-namespace-prefix*)
                       (symbol-name id)))
              (find-package :claps))
      id))

;;; ============================================================================
;;; Include-Aware Registration
;;; ============================================================================
;;;
;;; The register-task and register-resource functions should call
;;; apply-namespace-prefix when *current-namespace-prefix* is set.
;;; This is handled by wrapping the ID at definition time.
;;;

;;; ============================================================================
;;; Convenience Macros for Common Patterns
;;; ============================================================================

(defmacro include-resources (path)
  "Include a file containing only resource definitions.
   Alias for (include path) with clearer intent."
  `(include ,path))

(defmacro include-calendars (path)
  "Include a file containing only calendar definitions.
   Alias for (include path) with clearer intent."
  `(include ,path))

(defmacro include-tasks (path &key namespace)
  "Include a file containing task definitions.
   Commonly used with :namespace to prefix task IDs."
  `(include ,path :namespace ,namespace))

(defmacro include-timesheets (path)
  "Include a file containing booking/timesheet definitions."
  `(include ,path))

(defmacro include-subproject (path &key namespace)
  "Include a subproject file with its own tasks, resources, etc.
   The :namespace option is recommended to avoid ID conflicts."
  `(include ,path :namespace ,namespace))

;;; ============================================================================
;;; List Includes (for debugging/introspection)
;;; ============================================================================

(defun list-includes ()
  "Return the current include stack (useful for debugging)."
  (reverse *include-stack*))
