;;;; tests/namespace/test-include.lisp
;;;; Tests for include functionality

(in-package #:claps-tests)

(def-suite include-tests
  :description "Tests for modular include functionality"
  :in claps-suite)

(in-suite include-tests)

;;; ============================================================================
;;; Helper for Creating Temp Include Files
;;; ============================================================================

(defvar *test-include-dir* nil)

(defun make-temp-include-dir ()
  "Create a temporary directory for include test files."
  (let ((dir (merge-pathnames
              (format nil "claps-test-~A/" (random 1000000))
              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    dir))

(defun cleanup-temp-include-dir (dir)
  "Remove temporary include test directory."
  (when (and dir (probe-file dir))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))

(defmacro with-temp-include-dir (&body body)
  "Execute body with a temporary include directory."
  `(let ((*test-include-dir* (make-temp-include-dir)))
     (unwind-protect
          (progn ,@body)
       (cleanup-temp-include-dir *test-include-dir*))))

(defun write-include-file (filename content)
  "Write content to a file in the temp include directory."
  (let ((path (merge-pathnames filename *test-include-dir*)))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output :if-exists :supersede)
      (write-string content stream))
    path))

;;; ============================================================================
;;; Basic Include Tests
;;; ============================================================================

(test resolve-include-path-absolute
  "Test that absolute paths are returned unchanged."
  (let ((path "/absolute/path/to/file.lisp"))
    (is (equal (pathname path)
               (claps::resolve-include-path path)))))

(test resolve-include-path-relative
  "Test that relative paths are resolved from base path."
  (let ((claps::*include-base-path* #p"/base/path/"))
    (is (equal #p"/base/path/subdir/file.lisp"
               (claps::resolve-include-path "subdir/file.lisp")))))

(test apply-namespace-prefix-without-prefix
  "Test that IDs pass through unchanged when no prefix is set."
  (let ((claps::*current-namespace-prefix* nil))
    (is (eq 'my-task (claps:apply-namespace-prefix 'my-task)))))

(test apply-namespace-prefix-with-prefix
  "Test that IDs are prefixed when namespace prefix is set."
  (let ((claps::*current-namespace-prefix* 'backend))
    (let ((prefixed (claps:apply-namespace-prefix 'api-task)))
      (is (string= "BACKEND/API-TASK" (symbol-name prefixed))))))

(test list-includes-empty-stack
  "Test that list-includes returns empty list when no includes."
  (let ((claps::*include-stack* nil))
    (is (null (claps:list-includes)))))

;;; ============================================================================
;;; Include File Loading Tests
;;; ============================================================================

(test include-resources-file
  "Test including a file with resource definitions."
  (with-temp-include-dir
    ;; Create resource file
    (write-include-file "resources.lisp"
                        "(in-package #:claps)
                         (defresource included-dev \"Included Developer\"
                           :efficiency 1.2)")

    ;; Create main project that includes it
    (claps:defproject include-test "Include Test Project"
      :start (claps:date 2024 1 1)
      :end (claps:date 2024 12 31))

    ;; Include the file
    (let ((claps::*include-base-path* *test-include-dir*))
      (claps:include-file "resources.lisp"))

    ;; Verify resource was added
    (let ((resource (gethash 'claps::included-dev
                             (claps:project-resources claps:*current-project*))))
      (is-true resource)
      (is (equal "Included Developer" (claps:resource-name resource)))
      (is (= 1.2 (claps:resource-efficiency resource))))))

(test include-tasks-file
  "Test including a file with task definitions."
  (with-temp-include-dir
    ;; Create task file
    (write-include-file "tasks.lisp"
                        "(in-package #:claps)
                         (deftask included-task \"Included Task\"
                           :duration (duration 5 :days))")

    ;; Create main project
    (claps:defproject include-test2 "Include Test Project 2"
      :start (claps:date 2024 1 1)
      :end (claps:date 2024 12 31))

    ;; Include the file
    (let ((claps::*include-base-path* *test-include-dir*))
      (claps:include-file "tasks.lisp"))

    ;; Verify task was added
    (let ((task (gethash 'claps::included-task
                         (claps:project-tasks claps:*current-project*))))
      (is-true task)
      (is (equal "Included Task" (claps:task-name task))))))

(test include-with-namespace-prefix
  "Test that namespace prefix is applied to included definitions."
  (with-temp-include-dir
    ;; Create file with tasks
    (write-include-file "backend-tasks.lisp"
                        "(in-package #:claps)
                         (deftask api \"API Development\"
                           :duration (duration 10 :days))")

    ;; Create main project
    (claps:defproject include-ns-test "Namespace Include Test"
      :start (claps:date 2024 1 1)
      :end (claps:date 2024 12 31))

    ;; Include with namespace
    (let ((claps::*include-base-path* *test-include-dir*))
      (claps:include-file "backend-tasks.lisp" :namespace 'backend))

    ;; Verify task was added with namespace prefix
    (let ((task (gethash 'claps::backend/api
                         (claps:project-tasks claps:*current-project*))))
      (is-true task "Task should be registered with prefixed ID 'backend/api")
      (when task
        (is (equal "API Development" (claps:task-name task)))))))

;;; ============================================================================
;;; Circular Include Detection Tests
;;; ============================================================================

(test circular-include-detection
  "Test that circular includes are detected."
  (with-temp-include-dir
    ;; Create two files that include each other
    (write-include-file "file-a.lisp"
                        "(in-package #:claps)
                         (include-file \"file-b.lisp\")")

    (write-include-file "file-b.lisp"
                        "(in-package #:claps)
                         (include-file \"file-a.lisp\")")

    ;; Try to include - should signal error
    (let ((claps::*include-base-path* *test-include-dir*))
      (signals error
        (claps:include-file "file-a.lisp")))))

(test nested-include
  "Test nested includes work correctly."
  (with-temp-include-dir
    ;; Create nested include structure
    (write-include-file "level1.lisp"
                        "(in-package #:claps)
                         (deftask level1-task \"Level 1 Task\"
                           :duration (duration 1 :days))
                         (include-file \"level2.lisp\")")

    (write-include-file "level2.lisp"
                        "(in-package #:claps)
                         (deftask level2-task \"Level 2 Task\"
                           :duration (duration 2 :days))")

    ;; Create main project
    (claps:defproject nested-include-test "Nested Include Test"
      :start (claps:date 2024 1 1)
      :end (claps:date 2024 12 31))

    ;; Include top level file
    (let ((claps::*include-base-path* *test-include-dir*))
      (claps:include-file "level1.lisp"))

    ;; Verify both tasks were added
    (is-true (gethash 'claps::level1-task
                      (claps:project-tasks claps:*current-project*)))
    (is-true (gethash 'claps::level2-task
                      (claps:project-tasks claps:*current-project*)))))

;;; ============================================================================
;;; Include File Not Found Test
;;; ============================================================================

(test include-file-not-found
  "Test that including a non-existent file signals an error."
  (signals error
    (claps:include-file "/nonexistent/path/to/file.lisp")))

;;; ============================================================================
;;; Convenience Macro Tests
;;; ============================================================================

(test include-resources-macro-alias
  "Test that include-resources is an alias for include."
  ;; Just verify the macro expands correctly
  (is (equal '(claps:include "resources.lisp")
             (macroexpand-1 '(claps:include-resources "resources.lisp")))))

(test include-tasks-macro-with-namespace
  "Test that include-tasks supports :namespace keyword."
  (let ((expansion (macroexpand-1 '(claps:include-tasks "tasks.lisp" :namespace backend))))
    (is (equal '(claps:include "tasks.lisp" :namespace backend) expansion))))

(test include-subproject-macro
  "Test include-subproject macro."
  (let ((expansion (macroexpand-1 '(claps:include-subproject "sub/project.lisp" :namespace sub))))
    (is (equal '(claps:include "sub/project.lisp" :namespace sub) expansion))))
