;;;; tests/dsl/test-defresource.lisp
;;;; Tests for defresource macro

(in-package #:project-juggler-tests)

(in-suite dsl-suite)

;;; Basic defresource tests

(test defresource-basic
  "Can define a basic resource"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))

    (let ((resource (resolve-resource-reference 'dev1)))
      (is (not (null resource)))
      (is (resource-p resource))
      (is (eq 'dev1 (resource-id resource)))
      (is (string= "Developer 1" (resource-name resource)))
      (is (eq *current-project* (resource-project resource))))))

(test defresource-with-efficiency
  "Can define resource with efficiency"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"
             :efficiency 1.2))

    (let ((resource (resolve-resource-reference 'dev1)))
      (is (= 1.2 (resource-efficiency resource))))))

(test defresource-with-rate
  "Can define resource with rate"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"
             :rate 150.0))

    (let ((resource (resolve-resource-reference 'dev1)))
      (is (= 150.0 (resource-rate resource))))))

(test defresource-registers-in-namespace
  "defresource registers resource in current namespace"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))

    (is (not (null (gethash 'dev1 (namespace-resources *current-namespace*)))))))

(test defresource-registers-in-project
  "defresource registers resource in current project"
  (with-test-project
    (eval '(defresource dev1 "Developer 1"))

    (is (not (null (gethash 'dev1 (project-resources *current-project*)))))))
