;;;; tests/dsl/test-defscenario.lisp
;;;; Tests for defscenario macro and scenario management

(in-package #:claps-tests)

(def-suite defscenario-suite
  :in project-juggler-suite
  :description "Tests for defscenario DSL")

(in-suite defscenario-suite)

;;; ============================================================================
;;; defscenario Macro Tests
;;; ============================================================================

(test add-scenario-basic
  "Test adding a new scenario to a project"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))
    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Initially only have 'plan scenario from with-test-project
    (is (= 1 (length (list-scenarios *current-project*))))

    ;; Add a new scenario
    (add-scenario *current-project* 'revised "Revised Plan")

    (let ((scenarios (list-scenarios *current-project*)))
      ;; Should now have 2 scenarios: plan, revised
      (is (= 2 (length scenarios)))
      (is (member 'revised scenarios :test #'string= :key #'symbol-name)))))

(test add-scenario-captures-values
  "Test that add-scenario captures current task values"
  (with-test-project
    ;; Task with only duration (not both effort and duration)
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))
    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Modify task values
    (let ((task (gethash 't1 (project-tasks *current-project*))))
      (setf (task-duration task) (duration 15 :days)))

    ;; Add scenario - should capture modified values
    (add-scenario *current-project* 'modified "Modified Plan")

    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (modified-dur (task-duration-for-scenario task 'modified)))
      (is (= 15 (duration-in-days modified-dur))))))

(test copy-scenario
  "Test copying a scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))
    (finalize-project *current-project*)

    ;; First add a scenario to copy
    (add-scenario *current-project* 'original "Original")
    ;; Modify the value in original
    (set-scenario-value *current-project* 'original 't1 :duration (duration 20 :days))

    ;; Copy the scenario
    (copy-scenario *current-project* 'original 'copy "Copy of Original")

    (let ((scenarios (list-scenarios *current-project*)))
      ;; plan, original, copy = 3
      (is (= 3 (length scenarios))))

    ;; Values should match source
    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (original-dur (task-duration-for-scenario task 'original))
           (copy-dur (task-duration-for-scenario task 'copy)))
      (is (= (duration-in-days original-dur)
             (duration-in-days copy-dur))))))

(test remove-scenario
  "Test removing a scenario"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))
    (finalize-project *current-project*)

    ;; Add and then remove a scenario
    (add-scenario *current-project* 'temporary "Temporary")
    (is (= 2 (length (list-scenarios *current-project*))))

    (remove-scenario *current-project* 'temporary)
    (is (= 1 (length (list-scenarios *current-project*))))))

(test cannot-remove-baseline-scenario
  "Test that baseline scenario cannot be removed"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))
    (finalize-project *current-project*)

    ;; Try to remove baseline (first scenario)
    (signals error
      (remove-scenario *current-project* 'plan))))

(test set-scenario-value
  "Test setting individual scenario values"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))
    (finalize-project *current-project*)

    ;; Add a scenario to set values on
    (add-scenario *current-project* 'modified "Modified")

    ;; Set a specific value
    (set-scenario-value *current-project* 'modified 't1 :duration (duration 25 :days))

    (let* ((task (gethash 't1 (project-tasks *current-project*)))
           (modified-dur (task-duration-for-scenario task 'modified)))
      (is (= 25 (duration-in-days modified-dur))))))

;;; ============================================================================
;;; Comparison Report Tests
;;; ============================================================================

(test comparison-report-creation
  "Test creating a comparison report"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))

    ;; Add a second scenario for comparison
    (add-scenario *current-project* 'revised "Revised Plan")
    (set-scenario-value *current-project* 'revised 't1 :duration (duration 15 :days))

    (eval '(defreport comp "Plan vs Revised"
             :type :comparison
             :format :html
             :scenario-1 plan
             :scenario-2 revised))

    (let ((report (gethash 'comp (project-reports *current-project*))))
      (is (not (null report)))
      (is (typep report 'comparison-report))
      (is (string= "PLAN" (symbol-name (comparison-scenario-1 report))))
      (is (string= "REVISED" (symbol-name (comparison-scenario-2 report)))))))

(test comparison-report-generation
  "Test generating comparison report HTML"
  (with-test-project
    (eval '(deftask t1 "Task 1"
             :duration (duration 10 :days)))

    ;; Add a second scenario for comparison
    (add-scenario *current-project* 'revised "Revised Plan")
    (set-scenario-value *current-project* 'revised 't1 :duration (duration 15 :days))

    (eval '(defreport comp "Plan vs Revised"
             :type :comparison
             :format :html
             :scenario-1 plan
             :scenario-2 revised))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let* ((report (gethash 'comp (project-reports *current-project*)))
           (html (generate-report report *current-project*)))
      (is (stringp html))
      (is (search "Plan vs Revised" html))
      (is (search "PLAN" html))
      (is (search "REVISED" html)))))
