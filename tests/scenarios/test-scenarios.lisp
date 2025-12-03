;;;; tests/scenarios/test-scenarios.lisp
;;;; Tests for what-if scenario management

(in-package #:project-juggler-tests)

(def-suite scenarios-suite
  :in project-juggler-suite
  :description "Tests for what-if scenario management")

(in-suite scenarios-suite)

;;; ============================================================================
;;; Scenario Creation Tests
;;; ============================================================================

(test create-scenario-basic
  "Test creating a basic scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; Create a scenario
    (let ((scenario (create-scenario *current-project* "optimistic")))
      (is (not (null scenario)))
      (is (string= "optimistic" (scenario-name scenario)))
      (is (eq *current-project* (scenario-project scenario))))))

(test create-scenario-with-description
  "Test creating a scenario with description"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (let ((scenario (create-scenario *current-project* "best-case"
                                    :description "Best case estimates")))
      (is (string= "Best case estimates" (scenario-description scenario))))))

(test scenario-clones-task-data
  "Test that scenario clones task schedule data"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let* ((scenario (create-scenario *current-project* "test"))
           (task (gethash 'task1 (project-tasks *current-project*)))
           (task-snapshot (get-scenario-task-data scenario 'task1)))
      (is (not (null task-snapshot)))
      (is (date= (task-start task) (getf task-snapshot :start)))
      (is (date= (task-end task) (getf task-snapshot :end))))))

;;; ============================================================================
;;; Scenario Modification Tests
;;; ============================================================================

(test scenario-modify-task-duration
  "Test modifying task duration in a scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((scenario (create-scenario *current-project* "extended")))
      ;; Modify task duration in scenario
      (scenario-modify-task scenario 'task1 :duration (duration 10 :days))
      ;; Reschedule scenario
      (schedule-scenario scenario)
      ;; Check scenario has different end date
      (let ((task-data (get-scenario-task-data scenario 'task1)))
        (is (= 10 (duration-in-days (getf task-data :duration))))))))

(test scenario-modify-task-effort
  "Test modifying task effort in a scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((scenario (create-scenario *current-project* "reduced")))
      ;; Reduce effort in scenario
      (scenario-modify-task scenario 'task1 :effort (duration 5 :days))
      (schedule-scenario scenario)
      (let ((task-data (get-scenario-task-data scenario 'task1)))
        (is (= 5 (duration-in-days (getf task-data :effort))))))))

(test scenario-add-dependency
  "Test adding a dependency in a scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 5 :days) :allocate (dev1))
    (deftask task2 "Task 2" :duration (duration 3 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; In base: tasks can run in parallel
    (let ((scenario (create-scenario *current-project* "sequential")))
      ;; Add dependency: task2 depends on task1
      (scenario-add-dependency scenario 'task2 'task1)
      (schedule-scenario scenario)
      ;; Task2 should now start after task1
      (let ((task1-data (get-scenario-task-data scenario 'task1))
            (task2-data (get-scenario-task-data scenario 'task2)))
        (is (date>= (getf task2-data :start) (getf task1-data :end)))))))

;;; ============================================================================
;;; Scenario Comparison Tests
;;; ============================================================================

(test compare-scenarios-duration
  "Test comparing total duration between scenarios"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    ;; Create optimistic scenario with shorter duration
    (let ((optimistic (create-scenario *current-project* "optimistic"))
          (pessimistic (create-scenario *current-project* "pessimistic")))
      (scenario-modify-task optimistic 'task1 :duration (duration 5 :days))
      (scenario-modify-task pessimistic 'task1 :duration (duration 15 :days))
      (schedule-scenario optimistic)
      (schedule-scenario pessimistic)
      (let ((comparison (compare-scenarios optimistic pessimistic)))
        (is (< (getf comparison :optimistic-duration)
               (getf comparison :pessimistic-duration)))))))

(test compare-scenarios-cost
  "Test comparing total cost between scenarios"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((cheap (create-scenario *current-project* "cheap"))
          (expensive (create-scenario *current-project* "expensive")))
      (scenario-modify-task cheap 'task1 :effort (duration 5 :days))
      (scenario-modify-task expensive 'task1 :effort (duration 20 :days))
      (schedule-scenario cheap)
      (schedule-scenario expensive)
      (let ((comparison (compare-scenarios cheap expensive)))
        (is (< (getf comparison :cheap-cost)
               (getf comparison :expensive-cost)))))))

(test compare-scenarios-end-date
  "Test comparing end dates between scenarios"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 10 :days)
      :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let ((fast (create-scenario *current-project* "fast"))
          (slow (create-scenario *current-project* "slow")))
      (scenario-modify-task fast 'task1 :duration (duration 5 :days))
      (scenario-modify-task slow 'task1 :duration (duration 20 :days))
      (schedule-scenario fast)
      (schedule-scenario slow)
      (let ((comparison (compare-scenarios fast slow)))
        (is (date< (getf comparison :fast-end)
                   (getf comparison :slow-end)))))))

;;; ============================================================================
;;; Scenario Listing and Selection Tests
;;; ============================================================================

(test list-project-scenarios
  "Test listing all scenarios for a project"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 5 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (create-scenario *current-project* "scenario-a")
    (create-scenario *current-project* "scenario-b")
    (create-scenario *current-project* "scenario-c")
    (let ((scenarios (list-scenarios *current-project*)))
      (is (= 3 (length scenarios)))
      (is (member "scenario-a" scenarios :test #'string=))
      (is (member "scenario-b" scenarios :test #'string=))
      (is (member "scenario-c" scenarios :test #'string=)))))

(test get-scenario-by-name
  "Test retrieving a scenario by name"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 5 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (create-scenario *current-project* "test-scenario")
    (let ((scenario (get-scenario *current-project* "test-scenario")))
      (is (not (null scenario)))
      (is (string= "test-scenario" (scenario-name scenario))))))

(test delete-scenario
  "Test deleting a scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 5 :days) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (create-scenario *current-project* "to-delete")
    (is (= 1 (length (list-scenarios *current-project*))))
    (delete-scenario *current-project* "to-delete")
    (is (= 0 (length (list-scenarios *current-project*))))))

;;; ============================================================================
;;; Scenario Summary Tests
;;; ============================================================================

(test scenario-summary
  "Test getting summary statistics for a scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :effort (duration 10 :days) :allocate (dev1))
    (deftask task2 "Task 2" :effort (duration 5 :days) :depends-on (task1) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let* ((scenario (create-scenario *current-project* "test"))
           (summary (scenario-summary scenario)))
      (is (not (null summary)))
      (is (numberp (getf summary :total-duration)))
      (is (numberp (getf summary :total-cost)))
      (is (not (null (getf summary :end-date)))))))

(test scenario-critical-path
  "Test getting critical path for a scenario"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1" :duration (duration 5 :days) :allocate (dev1))
    (deftask task2 "Task 2" :duration (duration 3 :days) :depends-on (task1) :allocate (dev1))
    (finalize-project *current-project*)
    (schedule *current-project*)
    (let* ((scenario (create-scenario *current-project* "test"))
           (cp (scenario-critical-path scenario)))
      (is (not (null cp)))
      (is (= 2 (length cp))))))
