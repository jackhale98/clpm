;;;; tests/tracking/test-costs.lisp
;;;; Tests for cost tracking functionality

(in-package #:project-juggler-tests)

(def-suite cost-tracking-suite
  :in project-juggler-suite
  :description "Cost tracking tests")

(in-suite cost-tracking-suite)

;;; ============================================================================
;;; Task Cost Calculation Tests
;;; ============================================================================

(test calculate-task-planned-cost-simple
  "Calculate planned cost for a task with effort and allocated resource"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)  ; $100/day
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; 10 days × $100/day = $1000
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (is (= 1000.0 (calculate-task-planned-cost task))))))

(test calculate-task-planned-cost-multiple-resources
  "Calculate planned cost with multiple allocated resources"
  (with-test-project
    (defresource dev1 "Developer 1" :rate 100.0)
    (defresource dev2 "Developer 2" :rate 150.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1 dev2))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; With 2 resources, effort is split: 5 days each
    ;; Cost = 5 × $100 + 5 × $150 = $500 + $750 = $1250
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (is (= 1250.0 (calculate-task-planned-cost task))))))

(test calculate-task-planned-cost-duration-based
  "Calculate planned cost for duration-based task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; 5 days × $100/day = $500
    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (is (= 500.0 (calculate-task-planned-cost task))))))

(test calculate-task-planned-cost-no-resources
  "Task with no resources has zero labor cost"
  (with-test-project
    (deftask task1 "Task 1"
      :start (date 2024 3 1)
      :duration (duration 5 :days))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      (is (= 0.0 (calculate-task-planned-cost task))))))

(test calculate-task-actual-cost-from-bookings
  "Calculate actual cost from time bookings"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Book 3 days of work
      (add-booking task resource
                   (date 2024 3 1 9 0 0)
                   (date 2024 3 1 17 0 0))  ; 8 hours = 1 day
      (add-booking task resource
                   (date 2024 3 2 9 0 0)
                   (date 2024 3 2 17 0 0))  ; 8 hours = 1 day
      (add-booking task resource
                   (date 2024 3 3 9 0 0)
                   (date 2024 3 3 17 0 0))  ; 8 hours = 1 day

      ;; 3 days × $100/day = $300
      (is (= 300.0 (calculate-task-actual-cost task))))))

(test calculate-task-fixed-cost
  "Task with fixed cost adds to total"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :duration (duration 5 :days)
      :allocate (dev1)
      :fixed-cost 500.0)  ; Software license, etc.

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*))))
      ;; Labor: 5 × $100 = $500, Fixed: $500, Total: $1000
      (is (= 1000.0 (calculate-task-planned-cost task :include-fixed t))))))

;;; ============================================================================
;;; Project Cost Calculation Tests
;;; ============================================================================

(test calculate-project-planned-cost
  "Calculate total planned cost for entire project"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 5 :days)
      :allocate (dev1))
    (deftask task2 "Task 2"
      :effort (duration 10 :days)
      :allocate (dev1)
      :depends-on (task1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Task1: 5 × $100 = $500
    ;; Task2: 10 × $100 = $1000
    ;; Total: $1500
    (is (= 1500.0 (calculate-project-planned-cost *current-project*)))))

(test calculate-project-actual-cost
  "Calculate total actual cost from all bookings"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 5 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Book 2 days
      (add-booking task resource
                   (date 2024 3 1 9 0 0)
                   (date 2024 3 1 17 0 0))
      (add-booking task resource
                   (date 2024 3 2 9 0 0)
                   (date 2024 3 2 17 0 0))

      ;; 2 days × $100 = $200
      (is (= 200.0 (calculate-project-actual-cost *current-project*))))))

(test project-budget-tracking
  "Track project budget vs actual spending"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; Set project budget
    (setf (project-budget *current-project*) 1500.0)

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Book 5 days
      (dotimes (i 5)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; Planned: $1000, Budget: $1500, Actual: $500
      (is (= 1500.0 (project-budget *current-project*)))
      (is (= 1000.0 (calculate-project-planned-cost *current-project*)))
      (is (= 500.0 (calculate-project-actual-cost *current-project*)))
      (is (= 1000.0 (project-budget-remaining *current-project*))))))

;;; ============================================================================
;;; EVM Cost Metrics Tests
;;; ============================================================================

(test calculate-actual-cost-evm
  "Calculate Actual Cost (AC) for EVM"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      (dotimes (i 3)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; AC = actual cost of work performed = $300
      (is (= 300.0 (calculate-actual-cost *current-project*))))))

(test calculate-cost-variance
  "Calculate Cost Variance (CV = EV - AC)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1)
      :complete 50)  ; 50% complete

    (finalize-project *current-project*)
    (schedule *current-project*)
    (set-project-baseline *current-project* (create-baseline *current-project*))

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Book 6 days of work (over budget for 50% complete)
      (dotimes (i 6)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; EV = 50% × $1000 = $500
      ;; AC = 6 × $100 = $600
      ;; CV = EV - AC = $500 - $600 = -$100 (over budget)
      (is (= -100.0 (calculate-cost-variance *current-project*))))))

(test calculate-cost-performance-index
  "Calculate Cost Performance Index (CPI = EV / AC)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1)
      :complete 50)

    (finalize-project *current-project*)
    (schedule *current-project*)
    (set-project-baseline *current-project* (create-baseline *current-project*))

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Book 6 days for 50% complete (should have been 5 days)
      (dotimes (i 6)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; CPI = EV / AC = $500 / $600 = 0.833...
      ;; CPI < 1.0 means over budget
      (let ((cpi (calculate-cpi-cost *current-project*)))
        (is (< cpi 1.0))
        (is (< (abs (- cpi 0.833)) 0.01))))))

(test calculate-budget-at-completion
  "Calculate Budget at Completion (BAC)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1))
    (deftask task2 "Task 2"
      :effort (duration 5 :days)
      :allocate (dev1)
      :depends-on (task1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    ;; BAC = total planned cost = $1000 + $500 = $1500
    (is (= 1500.0 (calculate-bac *current-project*)))))

(test calculate-estimate-at-completion
  "Calculate Estimate at Completion (EAC = BAC / CPI)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1)
      :complete 50)

    (finalize-project *current-project*)
    (schedule *current-project*)
    (set-project-baseline *current-project* (create-baseline *current-project*))

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      ;; Over budget: 6 days for 50% complete
      (dotimes (i 6)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; BAC = $1000
      ;; CPI = 0.833
      ;; EAC = BAC / CPI = $1000 / 0.833 = ~$1200
      (let ((eac (calculate-eac *current-project*)))
        (is (> eac 1000.0))  ; Will cost more than planned
        (is (< (abs (- eac 1200.0)) 50.0))))))

(test calculate-estimate-to-complete
  "Calculate Estimate to Complete (ETC = EAC - AC)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1)
      :complete 50)

    (finalize-project *current-project*)
    (schedule *current-project*)
    (set-project-baseline *current-project* (create-baseline *current-project*))

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      (dotimes (i 6)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; EAC = ~$1200, AC = $600
      ;; ETC = EAC - AC = ~$600 remaining
      (let ((etc (calculate-etc *current-project*)))
        (is (> etc 0.0))))))

(test calculate-variance-at-completion
  "Calculate Variance at Completion (VAC = BAC - EAC)"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1)
      :complete 50)

    (finalize-project *current-project*)
    (schedule *current-project*)
    (set-project-baseline *current-project* (create-baseline *current-project*))

    (let ((task (gethash 'task1 (project-tasks *current-project*)))
          (resource (gethash 'dev1 (project-resources *current-project*))))
      (dotimes (i 6)
        (add-booking task resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))

      ;; BAC = $1000, EAC = ~$1200
      ;; VAC = BAC - EAC = ~-$200 (over budget)
      (let ((vac (calculate-vac *current-project*)))
        (is (< vac 0.0))))))  ; Negative = over budget

;;; ============================================================================
;;; Cost Reports Tests
;;; ============================================================================

(test cost-report-by-task
  "Generate cost report grouped by task"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (deftask task1 "Task 1"
      :effort (duration 5 :days)
      :allocate (dev1))
    (deftask task2 "Task 2"
      :effort (duration 10 :days)
      :allocate (dev1)
      :depends-on (task1))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((report (generate-cost-report *current-project* :by :task)))
      (is (listp report))
      (is (= 2 (length report)))
      ;; Each entry has task-id, planned-cost, actual-cost
      (is (getf (first report) :planned-cost)))))

(test cost-report-by-resource
  "Generate cost report grouped by resource"
  (with-test-project
    (defresource dev1 "Developer 1" :rate 100.0)
    (defresource dev2 "Developer 2" :rate 150.0)
    (deftask task1 "Task 1"
      :effort (duration 10 :days)
      :allocate (dev1 dev2))

    (finalize-project *current-project*)
    (schedule *current-project*)

    (let ((report (generate-cost-report *current-project* :by :resource)))
      (is (listp report))
      (is (= 2 (length report))))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test cost-tracking-full-workflow
  "Full cost tracking workflow from planning to completion"
  (with-test-project
    (defresource dev1 "Developer" :rate 100.0)
    (defresource tester "Tester" :rate 80.0)

    (deftask development "Development"
      :effort (duration 10 :days)
      :allocate (dev1))

    (deftask testing "Testing"
      :effort (duration 5 :days)
      :allocate (tester)
      :depends-on (development))

    (finalize-project *current-project*)
    (schedule *current-project*)
    (set-project-baseline *current-project* (create-baseline *current-project*))

    ;; Verify planned costs
    ;; Dev: 10 × $100 = $1000
    ;; Test: 5 × $80 = $400
    ;; Total BAC = $1400
    (is (= 1400.0 (calculate-bac *current-project*)))

    ;; Simulate work progress
    (let ((dev-task (gethash 'development (project-tasks *current-project*)))
          (dev-resource (gethash 'dev1 (project-resources *current-project*))))

      ;; Complete development (10 days)
      (dotimes (i 10)
        (add-booking dev-task dev-resource
                     (date+ (date 2024 3 1) (duration i :days))
                     (duration 8 :hours)))
      (setf (task-complete dev-task) 100))

    ;; At this point: development complete, testing not started
    ;; EV = 100% of dev ($1000) + 0% of test = $1000
    ;; AC = $1000
    ;; CPI = 1.0 (on budget so far)
    (let ((cpi (calculate-cpi-cost *current-project*)))
      (is (= 1.0 cpi)))))
