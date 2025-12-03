# Project Juggler - Feature Implementation Roadmap

## Overview

This document outlines the systematic implementation of missing project management features, following strict TDD methodology.

---

## Phase 13: Cost Tracking

**Priority:** High
**Estimated Tests:** 25-30

### Features
1. **Project Budget**
   - Total budget amount
   - Budget allocation by task/phase

2. **Resource Costs**
   - Hourly/daily rates (already have `resource-rate`)
   - Calculate planned cost from effort × rate
   - Calculate actual cost from bookings × rate

3. **Task Costs**
   - Planned cost (from allocated resources and effort)
   - Actual cost (from bookings)
   - Fixed costs (non-labor costs)

4. **Cost Metrics (EVM Extension)**
   - Actual Cost (AC) - cost of work performed
   - Cost Variance (CV) = EV - AC
   - Cost Performance Index (CPI) = EV / AC
   - Budget at Completion (BAC)
   - Estimate at Completion (EAC)
   - Estimate to Complete (ETC)
   - Variance at Completion (VAC)

5. **Cost Reports**
   - Cost by task
   - Cost by resource
   - Cost variance report

### Implementation Files
- `src/tracking/costs.lisp` - Cost calculation functions
- `src/core/classes.lisp` - Add cost slots to project/task
- `tests/tracking/test-costs.lisp` - Cost tests

---

## Phase 14: Additional Dependency Types

**Priority:** High
**Estimated Tests:** 20-25

### Features
1. **Dependency Types**
   - Finish-to-Start (FS) - current default
   - Start-to-Start (SS) - B starts when A starts
   - Finish-to-Finish (FF) - B finishes when A finishes
   - Start-to-Finish (SF) - B finishes when A starts (rare)

2. **Lag and Lead Times**
   - Positive lag: delay after dependency (+2d)
   - Negative lag (lead): overlap before dependency (-1d)

3. **DSL Syntax**
   ```lisp
   :depends-on ((task1 :type :fs :lag (duration 2 :days))
                (task2 :type :ss)
                (task3 :type :ff :lag (duration -1 :days)))
   ```

### Implementation Files
- `src/core/classes.lisp` - Extend dependency class
- `src/dsl/deftask.lisp` - Extended depends-on parsing
- `src/scheduling/scheduler.lisp` - Handle all dependency types
- `tests/scheduling/test-dependencies.lisp` - Dependency tests

---

## Phase 15: Resource Availability

**Priority:** Medium-High
**Estimated Tests:** 20-25

### Features
1. **Resource Leaves/Vacation**
   - Date ranges when resource unavailable
   - Types: vacation, sick, holiday, training

2. **Daily Work Limits**
   - Max hours per day (default 8)
   - Max hours per week (default 40)

3. **Percent Allocation**
   - Resource works X% on a task
   - Affects effort calculation

4. **Resource Calendar**
   - Link resource to specific calendar
   - Override project calendar

### DSL Syntax
```lisp
(defresource dev1 "Developer"
  :rate 100.0
  :daily-limit (duration 8 :hours)
  :weekly-limit (duration 40 :hours)
  :leaves ((vacation (date 2024 7 1) (date 2024 7 14))
           (holiday (date 2024 12 25) (date 2024 12 25))))

(deftask task1 "Task"
  :allocate ((dev1 :percent 50)
             (dev2 :percent 100)))
```

### Implementation Files
- `src/core/classes.lisp` - Extend resource class
- `src/dsl/defresource.lisp` - Extended resource parsing
- `src/scheduling/availability.lisp` - Availability calculations
- `tests/scheduling/test-availability.lisp` - Availability tests

---

## Phase 16: PERT Three-Point Estimation

**Priority:** Medium
**Estimated Tests:** 15-20

### Features
1. **Three-Point Estimates**
   - Optimistic (O) - best case
   - Most Likely (M) - normal case
   - Pessimistic (P) - worst case

2. **PERT Formulas**
   - Expected duration: (O + 4M + P) / 6
   - Standard deviation: (P - O) / 6
   - Variance: σ²

3. **Confidence Intervals**
   - 68% confidence: E ± σ
   - 95% confidence: E ± 2σ
   - 99% confidence: E ± 3σ

4. **Project Duration Probability**
   - Probability of completing by date X

### DSL Syntax
```lisp
(deftask task1 "Task"
  :estimate (:optimistic (duration 5 :days)
             :likely (duration 8 :days)
             :pessimistic (duration 15 :days)))
```

### Implementation Files
- `src/scheduling/pert.lisp` - PERT calculations
- `src/core/classes.lisp` - Add estimate slots
- `tests/scheduling/test-pert.lisp` - PERT tests

---

## Phase 17: What-If Scenarios

**Priority:** Medium
**Estimated Tests:** 15-20

### Features
1. **Scenario Management**
   - Create named scenarios
   - Clone project state into scenario
   - Compare scenarios side-by-side

2. **Scenario Operations**
   - Modify task durations/effort
   - Change resource allocations
   - Add/remove dependencies
   - Adjust resource availability

3. **Scenario Comparison**
   - Duration comparison
   - Cost comparison
   - Critical path comparison
   - Resource utilization comparison

### DSL Syntax
```lisp
(defscenario "optimistic"
  :based-on :plan
  :changes ((task1 :duration (duration 3 :days))
            (task2 :effort (duration 10 :days))))

(compare-scenarios :plan :optimistic :columns (:duration :cost :end-date))
```

### Implementation Files
- `src/scenarios/scenario.lisp` - Scenario management
- `src/scenarios/comparison.lisp` - Scenario comparison
- `tests/scenarios/test-scenarios.lisp` - Scenario tests

---

## Phase 18: Risk Register

**Priority:** Medium
**Estimated Tests:** 15-20

### Features
1. **Risk Definition**
   - Risk ID and description
   - Category (technical, resource, schedule, cost, external)
   - Probability (1-5 or percentage)
   - Impact (1-5 or cost/duration)
   - Risk score = Probability × Impact

2. **Risk Response**
   - Response strategy (avoid, mitigate, transfer, accept)
   - Mitigation actions
   - Contingency plan
   - Owner (resource)

3. **Risk Tracking**
   - Status (identified, analyzing, mitigating, closed)
   - Trigger conditions
   - Actual impact (if occurred)

4. **Risk Reports**
   - Risk matrix (probability vs impact)
   - Top risks by score
   - Risks by category

### DSL Syntax
```lisp
(defrisk sql-library "SQL Library Compatibility"
  :category :technical
  :probability 0.3
  :impact (duration 5 :days)
  :response :mitigate
  :mitigation "Contact vendor for compatibility patch"
  :owner dev1
  :affects (database backend))
```

### Implementation Files
- `src/risk/risk.lisp` - Risk class and functions
- `src/risk/reports.lisp` - Risk reports
- `tests/risk/test-risk.lisp` - Risk tests

---

## Phase 19: Task Constraints & Recurring Tasks

**Priority:** Low-Medium
**Estimated Tests:** 15-20

### Features
1. **Task Constraints**
   - Must Start On (MSO)
   - Must Finish On (MFO)
   - Start No Earlier Than (SNET)
   - Start No Later Than (SNLT)
   - Finish No Earlier Than (FNET)
   - Finish No Later Than (FNLT)
   - As Soon As Possible (ASAP) - default
   - As Late As Possible (ALAP)

2. **Recurring Tasks**
   - Daily, weekly, monthly, yearly
   - Specific days (every Monday)
   - N occurrences or until date

3. **Constraint Validation**
   - Warn on conflicts
   - Priority resolution

### DSL Syntax
```lisp
(deftask milestone "Milestone"
  :constraint (:must-finish-on (date 2024 6 30)))

(deftask weekly-meeting "Weekly Status Meeting"
  :recurring (:weekly :days (:monday)
              :duration (duration 1 :hours)
              :until (date 2024 12 31)))
```

### Implementation Files
- `src/scheduling/constraints.lisp` - Constraint handling
- `src/scheduling/recurring.lisp` - Recurring task expansion
- `tests/scheduling/test-constraints.lisp` - Constraint tests

---

## Implementation Order

1. **Phase 13: Cost Tracking** - Builds on existing EVM, high value
2. **Phase 14: Dependency Types** - Core scheduling improvement
3. **Phase 15: Resource Availability** - Realistic scheduling
4. **Phase 16: PERT Estimation** - Risk-aware estimation
5. **Phase 17: What-If Scenarios** - Planning flexibility
6. **Phase 18: Risk Register** - Project risk management
7. **Phase 19: Constraints & Recurring** - Advanced scheduling

## Success Criteria

- All tests pass (TDD methodology)
- Backward compatible with existing projects
- Documentation updated
- Example files updated

---

*Created: 2024-11-25*
