# Monte Carlo PERT Simulation - Implementation Plan

## Overview

Monte Carlo simulation uses random sampling to model uncertainty in project schedules. By running thousands of simulations with randomly sampled task durations (based on PERT estimates), we can generate probability distributions for:

- Project completion dates
- Individual task completion
- Critical path variations
- Risk impact analysis

## Design

### Core Concepts

1. **PERT Distribution Sampling**: For each task with a PERT estimate, sample a random duration using a Beta distribution (or triangular approximation) bounded by optimistic and pessimistic values.

2. **Simulation Trial**: One complete run through the project schedule with sampled durations.

3. **Simulation Results**: Aggregate statistics from multiple trials (mean, std dev, percentiles, histograms).

### Key Functions to Implement

```lisp
;; Random sampling from PERT distribution
(defun sample-pert-duration (task) ...)

;; Run a single simulation trial
(defun run-simulation-trial (project) ...)

;; Run Monte Carlo simulation
(defun run-monte-carlo-simulation (project &key (trials 1000)) ...)

;; Get simulation statistics
(defun simulation-mean (results) ...)
(defun simulation-percentile (results percentile) ...)
(defun simulation-histogram (results &key (bins 20)) ...)

;; Risk-integrated simulation
(defun run-risk-simulation (project &key (trials 1000)) ...)
```

### Data Structures

```lisp
;; Simulation result for a single trial
(defclass simulation-trial ()
  ((trial-number :reader trial-number)
   (project-duration :reader trial-project-duration)
   (task-durations :reader trial-task-durations)   ; hash: task-id -> sampled duration
   (critical-path :reader trial-critical-path)
   (end-date :reader trial-end-date)))

;; Aggregate simulation results
(defclass simulation-results ()
  ((project :reader simulation-project)
   (trials :reader simulation-trials)              ; list of simulation-trial objects
   (trial-count :reader simulation-trial-count)
   (durations :reader simulation-durations)        ; sorted list of project durations
   (mean :reader simulation-mean)
   (std-dev :reader simulation-std-dev)
   (min :reader simulation-min)
   (max :reader simulation-max)))
```

### PERT Distribution Sampling

The Beta-PERT distribution is ideal for project estimation:
- Bounded by optimistic (a) and pessimistic (b)
- Mode at most likely (m)
- Can be approximated using Beta distribution with shape parameters

For simplicity, we can use a triangular distribution or Beta-PERT:

**Triangular Distribution** (simpler):
```
Generate u ~ Uniform(0,1)
F_c = (m - a) / (b - a)
if u < F_c:
    x = a + sqrt(u * (b - a) * (m - a))
else:
    x = b - sqrt((1 - u) * (b - a) * (b - m))
```

**Beta-PERT** (more accurate):
```
α = 1 + 4 * (m - a) / (b - a)
β = 1 + 4 * (b - m) / (b - a)
Sample y ~ Beta(α, β)
x = a + y * (b - a)
```

### Simulation Algorithm

```
FOR trial = 1 to N:
    FOR each task in topological order:
        IF task has PERT estimate:
            sampled_duration = sample_pert_duration(task)
        ELSE:
            sampled_duration = task.duration or task.effort

        Calculate task start based on dependencies
        Calculate task end = start + sampled_duration

    Record project end date
    Record critical path (may vary per trial!)

Calculate aggregate statistics from all trials
```

### Risk Integration

Optionally incorporate risks:
- For each risk with P > random(), add schedule/cost impact
- Track how often each risk "occurs" across trials

## Test Plan (TDD)

### Test File: tests/risk/test-simulation.lisp

1. **Basic Sampling Tests**
   - `test-sample-pert-duration-bounds` - Sampled values within [O, P]
   - `test-sample-pert-duration-distribution` - Mean approximates expected value

2. **Single Trial Tests**
   - `test-run-single-trial` - Returns valid trial result
   - `test-trial-respects-dependencies` - Task order maintained

3. **Monte Carlo Tests**
   - `test-monte-carlo-basic` - Runs N trials successfully
   - `test-monte-carlo-statistics` - Mean/std dev calculated correctly
   - `test-monte-carlo-percentiles` - P10, P50, P90 ordered correctly
   - `test-monte-carlo-histogram` - Histogram bins sum to trial count

4. **Convergence Tests**
   - `test-monte-carlo-convergence` - More trials -> stable results

5. **Risk Integration Tests**
   - `test-risk-simulation` - Risks affect project duration
   - `test-risk-occurrence-tracking` - Risk frequency tracked

6. **Edge Cases**
   - `test-simulation-no-pert-tasks` - Handles tasks without estimates
   - `test-simulation-single-task` - Works with minimal project
   - `test-simulation-deterministic` - Same results when no uncertainty

## Implementation Steps

1. Write test file with all test cases (failing)
2. Implement `sample-pert-duration` using triangular distribution
3. Implement `simulation-trial` class and `run-simulation-trial`
4. Implement `simulation-results` class and `run-monte-carlo-simulation`
5. Implement statistics functions (mean, percentile, histogram)
6. Implement risk-integrated simulation
7. Add package exports
8. Update ASDF and run-tests.lisp
9. Create example file
10. Update documentation

## Files to Create/Modify

### New Files
- `tests/risk/test-simulation.lisp` - Test suite
- `examples/monte-carlo-example.lisp` - Usage example
- `SIMULATION.md` - Documentation

### Modified Files
- `src/risk/simulation.lisp` - Implementation (replace stub)
- `src/package.lisp` - Add exports
- `project-juggler.asd` - Add test module
- `run-tests.lisp` - Add simulation-suite
- `README.md` - Update features section
- `examples/README.md` - Add new example

## API Design

```lisp
;; Basic usage
(run-monte-carlo-simulation project :trials 1000)
;; => simulation-results object

;; Get statistics
(simulation-mean results)           ; => 45.3 (days)
(simulation-std-dev results)        ; => 5.2 (days)
(simulation-percentile results 90)  ; => 52.1 (days) - P90

;; Probability queries
(simulation-probability-of-completion results 50)  ; => 0.72 (72% chance of completing in 50 days)

;; Histogram for visualization
(simulation-histogram results :bins 10)
;; => ((40 45 23) (45 50 156) (50 55 312) ...)  ; (min max count)

;; Risk-aware simulation
(run-risk-simulation project :trials 1000)
;; => simulation-results with risk tracking
```
