# Monte Carlo PERT Simulation

Project Juggler includes a Monte Carlo simulation engine for quantitative schedule risk analysis. This document covers the simulation capabilities, API, and best practices.

## Overview

Traditional project scheduling uses single-point estimates for task durations. In reality, task durations are uncertain. Monte Carlo simulation addresses this by:

1. **PERT Estimation**: Using three-point estimates (optimistic, likely, pessimistic)
2. **Probabilistic Sampling**: Generating thousands of random scenarios using Beta-PERT distribution
3. **Statistical Analysis**: Calculating confidence intervals and completion probabilities
4. **Risk Integration**: Modeling discrete risk events that can impact schedules

## Quick Start

```lisp
;; Define a project with PERT estimates
(defproject my-project "Project with Uncertainty"
  :start (date 2024 6 1)

  (defresource dev "Developer")

  ;; Task with three-point estimate (plist format)
  (deftask development "Development Phase"
    :estimate (:optimistic (duration 10 :days)
               :likely (duration 15 :days)
               :pessimistic (duration 30 :days))
    :allocate (dev)))

(finalize-project *current-project*)
(schedule *current-project*)

;; Run Monte Carlo simulation
(let ((results (run-monte-carlo-simulation *current-project* :trials 10000)))
  (format t "P90 Duration: ~,1F days~%" (simulation-percentile results 90)))
```

## PERT Three-Point Estimation

PERT (Program Evaluation and Review Technique) uses three duration estimates:

- **Optimistic (O)**: Best-case scenario, everything goes right
- **Most Likely (M)**: Most probable duration based on experience
- **Pessimistic (P)**: Worst-case scenario, significant problems occur

### Creating PERT Estimates

```lisp
;; Using the estimate plist in task definition
(deftask my-task "Task Name"
  :estimate (:optimistic (duration 5 :days)
             :likely (duration 8 :days)
             :pessimistic (duration 15 :days)))

;; The task-estimate accessor retrieves the PERT estimate
(let ((estimate (task-estimate my-task)))
  (estimate-optimistic estimate)   ; => duration object
  (estimate-likely estimate)       ; => duration object
  (estimate-pessimistic estimate)) ; => duration object
```

### PERT Statistics Functions

```lisp
(let ((estimate (task-estimate task)))
  ;; Expected (mean) duration using PERT formula: (O + 4M + P) / 6
  (pert-expected-duration estimate)  ; => days as float

  ;; Standard deviation: (P - O) / 6
  (pert-standard-deviation estimate) ; => days as float

  ;; Variance: ((P - O) / 6)^2
  (pert-variance estimate)           ; => days^2 as float

  ;; Confidence interval at given confidence level
  (pert-confidence-interval estimate 0.95)) ; => (low . high)
```

### Project-Level PERT Analysis

```lisp
;; Sum of expected durations across critical path
(project-pert-expected-duration *current-project*)

;; Combined variance (sum of task variances)
(project-pert-variance *current-project*)

;; Project standard deviation
(project-pert-standard-deviation *current-project*)

;; Probability of completing by a target date
(project-probability-of-completion-by *current-project* target-date)
```

## Monte Carlo Simulation

Monte Carlo simulation runs thousands of independent trials, sampling random durations from the PERT distribution for each task.

### Basic Simulation

```lisp
;; Run simulation with 10,000 trials (default: 1,000)
(let ((results (run-monte-carlo-simulation *current-project* :trials 10000)))

  ;; Access raw statistics
  (simulation-mean results)       ; Mean project duration
  (simulation-std-dev results)    ; Standard deviation
  (simulation-min results)        ; Minimum observed
  (simulation-max results)        ; Maximum observed

  ;; Percentile analysis
  (simulation-percentile results 50)  ; P50 (median)
  (simulation-percentile results 75)  ; P75
  (simulation-percentile results 90)  ; P90 (commonly used for planning)
  (simulation-percentile results 95)) ; P95 (high confidence)
```

### Probability of Completion

```lisp
;; What's the probability of finishing within 60 days?
(simulation-probability-of-completion results 60)  ; => 0.0 to 1.0
```

### Simulation Summary

```lisp
;; Get a comprehensive summary as a plist
(simulation-summary results)
;; => (:TRIAL-COUNT 10000
;;     :MEAN 45.3
;;     :STD-DEV 8.2
;;     :MIN 28.0
;;     :MAX 78.0
;;     :P10 35.0
;;     :P50 44.0
;;     :P75 50.0
;;     :P90 56.0
;;     :P95 61.0)
```

### Histogram Data

```lisp
;; Generate histogram with 10 bins
(simulation-histogram results :bins 10)
;; => ((28.0 33.0 120)    ; bin-min, bin-max, count
;;     (33.0 38.0 890)
;;     (38.0 43.0 2100)
;;     ...)
```

## Risk Integration

Monte Carlo simulation can incorporate discrete risk events from the risk register.

### Defining Risks

```lisp
(create-risk *current-project*
             'scope-creep "Scope Creep"   ; id and name as positional args
             :probability 0.4             ; 40% chance of occurring
             :impact 0.8                  ; Impact severity (0.0-1.0)
             :tasks '(dev-task1 dev-task2)  ; Affected tasks
             :schedule-impact 0.3)        ; Adds 30% to affected task durations
```

### Running Risk-Aware Simulation

```lisp
(let ((results (run-risk-simulation *current-project* :trials 10000)))

  ;; Standard statistics are available
  (simulation-mean results)
  (simulation-percentile results 90)

  ;; Risk occurrence tracking
  (simulation-risk-occurrences results 'scope-creep)  ; Count of occurrences

  ;; Access risk counts hash table
  (simulation-risk-occurrence-counts results))  ; => hash table
```

### Comparing With and Without Risks

```lisp
(let ((base-results (run-monte-carlo-simulation *current-project*))
      (risk-results (run-risk-simulation *current-project*)))

  (format t "Without risks - P90: ~,1F days~%"
          (simulation-percentile base-results 90))
  (format t "With risks    - P90: ~,1F days~%"
          (simulation-percentile risk-results 90))
  (format t "Risk buffer needed: ~,1F days~%"
          (- (simulation-percentile risk-results 90)
             (simulation-percentile base-results 90))))
```

## Accessing Trial Data

For advanced analysis, you can access individual trial results:

```lisp
(let ((results (run-monte-carlo-simulation *current-project*)))

  ;; List of all trials
  (simulation-trials results)

  ;; Individual trial data
  (dolist (trial (simulation-trials results))
    (trial-number trial)           ; Trial sequence number
    (trial-project-duration trial) ; Total project duration
    (trial-end-date trial)         ; Projected end date

    ;; Task-specific data from this trial
    (trial-task-durations trial)   ; Hash: task-id -> sampled duration
    (trial-task-end-dates trial)   ; Hash: task-id -> calculated end date

    ;; Risk occurrences (only for risk simulations)
    (trial-risk-occurrences trial))) ; List of risk IDs that occurred
```

## Sampling Functions

The simulation uses Beta-PERT distribution for realistic task duration sampling:

```lisp
;; Sample a random duration for a single task
(sample-pert-duration task)  ; => duration in days (float)
```

### Distribution Implementation

The Beta-PERT distribution provides:
- More weight around the most likely value than triangular distribution
- Bounded between optimistic and pessimistic values
- Shape controlled by lambda parameter (default: 4)

For edge cases or numerical stability issues, the implementation falls back to triangular distribution.

## Best Practices

### Choosing Number of Trials

- **Quick estimates**: 1,000 trials
- **Standard analysis**: 5,000-10,000 trials
- **High precision**: 50,000+ trials

More trials increase precision but take longer. For most purposes, 10,000 trials provide stable percentile estimates.

### Interpreting Results

| Percentile | Use Case |
|------------|----------|
| P50 (median) | Expected outcome, aggressive planning |
| P75 | Reasonable buffer, typical planning |
| P90 | Conservative estimate, external commitments |
| P95 | High-confidence deadline, contractual obligations |

### PERT Estimation Guidelines

1. **Optimistic**: Don't assume everything goes perfectly, but assume no major problems
2. **Most Likely**: Use historical data and expert judgment
3. **Pessimistic**: Include realistic worst cases, not catastrophic scenarios
4. **Ratio Check**: Pessimistic is typically 2-4x the optimistic value

### Risk Modeling

1. Start with probability estimates from risk register
2. Use schedule impact as percentage (0.3 = 30% duration increase)
3. Run simulations with and without risks to quantify risk exposure
4. Focus risk mitigation on risks that significantly impact P90

## Example Output

```
Duration Statistics (with risk events):
  Minimum:     32.0 days
  Maximum:     98.0 days
  Mean:        52.3 days
  Std Dev:     12.4 days

Percentile Analysis (with risks):
  P10 (optimistic):  38.0 days
  P50 (median):      51.0 days
  P75:               59.0 days
  P90 (conservative):68.0 days
  P95 (high conf):   74.0 days

Risk Occurrence Frequency:
  Scope Creep: 39.8% of trials (3980 occurrences)
  Integration Issues: 30.2% of trials (3020 occurrences)
  Critical Defects: 25.1% of trials (2510 occurrences)
```

## API Reference

### Classes

- `simulation-trial` - Results from a single simulation run
- `simulation-results` - Aggregate results from all trials

### Core Functions

| Function | Description |
|----------|-------------|
| `run-monte-carlo-simulation` | Run simulation with PERT uncertainty |
| `run-risk-simulation` | Run simulation with PERT + risk events |
| `run-simulation-trial` | Execute a single trial |
| `simulation-percentile` | Get specific percentile value |
| `simulation-probability-of-completion` | Probability of meeting target |
| `simulation-histogram` | Generate histogram data |
| `simulation-summary` | Get comprehensive statistics plist |
| `sample-pert-duration` | Sample random task duration |

### PERT Functions

| Function | Description |
|----------|-------------|
| `pert-expected-duration` | PERT weighted mean |
| `pert-standard-deviation` | PERT standard deviation |
| `pert-variance` | PERT variance |
| `pert-confidence-interval` | Confidence bounds |

## See Also

- [examples/monte-carlo-example.lisp](examples/monte-carlo-example.lisp) - Complete working example
- [RISK.md](RISK.md) - Risk register documentation (if available)
- [PERT.md](PERT.md) - Detailed PERT estimation guide (if available)
