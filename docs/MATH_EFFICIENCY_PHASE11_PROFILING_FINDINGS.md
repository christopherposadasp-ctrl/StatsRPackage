# Phase 11 Runtime Profiling And Hotspot Triage Findings

Baseline date: 2026-05-08

Branch: `codex/code-efficiency-audit`

Starting commit: `e3899f6` (`Record efficiency baseline inventory`)

Package version: `1.1.0`

Exported function count: `29`

Published release tag checked and not altered: `1.1.0` at `e64d2f70390febdea21146bc868b4c6934b211ac`

## Summary

Phase 11 profiled the Phase 10 candidate hotspots using temporary local R
scripts outside the repository. No production code, tests, generated docs,
README, vignette, cheat sheet, release tags, or public APIs were changed.

The measured workloads do not support broad efficiency refactoring. Most
representative calls are fast enough that helper extraction would mainly create
behavior and printed-output risk. The only substantive finding is a
non-efficiency blocker in an exploratory Poisson goodness-of-fit workload:
`chisq_gof_dist(..., dist = "pois", estimate = TRUE)` can collapse classes so
far that degrees of freedom become nonpositive for a high-lambda raw sample.

## Profiling Method

Profiling used:

- `devtools::load_all("StatsPackage", quiet = TRUE)`
- package calls with `quiet = TRUE`
- base R `system.time()`
- attempted base R `Rprof()` and `summaryRprof()`
- temporary scripts and profiler files under the local temp directory only

Local R version:

- `R version 4.6.0 (2026-04-24 ucrt)`

Local process note: the profiling commands returned a nonzero Rscript process
status after printing complete output. This matches the local Rscript /
`Rcmd.exe` anomaly recorded in earlier phases and is not counted as a package
failure.

`Rprof()` limitation: `Rprof()` was invoked for the slowest required-n, power,
and chi-square workloads, then checked with a calibration loop. In this local
Rscript environment it wrote only an empty header and `summaryRprof()` reported
zero sampling time. Therefore Phase 11 uses `system.time()` results and static
code evidence for triage instead of call-stack samples.

## Required-N Workloads

Single-call timing pass:

| Workload | Elapsed | Result |
| --- | ---: | --- |
| `n_required_t_mu()` one-sample high power | 0.1600s | `n = 15201`, power `0.9800`, `n_evaluations = 28` |
| `n_required_t_mu()` Welch, `n_ratio = 1.7` | 0.1300s | `n1 = 3286`, `n2 = 5587`, power `0.9500`, `n_evaluations = 24` |
| `n_required_p_z()` two-sample continuity | 0.1100s | `n1 = 1188`, `n2 = 1664`, power `0.9502`, `n_evaluations = 19` |
| `n_required_var_ratio_F()` non-1 ratio | 0.0500s | `n1 = 80`, `n2 = 128`, power `0.9515`, `n_evaluations = 14` |
| `n_required_z_mu()` one-sided closed form | 0.0600s | `n = 701`, power `0.9500`, `n_evaluations = NA` |

Repeated warm timing pass:

| Workload | Iterations | Total elapsed | Average elapsed |
| --- | ---: | ---: | ---: |
| `n_required_t_mu()` one-sample high power | 5 | 0.3700s | 0.0740s |
| `n_required_t_mu()` Welch, `n_ratio = 1.7` | 5 | 0.0100s | 0.0020s |
| `n_required_p_z()` two-sample continuity | 5 | 0.2700s | 0.0540s |
| `n_required_var_ratio_F()` non-1 ratio | 5 | 0.1000s | 0.0200s |
| `n_required_z_mu()` one-sided closed form | 5 | 0.2600s | 0.0520s |

Triage:

- `.nreq_find_min_n()` search cost is not a runtime hotspot for these
  representative workloads.
- The number of power evaluations is already low for the searched examples.
- The final achieved-power calls are not expensive enough to justify changing
  `n_evaluations`, minimality, or returned-field semantics.
- No required-n efficiency implementation is approved from Phase 11 timing
  evidence alone.

## Repeated Power Workloads

Each workload ran 2,000 calls with `quiet = TRUE`.

| Workload | Elapsed | Last power |
| --- | ---: | ---: |
| `power_t_mu()` one-sample | 0.2000s | 0.2892 |
| `power_t_mu()` paired | 0.2200s | 0.7030 |
| `power_t_mu()` Welch | 0.2300s | 0.4279 |
| `power_t_mu()` pooled | 0.2300s | 0.3454 |
| `power_p_z()` one-sample | 0.4300s | 0.7008 |
| `power_p_z()` pooled two-sample | 0.6100s | 0.9172 |
| `power_p_z()` unpooled two-sample | 0.7200s | 0.9991 |
| `power_p_z()` continuity-corrected two-sample | 0.6300s | 0.9096 |

Triage:

- `power_p_z()` is the slowest repeated power family in this workload set.
- Even the slowest case is about 0.36 ms per call locally, so broad helper
  extraction is unlikely to produce meaningful user-visible speedup.
- The two-sample proportion branches remain candidates only if Phase 12 can
  reduce code length or duplicate logic without changing defaults, warnings,
  fields, or printed output.

## Chi-Square GOF Workloads

Single-call timing pass:

| Workload | Status | Elapsed | Result |
| --- | --- | ---: | --- |
| raw normal, `n = 50000`, `k = 40`, estimated params | ok | 0.0000s | `df = 37`, classes `40`, combined `FALSE` |
| raw exponential, `n = 50000`, `k = 40`, estimated params | ok | 0.1800s | `df = 38`, classes `40`, combined `FALSE` |
| raw uniform, `n = 50000`, `k = 40`, estimated params | ok | 0.0200s | `df = 37`, classes `40`, combined `FALSE` |
| grouped sparse normal | ok | 0.0000s | `df = 31`, classes `32`, combined `TRUE` |
| raw Poisson, fixed `lambda = 12` | ok | 0.0000s | `df = 1`, classes `2`, combined `TRUE` |
| raw Poisson, estimated lambda exploratory variant | error | 0.0300s | `df <= 0 after adjustment` |

Repeated timing pass:

| Workload | Iterations | Total elapsed | Average elapsed |
| --- | ---: | ---: | ---: |
| raw normal, `n = 50000`, `k = 40`, estimated params | 20 | 0.2600s | 0.0130s |
| raw exponential, `n = 50000`, `k = 40`, estimated params | 20 | 0.0800s | 0.0040s |
| raw uniform, `n = 50000`, `k = 40`, estimated params | 20 | 0.0900s | 0.0045s |
| grouped sparse normal | 20 | 0.1300s | 0.0065s |
| raw Poisson, fixed `lambda = 12` | 20 | 0.1300s | 0.0065s |

Triage:

- Continuous raw-data GOF and sparse class combining are not runtime hotspots
  in the representative workloads.
- The Poisson path deserves correctness attention before efficiency work:
  estimating lambda for a high-lambda raw sample can reduce the combined class
  count enough that `df <= 0`.
- Phase 12 should not optimize chi-square combining until this behavior is
  classified. A fix, if made, must preserve labels, `group_map`, expected
  counts, warnings, `combined`, `params_estimated_n`, and df policy.

## Result Construction Workloads

Each workload ran 2,000 calls with `quiet = TRUE`.

| Workload | Elapsed |
| --- | ---: |
| `z_test_mu()` | 0.4400s |
| `t_test_mu()` | 0.4700s |
| `power_z_mu()` | 0.3900s |
| `power_t_mu()` | 0.2500s |

Triage:

- Classed result construction and validation are measurable in high-volume
  loops, but the absolute timings are small.
- Because result fields and classes are public contracts, result construction
  should not be refactored solely for speed.
- Printed-output paths were not profiled because Phase 11 targets computational
  calls and printed text is intentionally stable.

## Candidates Approved For Phase 12

Approved for Phase 12 investigation:

1. `chisq_gof_dist()` Poisson class combining with estimated lambda when df
   collapses to zero or below. This is a correctness/robustness blocker found
   during profiling, not a speed optimization.
2. A narrowly scoped review of `power_p_z()` two-sample branches for a possible
   local simplification only if it reduces production code length and preserves
   all fields, warnings, defaults, and printed output.

No required-n search optimization is approved from Phase 11 evidence.

## Candidates Rejected

Rejected for Phase 12 unless new evidence appears:

- broad extraction of print helpers;
- broad extraction of result-construction helpers;
- changing `.nreq_find_min_n()` caching, bracketing, binary search, or
  previous-`n` minimality logic;
- removing final achieved-power checks from required-n wrappers;
- optimizing continuous GOF binning or sparse combining for speed alone;
- adding benchmarking dependencies.

## Phase 11 Conclusion

Phase 11 found no general runtime hotspot that justifies broad refactoring.
The package is fast for the representative introductory-statistics workloads
measured locally.

The next phase should first handle or explicitly defer the Poisson estimated
GOF df-collapse blocker. After that, Phase 12 should make no production changes
unless a candidate reduces code length or fixes the confirmed robustness issue
without public-contract drift.
