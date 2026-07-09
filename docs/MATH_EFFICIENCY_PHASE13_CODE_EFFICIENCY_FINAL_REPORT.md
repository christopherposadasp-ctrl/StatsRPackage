# Phase 13 Code Efficiency Audit Final Report

Final closeout date: 2026-05-08

Branch: `codex/code-efficiency-audit`

Baseline commit: `e240ae9` (`Merge pull request #13 from codex/update-audit-plan-efficiency-roadmap`)

Phase 10-12 change range before this report: `e240ae9..646b302`

Package version: `1.1.0`

Exported function count: `29`

Published release tag checked and not altered: `1.1.0` at `e64d2f70390febdea21146bc868b4c6934b211ac`

## Summary

The deliberate code-efficiency audit is complete through Phase 13 closeout.

The audit did not support broad production refactoring. Phase 10 static review
and Phase 11 runtime profiling found that most candidate refactors would be
cosmetic, would increase code length, or would risk public-output drift without
meaningful runtime gain.

One production change was made during Phase 12 because profiling exposed a
correctness and robustness blocker, not a cosmetic efficiency issue:
`chisq_gof_dist(..., dist = "pois", estimate = TRUE)` could collapse sparse
Poisson classes until the fitted test had nonpositive degrees of freedom for a
high-lambda raw sample. The helper now builds the right-tail class by tail
expected count and then combines sparse adjacent classes from left to right.

No exported functions, signatures, defaults, result fields, result classes,
printed-output templates, generated docs, README, vignette, cheat sheet, or
release tags changed.

## Completed Phases

- Phase 10: recorded the efficiency baseline, baseline QA, static inventory,
  candidate hotspots, and rejected cosmetic-only refactors.
- Phase 11: profiled required-n, repeated power, chi-square GOF, and classed
  result construction workloads with base R timing tools.
- Phase 12: fixed the high-lambda estimated Poisson GOF sparse-combining
  blocker, added a regression test, and rejected `power_p_z()` helper
  extraction.
- Phase 13: reran final QA, confirmed public-contract status, and recorded this
  closeout report.

## Files Reviewed

Primary static and profiling targets:

- `StatsPackage/R/n-required-functions.R`
- `StatsPackage/R/power-functions.R`
- `StatsPackage/R/hypothesis-test-functions.R`
- `StatsPackage/R/chisq-functions.R`

Supporting files reviewed as needed:

- `StatsPackage/R/chisq-helpers.R.R`
- `StatsPackage/R/power-helpers.R`
- `StatsPackage/tests/testthat/test-chisq-functions.R`

## Production Change

Commit: `fba0b21` (`Fix Poisson GOF sparse class combining`)

Changed helper: `.combine_pois_right_tail()` in
`StatsPackage/R/chisq-helpers.R.R`

What changed:

- The right-tail cutoff now moves downward only until the right-tail expected
  count is acceptable.
- Low-count left-tail cells are passed through existing adjacent sparse-class
  combining instead of forcing each individual left-side cell to satisfy
  `min_expected`.
- The existing helper output shape is preserved: `observed`, `expected`,
  `labels`, `group_map`, `tail_start`, `combined`, and `all_expected_ok`.
- Upper-tail Poisson probability uses `stats::ppois(..., lower.tail = FALSE)`.

Regression coverage added:

- `set.seed(1101); x <- rpois(50000, lambda = 12)`
- `chisq_gof_dist(x = x, dist = "pois", estimate = TRUE, quiet = TRUE)` no
  longer errors.
- The test verifies `params_estimated_n == 1`, `df > 0`, positive expected
  counts, preserved totals, a retained right-tail `+` label, and
  `chi_stat == sum(contrib)`.

Production-code size impact: the helper became shorter overall while adding the
targeted regression test.

## Rejected Changes

Broad helper extraction was rejected for:

- repeated result construction;
- repeated printed-output construction;
- repeated tail-selection code in power and hypothesis functions;
- required-n solver changes that could affect minimality or `n_evaluations`.

Specific `power_p_z()` simplification review:

- No production change was made.
- The duplicated-looking two-sample branches encode pooled, unpooled,
  nonzero-null, continuity-corrected, warning, return-field, and printed-output
  behavior.
- A helper extraction would likely increase production code length or risk
  output drift.
- Phase 11 timing did not show enough runtime benefit to justify that risk.

## Profiling Evidence

Phase 11 used `system.time()` and attempted `Rprof()` / `summaryRprof()` in
temporary local scripts. `Rprof()` produced no usable samples in this local
Rscript environment, so triage relied on elapsed timings and static evidence.

Representative elapsed timings:

- Required-n workloads were generally small: examples ranged from about
  `0.05s` to `0.16s` for single calls.
- Repeated power workloads were fast enough that branch-local refactoring would
  not be justified by runtime.
- Chi-square raw GOF workloads were acceptable except for the estimated Poisson
  sparse-combining blocker, which was corrected in Phase 12.

Conclusion: no broad efficiency refactor is justified by the measured evidence.

## Final QA Results

Final full test suite:

```r
devtools::test("StatsPackage")
```

Reported result:

- `FAIL 0 | WARN 0 | SKIP 0 | PASS 636`

Final cheat-sheet regression:

```powershell
Rscript scripts/qa_cheatsheet_audit.R
```

Reported result:

- `Issues found: 0`

Local process note: both successful Rscript commands returned a nonzero process
status after reporting clean package output. This matches the local Rscript /
`Rcmd.exe` anomaly recorded throughout the audit and remains non-blocking until
confirmed by CI or another clean R environment.

## Public Contract Status

Preserved:

- `29` exported functions.
- Exported function names, signatures, defaults, result fields, and result
  classes.
- Printed-output templates.
- Generated `.Rd` docs, README, getting-started vignette, cheat sheet, and
  release tag `1.1.0`.

No user-facing documentation updates are required because the public interface
and documented behavior are unchanged.

## Final Recommendation

The efficiency audit branch is ready for PR review. Merge only after GitHub
Actions confirm clean `CI`, `Coverage`, and `Lint`, including clean
`devtools::check()` / `R CMD check` on the existing matrix.
