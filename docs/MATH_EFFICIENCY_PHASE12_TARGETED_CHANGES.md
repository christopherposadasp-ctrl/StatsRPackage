# Phase 12 Targeted Robustness Fix And Narrow Simplification Review

Baseline date: 2026-05-08

Branch: `codex/code-efficiency-audit`

Starting commit: `033a2a9` (`Record runtime profiling findings`)

Fix commit: `fba0b21` (`Fix Poisson GOF sparse class combining`)

Package version: `1.1.0`

Exported function count: `29`

Published release tag checked and not altered: `1.1.0` at `e64d2f70390febdea21146bc868b4c6934b211ac`

## Summary

Phase 12 addressed the Phase 11 blocker in
`chisq_gof_dist(..., dist = "pois", estimate = TRUE)`. For high-lambda raw
Poisson samples, the old right-tail combiner could move the cutoff down until
the fitted distribution collapsed into too few classes, producing nonpositive
degrees of freedom after estimating `lambda`.

The fix is limited to `StatsPackage/R/chisq-helpers.R.R` and is covered by
a focused regression test in `StatsPackage/tests/testthat/test-chisq-functions.R`.
No exported functions, signatures, defaults, result classes, printed-output
templates, generated docs, README, vignette, cheat sheet, release tags, or
published release artifacts were changed.

## Poisson GOF Fix

Changed helper: `.combine_pois_right_tail()`

Previous behavior:

- Built a Poisson right-tail class.
- Continued moving the right-tail cutoff downward until the right tail and every
  individual left-side class met `min_expected`.
- For sparse high-lambda samples, this could collapse almost all fitted mass
  into `0` and `1+`, leaving too few classes for `df = classes - 1 - params`.

New behavior:

- Moves the right-tail cutoff downward only until the right-tail expected count
  is acceptable.
- Leaves the low-count left-tail cells as separate candidate classes.
- Reuses `.combine_sparse_adjacent()` to group sparse adjacent classes from
  left to right.
- Preserves the existing output shape: `observed`, `expected`, `labels`,
  `group_map`, `tail_start`, `combined`, and `all_expected_ok`.
- Uses `stats::ppois(..., lower.tail = FALSE)` for explicit upper-tail
  probability calculation.

This is a correctness and robustness change, not a public-interface change.
The production helper became shorter overall while preserving the existing
class-combining contract.

## Regression Coverage

Added one targeted regression test:

- Reproduces the Phase 11 blocker with
  `set.seed(1101); x <- rpois(50000, lambda = 12)`.
- Verifies `chisq_gof_dist(x = x, dist = "pois", estimate = TRUE, quiet = TRUE)`
  no longer errors.
- Verifies `params_estimated_n == 1`, `df > 0`, positive expected counts,
  observed and expected totals, a retained right-tail `+` label, and
  `chi_stat == sum(contrib)`.

Existing fixed-parameter and estimated-lambda Poisson tests continue to pass.

## `power_p_z()` Simplification Review

`power_p_z()` two-sample branches were reviewed after the chi-square fix.
No production change was made.

Reason:

- The reviewed duplication is mostly branch-local contract handling for pooled,
  unpooled, nonzero-null, continuity-corrected, warning, return-field, and
  printed-output behavior.
- A helper extraction would likely increase production code length or risk
  public-output drift.
- Phase 11 timing did not show a runtime gain that justifies changing this
  code path.

This area should stay unchanged unless a later defect or substantially clearer
net-short simplification is identified.

## QA Results

Targeted chi-square tests:

- Command: `devtools::test("StatsPackage", filter = "chisq")`
- Package output: `FAIL 0 | WARN 0 | SKIP 0 | PASS 99`
- Local process status: nonzero Rscript exit after clean output, matching the
  known local Rscript / `Rcmd.exe` anomaly.

Full test suite:

- Command: `devtools::test("StatsPackage")`
- Package output: `FAIL 0 | WARN 0 | SKIP 0 | PASS 636`
- Local process status: nonzero Rscript exit after clean output, matching the
  known local Rscript / `Rcmd.exe` anomaly.

Cheat-sheet audit:

- Command: `source("scripts/qa_cheatsheet_audit.R")`
- Package output: `Issues found: 0`
- Local process status: nonzero Rscript exit after clean output, matching the
  known local Rscript / `Rcmd.exe` anomaly.

Static diff check:

- Command: `git diff --check`
- Result: passed, with only existing line-ending warnings for files touched on
  Windows.

## Public Contract Status

Preserved:

- `29` exported functions.
- All exported function names, signatures, defaults, and return classes.
- `chisq_gof_dist()` result-field names and printed-output contract.
- Generated `.Rd` docs, README, vignette, cheat sheet, and release tag `1.1.0`.

No documentation update is required because the public API and documented
behavior are unchanged; the fix only prevents an invalid sparse-class collapse
in an existing supported Poisson GOF path.

## Recommendation

Proceed to Phase 13 closeout from the current branch after confirming the
working tree is clean. Phase 13 should summarize the code-efficiency audit,
repeat final API/export checks, rerun final QA if desired, and prepare the
branch for review.
