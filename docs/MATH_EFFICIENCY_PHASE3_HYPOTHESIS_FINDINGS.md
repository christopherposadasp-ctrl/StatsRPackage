# Phase 3 Findings: Hypothesis-Test Families

Date: 2026-05-08

Branch: `codex/math-efficiency-audit`

Starting point: `369458e` (`Record confidence and width audit findings`)

## Summary

The hypothesis-test families were audited after the completed Phase 2
confidence, prediction, and width audit.

No mathematical defects were found in production code. No package R source
files, generated documentation, public API signatures, release tags, or
user-facing docs were changed.

Focused tests were added for under-covered critical-value, estimate-critical,
tail, continuity-correction, and variance-ratio-null formulas.

## Mean-Test Checks

Independent hand-calculation checks confirmed:

- `z_test_mu()` one-sample and two-sample standard errors, statistics,
  p-values, critical values, and estimate-critical boundaries;
- scalar `sigma` recycling in the two-sample z branch;
- `t_test_mu()` one-sample, paired, Welch, and pooled standard errors,
  statistics, p-values, critical values, degrees of freedom, and
  estimate-critical boundaries;
- one-sided less/greater alternatives use the correct tail and critical
  boundary;
- two-sided mean-test rejection regions use symmetric absolute-value statistic
  regions with equivalent estimate boundaries.

The independent mean-test formula audit passed.

## Proportion-Test Checks

Independent checks confirmed:

- one-sample `p_test()` exact p-values and confidence intervals match
  `stats::binom.test()` for `two.sided`, `less`, and `greater` alternatives;
- two-sample `p_test()` defaults to pooled when `p0 = 0`;
- two-sample `p_test()` defaults to unpooled when `p0 != 0`;
- `pooled = TRUE` with nonzero `p0` still errors;
- continuity correction formulas are correct for `less`, `greater`, and
  `two.sided` alternatives;
- continuity-corrected tests store the expected `cc`, statistic, p-value,
  critical value, and estimate-critical boundary;
- normal-approximation adequacy warnings do not alter computations.

The independent proportion-test formula audit passed.

## Variance-Test Checks

Independent checks confirmed:

- one-sample `var_test_chisq()` chi-square statistic, p-values, critical
  values, and rejection regions for `less`, `greater`, and `two.sided`
  alternatives;
- two-sample `var_test_chisq()` F statistic uses `ratio_hat / ratio0`,
  including non-1 `ratio0`;
- two-sided chi-square and F p-values remain tail-based;
- two-sided chi-square and F rejection regions are lower-tail OR upper-tail,
  not absolute-value regions;
- `ratio0` is ignored only in the one-sample branch and `sigma0` is ignored
  only in the two-sample branch.

The independent variance-test formula audit passed.

## Test Changes

Added focused formula-lock tests in
`StatsPackage/tests/testthat/test-hypothesis-tests.R` for:

- `z_test_mu()` critical and estimate-critical boundaries;
- `t_test_mu()` critical and estimate-critical boundaries;
- `p_test()` exact binomial alternatives;
- `p_test()` continuity correction for less and greater alternatives;
- `var_test_chisq()` one-sample tail-specific critical values;
- `var_test_chisq()` two-sample F tests with non-1 `ratio0`.

No production code changes were made.

## Validation

- Independent mean-test formula audit script:
  passed.
- Independent proportion and variance formula audit script:
  passed.
- `devtools::test("StatsPackage", filter = "hypothesis")`:
  128 passed, 0 failed, 0 warnings, 0 skipped.
- `devtools::test("StatsPackage")`:
  511 passed, 0 failed, 0 warnings, 0 skipped.

The known local Windows R process-exit anomaly persisted after the
`devtools::test()` output completed successfully, with exit status
`-1073741569`. This is consistent with earlier audit notes and is not treated
as a hypothesis-test finding.

The cheat-sheet regression was not rerun because this audit did not change
printed output, defaults, return fields, production code, or user-facing
examples.

## Conclusion

The Phase 3 hypothesis-test families are mathematically correct for the audited
1.1.0 scope. The audit can proceed to Phase 4 power families.
